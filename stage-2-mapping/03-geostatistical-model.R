## #######################################################################################
##
## 03) GEOSTATISTICAL MAPPING WORKFLOW
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 10 June 2024
## PURPOSE: MBG workflow using packages `mbg`, `versioning`, and `pixel2poly`
##
## #######################################################################################

## 00) SETTINGS ------------------------------------------------------------------------->

## Load multi-run settings
MBG_PATH <- '~/repos/mbg/'
REPO_PATH <- '~/repos/usaid-mch-ml/r-package/'
CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'
VERSION_BASE <- '20240618'

# Load standard packages
load_packages <- c(
  'assertthat', 'data.table', 'glue', 'sf', 'terra', 'tictoc', 'versioning', 'fedmatch',
  'ggplot2', 'rdhs', 'scales', 'RColorBrewer', 'viridisLite', 'pixel2poly'
)
lapply(load_packages, library, character.only = T) |> invisible() |> 
  suppressPackageStartupMessages()
# Load MBG
devtools::load_all(MBG_PATH)
devtools::load_all(REPO_PATH)

# Load config
config <- versioning::Config$new(CONFIG_PATH)

# Load config containing all model runs
all_runs <- config$read("metadata", "mbg_country_runs")
run_countries <- unique(all_runs$country)

# Create underlying spatial objects for all countries
adm2_data_full <- config$read('shps', 'adm2')
adm2_data_full$ADM0_NAME <- iconv(adm2_data_full$ADM0_NAME, to = 'ASCII//TRANSLIT')
adm2_list <- lapply(run_countries, function(this_country){
  adm2_data_sub <- adm2_data_full[tolower(adm2_data_full$ADM0_NAME) == tolower(this_country), ]
  # Fix for a shapefile issue in Ghana
  if(this_country == "Ghana") adm2_data_sub <- adm2_data_sub[adm2_data_sub$ADM2_CODE != 190611, ]
  adm2_data_sub$polygon_id <- seq_len(nrow(adm2_data_sub))
  return(adm2_data_sub)
})
adm2_ids_list <- lapply(adm2_list, terra::vect) |> lapply(pixel2poly::build_id_raster)
adm1_list <- lapply(
  adm2_list,
  mbg::dissolve_sf_by_attribute,
  by = config$get('mbg_settings', 'shapefile_settings', 'ids', 'adm1')
)
aggregation_table_list <- lapply(seq_along(adm2_list), function(country_ii){
  pixel2poly::build_aggregation_table(
    polygons = terra::vect(adm2_list[[country_ii]]),
    id_raster = adm2_ids_list[[country_ii]],
    polygon_id_field = 'polygon_id'
  )
})
names(adm2_list) <- names(adm2_ids_list) <- names(adm1_list) <-
  names(aggregation_table_list) <- run_countries
rm(list = 'adm2_data_full')

# Load covariates table
covariates_table <- config$read(
  "metadata", "mbg_covariates_table",
   header = TRUE,
  colClasses = list(
    character = c('covariate', 'transform'),
    logical = c('include', 'annual', 'normalize')
  )
)[include == TRUE, ]
# Load population settings
pop_covariate_table <- config$get('mbg_settings', 'pop_covariate_settings') |> 
  data.table::as.data.table()


## RUN ALL ------------------------------------------------------------------------------>

tictoc::tic("All model runs")

for(run_ii in seq_len(nrow(all_runs))) try({

  ## SETUP * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  run_meta <- as.list(all_runs[run_ii, ])
  run_config <- config
  run_version <- glue::glue("{VERSION_BASE}_{run_meta$indicator}_{run_meta$iso3}")
  tictoc::tic(run_version)

  run_config$config_list$versions$mapped_indicators <- run_version
  dir.create(run_config$get_dir_path('mapped_indicators'), recursive = TRUE)

  # Load input data
  input_data <- glue::glue(
    "{config$get_dir_path('prepared_data_stage_2')}/collapsed_{run_meta$indicator}.csv"
  ) |>
    data.table::fread() |>
    _[country == (run_meta$country), ] |>
    setnames(c('longitude', 'latitude', 'cluster'), c('x', 'y', 'cluster_id')) |>
    na.omit()
  # Rescale wealth data to N(0, 1)
  if(startsWith(run_meta$indicator, 'hhwi')){
    data_mean <- mean(input_data$mean)
    data_sd <- sd(input_data$mean)
    input_data[, mean := (mean - data_mean)/data_sd][, sd := sd / data_sd ]
  }
  # Add some aliases
  if(run_meta$family == 'binomial') input_data[, `:=` (indicator = outcome, samplesize = count)]
  if(run_meta$family == 'gaussian') input_data[, `:=` (indicator = mean, samplesize = count)]


  # Load covariates
  covariates_list <- mbg::load_covariates(
    directory = config$get_dir_path('covariates'),
    covariates_table = covariates_table,
    id_raster = adm2_ids_list[[run_meta$country]],
    year = run_meta$year,
    file_format = config$get('mbg_settings', 'covariate_settings', 'file_format'),
    add_intercept = config$get('mbg_settings', 'covariate_settings', 'add_intercept')
  ) |> suppressWarnings()
  # Load the population raster
  population_raster <- mbg::load_covariates(
    directory = config$get_dir_path('covariates'),
    covariates_table = pop_covariate_table,
    id_raster = adm2_ids_list[[run_meta$country]],
    year = run_meta$year,
    file_format = config$get('mbg_settings', 'covariate_settings', 'file_format'),
    add_intercept = FALSE
  )[[1]]


  ## RUN GEOSTATISTICAL MODEL * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  model_runner <- mbg::MbgModelRunner$new(
    input_data = input_data,
    id_raster = adm2_ids_list[[run_meta$country]],
    covariate_rasters = covariates_list,
    admin_bounds = adm2_list[[run_meta$country]],
    admin_bounds_id = 'polygon_id',
    use_covariates = config$get("mbg_settings", "inla_settings", "effects", "covariates"),
    use_gp = config$get("mbg_settings", 'inla_settings', 'effects', 'spde'),
    use_admin_effect = config$get("mbg_settings", 'inla_settings', 'effects', 'admin1'),
    use_nugget = config$get("mbg_settings", 'inla_settings', 'effects', 'nugget'),
    use_stacking = config$get("mbg_settings", "stacking_settings", "run_stacking"),
    stacking_cv_settings = config$get("mbg_settings", "stacking_settings", "cv"),
    stacking_model_settings = config$get("mbg_settings", "stacking_settings", "submodels"),
    stacking_use_admin_bounds = config$get("mbg_settings", "stacking_settings", "adm1_fixed_effects"),
    stacking_prediction_range = if(run_meta$family == 'gaussian') c(-Inf, Inf) else c(1e-4, 1-1e-4),
    mesh_max_edge = c(run_meta$max_edge_internal, 5.0),
    mesh_cutoff = config$get("mbg_settings", 'inla_settings', 'mesh', 'cutoff'),
    spde_integrate_to_zero = config$get("mbg_settings", 'inla_settings', 'mesh', 'integrate_to_zero'),
    prior_spde_range = config$get("mbg_settings", 'inla_settings', 'priors', 'range'),
    prior_spde_sigma = config$get("mbg_settings", 'inla_settings', 'priors', 'sigma'),
    prior_nugget = config$get("mbg_settings", 'inla_settings', 'priors', 'nugget'),
    prior_admin_effect = config$get("mbg_settings", 'inla_settings', 'priors', 'admin1'),
    prior_covariate_effect = config$get("mbg_settings", 'inla_settings', 'priors', 'fixed_effects'),
    inla_link = run_meta$link,
    inverse_link = run_meta$inverse_link,
    inla_family = run_meta$family,
    nugget_in_predict = config$get('mbg_settings', 'prediction_settings', 'nugget_in_predict')
  )
  grid_cell_predictions <- model_runner$run_mbg_pipeline(
    n_samples = config$get('mbg_settings', 'prediction_settings', 'n_samples'),
    ui_width = config$get('mbg_settings', 'prediction_settings', 'ui_width')
  )
  # For household wealth only, rescale the predictions
  if(startsWith(run_meta$indicator, 'hhwi')){
    grid_cell_predictions$cell_draws <- grid_cell_predictions$cell_draws * data_sd + data_mean 
    grid_cell_predictions$cell_pred_mean <- grid_cell_predictions$cell_pred_mean * data_sd + data_mean 
    grid_cell_predictions$cell_pred_lower <- grid_cell_predictions$cell_pred_lower * data_sd + data_mean 
    grid_cell_predictions$cell_pred_upper <- grid_cell_predictions$cell_pred_upper * data_sd + data_mean
    input_data[, mean := mean * data_sd + data_mean ]
    input_data[, indicator := (indicator * data_sd) + data_mean ]
    input_data[, sd := sd * data_sd ]
  }

  # Prepare objects to store admin draws and summaries
  max_adm_level <- config$get('mbg_settings', 'shapefile_settings', 'modeling_level')
  max_adm_level_label <- paste0('adm', max_adm_level)
  all_adm_levels <- seq(0, max_adm_level)
  adm_draws_list <- adm_summaries_list <- vector('list', length = length(all_adm_levels))
  names(adm_draws_list) <- names(adm_summaries_list) <- paste0('adm', all_adm_levels)
  draw_fields <- paste0('draw_', seq_len(config$get('mbg_settings', 'prediction_settings', 'n_samples')))
  # Aggregate to the most detailed admin units
  agg_cols <- config$get('mbg_settings', "shapefile_settings", "ids", max_adm_level_label)
  detailed_adm_draws <- pixel2poly::aggregate_draws_to_polygons(
    draws_matrix = grid_cell_predictions$cell_draws,
    aggregation_table = aggregation_table_list[[run_meta$country]],
    aggregation_cols = agg_cols,
    method = 'weighted.mean',
    weighting_raster = population_raster
  )
  admin_pop <- pixel2poly::aggregate_raster_to_polygons(
    data_raster = population_raster,
    aggregation_table = aggregation_table_list[[run_meta$country]],
    aggregation_cols = agg_cols,
    method = 'sum',
    aggregated_field = 'population'
  )
  detailed_adm_draws[admin_pop, population := i.population, on = agg_cols]
  adm_draws_list[[max_adm_level_label]] <- data.table::copy(detailed_adm_draws)
  # Aggregate to less-detailed admin units
  for(higher_level in setdiff(names(adm_draws_list), max_adm_level_label)){
    agg_cols <- config$get("mbg_settings", "shapefile_settings", "ids", higher_level)
    adm_draws_list[[higher_level]] <- detailed_adm_draws[
      , c(
        list(population = sum(population)),
        lapply(.SD, weighted.mean, w = population, na.rm = TRUE)
      ),
      .SDcols = draw_fields,
      by = agg_cols
    ]
  }
  # Summarize admin draws at all levels
  for(adm_level in names(adm_summaries_list)){
    id_fields <- config$get("mbg_settings", "shapefile_settings", "ids", adm_level)
    adm_summary <- mbg::summarize_draws(
      draws = adm_draws_list[[adm_level]],
      id_fields = id_fields,
      draw_fields = draw_fields,
      ui_width = config$get("mbg_settings", "prediction_settings", "ui_width")
    )
    adm_summary[adm_draws_list[[adm_level]], population := i.population, on = id_fields]
    if(run_meta$family == 'binomial') adm_summary[, `:=` (
      count_mean = mean * population,
      count_lower = lower * population,
      count_upper = upper * population
    )]
    adm_summaries_list[[adm_level]] <- adm_summary
  }

  ## SAVE MODEL RESULTS * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  run_config$write(input_data, 'mapped_indicators', 'formatted_input_data')
  run_config$write(grid_cell_predictions$cell_pred_mean, 'mapped_indicators', 'cell_pred_mean')
  run_config$write(grid_cell_predictions$cell_pred_lower, 'mapped_indicators', 'cell_pred_lower')
  run_config$write(grid_cell_predictions$cell_pred_upper, 'mapped_indicators', 'cell_pred_upper')
  run_config$write(
    grid_cell_predictions$cell_pred_upper - grid_cell_predictions$cell_pred_lower,
    'mapped_indicators', 'cell_pred_ui'
  )
  for(adm_level in names(adm_draws_list)){
    run_config$write(adm_summaries_list[[adm_level]], 'mapped_indicators', paste0(adm_level, '_summary_table'))
  }

  ## SUMMARY VISUALIZATIONS * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  # Create visualization directory
  viz_dir <- file.path(run_config$get_dir_path('mapped_indicators'), 'viz')
  if(!dir.exists(viz_dir)) dir.create(viz_dir)

  ## Helper definitions
  i_title <- run_meta$ind_title
  year <- run_meta$year
  summary_table <- adm_summaries_list[[max_adm_level_label]]
  admin_ids <- config$get("mbg_settings", "shapefile_settings", "ids", max_adm_level_label)
  admin_data_for_plotting <- merge(
    x = adm2_list[[run_meta$country]], y = summary_table, by = admin_ids, all.x = TRUE
  )
  label_function <- if(run_meta$family == 'binomial') scales::percent else scales::comma
  message(glue::glue(
    "Admin 2 UI widths: {summary_table[, round(mean(ui_width, na.rm=T), 3)]} ",
    "({summary_table[, round(quantile(ui_width, 0.025, na.rm=T), 3)]} to ",
    "{summary_table[, round(quantile(ui_width, 0.975, na.rm=T), 3)]})"
  ))
  # Set the color scheme for mean/lower/upper values
  low_is_better <- run_meta$low_is_better
  color_scheme <- RColorBrewer::brewer.pal(n = 9, name = 'Spectral')
  if(low_is_better) color_scheme <- rev(color_scheme)
  # Determine the labeling strategy for outcomes
  label_function <- if(run_meta$family == 'binomial') scales::percent else scales::comma
  # Set the values that will be plotted in the input data
  if(run_meta$family == 'binomial'){
    input_data[, rate := indicator / samplesize ]
  } else {
    input_data[, rate := mean ]
  }
  # Pull out the stacked covariates, if they were created
  run_stacking <- (
    config$get("mbg_settings", "inla_settings", "effects", "covariates") &
    config$get("mbg_settings", "stacking_settings", "run_stacking")
  )
  stacked_covariates <- if(run_stacking) model_runner$model_covariates else NULL
  if(startsWith(run_meta$indicator, 'hhwi') & run_stacking){
    for(sc in names(stacked_covariates)) stacked_covariates[[sc]] <- (
      stacked_covariates[[sc]] * data_sd + data_mean
    )
  }

  # Run summary visualizations
  mch.ml::mbg_visualizations(
    viz_dir = viz_dir,
    input_data = copy(input_data),
    mean_raster = grid_cell_predictions$cell_pred_mean,
    ui_raster = grid_cell_predictions$cell_pred_upper - grid_cell_predictions$cell_pred_lower,
    admin_data_for_plotting = admin_data_for_plotting,
    adm1_boundaries = adm1_list[[run_meta$country]],
    mesh = model_runner$inla_inputs_list$mesh,
    stacked_covariates = stacked_covariates,
    title_base = glue::glue("{i_title} in {run_meta$country}, {year}"),
    color_scheme = color_scheme,
    indicator_name = i_title,
    label_function = label_function
  )

  ## VALIDATE ADM1 RESULTS * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  if(!is.na(run_meta$SC_name) & (run_meta$SC_name != "")){
    mch.ml::mbg_statcompiler_validation(
      viz_dir = viz_dir,
      input_data = input_data,
      adm1_mbg_summaries = adm_summaries_list$adm1,
      country_name = run_meta$country,
      sc_indicator = run_meta$SC_name,
      survey_year = run_meta$year,
      sc_denominator = run_meta$SC_denom,
      label_function = label_function
    ) |> try()
  }

  ## End run-specific timer
  tictoc::toc()
})

# End overall script timer
tictoc::toc()
