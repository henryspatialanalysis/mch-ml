## #######################################################################################
##
## 05) COMPILE BEST MODEL RESULTS FOR EACH COUNTRY AND INDICATOR
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 20 July 2024
## PURPOSE: Prepare full input data for the best version of each model and indicator
##
## Example call:
## Rscript ~/efs-mount/repos/usaid-mch-ml/stage-2-mapping/06-compile-best.R \
##   --run_set 20240805;
##
## #######################################################################################

DEFAULT_CONFIG_PATH <- '~/efs-mount/repos/usaid-mch-ml/config_remote.yaml'

## Pass globals via command line
library(argparse)
parser <- argparse::ArgumentParser()
parser$add_argument("--run_set", type = 'character')
parser$add_argument("--config_path", type = 'character', default = DEFAULT_CONFIG_PATH)
globals <- parser$parse_args(commandArgs(trailingOnly = TRUE))

RUN_SET <- globals$run_set
CONFIG_PATH <- globals$config_path
if(is.null(RUN_SET)) stop("Run set undefined")
if(is.null(CONFIG_PATH)) stop("Config path undefined")
message(glue::glue("Compiling performance metrics for run set {RUN_SET}"))


## SETUP -------------------------------------------------------------------------------->

# Load packages
load_packages <- c('data.table', 'pixel2poly', 'versioning', 'mch.ml')
lapply(load_packages, library, character.only = TRUE) |> invisible()

# Load config
config <- versioning::Config$new(CONFIG_PATH)

# Load table of best-performing models
outputs_dir <- config$get_dir_path("mbg_model_results") |> dirname()
run_metrics_full <- versioning::autoread(glue::glue("{outputs_dir}/run_metrics_{RUN_SET}.csv"))
selected_models <- run_metrics_full[selected == 1L, ]

# Load model specs to inform visualization
model_specs <- config$read('repo', 'model_specs')

# Create the selected runs directory
selected_dir <- glue::glue("{outputs_dir}/selected_{RUN_SET}")
dir.create(selected_dir)

# Other settings common to all models
old_stackers_dir <- config$get_dir_path("mbg_stacking_cache")
stackers <- config$get("mbg_settings", "stacking_settings", "submodels") |> names()
ui_width <- config$get("mbg_settings", "prediction_settings", "ui_width")
max_adm_level <- config$get('mbg_settings', 'shapefile_settings', 'modeling_level')
max_adm_level_label <- paste0('adm', max_adm_level)
all_adm_levels <- seq(0, max_adm_level)
agg_cols <- config$get("mbg_settings", "shapefile_settings", "ids", max_adm_level_label)
draw_fields <- paste0('draw_', seq_len(config$get('mbg_settings', 'prediction_settings', 'n_samples')))


## Save full model results and preliminary visualization for each selected model -------->

for(COUNTRY in unique(selected_models$country)){

  ## Prepare country-specific inputs
  country_runs <- copy(selected_models[country == COUNTRY, ])
  country_indicators <- country_runs[, unique(indicator)]
  adm_boundaries <- config$read('mbg_shapefile', 'adm2') |>
    dplyr::filter(ADM0_NAME == COUNTRY)
  adm_boundaries$polygon_id <- seq_len(nrow(adm_boundaries))
  id_raster <- pixel2poly::build_id_raster(polygons = terra::vect(adm_boundaries))
  to_fill <- which(!is.na(terra::values(id_raster)))
  adm1_boundaries <- mbg::dissolve_sf_by_attribute(
    adm_boundaries,
    by = config$get('mbg_settings', 'shapefile_settings', 'ids', 'adm1')
  )
  adm1_boundaries$polygon_id <- seq_len(nrow(adm1_boundaries))
  # Create admin link table
  aggregation_table <- pixel2poly::build_aggregation_table(
    polygons = terra::vect(adm_boundaries),
    id_raster = id_raster,
    polygon_id_field = 'polygon_id',
    verbose = FALSE
  )

  ## Prepare indicator-specific inputs
  for(INDICATOR in country_indicators){
    message(glue::glue("Preparing {COUNTRY} - {INDICATOR}"))
    run_meta <- as.list(country_runs[indicator == INDICATOR, ])
    run_specs <- as.list(model_specs[suffix == run_meta$specs, ])
    # Move outputs to the selected directory
    old_run_dir <- file.path(outputs_dir, run_meta$run_version)
    new_run_dir <- glue::glue('{selected_dir}/{INDICATOR}_{run_meta$iso3}')
    dir.create(new_run_dir)
    for(fp in list.files(old_run_dir)){
      file.copy(
        from = file.path(old_run_dir, fp),
        to = file.path(new_run_dir, fp),
        overwrite = TRUE
      )
    }
    # Load input data
    input_data <- glue::glue('{config$get_dir_path("mbg_input_data")}/collapsed_{INDICATOR}.csv') |>
      data.table::fread() |>
      dplyr::filter(country == COUNTRY) |>
      setnames(c('longitude', 'latitude', 'cluster'), c('x', 'y', 'cluster_id')) |>
      stats::na.omit() |>
      data.table::copy()
    if(indicator == 'hhwi_cont'){
      input_data[, `:=` (indicator = mean, samplesize = count)]      
    } else {
      input_data[, `:=` (indicator = outcome, samplesize = count)]
    }
    versioning::autowrite(input_data, file = file.path(new_run_dir, 'input_data.csv'))

    # Load population raster corresponding to this indicator
    if(INDICATOR %in% c('stunting', 'wasting')){
      config$config_list$mbg_settings$pop_covariate_settings$covariate <- 'worldpop_u5'
    } else if(INDICATOR %in% c('first_birth_u18', 'w_empowered_sdg')){
      config$config_list$mbg_settings$pop_covariate_settings$covariate <- 'worldpop_wora'
    } else {
      config$config_list$mbg_settings$pop_covariate_settings$covariate <- 'worldpop_total'
    }
    pop_covariate_table <- config$get('mbg_settings', 'pop_covariate_settings') |>
      data.table::as.data.table()
    population_raster <- mbg::load_covariates(
      directory = config$get_dir_path('mbg_covariates'),
      covariates_table = pop_covariate_table,
      id_raster = id_raster,
      year = run_meta$year,
      file_format = config$get('mbg_settings', 'covariate_settings', 'file_format'),
      add_intercept = FALSE
    )[[1]]

    # If stacking was run, move stacking rasters to a subfolder and load them
    model_used_stackers <- (run_specs$covariates && run_specs$run_stacking)
    if(model_used_stackers){
      new_stackers_dir <- file.path(new_run_dir, 'stackers')
      dir.create(new_stackers_dir)
      stacked_covariates <- lapply(stackers, function(submodel_name){
        fp <- list.files(old_stackers_dir, pattern = glue::glue("{run_meta$iso3}_{INDICATOR}")) |>
          grep(pattern = '_0_', value = TRUE) |>
          grep(pattern = submodel_name, value = TRUE)
        file.copy(
          from = file.path(old_stackers_dir, fp),
          to = glue::glue("{new_stackers_dir}/{submodel_name}.tif")
        )
        versioning::autoread(file.path(old_stackers_dir, fp))
      })
      names(stacked_covariates) <- stackers
    } else {
      stacked_covariates <- NULL
    }

    # Load grid cell draws and summarize
    cell_draws <- versioning::autoread(file.path(new_run_dir, 'cell_draws.RDS'))
    # Convert back to measurement space for household wealth
    if(startsWith(INDICATOR, 'hhwi')){
      data_mean <- mean(input_data$mean)
      data_sd <- sd(input_data$mean)
      cell_draws <- (cell_draws * data_sd) + data_mean
      saveRDS(cell_draws, file = file.path(new_run_dir, 'cell_draws.RDS'))
    }
    r_mean <- r_lower <- r_upper <- id_raster
    terra::values(r_mean)[to_fill] <- cell_draws |> Matrix::rowMeans()
    terra::values(r_lower)[to_fill] <- cell_draws |>
      matrixStats::rowQuantiles(probs = (1 - ui_width) / 2)
    terra::values(r_upper)[to_fill] <- cell_draws |>
      matrixStats::rowQuantiles(probs = 1 - (1 - ui_width) / 2)
    r_ui <- r_upper - r_lower
    versioning::autowrite(r_mean, file = file.path(new_run_dir, 'mean_raster.tif'))
    versioning::autowrite(r_lower, file = file.path(new_run_dir, 'lower_raster.tif'))
    versioning::autowrite(r_upper, file = file.path(new_run_dir, 'upper_raster.tif'))
    versioning::autowrite(r_ui, file = file.path(new_run_dir, 'ui_raster.tif'))

    # Aggregate to admin draws and summarize
    adm_draws_list <- adm_summaries_list <- vector('list', length = length(all_adm_levels))
    names(adm_draws_list) <- names(adm_summaries_list) <- paste0('adm', all_adm_levels)
    # Aggregate to the most detailed admin units
    detailed_adm_draws <- pixel2poly::aggregate_draws_to_polygons(
      draws_matrix = cell_draws,
      aggregation_table = aggregation_table,
      aggregation_cols = agg_cols,
      method = 'weighted.mean',
      weighting_raster = population_raster
    )
    admin_pop <- pixel2poly::aggregate_raster_to_polygons(
      data_raster = population_raster,
      aggregation_table = aggregation_table,
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
          lapply(.SD, weighted.mean, w = population)
        ),
        .SDcols = draw_fields, by = agg_cols
      ]
    }
    # Summarize admin draws at all levels
    for(adm_level in names(adm_summaries_list)){
      id_fields <- config$get("mbg_settings", "shapefile_settings", "ids", adm_level)
      adm_summary <- mbg::summarize_draws(
        draws = adm_draws_list[[adm_level]],
        id_fields = id_fields,
        draw_fields = draw_fields,
        ui_width = ui_width
      )
      adm_summary[adm_draws_list[[adm_level]], population := i.population, on = id_fields]
      adm_summary[, `:=` (
        count_mean = mean * population,
        count_lower = lower * population,
        count_upper = upper * population
      )]
      adm_summaries_list[[adm_level]] <- adm_summary
    }
    # Save admin draws and summaries
    for(adm_level in names(adm_draws_list)){
      adm_draws_list[[adm_level]] |>
        versioning::autowrite(file = glue::glue('{new_run_dir}/{adm_level}_draws.csv'))
      adm_summaries_list[[adm_level]] |>
        versioning::autowrite(file = glue::glue('{new_run_dir}/{adm_level}_summaries.csv'))
    }
    # Merge most detailed admin summaries with admin boundaries
    detailed_adm_summaries <- adm_summaries_list[[max_adm_level_label]] |> copy()
    admin_data_for_plotting <- merge(
      x = adm_boundaries,
      y = detailed_adm_summaries,
      by = intersect(colnames(adm_boundaries), colnames(detailed_adm_summaries))
    )

    ## Create standard MBG plots
    high_is_better <- (INDICATOR %in% c('hhwi_cont', 'w_empowered_sdg'))
    color_scheme <- RColorBrewer::brewer.pal("Spectral", n = 9)
    if(!high_is_better) color_scheme <- rev(color_scheme)
    if(INDICATOR == 'hhwi_cont'){
      indicator_family <- 'gaussian'
      input_data[, rate := mean]
    } else {
      indicator_family <- 'binomial'
      input_data[, rate := indicator / samplesize]
    }
    indicator_name <- config$get("indicator_labels", INDICATOR)
    title_base <- glue::glue("{indicator_name} in {COUNTRY}")

    # Run standard MBG visualizations
    viz_dir <- file.path(new_run_dir, 'viz')
    dir.create(viz_dir)
    mch.ml::mbg_visualizations(
      viz_dir = viz_dir,
      input_data = copy(input_data),
      mean_raster = r_mean,
      ui_raster = r_ui,
      admin_data_for_plotting = admin_data_for_plotting,
      adm1_boundaries = adm1_boundaries,
      stacked_covariates = stacked_covariates,
      plot_stackers = model_used_stackers,
      title_base = title_base,
      color_scheme = color_scheme,
      indicator_name = indicator_name,
      indicator_family = indicator_family
    )
  }
}
