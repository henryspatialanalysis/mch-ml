## #######################################################################################
##
## 03) GEOSTATISTICAL MAPPING WORKFLOW
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 10 June 2024
## PURPOSE: MBG workflow using packages `mbg`, `versioning`, and `pixel2poly`
##
## Example call:
## Rscript ~/efs-mount/repos/usaid-mch-ml/stage-2-mapping/04-geostatistical-model-parallel.R \
##   --indicator stunting --country Senegal --iso3 SEN --year 2019 --run_set 20240805 \ 
##   --specs default;
##
## #######################################################################################

DEFAULT_CONFIG_PATH <- '~/efs-mount/repos/usaid-mch-ml/config_remote.yaml'

## Pass globals via command line
library(argparse)
parser <- argparse::ArgumentParser()
parser$add_argument("--indicator", type = 'character')
parser$add_argument("--country", type = 'character', nargs = '+')
parser$add_argument("--iso3", type = 'character')
parser$add_argument("--year", type = 'integer')
parser$add_argument("--run_set", type = 'character')
parser$add_argument('--specs', type = 'character')
parser$add_argument("--config_path", type = 'character', default = DEFAULT_CONFIG_PATH)
globals <- parser$parse_args(commandArgs(trailingOnly = TRUE))

INDICATOR <- globals$indicator
COUNTRY <- globals$country |> paste() # Enable multi-word country names
ISO3 <- globals$iso3
YEAR <- globals$year
RUN_SET <- globals$run_set
SPECS <- globals$specs
CONFIG_PATH <- globals$config_path
message(glue::glue(c(
  "Running {INDICATOR} in {COUNTRY} ({ISO3}) {YEAR}, run set {RUN_SET}, {SPECS} model, ",
  "config at {CONFIG_PATH}"
)))


## 00) SETTINGS ------------------------------------------------------------------------->

# Load standard packages
load_packages <- c(
  'pixel2poly', 'mbg', 'mch.ml', 'data.table', 'sf', 'glue', 'versioning'
)
lapply(load_packages, library, character.only = T) |> invisible() |> 
  suppressPackageStartupMessages()

# Load config
config <- versioning::Config$new(CONFIG_PATH)

# Set up output directory
run_version <- glue::glue('{RUN_SET}_{ISO3}_{INDICATOR}_{SPECS}')
config$config_list$versions$mbg_model_results <- run_version
dir.create(config$get_dir_path('mbg_model_results'), showWarnings = FALSE)


## PREPARE INPUT DATA ------------------------------------------------------------------->

# Load the run specifications
all_run_specs <- config$read('repo', 'model_specs')
if(!SPECS %in% all_run_specs$suffix) stop('Model version not valid.')
run_specs <- as.list(all_run_specs[suffix == SPECS, ])

# Update config settings to match the run specs
config$config_list$mbg_settings$stacking_settings$run_stacking <- run_specs$run_stacking
config$config_list$mbg_settings$prediction_settings$nugget_in_predict <- run_specs$nugget_in_predict
for(effect_type in names(config$get('mbg_settings', 'inla_settings', 'effects'))){
  config$config_list$mbg_settings$inla_settings$effects[[effect_type]] <- run_specs[[effect_type]]
}
for(prior_type in c('range', 'sigma', 'nugget', 'admin1')){
  config$config_list$mbg_settings$inla_settings$priors[[prior_type]]$threshold <- (
    run_specs[[paste0('prior_', prior_type)]]
  )
}
# Save config to output directory
config$write_self("mbg_model_results")

# Load country boundaries and create spatial objects
adm_boundaries <- config$read('mbg_shapefile', 'adm2') |>
  dplyr::filter(ADM0_NAME == COUNTRY)
id_raster <- pixel2poly::build_id_raster(polygons = terra::vect(adm_boundaries))
adm1_boundaries <- mbg::dissolve_sf_by_attribute(
  adm_boundaries,
  by = config$get('mbg_settings', 'shapefile_settings', 'ids', 'adm1')
)
adm1_boundaries$polygon_id <- seq_len(nrow(adm1_boundaries))

# Load covariates table
covariates_table <- config$read(
  "repo", "covariates_table", header = TRUE,
  colClasses = list(
    character = c('covariate', 'transform'),
    logical = c('include', 'annual', 'normalize')
  )
)
# Drop covariates where "include" is FALSE
if('include' %in% colnames(covariates_table)) covariates_table <- covariates_table[include == T, ]
# Load a list of covariates
covariates_list <- mbg::load_covariates(
  directory = config$get_dir_path('mbg_covariates'),
  covariates_table = covariates_table,
  id_raster = id_raster,
  year = YEAR,
  file_format = config$get('mbg_settings', 'covariate_settings', 'file_format'),
  add_intercept = config$get('mbg_settings', 'covariate_settings', 'add_intercept')
)

# Load input data
input_data <- glue::glue('{config$get_dir_path("mbg_input_data")}/collapsed_{INDICATOR}.csv') |>
  data.table::fread() |>
  dplyr::filter(country == COUNTRY) |>
  setnames(c('longitude', 'latitude', 'cluster'), c('x', 'y', 'cluster_id')) |>
  stats::na.omit() |>
  data.table::copy()
if(nrow(input_data) == 0) stop("After subsetting to data from ", COUNTRY, ", no rows of data remain.")
# For household wealth data only, rescale to N(0, 1)
if(startsWith(INDICATOR, 'hhwi')){
  data_mean <- mean(input_data$mean)
  data_sd <- sd(input_data$mean)
  input_data[, mean := (mean - data_mean)/data_sd][, sd := sd / data_sd ]
}

# Determine the indicator family
indicator_family <- if(INDICATOR %in% config$get('continuous_indicators')) 'gaussian' else 'binomial'
# Add some aliases
if(indicator_family == 'binomial') input_data[, `:=` (indicator = outcome, samplesize = count)]
if(indicator_family == 'gaussian') input_data[, `:=` (indicator = mean, samplesize = count)]

# Set max edge based on country
max_edge_internal <- if(COUNTRY %in% c('Kenya', 'Madagascar', 'Nigeria', 'Philippines')) 0.5 else 0.2 


## RUN ALL MBG MODELS ------------------------------------------------------------------->

full_data_points <- input_data |>
  sf::st_as_sf(coords = c('x', 'y'), crs = sf::st_crs("EPSG:4326")) |>
  terra::vect()
fast_extract <- function(rast) terra::extract(x = rast, y = full_data_points, xy = F, ID = F)[, 1]

## In-sample run
model_runner <- mbg::MbgModelRunner$new(
  input_data = copy(input_data),
  id_raster = id_raster,
  covariate_rasters = covariates_list,
  admin_bounds = adm1_boundaries,
  admin_bounds_id = 'polygon_id',
  use_covariates = config$get("mbg_settings", "inla_settings", "effects", "covariates"),
  use_gp = config$get("mbg_settings", 'inla_settings', 'effects', 'spde'),
  use_admin_effect = config$get("mbg_settings", 'inla_settings', 'effects', 'admin1'),
  use_nugget = config$get("mbg_settings", 'inla_settings', 'effects', 'nugget'),
  use_stacking = config$get("mbg_settings", "stacking_settings", "run_stacking"),
  stacking_cv_settings = config$get("mbg_settings", "stacking_settings", "cv"),
  stacking_model_settings = config$get("mbg_settings", "stacking_settings", "submodels"),
  stacking_use_admin_bounds = config$get("mbg_settings", "stacking_settings", "adm1_fixed_effects"),
  stacking_prediction_range = if(indicator_family == 'gaussian') c(-Inf, Inf) else c(1e-4, 1-1e-4),
  mesh_max_edge = c(max_edge_internal, 5.0),
  mesh_cutoff = config$get("mbg_settings", 'inla_settings', 'mesh', 'cutoff'),
  spde_integrate_to_zero = config$get("mbg_settings", 'inla_settings', 'mesh', 'integrate_to_zero'),
  prior_spde_range = config$get("mbg_settings", 'inla_settings', 'priors', 'range'),
  prior_spde_sigma = config$get("mbg_settings", 'inla_settings', 'priors', 'sigma'),
  prior_nugget = config$get("mbg_settings", 'inla_settings', 'priors', 'nugget'),
  prior_admin_effect = config$get("mbg_settings", 'inla_settings', 'priors', 'admin1'),
  prior_covariate_effect = config$get("mbg_settings", 'inla_settings', 'priors', 'fixed_effects'),
  inla_link = if(indicator_family == 'binomial') 'logit' else 'identity',
  inverse_link = if(indicator_family == 'binomial') 'plogis' else 'identity',
  inla_family = indicator_family,
  nugget_in_predict = config$get("mbg_settings", 'prediction_settings', 'nugget_in_predict')
)
# Set model covariates, if needed
if(model_runner$use_covariates & model_runner$use_stacking){
  # Load stackers from cache
  fp_template <- glue::glue("{ISO3}_{INDICATOR}_{YEAR}_0_")
  stacker_dir <- config$get_dir_path("mbg_stacking_cache")
  stacking_fps <- list.files(stacker_dir, pattern = fp_template)
  stackers_list <- lapply(file.path(stacker_dir, stacking_fps), terra::rast)
  names(stackers_list) <- substr(stacking_fps, nchar(fp_template) + 1, nchar(stacking_fps) - 4)
  # Set stacker predictions as model covariates
  model_runner$model_covariates <- stackers_list
} else if(model_runner$use_covariates){
  # Set raw covariates as model covariates
  model_runner$model_covariates <- covariates_list
} else {
  model_runner$model_covariates <- NULL
}
model_runner$fit_mbg_model()
is_predictions <- model_runner$generate_predictions(
  n_samples = config$get('mbg_settings', 'prediction_settings', 'n_samples'),
  ui_width = config$get('mbg_settings', 'prediction_settings', 'ui_width')
)

# Get estimates for all points
results_table <- data.table::copy(input_data)
results_table$pred_mean <- fast_extract(is_predictions$cell_pred_mean)
results_table$pred_lower <- fast_extract(is_predictions$cell_pred_lower)
results_table$pred_upper <- fast_extract(is_predictions$cell_pred_upper)


## RUN OUT-OF-SAMPLE MODELS ------------------------------------------------------------->

oos_table_full <- lapply(1:5, function(holdout_idx){
  train_data <- data.table::copy(input_data[holdout_id != holdout_idx, ])
  test_data <- data.table::copy(input_data[holdout_id == holdout_idx, ])
  test_points <- test_data |>
    sf::st_as_sf(coords = c('x', 'y'), crs = sf::st_crs("EPSG:4326")) |>
    terra::vect()
  fast_extract <- function(rast) terra::extract(x = rast, y = test_points, xy = F, ID = F)[, 1]

  # Run out-of-sample MBG model
  oos_runner <- model_runner$clone()
  oos_runner$input_data <- train_data
  if(model_runner$use_covariates & model_runner$use_stacking){
    fp_template <- glue::glue("{ISO3}_{INDICATOR}_{YEAR}_{holdout_idx}_")
    stacker_dir <- config$get_dir_path("mbg_stacking_cache")
    stacking_fps <- list.files(stacker_dir, pattern = fp_template)
    stackers_list <- lapply(file.path(stacker_dir, stacking_fps), terra::rast)
    names(stackers_list) <- substr(stacking_fps, nchar(fp_template) + 1, nchar(stacking_fps) - 4)
    # Set stacker predictions as model covariates
    oos_runner$model_covariates <- stackers_list
  }
  oos_runner$fit_mbg_model()
  oos_predictions <- oos_runner$generate_predictions(
    n_samples = config$get('mbg_settings', 'prediction_settings', 'n_samples'),
    ui_width = config$get('mbg_settings', 'prediction_settings', 'ui_width')
  )

  # Get estimates at held-out points
  test_data$pred_mean <- fast_extract(oos_predictions$cell_pred_mean)
  test_data$pred_lower <- fast_extract(oos_predictions$cell_pred_lower)
  test_data$pred_upper <- fast_extract(oos_predictions$cell_pred_upper)

  return(test_data)
}) |> data.table::rbindlist()


## SAVE RESULTS ------------------------------------------------------------------------->

config$write(results_table, 'mbg_model_results', 'in_sample_results')
config$write(oos_table_full, 'mbg_model_results', 'out_of_sample_results')
config$write(id_raster, 'mbg_model_results', 'id_raster')
config$write(is_predictions$cell_draws, 'mbg_model_results', 'cell_draws')
