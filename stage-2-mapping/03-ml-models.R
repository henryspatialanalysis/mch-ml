## #######################################################################################
##
## 03) CACHE ML SUBMODEL PREDICTIONS FOR EACH OUTCOME
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 20 July 2024
## PURPOSE: Run ML submodels for each outcome once. These will be used repeatedly for
##   in the (04) model-based geostatistics workflow
##
## Example script call:
##
## Rscript ~/efs-mount/repos/usaid-mch-ml/stage-2-mapping/03-ml-models.R \
##   --country Senegal --iso3 SEN --year 2019 --run_set 20240805;
##
## #######################################################################################

## 00) SETTINGS ------------------------------------------------------------------------->

DEFAULT_CONFIG_PATH <- '~/efs-mount/repos/usaid-mch-ml/config_remote.yaml'

## Pass globals via command line
library(argparse)
parser <- argparse::ArgumentParser()
parser$add_argument("--country", type = 'character', default = NULL)
parser$add_argument("--iso3", type = 'character', default = NULL)
parser$add_argument("--year", type = 'integer', default = NULL)
parser$add_argument("--run_set", type = 'character')
parser$add_argument("--config_path", type = 'character', default = DEFAULT_CONFIG_PATH)
globals <- parser$parse_args(commandArgs(trailingOnly = TRUE))

COUNTRY <- globals$country
ISO3 <- globals$iso3
YEAR <- globals$year
RUN_SET <- globals$run_set
CONFIG_PATH <- globals$config_path

# Load packages
load_packages <- c('pixel2poly', 'mbg', 'data.table', 'sf', 'glue', 'tictoc', 'versioning')
lapply(load_packages, library, character.only = T) |> invisible() |> 
  suppressPackageStartupMessages()

# Load config
config <- versioning::Config$new(CONFIG_PATH)

# Set up output directory
config$config_list$versions$mbg_stacking_cache <- RUN_SET
cache_dir <- config$get_dir_path('mbg_stacking_cache')
dir.create(cache_dir, recursive = TRUE)

# Start script timer
tictoc::tic("Full script")


## PREPARE INPUT DATA ------------------------------------------------------------------->

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


## RUN IN-SAMPLE STACKING --------------------------------------------------------------->

for(INDICATOR in config$get("map_indicators")){
  # Timer for this indicator
  tictoc::tic(glue::glue("  ML model predictions for {INDICATOR} in {COUNTRY}"))

  ## Load input data
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

  ## Run stacking
  for(holdout_idx in 0:5){
    tictoc::tic(glue::glue("    Holdout {holdout_idx}"))
    # Run all ML submodels
    ml_predictions <- mbg::run_regression_submodels(
      input_data = copy(input_data[holdout_id != holdout_idx, ]),
      id_raster = id_raster,
      covariates = covariates_list,
      cv_settings = config$get("mbg_settings", "stacking_settings", "cv"),
      model_settings = config$get("mbg_settings", "stacking_settings", "submodels"),
      family = indicator_family,
      clamping = TRUE,
      use_admin_bounds = TRUE,
      admin_bounds = adm1_boundaries,
      admin_bounds_id = 'polygon_id',
      prediction_range = if(indicator_family == 'gaussian') c(-Inf, Inf) else c(1e-4, 1-1e-4)
    )$predictions |> suppressWarnings()
    for(model_type in names(ml_predictions)){
      fn <- glue::glue("{cache_dir}/{ISO3}_{INDICATOR}_{YEAR}_{holdout_idx}_{model_type}.tif")
      if(file.exists(fn)) file.remove(fn)
      terra::writeRaster(x = ml_predictions[[model_type]], filename = fn)
    }
    tictoc::toc() # End this holdout
  }
  tictoc::toc() # End stacking for all holdouts in this indicator
}

# End overall script timer
tictoc::toc()
