## #######################################################################################
##
## 05) EVALUATE RELATIVE MODEL PERFORMANCE
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 20 July 2024
## PURPOSE: Compare performance of many spatial models to inform final model selection
##
## Example call:
## Rscript ~/efs-mount/repos/usaid-mch-ml/stage-2-mapping/05-performance-metrics.R \
##   --run_set 20240805;
##
## #######################################################################################

DEFAULT_CONFIG_PATH <- '~/efs-mount/repos/usaid-mch-ml/config_remote.yaml'
db_repo <- '/home/ubuntu/efs-mount/repos/wtm.ingest'

## Pass globals via command line
library(argparse)
parser <- argparse::ArgumentParser()
parser$add_argument("--run_set", type = 'character')
parser$add_argument("--config_path", type = 'character', default = DEFAULT_CONFIG_PATH)
globals <- parser$parse_args(commandArgs(trailingOnly = TRUE))

RUN_SET <- globals$run_set
CONFIG_PATH <- globals$config_path
message(glue::glue("Compiling performance metrics for run set {RUN_SET}"))


## SETUP -------------------------------------------------------------------------------->

# Load packages
load_packages <- c('data.table', 'pixel2poly', 'versioning')
lapply(load_packages, library, character.only = TRUE) |> invisible()
devtools::load_all(db_repo)

# Load config
config <- versioning::Config$new(CONFIG_PATH)

# Set up database connection
scheduler_schema <- 'scheduler'
scheduler_table <- 'mbg_runner'
conn <- wtm.ingest::PostGISConnection$new(default_schema = scheduler_schema)

# Load run metadata and model specifications
run_metadata <- conn$queryTable(
  glue::glue("{scheduler_schema}.{scheduler_table}"),
  criteria = list(run_set = RUN_SET, status = 'finished')
)[order(runner_id)]
model_specs <- config$read('repo', 'model_specs')


## Define helper functions to prepare model metrics ------------------------------------->

# Function: Generate predictive metrics for a single table "dt"
generate_metrics <- function(dt, model_family = 'binomial'){
  if(model_family == 'gaussian'){
    # Case: normally-distributed data
    (dt
      [, obs_mean := mean ]
      [, extreme := 0 ]
      [, loglike := dnorm(x = pred_mean, mean = obs_mean, sd = sd, log = TRUE)]
    )
  } else {
    # Case: binomial data
    (dt
      [, obs_mean := indicator / samplesize ]
      [, extreme := as.integer((indicator == 0L) | (indicator == samplesize))]
      [, loglike := dbinom(x = indicator, size = samplesize, prob = pred_mean, log = TRUE)]
    )
  }
  # Get aggregate PV metrics
  dt_agg <- (dt
    [!is.na(obs_mean) & !is.na(pred_mean), ]
    [, covered := as.integer((obs_mean > pred_lower) & (obs_mean < pred_upper)) ]
    [, .(
      rmse = sqrt(mean((pred_mean - obs_mean)**2, na.rm = T)),
      mae = mean(abs(pred_mean - obs_mean), na.rm = T),
      bias = mean(pred_mean - obs_mean, na.rm = T),
      r2 = cor(x = obs_mean, y = pred_mean)**2,
      ui_width = mean(pred_upper - pred_lower, na.rm = T),
      covg = mean(covered, na.rm = T),
      covg_noextremes = mean(covered * (1 - extreme), na.rm = T) / mean(1 - extreme, na.rm = T),
      loglike = sum(loglike, na.rm = T),
      samplesize = sum(count, na.rm = T)
    )]
  )
  return(dt_agg)
}


## Generate directly-calculated predictive metrics: errors, coverage, log-likelihood ---->

run_metadata[, run_version := paste(RUN_SET, iso3, indicator, specs, sep = '_')]
outputs_dir <- config$get_dir_path("mbg_model_results") |> dirname()
continuous_indicators <- config$get('continuous_indicators')

# Helper function: Prepare model metrics for all tables
prepare_all_metrics <- function(run_index_row, results_file){
  run_version <- run_metadata$run_version[run_index_row]
  indicator <- run_metadata$indicator[run_index_row]
  model_family <- if(indicator %in% continuous_indicators) 'gaussian' else 'binomial'
  run_metrics <- file.path(outputs_dir, run_version, results_file) |>
    data.table::fread() |>
    generate_metrics(model_family = model_family)
  run_metrics$run_version <- run_version
  return(run_metrics)
}

tictoc::tic("In-sample metrics")
in_sample_metrics <- lapply(
  seq_len(nrow(run_metadata)),
  prepare_all_metrics,
  results_file = 'in_sample_results.csv'
) |> data.table::rbindlist(use.names = T, fill = T)
tictoc::toc()

tictoc::tic("Out-of-sample metrics")
out_of_sample_metrics <- lapply(
  seq_len(nrow(run_metadata)),
  prepare_all_metrics,
  results_file = 'out_of_sample_results.csv'
) |> data.table::rbindlist(use.names = T, fill = T)
tictoc::toc()

# Combine into a single table
run_metrics_full <- copy(run_metadata) |>
  merge(y = in_sample_metrics, by = 'run_version', all = T) |>
  merge(y = out_of_sample_metrics, by = 'run_version', all = T, suffixes = c('_is', '_oos'))

# Calculate score as a geometric mean of three quantities:
#  --> Data coverage, capped above 95% (out-of-sample)
#  --> *Inverse* of RMSE, capped below 5% (out-of-sample)
(run_metrics_full
  [, model_score := log(pmax(covg_noextremes_oos, 0.95)) - log(pmax(rmse_oos, 0.05)) ]
  # Normalize to positive numbers starting at 1
  [, model_score := model_score - min(model_score), by = .(country, indicator)]
  [, model_score_rank := frank(-model_score, ties.method = 'random'), by = .(country, indicator)]
  # Get ranking based on data likelihoods as well
  [, loglike_is_rank := frank(-loglike_is, ties.method = 'random'), by = .(country, indicator)]
  [, loglike_oos_rank := frank(-loglike_oos, ties.method = 'random'), by = .(country, indicator)]
)

# Save to file
data.table::fwrite(run_metrics_full, file = glue::glue("{outputs_dir}/run_metrics_{RUN_SET}.csv"))
