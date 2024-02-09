## #######################################################################################
##
## TEST RUN OF THE SHAPLEY DECOMPOSITION USING A TOY DATASET
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 7 February 2024
## PURPOSE: Test run the Shapley decomposition. This example uses a small number of
##   covariates -- when number of covariates exceeds > 15, we will have to sample from
##   around the model space instead.
##
## #######################################################################################

## SETTINGS

# All other settings set through the config file
CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'


## SETUP -------------------------------------------------------------------------------->

# Load packages
library(versioning); library(devtools); library(data.table)

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))


## CREATE TEST DATASET ------------------------------------------------------------------>

# Simple linear model
n_observations <- 1e3
model_data <- data.table::data.table(
  a = rnorm(n_observations), b = rnorm(n_observations), c = rnorm(n_observations),
  error = rnorm(n_observations)
)[, outcome := a + .2 * b - .1 * c + .05 * error ]

loss_fun <- get_loss_function('L2')

# Run all possible model combinations
model_performance_list <- run_all_covariate_combinations(
  model_data = model_data,
  outcome = 'outcome',
  predictors = c('a','b','c'),
  model_type = 'lm',
  loss_function = loss_fun
)
# Decompose effects by predictor
shapley_vals <- run_shapley_decomposition(
  predictors = c('a','b','c'),
  model_performance_list = model_performance_list
)

# Print results to console
message('Absolute Shapley values:')
knitr::kable(shapley_vals$absolute)
message("Normalized Shapley values:")
knitr::kable(shapley_vals$normalized)
