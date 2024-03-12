## #######################################################################################
##
## TEST RUN OF THE SAMPLED SHAPLEY DECOMPOSITION USING A TOY DATASET
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 7 February 2024
## PURPOSE: Test run the sampled approximation to the Shapley decomposition.
##
## NOTES: When the number of features is less than seven, all permutations will be run
##   by default rather than sampling from the universe of permutations. The sampler was
##   adapted from the Python `sage` package: https://github.com/iancovert/sage/
## 
## #######################################################################################

## SETTINGS

# All other settings set through the config file
CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'


## SETUP -------------------------------------------------------------------------------->

# Load packages
load_libs <- c('versioning', 'data.table', 'caret')

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))


## CREATE TEST DATASET ------------------------------------------------------------------>

# Simple linear model
n_observations <- 1e3
model_data <- data.table::data.table(
  a = rnorm(n_observations), b = rnorm(n_observations), c = rnorm(n_observations),
  d = rnorm(n_observations), e = rnorm(n_observations), f = rnorm(n_observations),
  error = rnorm(n_observations)
)[, outcome := .5  * a + .2 * b + .1 * c  - .1 * d - .2 * e - .5 * f + .5 * error ]

loss_fun <- mch.ml::get_loss_function('L2')

# Fit the model
model_fit <- caret::train(
  form = as.formula('outcome ~ a + b + c + d + e + f'),
  data = model_data,
  method = 'lm',
  trControl = caret::trainControl(method = 'cv', number = 5)
)

# Initialize models for Shapley value estimation
marginal_imputer <- mch.ml::MarginalImputer$new(
  model = model_fit,
  features_table = model_data,
  outcomes = model_data$outcome,
  loss_fun = loss_fun
)
model_sampler <- mch.ml::PermutationSampler$new(imputer = marginal_imputer, verbose = T)

# Run Shapley decomposition
model_sampler$sample(run_all = TRUE)
shapley_vals <- list(
  absolute = model_sampler$get_shapley_vals(),
  normalized = model_sampler$get_shapley_vals(normalize = T)
)

# Print results to console
message('Absolute Shapley values:')
knitr::kable(shapley_vals$absolute)
message("Normalized Shapley values:")
knitr::kable(shapley_vals$normalized)
