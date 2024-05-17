## #######################################################################################
##
## SHAPLEY DECOMPOSITION
##
## AUTHOR: Nat Henry
## CREATED: 7 April 2024
## PURPOSE: Shapley decomposition code for full model data
## 
## #######################################################################################

## Setup

# Set globals
REPO_FP <- '~/efs-mount/repos/usaid-mch-ml/r-package'
CONFIG_FP <- file.path('~/efs-mount/repos/usaid-mch-ml/config_remote.yaml')

parser <- argparse::ArgumentParser()
parser$add_argument('--imp', type = 'integer')
parser$add_argument('--method', type = 'character')
parser$add_argument('--survey', type = 'character')
parser$add_argument('--holdout', default = 0, type = 'integer')
args <- parser$parse_args(commandArgs(trailingOnly = TRUE))
imp_ii <- args$imp # Imputation version
METHOD <- args$method # 'plr', 'xgbTree', 'treebag', 'rf', 'LogitBoost', 'gam', 'glm', 'ada'
survey_id <- args$survey # e.g. 'GH2022DHS'
holdout_id <- args$holdout

if(is.null(holdout_id) | is.null(imp_ii) | is.null(METHOD) | is.null(survey_id)){
  stop("Issue with command line arguments.")
}

# Load packages
load_libs <- c('versioning', 'data.table', 'caret', 'glue')
lapply(load_libs, library, character.only = T) |> invisible()
devtools::load_all(REPO_FP)

# Load configuration object
config <- versioning::Config$new(CONFIG_FP)

results_dir <- config$get_dir_path('model_results')
if(!dir.exists(results_dir)) stop("Results directory does not exist")
out_file_base <- glue::glue("{results_dir}/{survey_id}_{METHOD}_{imp_ii}_h{holdout_id}")

## Fit model

tictoc::tic(paste("Imputation", imp_ii))

# Load imputed data
model_data <- glue::glue('{config$get_dir_path("imputed_data")}/{survey_id}_imp{imp_ii}.csv') |>
  versioning::autoread()

# Skip processing for reserved (special) fields and fields with any NAs
age_group_fields <- grep('age_groupa', colnames(model_data), value = T)
special_fields <- c(
  age_group_fields,
  config$get('fields')[c('ids', 'outcome', 'hidden')] |> unlist(),
  'holdout'
)
feature_fields <- setdiff(colnames(model_data), special_fields)
# Drop any fields with missing values
for(ff in feature_fields){
  # Normalize
  model_data[[ff]] <- (model_data[[ff]] - mean(model_data[[ff]])) / sd(model_data[[ff]])
  # Drop NAs
  if(any(is.na(model_data[[ff]]))) feature_fields <- setdiff(feature_fields, ff)
}
message(glue::glue(
  "Running model with {length(age_group_fields)} age groups and ",
  "{length(feature_fields)} features."
))

# Determine test and train data based on the holdout
if(holdout_id == 0L){
  train_data <- copy(model_data)
  test_data <- copy(model_data)
} else {
  train_data <- copy(model_data[holdout != holdout_id, ])
  test_data <- copy(model_data[holdout == holdout_id, ])
}
rm(model_data)

# Outcome field should be a factor
outcome_field <- config$get('fields', 'outcome')
train_data[[outcome_field]] <- factor(train_data[[outcome_field]])

# Fit full model
model_terms_list <- list(
  form = glue::glue(
    "{outcome_field} ~ ",
    "{paste(c(age_group_fields, feature_fields), collapse = ' + ')}"
  ) |> as.formula(),
  data = train_data,
  method = METHOD,
  trControl = caret::trainControl(method = 'cv', number = 5)
)
if(METHOD == 'glm') model_terms_list$family <- 'binomial'
model_fit <- do.call(what = caret::train, args = model_terms_list)

# Save out test model predictions
full_model_predictions <- suppressWarnings(data.table::data.table(
  predicted = predict(model_fit, newdata = test_data, type = 'prob')[, as.character(1)],
  observed = test_data[[outcome_field]]
))
data.table::fwrite(full_model_predictions, file = glue::glue("{out_file_base}_predictions.csv"))

# We don't need to run the Shapley decomposition on the holdouts
if(holdout_id != 0L) quit(save = 'no', status = 0)


## Run shapley decomposition ------------------------------------------------------------>

# Create marginal imputer
marginal_imputer <- mch.ml::MarginalImputer$new(
  model = model_fit,
  features_table = test_data,
  outcomes = test_data[[outcome_field]],
  default_features = age_group_fields,
  loss_fun = mch.ml::get_loss_function('L2')
)

# Create permutation sampler object
model_sampler <- mch.ml::PermutationSampler$new(
  imputer = marginal_imputer,
  convergence_threshold = 0.125,
  verbose = TRUE
)

# Save full model loss and null model loss
loss_types_dt <- data.table::data.table(
  model_type = c("Full", "Null"),
  loss = c(model_sampler$full_model_loss, model_sampler$null_model_loss)
)
data.table::fwrite(loss_types_dt, file = glue::glue("{out_file_base}_loss_types.csv"))

# Sample permutations
model_sampler$sample(run_all = FALSE, min_iterations = 3L)

# Save feature importance results
message("Finished decomposition after ", max(model_sampler$importance_tracker$n_obs), " iterations.")
shapley_vals <- model_sampler$get_shapley_vals()
shapley_vals_dt <- data.table::data.table(
  survey_id = survey_id,
  method = METHOD,
  imputation = imp_ii,
  feature = marginal_imputer$features,
  shapley_value = shapley_vals
)
# Normalize for negative values
total_improvement <- shapley_vals_dt[, sum(shapley_value)]
(shapley_vals_dt
  [shapley_value < 0, shapley_value := 0L ]
  [, shapley_value_norm := shapley_value / sum(shapley_value)]
  [, shapley_value := shapley_value_norm * total_improvement ]
)
data.table::fwrite(shapley_vals_dt, file = glue::glue("{out_file_base}_shapley_vals.csv"))

tictoc::toc() # End model imputation
