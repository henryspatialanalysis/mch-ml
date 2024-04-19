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

survey_id <- 'GH2022DHS'
imp_ii <- 1
METHOD <- 'rf'

# Load packages
load_libs <- c('versioning', 'data.table', 'caret', 'glue')
lapply(load_libs, library, character.only = T) |> invisible()
devtools::load_all(REPO_FP)

# Load configuration object
config <- versioning::Config$new(CONFIG_FP)

results_dir <- config$get_dir_path('model_results')
if(!dir.exists(results_dir)) stop("Results directory does not exist")

## Fit model

tictoc::tic(paste("Imputation", imp_ii))

# Load imputed data and prepare features
model_data <- glue::glue('{config$get_dir_path("imputed_data")}/{survey_id}_imp{imp_ii}.csv') |>
  versioning::autoread()
age_group_fields <- grep('age_groupa', colnames(model_data), value = T)
feature_fields <- setdiff(
  colnames(model_data),
  c(unlist(config$get('fields')), age_group_fields)
)
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
# Outcome field should be a factor
outcome_field <- config$get('fields', 'outcome')
model_data[[outcome_field]] <- factor(model_data[[outcome_field]])


# Fit full model and create marginal imputer
model_fit <- caret::train(
  form = glue::glue(
    "{outcome_field} ~ ",
    "{paste(c(age_group_fields, feature_fields), collapse = ' + ')}"
  ) |> as.formula(),
  data = model_data,
  method = METHOD,
  trControl = caret::trainControl(method = 'cv', number = 5)
)
model_data[[outcome_field]] <- as.numeric(as.character(model_data[[outcome_field]]))
marginal_imputer <- mch.ml::MarginalImputer$new(
  model = model_fit,
  features_table = model_data,
  outcomes = model_data[[outcome_field]],
  default_features = age_group_fields,
  loss_fun = mch.ml::get_loss_function('L2')
)

# Sample permutations
model_sampler <- mch.ml::PermutationSampler$new(
  imputer = marginal_imputer,
  convergence_threshold = 0.1,
  verbose = TRUE
)
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
fwrite(shapley_vals_dt, file = glue::glue("{results_dir}/{survey_id}_{METHOD}_{imp_ii}.csv"))

tictoc::toc() # End model imputation
