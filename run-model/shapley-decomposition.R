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
REPO_FP <- '~/repos/usaid-mch-ml/r-package'
CONFIG_FP <- file.path('~/repos/usaid-mch-ml/config.yaml')
METHOD <- 'glm'
survey_id <- 'GH2022DHS'

# Load packages
load_libs <- c('versioning', 'data.table', 'caret', 'glue')
lapply(load_libs, library, character.only = T) |> invisible()
devtools::load_all(REPO_FP)

# Load configuration object
config <- versioning::Config$new(CONFIG_FP)

results_dir <- config$get_dir_path('model_results')
dir.create(results_dir)
config$write_self('model_results')

## Run by survey id
survey_ids <- config$get("survey_ids")
n_imputations <- config$get("n_imputations")
for(imp_ii in seq_len(n_imputations)){
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
    preProcess = c("center", "scale"),
    method = METHOD,
    trControl = caret::trainControl(method = 'cv', number = 5)
  )
  model_data[[outcome_field]] <- as.numeric(as.character(model_data[[outcome_field]]))
  marginal_imputer <- mch.ml::MarginalImputer$new(
    model = model_fit,
    features_table = model_data,
    outcomes = model_data[[outcome_field]],
    default_features = age_group_fields,
    loss_fun = mch.ml::get_loss_function('BCE')
  )

  # Sample permutations
  model_sampler <- mch.ml::PermutationSampler$new(
    imputer = marginal_imputer,
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
    feature = names(shapley_vals),
    shapley_value = shapley_vals
  )[, shapley_value_norm := shapley_value / sum(shapley_value) ]
  fwrite(shapley_vals_dt, file = glue::glue("{results_dir}/{survey_id}_{METHOD}_{imp_ii}.csv"))

  tictoc::toc() # End model imputation
}
