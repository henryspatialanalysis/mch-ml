## #######################################################################################
##
## SHAPLEY DECOMPOSITION
##
## AUTHOR: Nat Henry
## CREATED: 7 April 2024
## PURPOSE: Shapley decomposition code for full model data
## 
## #######################################################################################


## SETUP -------------------------------------------------------------------------------->

# Set globals
REPO_FP <- '~/repos/usaid-mch-ml/r-package'
CONFIG_FP <- file.path('~/repos/usaid-mch-ml/config.yaml')

parser <- argparse::ArgumentParser()
parser$add_argument('--imp', type = 'integer')
parser$add_argument('--method', type = 'character')
parser$add_argument('--survey', type = 'character')
parser$add_argument('--holdout', default = 0, type = 'integer')
args <- parser$parse_args(commandArgs(trailingOnly = TRUE))
imp_ii <- args$imp # Imputation version
METHOD <- args$method # 'plr', 'xgbTree', 'treebag', 'rf', 'LogitBoost', 'gam', 'glm', 'ada'
survey_id <- args$survey # e.g. 'GH2022DHS'
holdout <- args$holdout

if(is.null(holdout) | is.null(imp_ii) | is.null(METHOD) | is.null(survey_id)){
  stop("Issue with command line arguments.")
}

# Load packages
load_libs <- c('versioning', 'data.table', 'caret', 'glue', 'randomForestSRC')
lapply(load_libs, library, character.only = T) |> invisible()
devtools::load_all(REPO_FP)

# Load configuration object
config <- versioning::Config$new(CONFIG_FP)

results_dir <- config$get_dir_path('model_results')
if(!dir.exists(results_dir)) stop("Results directory does not exist")
out_file_base <- glue::glue("{results_dir}/{survey_id}_{METHOD}_{imp_ii}_h{holdout}")

tictoc::tic(paste("Imputation", imp_ii))


## DATA PREPARATION --------------------------------------------------------------------->

# Load imputed data
model_data <- glue::glue('{config$get_dir_path("imputed_data")}/{survey_id}_imp{imp_ii}.csv') |>
  versioning::autoread()

# Skip processing for reserved (special) fields and fields with any NAs
age_group_fields <- grep('age_groupa', colnames(model_data), value = T)
special_fields <- c(
  age_group_fields,
  config$get('fields')[c('ids', 'outcome', 'hidden')] |> unlist(),
  'holdout_id'
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
if(holdout == 0L){
  train_data <- copy(model_data)
  test_data <- copy(model_data)
} else {
  train_data <- copy(model_data[holdout_id != holdout, ])
  test_data <- copy(model_data[holdout_id == holdout, ])
}
rm(model_data)

# Special data preparation for random survival forests, not fit in `caret`
if(METHOD == 'rsf'){
  collapse_fields <- setdiff(config$get('fields', 'ids'), 'bh_year')
  train_data <- train_data[
    , c(lapply(.SD, mean), died = max(died), age_group = max(age_group)),
    .SDcols = feature_fields, by = collapse_fields
  ]
  # Merge on the age group at death, with an exception for the oldest age group
  age_group_merge_table <- data.table::data.table(
    age_group = c('a01mo', 'a06mo', 'a12mo', 'a24mo', 'a36mo', 'a48mo'),
    ttodead = 1:6,
    age_group_idx = 1:6
  )
  train_data[age_group_merge_table, ttodead := i.ttodead, on = 'age_group']
  # Add age group index to the original data for prediction purposes
  test_data[age_group_merge_table, age_group_idx := i.age_group_idx, on = 'age_group']
}

outcome_field <- config$get('fields', 'outcome')


## FIT MODEL ---------------------------------------------------------------------------->

# Different model fit and prediction functions for random survival forests vs. caret
if(METHOD == 'rsf'){
  # CASE: Random survival forests
  # Fit full model
  tictoc::tic('Parameter tuning')
  rsf_formula <- stats::as.formula(glue::glue("Surv(ttodead, {outcome_field}) ~ ."))
  best_params <- randomForestSRC::tune.rfsrc(
    formula = rsf_formula,
    data = as.data.frame(train_data)[, c('ttodead', outcome_field, feature_fields)],
    nodesizeTry = seq(6, 20, by = 2),
    ntree = 500L,
    ntime = NULL,
    doBest = FALSE,
    trace = FALSE
  )
  tictoc::toc()
  model_fit <- randomForestSRC::rfsrc(
    formula = rsf_formula,
    data = as.data.frame(train_data)[, c('ttodead', outcome_field, feature_fields)],
    mtry = best_params$optimal['mtry'],
    nodesize = best_params$optimal['nodesize'],
    ntree = 500L,
    ntime = NULL,
    importance = (holdout == 0L)
  )

  # Save out RSF-specific feature importance
  if(!is.null(model_fit$importance)){
    importance_table <- data.table::data.table(
      feature = names(model_fit$importance),
      importance = model_fit$importance
    )
    data.table::fwrite(importance_table, file = glue::glue('{out_file_base}_vimp.csv'))
  }

  # Set prediction function
  prediction_fun <- function(object, newdata){
    cov_matrix <- as.data.frame(newdata[, ..feature_fields])
    # Get nPx - probability of surviving to next age group = 1 - nQx
    predictions <- randomForestSRC::predict.rfsrc(
      object = object, newdata = cov_matrix
    )
    keep_intervals <- which(predictions$time.interest %in% age_group_merge_table$ttodead)
    npx_matrix <- predictions$survival[, keep_intervals]
    # In all age bins except the first, survival probabilities are conditional on
    #  surviving the previous age bin
    age_bins <- seq_len(ncol(npx_matrix))
    for(ii in rev(age_bins[-1])){
      npx_matrix[, ii] <- npx_matrix[, ii] / npx_matrix[, ii - 1]
    }
    # Mortality risk = 1 - survival probability
    nqx_matrix <- 1 - npx_matrix
    q_predictions <- rep(NA_real_, times = nrow(newdata))
    for(ii in seq_len(ncol(npx_matrix))){
      which_in_bin <- which(newdata$age_group_idx == ii)
      q_predictions[which_in_bin] <- 1 - npx_matrix[which_in_bin, ii]
    }
    q_predictions[is.na(q_predictions)] <- mean(na.omit(q_predictions))
    return(q_predictions)
  }
} else {
  # CASE: caret models
  # Outcome field should be a factor
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

  # Set prediction function
  prediction_fun <- function(object, newdata){
    stats::predict(object, newdata = newdata, type = "prob")[, as.character(1)]
  }  
}

# Save out test model predictions
full_model_predictions <- suppressWarnings(data.table::data.table(
  predicted = prediction_fun(object = model_fit, newdata = test_data),
  observed = test_data[[outcome_field]]
))
data.table::fwrite(full_model_predictions, file = glue::glue("{out_file_base}_predictions.csv"))

# We don't need to run the Shapley decomposition on the holdouts
if(holdout != 0L) quit(save = 'no', status = 0)


## Run shapley decomposition ------------------------------------------------------------>

# Create marginal imputer
marginal_imputer <- mch.ml::MarginalImputer$new(
  model = model_fit,
  features_table = test_data,
  feature_fields = feature_fields,
  outcomes = test_data[[outcome_field]],
  default_features = if(METHOD == 'rsf') character(0) else age_group_fields,
  loss_fun = mch.ml::get_loss_function('L2'),
  prediction_fun = prediction_fun
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
