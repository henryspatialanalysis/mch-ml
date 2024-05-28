## #######################################################################################
## 
## VISUALIZE MODEL PERFORMANCE USING CONFUSION MATRICES
## 
## #######################################################################################

load_pkgs <- c('data.table', 'ggplot2', 'scales', 'glue', 'pROC')
lapply(load_pkgs, library, character.only = T) |> suppressPackageStartupMessages() |> invisible()

working_dir <- '~/temp_data/usaid-mch-ml'
all_methods <- c("treebag", "rf", "glm")
results_version <- '20240521'
imputed_data_version <- '20240516'
holdouts <- 1:5
imputations <- 1:5
id_cols <- c('cluster', 'hh_id', 'w_id', 'birth_id')

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "NG2018DHS", "PH2022DHS", "SN2019DHS"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Nigeria", "Philippines", "Senegal")
)



## Create ROC curves and get AUC for each survey and method ----------------------------->

model_dir <- file.path(working_dir, 'model_results', results_version)
imputed_data_dir <- file.path(working_dir, 'imputed_data', imputed_data_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)

# Helper functions to load data and results files
imputed <- function(svy, imp){
  data.table::fread(glue::glue("{imputed_data_dir}/{svy}_imp{imp}.csv"))
}
preds <- function(svy, method, imp, holdout){
  data.table::fread(glue::glue("{model_dir}/{svy}_{method}_{imp}_h{holdout}_predictions.csv"))
}

# Names of all in-sample and out-of-sample columns
is_cols <- paste0('is_', imputations)
oos_cols <- paste0('oos_', imputations)

# Iterate by survey
full_results <- lapply(all_surveys[, .I], function(svy_ii){
  svy <- all_surveys[svy_ii, survey_id]
  country <- all_surveys[svy_ii, country]
  # Load input data
  # Can be any imputed version -- outcomes are complete and the same across imputations
  input_data <- imputed(svy, 1)  
  # Iterate through methods
  check_methods <- if(country == 'Nigeria') setdiff(all_methods, 'rf') else all_methods
  method_results <- lapply(check_methods, function(method){
    # Create a clean dataset specific to this method
    method_data <- data.table::copy(input_data)
    # Iterate through imputations
    imputation_results <- lapply(imputations, function(imp){
      is <- preds(svy, method, imp, 0)
      method_data$is <- is$predicted
      method_data$oos <- NA_real_
      for(holdout in holdouts){
        oos <- preds(svy, method, imp, holdout)
        in_holdout <- method_data[, holdout_id == holdout]
        method_data$oos[in_holdout] <- oos$predicted
      }
      keep_cols <- c(id_cols, 'died', 'is', 'oos')
      return(method_data[, ..keep_cols])
    }) |> data.table::rbindlist()
    imputation_results$method <- method
    return(imputation_results)
  }) |> data.table::rbindlist()
  # Add survey and country metadata
  method_results$survey_id <- svy
  method_results$country <- country
  return(method_results)
}) |> data.table::rbindlist()

# Combine from age groups to individuals
combine_q <- function(q_vec) 1 - prod(1 - q_vec)
indiv_results <- full_results[
  , c(.(died = max(died)), lapply(.SD, combine_q)),
  .SDcols = c('is', 'oos'),
  by = c('survey_id', 'country', 'method', id_cols)
]

# Get AUC for the individual-level results
get_auc <- function(obs, pred) as.numeric(pROC::roc(obs, pred, auc = T)$auc) |> suppressMessages()

indiv_auc <- indiv_results[
  , .(is_auc = get_auc(died, is), oos_auc = get_auc(died, oos)),
  by = c('survey_id', 'country', 'method')
]
fwrite(indiv_auc, file = file.path(viz_dir, 'auc_results.csv'))