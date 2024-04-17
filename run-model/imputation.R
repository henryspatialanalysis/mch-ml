## #######################################################################################
##
## FILL MISSING ROWS IN PREPARED DATA USING MULTIPLE IMPUTATION
##
## AUTHOR: Nat Henry
## CREATED: 2 April 2024
## PURPOSE: Multiple imputation to fill missing data
## 
## #######################################################################################

## Setup
# Set config file path
CONFIG_FILE <- "~/repos/usaid-mch-ml/config.yaml"

load_packages <- c('versioning', 'data.table', 'mice')
lapply(load_packages, library, character.only = T) |> invisible()

config <- versioning::Config$new(CONFIG_FILE)
imputed_dir <- config$get_dir_path('imputed_data')
dir.create(imputed_dir)


## Impute data for each prepared dataset

survey_ids <- config$get('survey_ids')
for(survey_id in survey_ids){
  message("\n\n*** Performing multiple imputation for ", survey_id, " ***")

  # Load prepared data
  prepared_data <- glue::glue(
    "{config$get_dir_path('prepared_data')}/analysis_dataset_{survey_id}.csv"
  ) |> versioning::autoread()

  # Split out age group columns
  age_groups_dt <- stats::model.matrix(~ 0 + age_group, data = prepared_data[, .(age_group)]) |>
    data.table::as.data.table()
  prepared_data <- cbind(prepared_data, age_groups_dt)

  # Drop columns below completeness threshold
  completeness <- prepared_data[, lapply(.SD, function(x) mean(!is.na(x)))] |>
    as.list()
  to_drop <- names(completeness)[which(completeness < config$get('completeness_threshold'))]
  message("Dropping fields with low completeness: ", paste(to_drop, collapse=', '), "\n")
  prepared_data[, (to_drop) := NULL ]

  # Drop columns that have only one value and are therefore not predictive
  one_val_check <- prepared_data[, lapply(.SD, uniqueN, na.rm=T)] |> as.list()
  one_val_cols <- names(one_val_check)[which(one_val_check <= 1L)]
  message("Dropping fields with a single value: ", paste(one_val_cols, collapse=', '), "\n")
  prepared_data[, (one_val_cols) := NULL ]

  do_not_impute_fields <- config$get("fields") |> unlist()
  subset <- copy(prepared_data)[, (do_not_impute_fields) := NULL ]

  # Impute missing values
  n_imputations <- config$get('n_imputations')
  to_fill <- intersect(colnames(subset), names(completeness)[which(completeness < 1.0)])
  message("Imputing missing values for the fields: ", paste(to_fill, collapse =', '))
  imputed_list <- mice::mice(data = subset, m = n_imputations)

  # Add back to the prepared dataset and save
  for(imputation_ii in seq_len(n_imputations)){
    imputed_data <- copy(prepared_data)
    for(filled_col in to_fill) imputed_data[
      is.na(get(filled_col)),
      (filled_col) := imputed_list$imp[[filled_col]][[imputation_ii]]
    ]
    data.table::fwrite(
      imputed_data,
      file = glue::glue("{config$get_dir_path('imputed_data')}/{survey_id}_imp{imputation_ii}.csv")
    )
  }
  # Clean up
  rm(c('prepared_data', 'age_groups_dt', 'imputed_list', 'imputed_data')); gc()
}
