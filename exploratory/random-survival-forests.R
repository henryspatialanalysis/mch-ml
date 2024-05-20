## #######################################################################################
## 
## TESTING RANDOM SURVIVAL FORESTS: FITTING AND PREDICTION
## 
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 14 May 2024
## PURPOSE: Test random survival forest package
## 
## #######################################################################################

library(randomForestSRC); library(data.table)

## Run random survival forest with example data ----------------------------------------->

# Load example data
data(peakVO2, package = "randomForestSRC")
training_data <- as.data.table(peakVO2)
# Train model
obj <- randomForestSRC::rfsrc(
  formula = Surv(ttodead,died)~.,
  data = training_data,
  ntree = 1000,
  nodesize = 5,
  nsplit = 50,
  ntime = 1:10,
  importance = FALSE
)
# Create predictions
predictions <- predict(object = obj, newdata = training_data)

## Run random survival forest with real data -------------------------------------------->

example_data <- fread('~/efs-mount/usaid/imputed_data/GH2022DHS_imp1.csv')
special_fields <- list(
  outcome = 'died',
  holdout = 'holdout_id',
  ids = c('cluster', 'bh_year', 'hh_id', 'w_id', 'birth_id', 'admin1_code', 'admin1_name'),
  hidden = c(
    'age_group', 'c_birth_year', 'c_alive', 'c_age_months', 'c_died_pnn', 'b_10y_prior',
    'w_sample_weight', 'int_month_cmc', 'c_died_age_months', 'c_birth_int', "w_born_cmc"
  )
)

feature_fields <- setdiff(colnames(example_data), unlist(special_fields)) |>
  grep(pattern = 'age_groupa', invert = TRUE, value = TRUE)
model_data <- example_data[
  , c(lapply(.SD, mean), .(died = max(died), age_group = max(age_group))),
  .SDcols = feature_fields, by = setdiff(special_fields$ids, 'bh_year')
]
# Merge on the estimated age at death, with an exception for the oldest age group
age_group_merge_table <- data.table::data.table(
  age_group = c('a01mo', 'a06mo', 'a12mo', 'a24mo', 'a36mo', 'a48mo'),
  ttodead = c(2.5, 8, 17, 29, 41, 53),
  age_group_idx = 1:6
)
model_data[age_group_merge_table, ttodead := i.ttodead, on = 'age_group']
model_data[(died == 0L) & age_group == 'a48mo', ttodead := 59 ]
# Add age group index to the original data for prediction purposes
example_data[age_group_merge_table, age_group_idx := i.age_group_idx, on = 'age_group']

# Prepare feature fields
for(ff in feature_fields){
  # Normalize
  feature_mean <- mean(model_data[[ff]])
  feature_sd <- sd(model_data[[ff]])
  if(!is.na(feature_mean) & !is.na(feature_sd)){
    model_data[[ff]] <- (model_data[[ff]] - feature_mean) / feature_sd
    example_data[[ff]] <- (example_data[[ff]] - feature_mean) / feature_sd
  } else {
    # Drop NAs
    feature_fields <- setdiff(feature_fields, ff)
  }
}

# Run model fit
formula_string <- glue::glue('Surv(ttodead, died) ~ {paste(feature_fields, collapse = "+")}')
model_fit <- randomForestSRC::rfsrc.fast(
  formula = as.formula(formula_string),
  data = model_data,
  ntree = 1000,
  nodesize = 5,
  nsplit = 50,
  ntime = c(5, 11, 23, 35, 47, 59),
  importance = FALSE,
  forest = TRUE
)


# Function for predicting age-group-specific nQx
predict_fun <- function(object, newdata){
  cov_matrix <- as.data.frame(newdata[, ..feature_fields])
  # Get nPx - probability of surviving to next age group = 1 - nQx
  npx_matrix <- randomForestSRC::predict.rfsrc(
    object = object, newdata = cov_matrix, proximity = F, distance = F, forest.wt = F,
    outcome ='train'
  )$survival
  # In all age bins except the first, survival probabilities are conditional on
  #  surviving the previous age bin
  age_bins <- seq_len(ncol(npx_matrix))
  for(ii in rev(age_bins[-1])){
    npx_matrix[, ii] <- npx_matrix[, ii] / npx_matrix[, ii - 1]
  }
  predictions <- rep(NA_real_, times = nrow(example_data))
  for(ii in seq_len(ncol(npx_matrix))){
    which_in_bin <- which(example_data$age_group_idx == ii)
    predictions[which_in_bin] <- 1 - npx_matrix[which_in_bin, ii]
  }
  return(predictions)
}

# Create new predictions
example_data$predicted <- predict_fun(object = model_fit, newdata = example_data)
