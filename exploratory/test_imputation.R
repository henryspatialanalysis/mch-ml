## #######################################################################################
##
## MULTIPLE IMPUTATION EXAMPLE
##
## CREATED: 14 March 2024
## PURPOSE: Example of multiple imputation using the `mice` package based on a test
##  dataset
##
## #######################################################################################

library(data.table); library(mice); library(ggplot2)

# Create a test dataset
n_obs <- 1e3
full_data <- data.table::data.table(a = rnorm(n_obs), b = rnorm(n_obs), c = rnorm(n_obs))
# Add variables that are based on various combinations of the starting variables
(full_data
  [, narrow_noise := .1 * a + .4 * b - c + .1 * rnorm(n_obs)]
  [, moderate_noise := a + b + .5 * rnorm(n_obs) ]
  [, wide_noise := .8 * a + rnorm(n_obs) ]
)

# Create a censored dataset with aspects of d, e, and f, removed
censored_data <- data.table::copy(full_data)
censor_cols <- c('narrow_noise', 'moderate_noise', 'wide_noise')
# Get rows to drop for each censored column
censored_ids <- lapply(censor_cols, function(xx) sort(sample(seq_len(n_obs), size = 40, replace = F)))
names(censored_ids) <- censor_cols
for(censor_col in censor_cols){
  censored_data[censored_ids[[censor_col]], (censor_col) := NA_real_ ]
}

# Impute the censored data using the MICE package
imputed_data_list <- mice::mice(
  data = censored_data,
  m = 5,
  method = 'midastouch'
)

# Get the list of imputed values by dataset and compare to the actual (censored) values
comparison_table <- lapply(censor_cols, function(col){
  imputed_matrix <- imputed_data_list$imp[[col]]
  return(data.table::data.table(
    data_col = col,
    impute_version = rep(colnames(imputed_matrix), each = nrow(imputed_matrix)),
    true = rep(full_data[censored_ids[[col]], get(col)], times = ncol(imputed_matrix)),
    imputed = unlist(imputed_matrix)
  ))
}) |> data.table::rbindlist()

# Plot imputed data
imputed_plot <- ggplot(data = comparison_table, aes(x = true, y = imputed, color = impute_version)) + 
  facet_wrap("data_col", nrow = 1) + 
  geom_abline(intercept = 0, slope = 1, color = '#666666', linetype = 3, linewidth = .8) + 
  geom_point() + 
  labs(
    title = 'Exploring multiple imputation for a toy dataset',
    x = 'True (censored) value',
    y = 'Recovered from multiple imputation',
    color = 'Imputation\nround'
  ) +
  theme_bw()
png(
  "~/temp_data/usaid-mch-ml/viz/imputation_example.png",
  height = 4.5, width = 10, units = 'in', res = 300
)
print(imputed_plot)
dev.off()
