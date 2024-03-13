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
load_libs <- c('versioning', 'data.table', 'caret', 'ggplot2')
lapply(load_libs, library, character.only = T) |> invisible()

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))

# Maximum dimensions to test: 35k observations by 100 variables ------------------------->

n_obs <- 3.5e4
features_space <- c(seq(5, 25, by = 5), seq(30, 100, by = 10))
interaction_terms <- FALSE

for(n_features in features_space){
  message("Running with ", n_features, " features")
  tictoc::tic(n_features)
  # Simulate the dataset
  model_data <- data.table(error = rnorm(n_obs))
  model_data[, outcome := error ]

  for(ii in seq_len(n_features)){
    vv <- paste0('v', ii)
    model_data[[vv]] <- rnorm(n_obs)
    model_data$outcome <- model_data$outcome + rnorm(1) * model_data[[vv]]
  }

  tictoc::tic(paste0(n_features, ": model fitting"))
  model_fit <- caret::train(
    form = glue::glue("outcome ~ {paste0('v', seq_len(n_features), collapse = ' + ')}") |>
      as.formula(),
    data = model_data,
    method = 'lm',
    trControl = caret::trainControl(method = 'cv', number = 5)
  )
  tictoc::toc(log = TRUE)

  tictoc::tic(paste0(n_features, ": shapley decomposition"))
  marginal_imputer <- mch.ml::MarginalImputer$new(
    model = model_fit,
    features_table = model_data,
    outcomes = model_data$outcome,
    loss_fun = mch.ml::get_loss_function('L2')
  )
  model_sampler <- mch.ml::PermutationSampler$new(imputer = marginal_imputer)
  model_sampler$sample(run_all = FALSE, min_iterations = 10L)
  message("Finished decomposition after ", max(model_sampler$importance_tracker$n_obs), " iterations.")
  tictoc::toc(log = TRUE)
  tictoc::toc(log = TRUE)
}

timer_results <- tictoc::tic.log(format = FALSE) |> 
  lapply(as.data.frame) |> 
  lapply(data.table::as.data.table) |> 
  data.table::rbindlist()

timer_subset <- (timer_results
  [!grepl(':', msg), ]
  [, .(n_features = as.integer(msg), elapsed = toc - tic) ]
)

# Quick plot
timer_fig <- ggplot(data = timer_subset, aes(x = n_features, y = elapsed)) +
  geom_line() + 
  geom_point(pch = 18) +
  labs(
    x = 'Number of features',
    y = 'Time elapsed (s)',
    title = "Shapley approximation: fitting time by number of features",
    subtitle = glue::glue("Dataset with {scales::comma(n_obs)} observations")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
png(
  "~/temp_data/usaid-mch-ml/viz/shapley_fitting_timer.png",
  height = 4, width = 6.5, units = 'in', res = 300
)
print(timer_fig)
dev.off()
