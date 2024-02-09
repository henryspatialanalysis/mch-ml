#' Run all covariate combinations
#' 
#' @details Run a predictive model using all possible combinations from a list of
#'   covariates and return the loss function for each combination.
#' 
#' @param model_data Model dataset, cleaned of NA values and including both the outcome
#'   and predictor variables
#' @param outcome (`character(1)`) Character naming the field of the outcome to be
#'   predicted
#' @param predictors (`character(N)`) A vector of all candidate models to consider
#' @param loss_function A function that generates average model error given two vectors,
#'   the two observations and the predicted risks.
#' 
#' @return A named list, with names formatted as "cov_a+cov_b+cov_c+..." giving the model
#'   total error for each set of covariates referred to in the name
#' 
#' @seealso [loss_functions] For defining the loss function, [vec_to_name] for converting
#'   a vector of covariates to a character string
#' 
#' @importFrom caret trainControl train
#' @importFrom glue glue
#' @export
run_all_covariate_combinations <- function(
  model_data, outcome, predictors, model_type, loss_function, model_options = list()
){
  # Create all subsets of predictors with uniquely-identifying names
  predictor_power_set <- generate_power_set(predictors)
  power_set_names <- sapply(predictor_power_set, vector_to_names)
  names(predictor_power_set) <- power_set_names

  # Set up a consistent model training control scheme for all runs (5-fold cross-validation)
  trControl <- caret::trainControl(method = 'cv', number = 5)

  model_performance_list <- vector('list', length = length(power_set_names))
  names(model_performance_list) <- power_set_names
  model_data[, dummy__ := 1L ]

  for(ps_name in power_set_names){
    # Create the formula containing only the subset of predictors
    test_predictors <- predictor_power_set[[ps_name]]
    if(length(test_predictors) == 0){
      formula_string <- glue::glue("{outcome} ~ dummy__")
    } else {
      formula_string <- glue::glue("{outcome} ~ {paste(test_predictors, collapse = ' + ')}")
    }
    # Run the candidate model
    model_fit <- do.call(
      what = caret::train,
      args = c(
        list(
          form = as.formula(formula_string),
          data = model_data,
          method = model_type,
          trControl = trControl
        ),
        model_options
      )
    ) |> suppressWarnings()
    # Calculate the model's performance using the loss function and add it to the output
    #  list
    model_performance_list[[ps_name]] <- loss_function(
      model_data[[outcome]],
      predict(model_fit, newdata = model_data)
    )
  }

  # Return the model performance metrics list
  return(model_performance_list)
}


#' Run Shapley decomposition on a set of model results
#' 
#' @details Given a vector of all candidate predictors and the performance of models
#'   using all combinations of those predictors, assign relative importance of each
#'   covariate to overall model performance
#' 
#' @param predictors (`character(N)`) All candidate predictors used in a model
#' @param model_performance_list (`list(N)`) List of model performance (usually decrease
#'   in model error) for all combinations of those predictors
#' 
#' @return Named list with two items:
#'    - 'absolute': Shapley value decomposition of each predictor to model performance
#'    - 'normalized': Same decomposition normalized to sum to one
#' 
#' @seealso [vec_to_name] for converting a vector of covariates to a character string
#' 
#' @export 
run_shapley_decomposition <- function(predictors, model_performance_list){
  predictors <- sort(predictors)
  shapley_abs <- vector('numeric', length = length(predictors))
  names(shapley_abs) <- predictors

  predictor_power_set <- generate_power_set(predictors)

  # The shapley value for each predictor is the average improvement in model performance
  #  across all models that include the predictor, compared to those models with the
  #  predictor removed
  for(predictor in predictors){
    check_combos <- predictor_power_set |> Filter(f = function(a_set) predictor %in% a_set)
    total_improvement <- 0
    for(check_combo in check_combos){
      error_with <- model_performance_list[[vector_to_names(check_combo)]]
      error_without <- model_performance_list[[vector_to_names(setdiff(check_combo, predictor))]]
      total_improvement <- total_improvement + (error_without - error_with)
    }
    # Average across the number of models where the predictor was used
    shapley_abs[predictor] <- total_improvement / length(check_combos)
  }
  # Normalize
  shapley_norm <- shapley_abs / sum(shapley_abs)
  return(list(absolute = shapley_abs, normalized = shapley_norm))
}
