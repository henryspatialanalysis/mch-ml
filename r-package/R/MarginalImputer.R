#' R6 class used for imputation in Shapley decomposition 
#'
#' @details Stores the full model and fitting dataset. Predicts on subsets of the input
#'  data, replacing removed features with their marginal distributions. 
#'
#' @seealso https://github.com/iancovert/sage/blob/master/sage/imputers.py#L44
#' 
#' @importFrom R6 R6Class
#' @export
MarginalImputer <- R6::R6Class(
  "MarginalImputer",
  public = list(
    #' @field model (`predict`) Fitted model that can be used for prediction
    model = NULL,
    #' @field features_table Table of all possible features to use for prediction
    features_table = NULL,
    #' @field features (`character(N)`) Names of model features used in the full model
    features = NULL,
    #' @field outcomes (`numeric(nrow(features_table))`) Vector of outcomes corresponding
    #'   to each row of `features_table`
    outcomes = NULL,
    #' @field loss_fun (function) Loss function taking two numeric vectors of equal
    #'   length, outcomes and predicted outcomes, and returning the total model loss
    loss_fun = NULL,

    #' @description Create a new MarginalImputer object
    #' 
    #' @param model Fitted statistical model from which new predictions can be made using
    #'   the `predict()` function.
    #' @param features_table (`data.frame`) Table with columns corresponding to features
    #'   in the `model`
    #' @param outcomes (`numeric(nrow(features_table))`) Vector of outcomes corresponding
    #'   to each row of `features_table`
    #' @param loss_fun (function) Loss function taking two numeric vectors of equal
    #'   length, outcomes and predicted outcomes, and returning the total model loss
    initialize = function(model, features_table, outcomes, loss_fun){
      # Add as fields
      self$model <- model
      self$features_table <- features_table
      self$features <- model$terms |> attr('term.labels')
      if(is.null(self$features)) stop("Issue with model - no features found")
      missing_features <- setdiff(self$features, colnames(features_table))
      if(length(missing_features) > 0){
        stop("Missing some features in data: ", paste(missing_features, collapse = ', '))
      }
      self$outcomes <- outcomes
      self$loss_fun <- loss_fun
    },

    #' @description Fit the predictive model with a subset of features
    #' 
    #' @param feature_subset (`character(N)`) A subset of features to use for this model
    #'   version. All other features will be marginalized out.
    predict = function(feature_subset){
      # If a vector of integer indices was passed, convert to character field names
      if(is.numeric(feature_subset)){
        feature_subset <- self$features[feature_subset]
      }
      # Check for erroneous function arguments
      missing_features <- setdiff(feature_subset, self$features)
      if(length(missing_features) > 0) stop(
        "Subset features not in original model: ",
        paste(missing_features, collapse = ', ')
      )
      # Marginalize out any of the features NOT in the subset
      marginal_features <- setdiff(self$features, feature_subset)
      subset_data <- copy(self$features_table)
      for(marg in marginal_features) subset_data[[marg]] <- mean(subset_data[[marg]])
      # Return predictions based on the subset data
      return(predict(self$model, newdata = subset_data))
    },

    #' @description Get total model loss for a subset of features
    #' 
    #' @param feature_subset (`character(N)`) A subset of features to use for this model
    #'   version. All other features will be marginalized out.
    #' 
    #' @seealso wrapper function for MarginalImputer$predict()
    get_loss = function(feature_subset){
      predictions <- self$predict(feature_subset)
      loss <- self$loss_fun(self$outcomes, predictions)
      return(loss)
    }
  )
)
