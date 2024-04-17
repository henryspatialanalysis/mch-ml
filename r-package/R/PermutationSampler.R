#' R6 class used for testing feature importance across many feature permutations
#' 
#' @details Tests many subsets of features using MarginalImputer until the relative
#'   feature importance converges, as measured by an ImportanceTracker.
#' 
#' @seealso https://github.com/iancovert/sage/blob/master/sage/permutation_estimator.py
#' 
#' @importFrom R6 R6Class
#' @importFrom gtools permutations
#' @importFrom tictoc tic toc
#' @importFrom glue glue
#' @export
PermutationSampler <- R6::R6Class(
  "PermutationSampler",
  public = list(
    #' @field imputer Object that can be used to fit models with many feature subsets.
    #'   Expects field 'features' and method 'get_loss()'
    imputer = NULL,
    #' @field num_features (`integer(1)`) Maximum number of model features
    num_features = NULL,
    #' @field importance_tracker Feature importance tracker. Expects field 'importance'
    #'   and methods 'update()' and 'converged()'
    importance_tracker = NULL,
    #' @field permutation_threshold (`numeric(1)`) Threshold for unexplained model
    #'   performance below which the model has effectively stopped improving. Expressed as
    #'   the ratio between current loss minus best loss (numerator) and worst loss minus
    #'   best loss (denominator).
    permutation_threshold = NULL,
    #' @field verbose Should permutation sampling yield regular progress messages?
    verbose = FALSE,
    #' @field null_model_loss Loss for a model with no predictive features included
    null_model_loss = NA_real_,
    #' @field full_model_loss Loss for the "full" model with all predictive features
    #'   included
    full_model_loss = NA_real_,

    #' @description Create a new PermutationSampler object
    #' 
    #' @param imputer Object that can be used to fit models with many feature subsets.
    #'   Expects field 'features' and method 'get_loss()'
    #' @param convergence_threshold (`numeric(1)`, default 0.025) Threshold below which
    #'   the importance values have effectively converged. Expressed as the ratio between
    #'   the largest standard deviation across the importance values (numerator) and the
    #'   difference between the highest and lowest importance values (denominator).
    #' @param permutation_threshold (`numeric(1)`, default 0.01) Threshold for unexplained
    #'   model performance below which the model has effectively stopped improving.
    #'   Expressed as the ratio between current loss minus best loss (numerator) and worst
    #'   loss minus best loss (denominator).
    #' @param verbose (`boolean(1)`, default FALSE) Should permutation sampling yield
    #'   regular progress messages?
    initialize = function(
      imputer, convergence_threshold = 0.025, permutation_threshold = 0.01, verbose = FALSE
    ){
      # Assign arguments to object
      self$imputer <- imputer
      self$num_features <- length(imputer$features)
      self$importance_tracker <- ImportanceTracker$new(
        n_var = self$num_features,
        convergence_threshold = convergence_threshold
      )
      self$permutation_threshold <- permutation_threshold
      self$verbose <- verbose
      # Get the loss functions for the null model and the "full" model (all features)
      self$null_model_loss <- imputer$get_loss(character(0))
      self$full_model_loss <- imputer$get_loss(imputer$features)
      # Check that adding features does improve model performance
      if((self$null_model_loss - self$full_model_loss) < 1e-8){
        stop("Issue with model - adding features does not improve over the null model.")
      }
      # Retrn self
      invisible(self)
    },

    #' @description Check if a model has effectively minimized loss
    #' 
    #' @param loss (`numeric(1)`) Loss function from a model with a subset of features
    #' 
    #' @return `logical(1)` Is ((loss - best_loss)/(worst_loss - best_loss)) less than
    #'   the permutation convergence threshold?
    loss_below_threshold = function(loss){
      ratio <- (loss - self$full_model_loss) / (self$null_model_loss - self$full_model_loss)
      return(ratio < self$permutation_threshold)
    },

    #' @description Get feature importance values for one permutation of features
    #' 
    #' @param feature_order (`integer(num_features)`, default NULL) the order to add
    #'   features
    run_one_permutation = function(feature_order = NULL){
      if(is.null(feature_order)){
        feature_order <- sample(seq_len(self$num_features), replace = FALSE)
      } else if(any(sort(feature_order) != seq_len(self$num_features))){
        stop("An integer vector with the order of features is expected")
      }
      # Set up a vector to store importance for each added feature
      losses <- c(self$null_model_loss, rep(NA_real_, times = self$num_features))
      ii <- 1
      # Iteratively calculate loss function when adding each additional feature
      while((ii < self$num_features) & !self$loss_below_threshold(losses[ii])){
        losses[ii + 1] <- self$imputer$get_loss(feature_order[seq_len(ii)])
        if(self$verbose) message(".", appendLF = FALSE)
        ii <- ii + 1
      }
      message("")
      # If the stopping condition is met early, split the tiny bit of remaining loss
      #  between the remaining items
      na_positions <- which(is.na(losses))
      best_fitted_loss <- min(losses, na.rm = T)
      best_loss <- min(best_fitted_loss, self$full_model_loss)
      if(length(na_positions) == 1){
        losses[na_positions] <- best_loss
      } else if(length(na_positions) > 1){
        losses[na_positions] <- seq(
          best_fitted_loss,
          best_loss,
          length.out = length(na_positions)
        )
      }
      # Relative importance is calculated as the difference between the fitted values
      #  and the *worst* loss
      worst_loss <- max(c(losses, self$null_model_loss), na.rm = T)
      relative_losses <- worst_loss - losses
      feature_importance_scores <- diff(relative_losses)
      # Add feature importance to the importance tracker
      self$importance_tracker$update(
        scores = feature_importance_scores,
        which_sampled = feature_order
      )
      invisible(NULL)
    },

    #' @description Sample across many (or all) feature importance permutations until the
    #'   importance values are fully described
    #' 
    #' @param run_all (`logical(1)` or "detect", default "detect") Should all permutations
    #'   of importance values be run rather than sampling from the universe of model
    #'   permutations? If "detect", only runs the full set of permutations for models with
    #'   7 features or fewer.
    #' @param min_iterations (`numeric(1)`, default 100) Minimum number of samples to take
    #'   before checking for feature importance convergence.
    #' @param max_iterations (`numeric(1)`, default 1e7) Maximum number of samples to take
    #'   before stopping the sampler, even if it has not converged.
    sample = function(run_all = 'detect', min_iterations = 100, max_iterations = 1e7){
      if(run_all == 'detect') run_all <- (self$num_features <= 7L)

      # Optionally set timer for permutation sampling
      if(self$verbose) tictoc::tic("Permutation sampling")

      if(run_all){
        # CASE: Running all possible variable permutations
        permutations <- gtools::permutations(n = self$num_features, r = self$num_features)
        if(self$verbose) message("Running all ", nrow(permutations), " variable permutations.")
        for(perm_ii in seq_len(nrow(permutations))){
          self$run_one_permutation(feature_order = permutations[perm_ii, ])
        }
      } else {
        if(self$verbose) message("Sampling from the universe of variable permutations")
        # CASE: Sampling feature permutations until importance values have converged or
        #  the iteration limit is reached
        perm_ii <- 1
        while(
          (perm_ii <= max_iterations) & 
          ((perm_ii <= min_iterations) | !self$importance_tracker$converged())
        ){
          self$run_one_permutation()
          if((perm_ii %% 1000 == 0) & self$verbose){
            # Optionally add logging message
            c_ratio <- self$importance_tracker$get_convergence_ratio()
            message(glue::glue(
              "After {perm_ii} permutations, convergence ratio is {round(c_ratio, 4)}."
            ))
          }
          perm_ii <- perm_ii + 1
        }
        # Send a different logging message depending on whether importance values
        #  converged
        if(self$importance_tracker$converged()){
          message("Feature importance values converged after ", perm_ii, " iterations.")
        } else {
          message("Failed to converge after ", max_iterations, " iterations.")
        }
      }

      # End timer for permutation sampling
      if(self$verbose) tictoc::toc()
      invisible(NULL)
    },

    #' @description get Shapley values for each feature.
    #' 
    #' @param normalize (`logical(1)`, default FALSE) Normalize the feature importance
    #'   values to sum to one?
    #' 
    #' @return Numeric vector named with features
    get_shapley_vals = function(normalize = FALSE){
      shapley_vals <- self$importance_tracker$importance
      if(normalize) shapley_vals <- shapley_vals / sum(shapley_vals)
      return(shapley_vals)
    }
  )
)
