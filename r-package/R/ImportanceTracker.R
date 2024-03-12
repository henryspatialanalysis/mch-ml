#' Variable Importance Tracker
#' 
#' @details Used to track feature importance across model iterations. Uses Welford's
#'   algorithm to update mean and sum of squares
#' 
#' @seealso https://github.com/iancovert/sage/blob/master/sage/utils.py#L142
#' 
#' @importFrom R6 R6Class
#' @export 
ImportanceTracker <- R6::R6Class(
  "ImportanceTracker",
  public = list(
    #' @field n_var (`integer(1)`) Number of variables
    n_var = NULL,
    #' @field importance (`numeric(n_var)`) Running mean of estimated feature importance
    #'   for each variable
    importance = NULL,
    #' @field sum_of_squares (`numeric(n_var)`) Running sum of squares of estimated
    #'   feature importance for each variable
    sum_of_squares = NULL,
    #' @field n_obs (`integer(N)`) Total number of model runs observed so far for each
    #'   variable
    n_obs = NULL,
    #' @field convergence_threshold (`numeric(1)`) threshold below which the importance
    #'   values have effectively converged.
    convergence_threshold = 0.025,

    #' @description Create a new ImportanceTracker object
    #' 
    #' @param n_var Number of variables for which importance will be tracked
    #' @param convergence_threshold threshold below which the importance values have
    #'   effectively converged. Expressed as the ratio between the largest standard
    #'   deviation across the importance values (numerator) and the difference between the
    #'   highest and lowest importance values (denominator).
    initialize = function(n_var, convergence_threshold = 0.025){
      self$n_var <- n_var
      self$importance <- self$sum_of_squares <- self$n_obs <- rep(0, n_var)
      self$convergence_threshold <- convergence_threshold
      return(self)
    },

    #' @description Update running mean and sum of squares using Welford's algorithm
    #' 
    #' @param scores Absolute importance scores calculated from a single run
    #' @param which_sampled (`integer(N)`, default NULL) Indices of which variables were
    #'   sampled for this run. If NULL, all variables were sampled
    update = function(scores, which_sampled = NULL){
      if(is.null(which_sampled)){
        ws <- seq_len(self$n_var)
      } else {
        ws <- which_sampled
      }
      self$n_obs[ws] <- self$n_obs[ws] + 1
      diff <- scores - self$importance[ws]
      self$importance[ws] <- self$importance[ws] + (diff / self$n_obs[ws])
      diff2 <- scores - self$importance[ws]
      self$sum_of_squares[ws] <- self$sum_of_squares[ws] + (diff * diff2)
      # Return null
      invisible(NULL)
    },

    #' @description Get variance for each importance observation
    #' 
    #' @return Numeric vector with length `n_var` giving variance of the importance 
    #'   measure for each variable
    #' 
    #' @seealso importance_std_dev()
    importance_variance = function(){
      return(self$sum_of_squares / pmax(self$n_obs, 1)**2)
    },

    #' @description Get standard deviation for each importance observation
    #' 
    #' @return Numeric vector with length `n_var` giving standard deviations of the 
    #'   importance measure for each variable
    #' 
    #' @seealso importance_variance(), converged()
    importance_std_dev = function(){
      return(suppressWarnings(sqrt(self$importance_variance())))
    },

    #' @description Get the 'convergence ratio' between the maximum standard deviation of
    #'   any observation and the difference between largest and smallest observations
    #' 
    #' @seealso self$importance_std_dev() self$converged() 
    #' 
    #' @return numeric(1) for convergence ratio; compare to self$convergence_threshold
    get_convergence_ratio = function(){
      # Cannot test before the second iteration
      if(max(self$n_obs) < 2L) return(NA_real_)

      max_std <- self$importance_std_dev() |> max()
      gap <- range(self$importance) |> diff() |> max(1e-12)
      convergence_ratio <- max_std / gap
      return(convergence_ratio)
    },

    #' @description Determine whether the importance values have converged
    #' 
    #' @seealso self$get_convergence_ratio()
    #' 
    #' @return logical(1) value for whether the model has converged
    converged = function(){
      c_ratio <- self$get_convergence_ratio()
      return(!is.na(c_ratio) & (c_ratio < self$convergence_threshold))
    }
  )
)
