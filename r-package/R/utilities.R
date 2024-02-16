#' Load environment variable
#' 
#' @description wrapper for `Sys.getenv()` that fails on empty values
#' 
#' @param var (character) environment variable to load
#' @param fail_on_empty (boolean, default `TRUE`) throw an error message if the variable
#'   is not defined?
#' 
#' @return Environment variable, as a character object
#' 
#' @export
load_environment_variable <- function(var, fail_on_empty = TRUE){
  val <- Sys.getenv(var)
  if(fail_on_empty){
    if(is.null(val)) stop("Environment variable is NULL")
    if(val == '') stop("Environment variable is empty")
  }
  return(val)
}


#' Generate power set
#' 
#' @description Generate the power set (all unique subsets) of a vector
#' 
#' @details Includes both the complete set and the empty set as options. Taken from this
#'   \href{https://stackoverflow.com/a/18718066/22642017}{Stackoverflow solution}
#' 
#' @param x Vector from which subsets will be generated
#' 
#' @return List where each entry is a member of the power set
#' @export
generate_power_set <- function(x){
  n <- length(x)
  masks <- 2^(1:n-1)
  power_set <- lapply(1:2^n-1, function(u) x[bitwAnd(u, masks) != 0 ])
  return(power_set)
}


#' Vector to names
#' 
#' @description Convert a vector into a unique name for e.g. a list
#' 
#' @param x Arbitrary vector
#' 
#' @return (`character(1)`) A uniquely-identifying string representing this vector
#' @export
vector_to_names <- function(x){
  # Special cases for empty vector
  if(length(x) == 0) return(";;")

  x <- x |> unique() |> sort() |> make.names()
  return(paste(x, collapse = ';'))
}


#' Get loss function
#' 
#' @description Helper function to pull L1, L2, or binary cross-entropy loss functions
#' 
#' @param name (`character(1)`) One of "L1", "L2", or "BCE"
#' 
#' @return A function that takes two arguments (observed and predicted) and generates
#'   total error for a model
#' @export
get_loss_function <- function(name){
  loss_funs <- list(
    L1 = function(y, p) sum(abs(y - p)),
    L2 = function(y, p) sum((y - p)**2),
    BCE = function(y, p) sum(-1 * (y * log(p) + (1 - y) * log(1 - p)))
  )
  if(!name %in% names(loss_funs)) stop("Name ", name, " not a valid loss function")
  return(loss_funs[[name]])
}
