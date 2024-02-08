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
