#' Download ACLED data through the API
#' 
#' @description Download geopositioned conflict data from ACLED using an API query
#' 
#' @details The api_key and api_user parameters can be set as environment variables, and
#'   are called from environment variables by default
#' 
#' @param api_key
#' @param api_user
#' @param iso
#' @param years
#' 
#' @return R [data.table][data.table] containing results for this country and year
#' 
#' @import data.table
#' @export
download_acled_data <- function(api_key, api_user, iso, years = 2015:2020){
  response <- httr::GET(
    url = 'https://api.acleddata.com/acled/',
    path = 'read?',
    query = list(
      key = api_key,
      email = api_user,
      iso = iso,
      event_date = paste0('{',min(years),'-01-01|',max(years),'-12-31}'),
      event_date_where = 'BETWEEN'
    )
  )

}