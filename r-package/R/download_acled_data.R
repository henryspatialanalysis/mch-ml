
#' Download ACLED data through the API
#' 
#' @description Download geopositioned conflict data from ACLED using an API query
#' 
#' @details The api_key and api_email parameters can be set as environment variables, and
#'   are called from environment variables by default. For more information, see
#'   [the ACLED developer dashboard][https://developer.acleddata.com/].
#' 
#' @param country_name (`character(1)`) Standardized name of the country where ACLED data
#'   will be downloaded
#' @param api_key (`character(1)`) ACLED API key. Default pulled from R environment
#'   variable `ACLED_API_KEY`.
#' @param api_email (`character(1)`) ACLED API username (an email address). Default pulled
#'   from R environment variable `ACLED_EMAIL`.
#' @param years (`integer(N)`, default 2015:2020) Years for which ACLED data will be
#'   downloaded
#' 
#' @return R [data.table][data.table] containing results for this country and year
#' 
#' @import data.table
#' @importFrom glue glue
#' @importFrom httr GET content http_error
#' @export
download_acled_data <- function(
  country_name,
  api_key = load_environment_variable('ACLED_API_KEY'),
  api_email = load_environment_variable('ACLED_EMAIL'),
  years = 2015:2020
){
  # Set some dummy variables to avoid R package warnings
  event_date <- year <- country <- NULL

  # Iteratively query ACLED data for this country
  content_rows <- c(
    'event_id_cnty', 'event_date', 'time_precision', 'longitude', 'latitude',
    'geo_precision', 'fatalities'
  )
  query_next_page <- TRUE
  page_max_rows <- 5000L # Maximum number of results returned per "page"
  results_page <- 1L
  results_list <- list()

  while(query_next_page){
    query_next_page <- FALSE
    acled_api_response <- httr::GET(
      url = 'https://api.acleddata.com/',
      path = 'acled/read.csv',
      query = list(
        key = api_key,
        email = api_email,
        country = country_name,
        event_date = glue::glue('{min(years)}-01-01|{max(years)}-12-31'),
        event_date_where = 'BETWEEN',
        page = results_page,
        fields = paste(content_rows, collapse = '|')
      )
    )
    # Try to parse results as a data.table
    if(!httr::http_error(acled_api_response)){
      acled_results_table <- httr::content(acled_api_response, as = 'text') |>
        data.table::fread()
      if(all(content_rows %in% colnames(acled_results_table))){
        results_list[[results_page]] <- acled_results_table
      }
      if(nrow(acled_results_table) == page_max_rows){
        results_page <- results_page + 1
        query_next_page <- TRUE
      }
    }
  }

  # Combine all pages into a single table
  full_results <- data.table::rbindlist(results_list, fill = TRUE)

  # Final formatting
  full_results[, country := country_name ]
  full_results[, year := strftime(event_date, format = '%Y')]

  return(full_results)
}
