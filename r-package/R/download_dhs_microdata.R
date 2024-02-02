#' Download DHS microdata
#' 
#' @description Download a subset of DHS microdata for a given country and year
#'
#' @details This function requires credentials to access the DHS API, requested via
#'   [the DHS Program website][https://dhsprogram.com] and loaded into the environment
#'   with [rdhs][rdhs::set_rdhs_config].
#' 
#' @param survey_id (`character(1)`) The text ID of the survey from which data and GPS
#'   will be downloaded. Can be found via StatCompiler.
#' 
#' @return A list of [data.table][data.table]s containing survey datasets.
#' 
#' @importFrom rdhs dhs_datasets get_datasets
#' @import data.table
#' @export
download_dhs_microdata <- function(survey_id){
  # Get metadata for all datasets associatd with this survey
  all_datasets <- rdhs::dhs_datasets(surveyIds = survey_id) |> 
    data.table::as.data.table() |>
    suppressMessages()

  # Keep selected dataset types and file formats
  select_datasets <- all_datasets[
    (DatasetType %in% c('Survey Datasets', 'GPS Datasets')) & 
    (FileFormat == 'Flat ASCII data (.dat)') &
    !(FileType %in% c('Fieldworker Questionnaire', 'Couples\' Recode')),
  ]

  # Download and prepare all datasets
  all_datasets <- rdhs::get_datasets(dataset_filenames = select_datasets$FileName)

  return(all_datasets)
}
