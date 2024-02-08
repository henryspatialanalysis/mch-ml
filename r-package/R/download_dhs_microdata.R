#' Get the RDHS cache directory
#' 
#' @description Get the folder where RDHS datasets are cached
#' 
#' @details Will stop and request the user to set up their RDHS config if not already
#'   initialized
#' 
#' @importFrom rdhs get_rdhs_config
#' @export
get_rdhs_cache_directory <- function(){
  # Check that top-level cache directory exists
  rdhs_cache_directory <- rdhs::get_rdhs_config()$cache_path
  if(is.null(rdhs_cache_directory)) stop("RDHS cache directory has not yet been set up.")

  # Create a subdirectory called 'datasets', where cached datasets will be stored
  datasets_directory <- file.path(rdhs_cache_directory, 'datasets')
  if(!dir.exists(datasets_directory)) dir.create(datasets_directory)
  return(datasets_directory)
}

#' Get DHS survey metadata
#' 
#' @description Pull DHS internal survey metadata based on country, year, and survey type
#' 
#' @details Wrapper for [rdhs][rdhs::dhs_surveys()]
#' 
#' @param country (`character(1)`) Country name to match to DHS survey list.
#'   Case-insensitive  
#' @param years (`integer(N)` or 'most_recent') Survey years to match to DHS survey list.
#'   If 'most_recent', the default, pulls the latest available DHS standard survey
#' @param survey_types (`character(N)`, default 'all') Survey types to match to the survey
#'   list. If 'all', the default, does not subset by survey type
#' 
#' @return Any DHS survey IDs matching the listed countries and years.
#' 
#' @import data.table
#' @importFrom rdhs dhs_surveys
#' @export
pull_dhs_survey_metadata <- function(country, years = 'most_recent', survey_types = 'all'){
  # Some dummy variables to avoid R CMD CHECK warnings
  SurveyYear <- CountryName <- SurveyType <- NULL

  surveys_dt <- rdhs::dhs_surveys() |> data.table::as.data.table()
  surveys_dt[, year := as.integer(SurveyYear)]
  # Optionally subset by survey type
  survey_types <- toupper(survey_types)
  if(!'ALL' %in% survey_types){
    surveys_dt <- surveys_dt[SurveyType %in% survey_types, ]
    if(nrow(surveys_dt) == 0) stop("Subsetting by survey type yielded no results.")
  }
  # Subset by country
  surveys_dt <- surveys_dt[grepl(country, CountryName, ignore.case = TRUE), ]
  if(nrow(surveys_dt) == 0) stop("Subsetting to country ", country, " yielded no results.")
  # Subset by year, or get most recent survey
  if(years == 'most_recent'){
    surveys_dt <- surveys_dt[order(year)][.N, ]
  } else {
    surveys_dt <- surveys_dt[year %in% years, ]
  }
  if(nrow(surveys_dt) == 0){
    stop("Subsetting to years ", paste(years, collapse = ', '), " yielded no results.")
  } else if(nrow(surveys_dt) > 1){
    # Message the user if query returned multiple surveys
    message("Query yielded ", nrow(surveys_dt), " results - returning all.")
  }
  return(surveys_dt)
}


#' Download DHS microdata
#' 
#' @description Download a subset of DHS microdata for a given country and year
#'
#' @details This function requires credentials to access the DHS API, requested via
#'   href{https://dhsprogram.com}{the DHS Program website} and loaded into the environment
#'   with [rdhs][rdhs::set_rdhs_config]. If you are unable to access some files via the
#'   DHS API but have manual access via the website downloads, you can save them in the
#'   proper ZIP file formats within the RDHS datasets folder instead.
#' 
#' @param survey_id (`character(1)`) The text ID of the survey from which data and GPS
#'   will be downloaded. Can be found via StatCompiler.
#' @param topics_regex (`character(N)`) The list of topics that should be considered for
#'   analysis from the survey files. Multiple topics can be passed - if so, all topics
#'   will be included. The default, NULL, pull all topics.
#' 
#' @return A list of [data.table][data.table]s containing survey datasets.
#' 
#' @importFrom rdhs dhs_datasets get_datasets
#' @importFrom haven read_dta
#' @importFrom sf st_read
#' @import data.table
#' @export
download_dhs_microdata <- function(survey_id, topics_regex = character(0)){
  # Some dummy variables to avoid R CMD CHECK warnings
  DatasetType <- FileFormat <- FileType <- NULL

  # Check that the RDHS cache has been set up
  rdhs_cache_directory <- get_rdhs_cache_directory()

  # Create a temporary workspace for reading RDS files
  temp_workspace <- tempdir()

  # Helper function to convert survey FileType variables to list names
  clean_file_type <- function(file_type){
    cleaned <- file_type |>
      gsub(pattern = ' Recode', replacement = '') |>
      gsub(pattern = "'s", replacement = '') |>
      tolower() |>
      make.names()
    return(cleaned)
  }

  # Get metadata for all datasets associatd with this survey
  all_datasets <- rdhs::dhs_datasets(surveyIds = survey_id) |> 
    data.table::as.data.table() |>
    suppressMessages()
  survey_files_table <- all_datasets[
      (DatasetType == 'Survey Datasets') & grepl('dta', FileFormat),
  ]
  if(length(topics_regex) > 0L) survey_files_table <- survey_files_table[
    grepl(paste(topics_regex, collapse='|'), survey_files_table$FileType),
  ]
  survey_files_table[, test_zip := file.path(rdhs_cache_directory, FileName) ]
  survey_files_table[, test_rds := file.path(rdhs_cache_directory, gsub('.ZIP$', '.rds', FileName)) ]
  survey_files_table[, cleaned_title := clean_file_type(FileType) ]

  # Download standard datasets using the rdhs API
  rdhs::get_datasets(
    dataset_filenames = survey_files_table$FileName,
    download_option = 'rds'
  ) |> suppressMessages() |> invisible()

  # Any option with no ZIP file cannot be downloaded via the RDHS API
  # These must be manually 
  missing_files <- survey_files_table[
    !file.exists(test_zip) & !file.exists(test_rds),
    FileName
  ]
  if(length(missing_files) > 0) message(
    "Could not download files ", paste(missing_files, collapse = ', '), " for survey ",
    survey_id, " from the RDHS API. Please manually add these files to ",
    rdhs_cache_directory, " instead."
  )
  survey_files_table <- survey_files_table[file.exists(test_zip) | file.exists(test_rds), ]

  # Load and save RDS files that have not yet been processed from ZIP files
  missing_rds <- survey_files_table[!file.exists(test_rds), ]
  for(ii in seq_len(nrow(missing_rds))){
    zip_file <- missing_rds[ii, test_zip]
    rds_file <- missing_rds[ii, test_rds]
    dta_temp_path <- unzip(
      zip_file, overwrite = T, junkpaths = T, exdir = temp_workspace
    ) |> grep(pattern = 'dta$', value = TRUE, ignore.case = TRUE)
    if(length(dta_temp_path) == 1L){
      missing_table <- dta_temp_path |> haven::read_dta() |> as.data.frame()
      saveRDS(missing_table, file = rds_file)
    }
  }
  survey_files_table <- survey_files_table[file.exists(test_rds), ]
  prepared_tables <-  lapply(survey_files_table$test_rds, readRDS) |> 
    lapply(data.table::as.data.table)
  names(prepared_tables) <- survey_files_table$cleaned_title

  # Custom setup for geospatial covariates
  dir.create(temp_workspace, showWarnings = FALSE)
  gc_row <- all_datasets[FileType == 'Geospatial Covariates', ]
  if(nrow(gc_row) == 1L){
    check_path <- rdhs::get_datasets(
      dataset_filenames = gc_row,
      download_option = 'zip'
    ) |> suppressMessages() |> unlist()
    if(grepl('not available', check_path)){
      check_path <- file.path(rdhs_cache_directory, gc_row$FileName)
    }
    if(file.exists(check_path)){
      csv_temp_path <- unzip(
        check_path, overwrite = T, junkpaths = T, exdir = temp_workspace
      ) |> grep(pattern = 'csv$', value = TRUE)
      if(length(csv_temp_path) == 1L){
        # Case when ZIP file exists and contained a CSV file: load it as a data.table
        prepared_tables$geospatial.covariates <- data.table::fread(csv_temp_path)
      } else {
        # Case when a ZIP file was found, but it did not contain a CSV file
        message("No CSV within geospatial covariates ZIP file for ",survey_id," -- skipping.")
      }
    } else {
      # Case ZIP file was listed but could not be downloaded
      message(
        "Could not pull spatial covariates file ", gc_row$FileName, " for survey ",
        survey_id, " -- skipping. If you want to load this data, manually add the ZIP to ",
        rdhs_cache_directory
      )
    }
  } else {
    # Case when no geospatial covariates were listed in the DHS datasets index
    message("No geospatial covariates listed for ", survey_id, " -- skipping")
  }

  # Custom setup for cluster GPS locations
  gps_row <- all_datasets[FileType == 'Geographic Data', ]
  if(nrow(gps_row) == 1L){
    check_path <- rdhs::get_datasets(
      dataset_filenames = gps_row,
      download_option = 'zip'
    ) |> suppressMessages() |> unlist()
    if(grepl('not available', check_path)){
      check_path <- file.path(rdhs_cache_directory, gps_row$FileName)
    }
    if(file.exists(check_path)){
      shp_temp_path <- unzip(
        check_path, overwrite = T, junkpaths = T, exdir = temp_workspace
      ) |> grep(pattern = 'shp$', value = TRUE)
      if(length(shp_temp_path) == 1L){
        # Case when ZIP file exists and contained a CSV file: load it as a data.table
        prepared_tables$geographic.data <- sf::st_read(shp_temp_path, quiet = TRUE)
      } else {
        # Case when a ZIP file was found, but it did not contain a CSV file
        message("No SHP file within GPS data ZIP file for ",survey_id," -- skipping")
      }
    } else {
      # Case when ZIP file could not be pulled for this survey
      message(
        "Could not pull GPS file ", gps_row$FileName, " for survey ", survey_id, 
        " -- skipping. If you want to load this data, manually add the ZIP to ",
        rdhs_cache_directory
      )
    }
  } else {
    # Case when no geospatial covariates were listed in the DHS datasets index
    message("No GPS data file listed for ", survey_id, " -- skipping")
  }
  # Remove the temporary workspace for loading spatial data
  unlink(temp_workspace)

  # Return all datasets:
  #  - All available survey data from relevant themes (data.tables)
  #  - Geospatial covariates by cluster, if available (data.table)
  #  - Displaced GPS data for each cluster, if available (sf)
  return(prepared_tables)
}
