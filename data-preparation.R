## #######################################################################################
##
## MCH-ML DATA PREPARATION SCRIPT
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 2 February 2024
## PURPOSE: Prepare DHS data and spatial covariates for child mortality analysis
##
## #######################################################################################

## SETTINGS

# All other settings set through the config file
CONFIG_PATH <- '~/repos/usaid-mch-ml/data-prep-config.yaml'


## SETUP -------------------------------------------------------------------------------->

# Load packages
library(versioning); library(devtools)

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))

# Get settings: variables and countries to prepare
prepare_countries <- config$get("countries")
codebook <- config$read('metadata', 'codebook')


## LOAD DATA FILES ---------------------------------------------------------------------->

for(prepare_country in prepare_countries){
  # Load all DHS data
  download_start_time <- Sys.time()
  survey_metadata <- mch.ml::pull_dhs_survey_metadata(
    country = prepare_country,
    survey_types = 'DHS'
  )
  # Download all relevant survey microdata
  # This will be much faster after the first download thanks to cacheing
  microdata_list <- mch.ml::download_dhs_microdata(
    survey_id = survey_metadata$SurveyId,
    topics_regex = 'Birth'
  )
  message(glue::glue(
    "Downloaded {prepare_country} {paste(names(microdata_list), collapse = ', ')} ",
    "in {round(Sys.time() - download_start_time, 1)}s."
  ))

  # Clean variables based on the codebook
  country_codebook <- copy(codebook[country == prepare_country, ])
  tables_to_clean <- intersect(names(microdata_list), unique(country_codebook$topic))
  for(table_to_clean in tables_to_clean){
    microdata_list[[table_to_clean]] <- mch.ml::clean_microdata_using_codebook(
      microdata = microdata_list[[table_to_clean]],
      codebook = country_codebook[topic == table_to_clean, ]
    )
  }
  message("Microdata tables cleaned.")

  # Prepare "siblings" table - average of living sibling characteristics by mother
  mother_id_cols <- c('cluster','hh_id','w_id')
  sibling_cols <- grepl('^s_', colnames(microdata_list$births), value = T)

  # Prepare subset births table

  # Merge on sibling characteristics

  # Merge on country conflict data

  # Merge on geospatial covariates

  # Save to file

}
