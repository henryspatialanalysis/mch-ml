## #######################################################################################
##
## MCH-ML STAGE 2 (MAPPING): COMBINE MICRODATA AND COLLAPSED DATASETS
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 3 June 2024
## PURPOSE: Combine tables from collapse stage into modeling datasets
##
## #######################################################################################

# All other settings set through the config file
CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'


## SETUP -------------------------------------------------------------------------------->

# Load packages
library(versioning); library(devtools); library(tictoc)

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))

# Create the output directory
out_dir <- config$get_dir_path('prepared_data_stage_2')
original_dir <- getwd()
on.exit(setwd(original_dir))
setwd(out_dir)

# Get (microdata) topics and (collapsed) indicators to merge
codebook <- config$read('metadata', 'codebook_stage_2')
prepare_topics <- sort(unique(codebook$topic))
prepare_indicators <- config$get("map_indicators")


## Combine files and remove originals --------------------------------------------------->

for(topic in prepare_topics){
  original_files <- list.files(pattern = glue::glue('^microdata_{topic}'))
  full_data <- lapply(original_files, data.table::fread) |> data.table::rbindlist()
  file.remove(original_files)
  data.table::fwrite(full_data, file = glue::glue('microdata_{topic}.csv'))
}

for(indicator in prepare_indicators){
  original_files <- list.files(pattern = glue::glue('^collapsed_{indicator}'))
  full_data <- lapply(original_files, data.table::fread) |> data.table::rbindlist()
  file.remove(original_files)
  data.table::fwrite(full_data, file = glue::glue('collapsed_{indicator}.csv'))
}
