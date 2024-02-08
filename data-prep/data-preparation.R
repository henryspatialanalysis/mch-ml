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
library(versioning); library(devtools); library(tictoc)

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))

# Create the output directory
dir.create(config$get_dir_path('prepared_data'), recursive = T, showWarnings = F)

# Get settings: variables and countries to prepare
prepare_countries <- config$get("countries")
codebook <- config$read('metadata', 'codebook')


## LOAD DATA FILES ---------------------------------------------------------------------->

for(prepare_country in prepare_countries){

  tictoc::tic(glue::glue("Full data preparation for {prepare_country}"))

  # Download all relevant survey microdata
  # This will be much faster after the first download thanks to cacheing
  tictoc::tic("  -> DHS survey microdata download")
  survey_metadata <- mch.ml::pull_dhs_survey_metadata(
    country = prepare_country,
    survey_types = 'DHS'
  )
  survey_id <- survey_metadata$SurveyId
  microdata_list <- mch.ml::download_dhs_microdata(
    survey_id = survey_id,
    topics_regex = 'Birth'
  )
  tictoc::toc() # End microdata

  # Clean variables based on the codebook
  tictoc::tic("  -> Clean variables based on codebook")
  country_codebook <- copy(codebook[country == prepare_country, ])
  tables_to_clean <- intersect(names(microdata_list), unique(country_codebook$topic))
  for(table_to_clean in tables_to_clean){
    microdata_list[[table_to_clean]] <- mch.ml::clean_microdata_using_codebook(
      microdata = microdata_list[[table_to_clean]],
      codebook = country_codebook[topic == table_to_clean, ]
    )
  }
  tictoc::toc() # End codebook cleaning

  # Prepare "siblings" table - average of living sibling characteristics by mother
  tictoc::tic("  -> Merging sibling characteristics")
  mother_id_cols <- c('cluster','hh_id','w_id')
  sibling_cols <- grep('^s_', colnames(microdata_list$births), value = T)
  # Prepare subset births table
  births_keep_cols <- setdiff(colnames(microdata_list$births), sibling_cols)
  analysis_table <- microdata_list$births[!is.na(c_died_pnn), ..births_keep_cols]
  # Merge on sibling characteristics
  averaged_sibling_characteristics <- (
    microdata_list$births
      [, c(mother_id_cols, sibling_cols), with = F]
      [, lapply(.SD, mean, na.rm = T), by = mother_id_cols ]
  )
  analysis_table <- merge(
    x = analysis_table,
    y = averaged_sibling_characteristics,
    by = c(mother_id_cols),
    all.x = TRUE
  )
  tictoc::toc() # End merging sibling characteristics

  # Merge on cluster metadata
  cluster_metadata <- as.data.table(microdata_list$geographic.data)[, .(
    cluster = DHSCLUST, latitude = LATNUM, longitude = LONGNUM, admin1_code = ADM1DHS,
    admin1_name = ADM1NAME
  )]
  analysis_table <- merge(
    x = analysis_table, y = cluster_metadata, by = 'cluster', all.x = TRUE
  )

  # Load country conflict data
  tictoc::tic("  -> Preparing buffered conflict data")
  birth_years <- sort(unique(analysis_table$c_birth_year))
  acled_country_conflict_data <- mch.ml::download_acled_data(
    country_name = if(prepare_country == "Cote D'Ivoire") "Ivory Coast" else prepare_country,
    years = birth_years
  )
  conflict_deaths_spatial <- acled_country_conflict_data[fatalities > 0, ] |>
    sf::st_as_sf(coords = c('longitude', 'latitude'), crs = sf::st_crs('EPSG:4326'))
  # Sum conflict deaths within 10, 25, and 50km by year of birth
  for(buffer_radius_km in c(10, 25, 50, 100)){
    buffered_points <- sf::st_buffer(
      microdata_list$geographic.data[, c('DHSCLUST')],
      dist = units::set_units(buffer_radius_km, 'km')
    )
    conflict_deaths_by_cluster <- sf::st_join(
      x = conflict_deaths_spatial,
      y = buffered_points[c('DHSCLUST')],
      join = sf::st_within
    )
    conflict_deaths_agg <- as.data.table(conflict_deaths_by_cluster)[
      , .(conflict_deaths = sum(fatalities)),
      by = .(cluster = DHSCLUST, c_birth_year = data.table::year(event_date))
    ]
    # Merge onto the analysis table
    new_conflict_col_name <- paste0('acled_deaths_', buffer_radius_km, 'km')
    analysis_table[
      conflict_deaths_agg,
      (new_conflict_col_name) := nafill(i.conflict_deaths, fill = 0),
      on = c('cluster', 'c_birth_year')
    ]
  }
  tictoc::toc() # End conflict data merge

  # Merge on geospatial covariates by year of birth
  tictoc::tic("  -> Preparing and merging DHS spatial covariates")
  reshaped_covariates <- mch.ml::reshape_dhs_covariates(
    covs_table = microdata_list$geospatial.covariates,
    measure_years = birth_years,
    id_col = 'DHSCLUST'
  )
  analysis_table <- merge(
    x = analysis_table,
    y = reshaped_covariates,
    by.x = c('cluster', 'c_birth_year'),
    by.y = c('DHSCLUST', 'year')
  )
  tictoc::toc() # End geospatial covariate prep

  # Save prepared country data to file
  versioning::autowrite(
    analysis_table,
    glue::glue("{config$get_dir_path('prepared_data')}/analysis_dataset_{survey_id}.csv")
  )
  tictoc::toc() # End country data prep
}