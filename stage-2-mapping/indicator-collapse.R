## #######################################################################################
##
## MCH-ML STAGE 2 (MAPPING): INDICATOR PREPARATION AND COLLAPSE 
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 3 June 2024
## PURPOSE: Create tables of all indicators collapsed by survey cluster
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
dir.create(out_dir, recursive = T, showWarnings = F)

# Get settings: variables and countries to prepare
prepare_countries <- config$get("countries")
codebook <- config$read('metadata', 'codebook_stage_2')


## LOAD DATA FILES ---------------------------------------------------------------------->

# Create lists to store prepared microdata
prepare_topics <- sort(unique(codebook$topic))
prepared_microdata_list <- lapply(prepare_topics, function(pt){
  null_list <- vector('list', length = length(prepare_countries))
  names(null_list) <- prepare_countries
  return(null_list)
})
names(prepared_microdata_list) <- prepare_topics

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
    topics_regex = c('Children', 'Household Recode', 'Individual')
  )
  tictoc::toc() # End microdata

  # Prepare cluster metadata
  cluster_metadata <- as.data.table(microdata_list$geographic.data)[, .(
    cluster = DHSCLUST, latitude = LATNUM, longitude = LONGNUM, admin1_code = ADM1DHS,
    admin1_name = ADM1NAME
  )]
  # Add holdouts by cluster
  cluster_metadata$holdout_id <- config$get('n_holdouts') |>
    seq_len() |> 
    rep_len(length.out = nrow(cluster_metadata)) |>
    sample()

  # Clean variables based on the codebook
  tictoc::tic("  -> Clean variables based on codebook")
  country_codebook <- copy(codebook[country == prepare_country, ])
  tables_to_clean <- intersect(names(microdata_list), prepare_topics)
  for(prepare_topic in tables_to_clean){
    cleaned_table <- mch.ml::clean_microdata_using_codebook(
      microdata = microdata_list[[prepare_topic]],
      codebook = country_codebook[topic == prepare_topic, ]
    ) |> merge(y = cluster_metadata, by = 'cluster', all.x = TRUE)
    # Add ISO code
    cleaned_table <- cbind(data.table(country = prepare_country), cleaned_table)
    # Add to prepared list
    prepared_microdata_list[[prepare_topic]][[prepare_country]] <- cleaned_table
  }
  tictoc::toc() # End codebook cleaning
  tictoc::toc() # End country data prep
}

# Combine prepared microdata across countries and collapse by indicator
for(prepare_topic in prepare_topics){
  topic_microdata <- data.table::rbindlist(prepared_microdata_list[[prepare_topic]])
  # Save microdata to file
  fwrite(topic_microdata, file = glue::glue("{out_dir}/microdata_{prepare_topic}.csv"))
  # Collapse indicators
  topic_indicators <- intersect(config$get('map_indicators'), colnames(prepare_topic))
  for(indic in topic_indicators){
    if(indic %in% config$get('continuous_indicators')){
      collapsed <- topic_microdata[
        !is.na(get(indic)),
        .(
          mean = mean(get(indic)),
          sd = sd(get(indic)) / sqrt(.N),
          count = .N
        ),
        by = .(country, cluster, latitude, longitude, admin1_code, admin1_name, holdout_id)
      ]
    } else {
      collapsed <- topic_microdata[
        !is.na(get(indic)),
        .(
          outcome = sum(get(indic)),
          count = .N
        ),
        by = .(country, cluster, latitude, longitude, admin1_code, admin1_name, holdout_id)
      ]
    }
    fwrite(collapsed, file = glue::glue("{out_dir}/collapsed_{indic}.csv"))
  }
}
