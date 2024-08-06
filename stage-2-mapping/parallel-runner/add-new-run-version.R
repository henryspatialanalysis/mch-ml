## #######################################################################################
##
## SHAPLEY RUN SCHEDULER
##
## AUTHOR: Nat Henry
## CREATED: 2 May 2024
## PURPOSE: Set up the scheduler table for a Shapley run using some custom AWS EC2 tools
## 
## NOTE: If this is the first time creating the table, a "runner_id" should be added to
##   the table to order the results:
## ALTER TABLE scheduler.mbg_runner ADD COLUMN runner_id BIGSERIAL PRIMARY KEY;
## 
## #######################################################################################

library(data.table) |> suppressPackageStartupMessages()
devtools::load_all('~/efs-mount/repos/wtm.ingest') |> suppressPackageStartupMessages()

run_set <- '20240805'
countries <- c('Senegal', 'Ghana', 'Kenya', 'Madagascar', 'Philippines', 'Nigeria')
iso3s <- c('SEN', 'GHA', 'KEN', 'MDG', 'PHL', 'NGA')
years <- c(2019, 2022, 2022, 2021, 2022, 2018)

indicators <- c('stunting', 'wasting', 'hhwi_cont', 'first_birth_u18', 'w_empowered_sdg')

specs_table <- data.table::fread('~/efs-mount/repos/usaid-mch-ml/mbg_model_specs.csv')

conn <- wtm.ingest::PostGISConnection$new(default_schema = 'scheduler')

iso3_lookup <- data.table::data.table(
  country = countries,
  iso3 = iso3s,
  year = years
)[, country_order := .I]

# Cross join all model types
full_table <- data.table::CJ(
  specs = specs_table$suffix,
  indicator = indicators,
  country = countries,
  run_set = run_set,
  status = 'not_started'
)[iso3_lookup, on = 'country'][order(country_order)][, country_order := NULL ]

# Drop some missing country-indicator combinations
full_table <- (full_table
  [!(country == 'Senegal' & indicator == 'w_empowered_sdg'), ]
  [!(country == 'Philippines' & indicator %in% c('stunting', 'wasting')), ]
)

conn$appendTable('mbg_runner', rows = full_table )
message("Finished preparing run set ", run_set)
