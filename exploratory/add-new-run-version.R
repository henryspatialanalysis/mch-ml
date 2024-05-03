## #######################################################################################
##
## SHAPLEY RUN SCHEDULER
##
## AUTHOR: Nat Henry
## CREATED: 2 May 2024
## PURPOSE: Set up the scheduler table for a Shapley run using some custom AWS EC2 tools
##
## #######################################################################################

library(data.table) |> suppressPackageStartupMessages()
devtools::load_all('~/efs-mount/repos/wtm.ingest') |> suppressPackageStartupMessages()

run_version <- '20240502'
methods <- c('glm', 'gam', 'ada', 'plr', 'xgbTree', 'treebag', 'rf', 'LogitBoost')
imputations <- 1:5
surveys <- 'GH2022DHS'

conn <- wtm.ingest::PostGISConnection$new(default_schema = 'scheduler')

# Cross join all model types
full_table <- data.table::CJ(
  method = methods,
  imputation = imputations,
  survey = surveys,
  run_version = run_version,
  status = 'not_started'
)
conn$appendTable('shapley_runner', rows = full_table )
message("Finished preparing run version ", run_version)
