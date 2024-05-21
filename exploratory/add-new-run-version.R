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

run_version <- '20240521'
methods <- c('glm', 'gam', 'ada', 'plr', 'xgbTree', 'treebag', 'rf', 'LogitBoost', 'rsf')
imputations <- 1:5
surveys <- c(
  "CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "SN2019DHS", "NG2018DHS", "PH2022DHS"
)

conn <- wtm.ingest::PostGISConnection$new(default_schema = 'scheduler')

# Cross join all model types
full_table <- data.table::CJ(
  method = methods,
  imputation = imputations,
  survey = surveys,
  run_version = run_version,
  status = 'not_started',
  holdout = 0:5
)
conn$appendTable('shapley_runner', rows = full_table )
message("Finished preparing run version ", run_version)
