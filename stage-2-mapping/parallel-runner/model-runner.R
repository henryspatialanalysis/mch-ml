## #######################################################################################
## 
## MBG BATCH RUNNER
## 
## AUTHOR: Nat Henry
## CREATED: 5 August 2024
## PURPOSE: Batch submit travel time jobs by county
## 
## #######################################################################################

# Load run set from CLI
parser <- argparse::ArgumentParser()
parser$add_argument('--run_set', type = 'character')
args <- parser$parse_args(commandArgs(trailingOnly = TRUE))
RUN_SET <- args$run_set
if(is.null(RUN_SET)) stop("Issue with command line arguments.")

# Globals
db_repo <- '/home/ubuntu/efs-mount/repos/wtm.ingest'
mch_repo <- '/home/ubuntu/efs-mount/repos/usaid-mch-ml'

scheduler_schema <- 'scheduler'
scheduler_table <- 'mbg_runner'

# Load packages
devtools::load_all(db_repo) |> suppressPackageStartupMessages()

# Set up database connection
conn <- wtm.ingest::PostGISConnection$new(default_schema = scheduler_schema)


## Helper functions --------------------------------------------------------------------->

update_status <- function(runner_id, new_status = 'in_progress'){
  conn$dbExecute(glue::glue(
    "UPDATE {scheduler_schema}.{scheduler_table} SET status = '{new_status}' 
      WHERE runner_id = {runner_id};"
  ))
}

claim_jobs <- function(){
  open_jobs <- conn$queryTable(
    glue::glue("{scheduler_schema}.{scheduler_table}"),
    criteria = list(run_set = RUN_SET, status = 'not_started')
  )[order(runner_id)]
  if(nrow(open_jobs >= 1)){
    claimed_job <- open_jobs[1, ]
    update_status(claimed_job$runner_id)
    return(claimed_job)    
  } else {
    return(NULL)
  }
}

ALL_ERRORS <- character(0)

run_cmd <- function(cmd_text, verbose = T){
  if(verbose) message("RUNNING ", cmd_text)
  stop_code <- system(cmd_text, ignore.stdout = !verbose, ignore.stderr = !verbose)
  if(length(stop_code) == 0) stop_code <- 0L
  if(stop_code != 0L){
    if(stop_code == 2L){
      stop('Interrupted')
    } else {
      warning_text <- paste("Command", cmd_text, "failed with code", stop_code)
      warning(warning_text)
      ALL_ERRORS <<- c(ALL_ERRORS, warning_text)
    }
  }
  return(stop_code)
}

## Model runner ------------------------------------------------------------------------->

tictoc::tic("All MBG runs")

claimed_job <- claim_jobs()
while(!is.null(claimed_job)){
  # Run the job
  job_meta <- as.list(claimed_job)
  command <- glue::glue(
    "Rscript {mch_repo}/stage-2-mapping/04-geostatistical-model-parallel.R ",
    "--indicator {job_meta$indicator} --country \"{job_meta$country}\" --iso3 {job_meta$iso3} ",
    "--year {job_meta$year} --run_set {job_meta$run_set} --specs {job_meta$specs}"
  ) |> gsub(pattern = "'", replacement = "\\\\'")
  job_finish_code <- run_cmd(command)
  # Update job exit status
  if(job_finish_code == 0L){
    update_status(job_meta$runner_id, new_status = 'finished')
  } else {
    update_status(job_meta$runner_id, new_status = 'errored')
  }
  # Claim the next job
  claimed_job <- claim_jobs()
}

tictoc::toc()