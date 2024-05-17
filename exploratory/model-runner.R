## #######################################################################################
## 
## TRAVEL TIME BATCH RUNNER
## 
## AUTHOR: Nat Henry
## CREATED: 4 December 2023
## PURPOSE: Batch submit travel time jobs by county
## 
## #######################################################################################

# Load run version from database
parser <- argparse::ArgumentParser()
parser$add_argument('--version', type = 'character')
args <- parser$parse_args(commandArgs(trailingOnly = TRUE))
run_version <- args$version
if(is.null(run_version)) stop("Issue with command line arguments.")

# Globals
db_repo <- '/home/ubuntu/efs-mount/repos/wtm.ingest'
mch_repo <- '/home/ubuntu/efs-mount/repos/usaid-mch-ml'

scheduler_schema <- 'scheduler'
scheduler_table <- 'shapley_runner'

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
    criteria = list(run_version = run_version, status = 'not_started')
  )
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

tictoc::tic("All Shapley decompositions")

claimed_job <- claim_jobs()
while(!is.null(claimed_job)){
  # Run the job
  job_meta <- as.list(claimed_job)
  job_finish_code <- run_cmd(glue::glue(
    "Rscript {mch_repo}/run-model/shapley-decomposition.R --imp {job_meta$imputation} ",
    "--survey {job_meta$survey} --method {job_meta$method} --holdout {job_meta$holdout}"
  ))
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