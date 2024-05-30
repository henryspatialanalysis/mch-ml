# Exploratory scripts

This folder contains scripts used to test simple versions of new code, as well as infrastructure for running the code in a remote cluster environment.


## Test scripts

The following scripts were designed as simple tests for concepts that were ultimately incorporated into the R package:

- `test_shapley_decomposition.R`: Test that a **full** Shapley decomposition (which exhaustively runs all subsets of variable combinations used in a model) accurately captures predictor importance.
- `test_importancetracker_convergence.R`: Test that the package `ImportanceTracker` class will converge when given values sampled from a distribution.
- `test_shapley_approximation.R`: Test that a **sampled** Shapley decomposition (which repeatedly samples subsets of variables used in a model) approximates the values yielded by a full Shapley decomposition.
- `test_imputation.R`: Test the `mice` package for imputing predictor values.
- `test_random_survival_forests.R`: Test the `randomForestSRC` package for generating random survival forests.


## Cluster execution

The following scripts can be used to run model fitting on remote Linux workstations, for example, on AWS EC2 instances. Some scripts rely on a separate `wtm.ingest` package for connecting to a central database, which is used to coordinate model runs across workstations.

- `aws_deploy.sh`: A bash script that installs command-line tools and R packages needed to run the ML models. Written to be run on a fresh AWS EC2 instance running Linux: Ubuntu Jammy 22.04.
- `add-new-run-version.R`: Script to add all planned model runs to a central database. The database table is read by all machines running `model-runner.R` and used to coordinate machines "claiming" each particular job.
- `model-runner.R`: Script to repeatedly fit machine learning models by country, model type, imputed dataset, and holdout. This is a wrapper to repeatedly run `../run-model/shapley-decomposition.R`, which can also be run interactively on a local machine.

