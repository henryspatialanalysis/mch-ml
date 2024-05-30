# ML model scripts

This package contains scripts to run individual mortality prediction models and visualize the results for the Stage 1 analysis.

## Scripts

Two scripts are used to prepare and interpret the ML models:

1. `shapley-decomposition.R`: Run a ML model for a single country, imputation, predictive method, and holdout. When the "holdout" variable is equal to zero (indicating that all data is used), this script also performs a Shapley decomposition to estimate feature importance in the predictive model. This script reads data from the "imputed_data" folder and saves outputs in the "model_results" folder.
2. `roc-pv-analyses.R`: This script is run once after all models have finished. It reads results from the "model_results" folder and generates various predictive validity comparisons across models and countries. Results are saved to the "model_results" and "viz" folders.

There are alo several visualization scripts for model results:

- `lexis-diagrams.R`: Create Lexis diagrams showing mortality probabilities (nQx) across age groups and over time, based on both observed data and modeled predictions.
- `visualize_shapley_absolute.R`: Visualize absolute Shapley values across different model types for a single country.
- `visualize_shapley_relative.R`: For a single model type, visualize relative Shapley values across countries.
