# Data preparation scripts

The scripts in this folder are used to prepare and visualize input data for statistical modeling.


## Stage 1

The data preparation scripts used in Stage 1 are, in order of execution:

1. `data-preparation.R`: This script downloads DHS data and covariates, then standardizes the data into detailed age groups for use in the statistical models. The results are saved in the "prepared_data" folder listed in the config file.
2. `imputation.R`: This script reads the standardized datasets saved in the "prepared_data" folder,  creates five imputed datasets that fill missing predictors with plausible values, and then saves the results in the "imputed_data" folder.

There are also three scripts that visualize the input data:

- `covariate_boxplots.R`: This script visualizes the importance of individual covariates (predictive features) in the raw data, showing differences in average post-neonatal mortality across groups of children with different covariate values. This script uses the standardized datasets from the "prepared_data" folder.
- `map_pnn_data.R`: This script maps differences cluster-level post-neonatal (1-59 months) mortality rates across each focus country, using the standardized datasets from the "prepared_data" folder.
- `plot_predictor_completeness.R`: This script plots the (in)completeness of each predictive field, using the standardized datasets from the "prepared_data" folder.


## Stage 2

Data preparation scripts for the Stage 2 analysis are forthcoming.
