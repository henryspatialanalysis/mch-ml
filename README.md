# USAID-DHS Post-Neonatal Mortality Predictors Project

This is the code repository for the USAID + DHS "MCH ML" project for predictors of child mortality at 1-59 months of age.


## Code structure

- `config.yaml`: The project configuration file, to be used with the [`versioning`](https://cran.r-project.org/web/packages/versioning/index.html) R package.
- `r-package/`: This contains project functions structured as an [R package](https://r-pkgs.org/). You can load all package functions in your R session using `devtools::load_all("/path/to/usaid-mch-ml/r-package/")`, which will give access to the functions and their documentation (via `help(function_name)`)
- `data-prep/`: Scripts used to prepare the data used for analysis. Saved to the "prepared_data" folder in the config.
- `run-model/`: Scripts used to run the machine learning models and visualize model results. Saved to the "model_results" folder in the config.
- `exploratory/`: Scripts used for one-off testing and simple use cases.


## Useful links

Data access:
- [Project SharePoint folder](https://icfonline.sharepoint.com/sites/ihd-dhs/Analysis/MCH%20Machine%20Learning%20Child%20Mortality%20docs/) (request access from Rachael Church)
- [DHS data platform](https://dhsprogram.com/data/dataset_admin/login_main.cfm)
- [ACLED data platform](https://developer.acleddata.com)

Methods:
- `caret` [package documentation](https://topepo.github.io/caret/) and [available models](https://topepo.github.io/caret/available-models.html)
- [SAGE introductory paper](https://arxiv.org/abs/2004.00668) and [blog post](https://iancovert.com/blog/understanding-shap-sage/)
- [SAGE Python repository](https://github.com/iancovert/sage)
