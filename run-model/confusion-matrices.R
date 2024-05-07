## #######################################################################################
## 
## VISUALIZE MODEL PERFORMANCE USING CONFUSION MATRICES
## 
## #######################################################################################

library(data.table); library(ggplot2); library(scales)

working_dir <- '~/temp_data/usaid-mch-ml'
survey_id <- 'MD2021DHS'
check_methods <- c("treebag", "rf", "glm")
title <- 'Relative feature importance: Bagged Regression Trees'
results_version <- '20240505'

example_risk_cutoff <- 0.5

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "PH2022DHS", "SN2019DHS", "AVERAGE"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Philippines", "Senegal", "AVERAGE")
)


## Create confusion matrices for a given model cutoff ----------------------------------->

model_dir <- file.path(working_dir, 'model_results', results_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)

confusion_all <- lapply(check_methods, function(method_name){
  message("Getting confusion matrices for ", method_name)
  confusion <- list.files(model_dir, pattern = 'predictions.csv$', full.names = TRUE) |>
    grep(pattern = method_name, value = TRUE) |>
    grep(pattern = survey_id, value = TRUE) |>
    lapply(data.table::fread) |>
    lapply(function(dt) (dt
      [, predicted := (predicted >= example_risk_cutoff) ]
      [, observed := (observed >= example_risk_cutoff) ]
      [, .(
        tp = sum(predicted & observed), fp = sum(predicted & !observed),
        fn = sum(!predicted & observed), tn = sum(!predicted & !observed)
      )]
    )) |>
    data.table::rbindlist(fill = TRUE)
  confusion <- confusion[, lapply(.SD, mean)][, method := method_name ]
  return(confusion)
}) |> data.table::rbindlist(fill = TRUE)

confusion_all[, precision := scales::percent(tp/(tp+fp), accuracy = 1)]
confusion_all[, recall := scales::percent(tp/(tp+fn), accuracy = 1)]
for(col in c('tp', 'tn', 'fp', 'fn')) confusion_all[, (col) := round(get(col), 0)]

knitr::kable(confusion_all[, .(method, tp, fp, fn, tn, precision, recall)])
