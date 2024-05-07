## #######################################################################################
## 
## VISUALIZE MODEL ESTIMATES USING LEXIS DIAGRAMS
## 
## #######################################################################################

library(data.table); library(ggplot2); library(scales)

working_dir <- '~/temp_data/usaid-mch-ml'
survey_id <- 'SN2019DHS'
check_methods <- c("treebag", "rf", "glm")
imputed_data_version <- '20240417'
results_version <- '20240505'

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "PH2022DHS", "SN2019DHS"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Philippines", "Senegal")
)
this_country <- all_surveys$country[which(all_surveys$survey_id == survey_id)]
age_groups_dt <- data.table::data.table(
  age_group = c("a01mo", "a06mo", "a12mo", "a24mo", "a36mo", "a48mo"),
  age_group_label = c('1-5mo.', '6-11mo.', '12-23mo.', '24-35mo', '36-47mo.', '48-59mo.')
)
age_groups_dt[, age_group_label := factor(age_group_label, levels = age_group_label)]



## Prepare matrix of hazards ------------------------------------------------------------>

model_dir <- file.path(working_dir, 'model_results', results_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)

true_obs <- glue::glue("{working_dir}/imputed_data/{imputed_data_version}/{survey_id}_imp1.csv") |>
  data.table::fread(select = c('bh_year', 'age_group', 'c_died_pnn'))
(true_obs
  [, method := 'Observed']
  [age_groups_dt, age_group_label := i.age_group_label, on = 'age_group']
)

preds_list <- lapply(check_methods, function(method_name){
  preds_vec <- list.files(model_dir, pattern = 'predictions.csv$', full.names = TRUE) |>
    grep(pattern = method_name, value = TRUE) |>
    grep(pattern = survey_id, value = TRUE) |>
    lapply(data.table::fread, select = 'predicted') |>
    lapply(unlist) |>
    do.call(what = 'cbind') |>
    rowMeans()
  preds_dt <- copy(true_obs)[, c_died_pnn := preds_vec][, method := method_name ]
  return(preds_dt)  
})

full_data <- data.table::rbindlist(c(list(true_obs), preds_list), use.names = T, fill = T)
hazards <- full_data[, .(haz = mean(c_died_pnn)), by = .(method, bh_year, age_group_label)]

# Fill missing values
hazards_square <- data.table::CJ(
  method = c('Observed', check_methods),
  bh_year = sort(unique(full_data$bh_year)),
  age_group_label = age_groups_dt$age_group_label,
  haz = NA_real_
)[hazards, haz := i.haz, on = c('method', 'bh_year', 'age_group_label')]
hazards_square$method <- factor(hazards_square$method, levels = c("Observed", sort(check_methods)))

# Represent per 1,000
hazards_square[, haz_per_1000 := round(haz * 1e3, 0)]


## Create Lexis diagram ----------------------------------------------------------------->

hazards_observed <- hazards_square[method == 'Observed', ]
lexis_observed <- ggplot(data = hazards_observed, aes(x = bh_year, y = age_group_label)) + 
  geom_tile(aes(fill = haz_per_1000)) + 
  geom_text(aes(label = haz_per_1000)) + 
  scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal('Spectral', n = 9) |> rev(),
    limits = c(0, 50), breaks = seq(0, 50, by = 10), labels = c(seq(0, 40, by = 10), '50+'),
    oob = scales::squish
  ) +
  labs(
    title = glue::glue('Observed mortality by detailed age group and year, {this_country}'),
    x = "Year",
    y = "Age group",
    fill = "nQx per\n1,000"
  ) +
  theme_bw()

png(
  glue::glue("{viz_dir}/lexis_observed_{survey_id}.png"),
  height = 5, width = 8, units = 'in', res = 300
)
print(lexis_observed)
dev.off()

predicted_risks <- hazards_square[method != 'Observed', ][, haz_labels := scales::percent(haz, accuracy = 0.1)]
lexis_risks <- ggplot(data = predicted_risks, aes(x = bh_year, y = age_group_label)) + 
  facet_wrap('method', ncol = 1) +
  geom_tile(aes(fill = haz)) + 
  geom_text(aes(label = haz_labels)) + 
  scale_fill_gradientn(colors = viridisLite::magma(100)[15:100], labels = scales::percent) +
  labs(
    title = glue::glue('Model classification probabilities, averaged by age group and year,\n{this_country}'),
    x = "Year",
    y = "Age group",
    fill = "Model-assigned\nclassification\nprobabilities\n(averages)"
  ) +
  theme_bw()

png(
  glue::glue("{viz_dir}/lexis_risks_{survey_id}.png"),
  height = 13, width = 8, units = 'in', res = 300
)
print(lexis_risks)
dev.off()
