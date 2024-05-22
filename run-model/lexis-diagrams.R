## #######################################################################################
## 
## VISUALIZE MODEL ESTIMATES USING LEXIS DIAGRAMS
## 
## #######################################################################################

library(data.table); library(ggplot2); library(scales)

working_dir <- '~/temp_data/usaid-mch-ml'
check_methods <- c("glm", "treebag", "rf", "rsf")
imputed_data_version <- '20240516'
results_version <- '20240521'

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "NG2018DHS", "PH2022DHS", "SN2019DHS"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Nigeria", "Philippines", "Senegal")
)
age_groups_dt <- data.table::data.table(
  age_group = c("a01mo", "a06mo", "a12mo", "a24mo", "a36mo", "a48mo"),
  age_group_label = c('1-5mo.', '6-11mo.', '12-23mo.', '24-35mo', '36-47mo.', '48-59mo.')
)
age_groups_dt[, age_group_label := factor(age_group_label, levels = age_group_label)]



## Prepare matrix of hazards ------------------------------------------------------------>

model_dir <- file.path(working_dir, 'model_results', results_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)

for(survey_id in all_surveys$survey_id){
  message("Preparing survey: ", survey_id)
  this_country <- all_surveys$country[which(all_surveys$survey_id == survey_id)]

  true_obs <- glue::glue("{working_dir}/imputed_data/{imputed_data_version}/{survey_id}_imp1.csv") |>
    data.table::fread(select = c('bh_year', 'age_group', 'died'))
  (true_obs
    [, method := 'OBSERVED']
    [age_groups_dt, age_group_label := i.age_group_label, on = 'age_group']
  )

  preds_list <- lapply(check_methods, function(method_name){
    pred_files <- list.files(model_dir, pattern = 'h0_predictions.csv$', full.names = TRUE) |>
      grep(pattern = method_name, value = TRUE) |>
      grep(pattern = survey_id, value = TRUE)
    if(length(pred_files) == 0L) return(NULL)
    preds_vec <- pred_files |>
      lapply(data.table::fread, select = 'predicted') |>
      lapply(unlist) |>
      do.call(what = 'cbind') |>
      rowMeans()
    preds_dt <- copy(true_obs)[, died := preds_vec ][, method := method_name ]
    return(preds_dt)  
  })

  full_data <- data.table::rbindlist(c(list(true_obs), preds_list), use.names = T, fill = T)
  hazards <- full_data[, .(haz = mean(died)), by = .(method, bh_year, age_group_label)]

  # Fill missing values
  hazards_square <- data.table::CJ(
    method = sort(unique(full_data$method)),
    bh_year = sort(unique(full_data$bh_year)),
    age_group_label = age_groups_dt$age_group_label,
    haz = NA_real_
  )[hazards, haz := i.haz, on = c('method', 'bh_year', 'age_group_label')]
  hazards_square$method <- factor(hazards_square$method, levels = c("OBSERVED", sort(check_methods)))
  # Drop final year (incompletely reported)
  final_year <- hazards_square[, max(bh_year) ]
  hazards_square <- hazards_square[bh_year < final_year, ]

  # Represent per 1,000
  hazards_square[, haz_per_1000 := round(haz * 1e3, 0)]

  # Reorder methods: Observed first, random survival forests last
  all_methods <- unique(hazards_square$method)
  if('rsf' %in% all_methods){
    hazards_square$method <- factor(
      hazards_square$method,
      levels = c('OBSERVED', all_methods |> setdiff(c('OBSERVED', 'rsf')) |> sort(), 'rsf')
    )
  } else {
    hazards_square$method <- factor(
      hazards_square$method,
      levels = c('OBSERVED', all_methods |> setdiff('OBSERVED') |> sort())
    )
  }


  ## Create Lexis diagram --------------------------------------------------------------->

  if(this_country == 'Nigeria'){
    plot_max <- 25
  } else if(this_country %in% c("Kenya", "Ghana", "Philippines")){
    plot_max <- 10
  } else {
    plot_max <- 15
  }
  plot_breaks <- seq(0, plot_max, length.out = 6)
  plot_lims <- range(plot_breaks)
  plot_labs <- c(plot_breaks[1:5], paste0(plot_max, '+'))

  hazards_observed <- hazards_square[method == 'OBSERVED', ]
  lexis_observed <- ggplot(data = hazards_observed, aes(x = bh_year, y = age_group_label)) + 
    geom_tile(aes(fill = haz_per_1000)) + 
    geom_text(aes(label = haz_per_1000)) + 
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal('Spectral', n = 9) |> rev(),
      limits = plot_lims, breaks = plot_breaks, labels = plot_labs,
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

  lexis_risks <- ggplot(data = hazards_square, aes(x = bh_year, y = age_group_label)) + 
    facet_wrap('method', ncol = all_methods |> length() |> sqrt() |> ceiling()) +
    geom_tile(aes(fill = haz_per_1000)) + 
    geom_text(aes(label = haz_per_1000)) + 
    scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal('Spectral', n = 9) |> rev(),
      limits = plot_lims, breaks = plot_breaks, labels = plot_labs,
      oob = scales::squish
    ) +
    labs(
      title = glue::glue('Observed and model-estimated mortality risk by detailed age group and year, {this_country}'),
      x = "Year",
      y = "Age group",
      fill = "Mortality\nrisk (nQx)\nper 1,000"
    ) +
    theme_bw()

  png(
    glue::glue("{viz_dir}/lexis_full_{survey_id}.png"),
    height = 8, width = 14, units = 'in', res = 300
  )
  print(lexis_risks)
  dev.off()
}
