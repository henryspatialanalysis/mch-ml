## #######################################################################################
## 
## VISUALIZE MODEL PERFORMANCE USING CONFUSION MATRICES
## 
## #######################################################################################

library(data.table); library(ggplot2); library(scales)

working_dir <- '~/temp_data/usaid-mch-ml'
survey_id <- 'SN2019DHS'
method_name <- 'treebag'
imputed_data_version <- '20240417'
results_version <- '20240505'

example_risk_cutoff <- 0.05

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "PH2022DHS", "SN2019DHS", "AVERAGE"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Philippines", "Senegal", "AVERAGE")
)
this_country <- all_surveys$country[which(all_surveys$survey_id == survey_id)]
age_groups_dt <- data.table::data.table(
  age_group = c("a01mo", "a06mo", "a12mo", "a24mo", "a36mo", "a48mo"),
  age_group_label = c('1-5mo.', '6-11mo.', '12-23mo.', '24-35mo', '36-47mo.', '48-59mo.')
)

model_dir <- file.path(working_dir, 'model_results', results_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)


## Load observations, predictions, shapefile -------------------------------------------->

obs <- glue::glue("{working_dir}/imputed_data/{imputed_data_version}/{survey_id}_imp1.csv") |>
  data.table::fread(select = c('bh_year', 'age_group', 'c_died_pnn', 'longitude', 'latitude'))

preds <- list.files(model_dir, pattern = 'predictions.csv$', full.names = TRUE) |>
  grep(pattern = method_name, value = TRUE) |>
  grep(pattern = survey_id, value = TRUE) |>
  lapply(data.table::fread) |>
  lapply(function(dt) (dt
    [, predicted := (predicted >= example_risk_cutoff) ]
    [, observed := (observed >= example_risk_cutoff) ]
    [, tp := as.integer(predicted & observed) ]
    [, fp := as.integer(predicted & !observed)]
  ))
obs$tps <- lapply(preds, function(dt) dt$tp) |> do.call(what = 'cbind') |> rowMeans()
obs$fps <- lapply(preds, function(dt) dt$fp) |> do.call(what = 'cbind') |> rowMeans()

adm_boundaries <- sf::st_read(
  "/mnt/c/Users/Lenovo/OneDrive - Henry Spatial Analysis LLC/Clients/DHS/Sharing/Updated GAUL shapefile/updated_gaul_v2/g2015_2014_1.shp",
  query = glue::glue("SELECT ADM1_NAME, ADM1_CODE FROM g2015_2014_1 WHERE ADM0_NAME = '{this_country}'")
)

# Get admin1 unit for each observation
obs_pts <- sf::st_as_sf(obs, coords = c('longitude', 'latitude'), crs = sf::st_crs("EPSG:4326"))
obs$ADM1_CODE <- sf::st_join(
  x = obs_pts, y = adm_boundaries[, c('ADM1_CODE')]
)$ADM1_CODE


## Get tables by age group, year, and cohort -------------------------------------------->

by_ag <- obs[, .(precision = sum(tps) / sum(tps + fps)), by = age_group][order(age_group)]
by_year <- obs[, .(precision = sum(tps) / sum(tps + fps)), by = bh_year][order(bh_year)]

by_adm1 <- obs[, .(precision = sum(tps)/ sum(tps + fps)), by = ADM1_CODE][order(ADM1_CODE)]
precision_sf <- merge(x = adm_boundaries, y = by_adm1, by = 'ADM1_CODE', all.x = TRUE)

# Make a map
precision_adm1_map <- ggplot() + 
  geom_sf(data = precision_sf, aes(fill = precision), color = '#222222', size = 0.5) + 
  scale_fill_gradientn(
    colors = RColorBrewer::brewer.pal(name = 'YlGn', n = 9)[2:9],
    labels = scales::percent
  ) +
  labs(
    title = glue::glue('Recall by first-level administrative unit, {this_country}'),
    subtitle = glue::glue("Risk cutoff: {scales::percent(example_risk_cutoff, accuracy = 1)}"),
    x = '', y = '',
    fill = 'Precision\n(TP / (TP + FP))'
  ) + 
  theme_minimal() + 
  theme(
    axis.ticks = element_blank(), axis.text = element_blank(),
    panel.grid.major = element_blank()
  )

png(
  glue::glue("{viz_dir}/precision_by_adm1_{this_country}.png"),
  height = 8, width = 8, units = 'in', res = 300
)
print(precision_adm1_map)
dev.off()

