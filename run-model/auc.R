## #######################################################################################
## 
## VISUALIZE MODEL PERFORMANCE USING CONFUSION MATRICES
## 
## #######################################################################################

load_pkgs <- c('data.table', 'ggplot2', 'scales', 'glue', 'pROC', 'gridExtra')
lapply(load_pkgs, library, character.only = T) |> suppressPackageStartupMessages() |> invisible()

working_dir <- '~/temp_data/usaid-mch-ml'
all_methods <- c("treebag", "rf", "glm")
results_version <- '20240521'
imputed_data_version <- '20240516'
holdouts <- 1:5
imputations <- 1:5
id_cols <- c('cluster', 'hh_id', 'w_id', 'birth_id')

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "NG2018DHS", "PH2022DHS", "SN2019DHS"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Nigeria", "Philippines", "Senegal")
)



## Create ROC curves and get AUC for each survey and method ----------------------------->

model_dir <- file.path(working_dir, 'model_results', results_version)
imputed_data_dir <- file.path(working_dir, 'imputed_data', imputed_data_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)

# Helper functions to load data and results files
imputed <- function(svy, imp){
  data.table::fread(glue::glue("{imputed_data_dir}/{svy}_imp{imp}.csv"))
}
preds <- function(svy, method, imp, holdout){
  data.table::fread(glue::glue("{model_dir}/{svy}_{method}_{imp}_h{holdout}_predictions.csv"))
}

# Names of all in-sample and out-of-sample columns
is_cols <- paste0('is_', imputations)
oos_cols <- paste0('oos_', imputations)

# Iterate by survey
full_results <- lapply(all_surveys[, .I], function(svy_ii){
  svy <- all_surveys[svy_ii, survey_id]
  country <- all_surveys[svy_ii, country]
  # Load input data
  # Can be any imputed version -- outcomes are complete and the same across imputations
  input_data <- imputed(svy, 1)  
  # Iterate through methods
  check_methods <- if(country == 'Nigeria') setdiff(all_methods, 'rf') else all_methods
  method_results <- lapply(check_methods, function(method){
    # Create a clean dataset specific to this method
    method_data <- data.table::copy(input_data)
    # Iterate through imputations
    imputation_results <- lapply(imputations, function(imp){
      is <- preds(svy, method, imp, 0)
      method_data$is <- is$predicted
      method_data$oos <- NA_real_
      for(holdout in holdouts){
        oos <- preds(svy, method, imp, holdout)
        in_holdout <- method_data[, holdout_id == holdout]
        method_data$oos[in_holdout] <- oos$predicted
      }
      method_data$imputation <- imp
      keep_cols <- c(id_cols, 'longitude', 'latitude', 'imputation', 'died', 'is', 'oos')
      return(method_data[, ..keep_cols])
    }) |> data.table::rbindlist()
    imputation_results$method <- method
    return(imputation_results)
  }) |> data.table::rbindlist()
  # Add survey and country metadata
  method_results$survey_id <- svy
  method_results$country <- country
  return(method_results)
}) |> data.table::rbindlist()

# Combine from age groups to individuals
combine_q <- function(q_vec) 1 - prod(1 - q_vec)
indiv_results <- full_results[
  , c(.(died = max(died), x = mean(longitude), y = mean(latitude)), lapply(.SD, combine_q)),
  .SDcols = c('is', 'oos'),
  by = c('survey_id', 'country', 'method', 'imputation', id_cols)
]

# Get AUC for the individual-level results
get_auc <- function(obs, pred) as.numeric(pROC::roc(obs, pred, auc = T)$auc) |> suppressMessages()
get_roc_curve <- function(dt, obs_col = 'died', pred_col = 'oos', max_size = 1000){
  roc <- pROC::roc(dt[[obs_col]], dt[[pred_col]]) |> suppressMessages()
  roc_table <- data.table::data.table(
    tp_rate = rev(roc$sensitivities),
    fp_rate = rev(1 - roc$specificities)
  )[order(fp_rate)]
  # Subsample, if necessary
  if(max_size < nrow(roc_table)){
    keep_rows <- c(
      1:10,
      seq(11, nrow(roc_table) - 10, length.out = max_size - 20),
      (-9:0) + nrow(roc_table)
    )
    keep_rows <- round(seq(1, nrow(roc_table), length.out = max_size))
    roc_table <- roc_table[keep_rows, ]
  } 
  return(roc_table)
}

## Calculate and plot AUC by country and method ----------------------------------------->

indiv_auc <- indiv_results[
  , .(oos_auc = get_auc(died, oos)),
  by = c('survey_id', 'country', 'method', 'imputation')
]
fwrite(indiv_auc, file = file.path(viz_dir, 'auc_results.csv'))

average_oos_auc <- indiv_auc[, .(oos_auc = mean(oos_auc)), by = c('country', 'method')]
auc_fill_colors <- RColorBrewer::brewer.pal(name = 'YlGn', n = 9)[2:8]
auc_plot <- ggplot(data = average_oos_auc, aes(x = method, y = country)) + 
  geom_tile(aes(fill = oos_auc)) + 
  geom_text(aes(label = scales::percent(oos_auc, accuracy = 0.1)), color = 'black') + 
  labs(
    title = 'Out-of-sample AUC by country and method',
    x = 'Method',
    y = 'Country'
  ) +
  scale_fill_gradientn(colors = auc_fill_colors, guide = 'none') + 
  theme_minimal()

png(file.path(viz_dir, 'auc_plot.png'), height = 6, width = 6, units = 'in', res = 300)
print(auc_plot)
dev.off()


## Plot ROC curves by country ----------------------------------------------------------->

roc_plot_data <- indiv_results[,
  get_roc_curve(.SD),
  by = c('survey_id', 'country', 'method', 'imputation')
]
roc_plot_data[, imputation_factor := factor(imputation)]

roc_plot <- ggplot() +
  facet_wrap('country', nrow = 2) + 
  geom_line(data = roc_plot_data[imputation == 1, ], aes(x = fp_rate, y = tp_rate, color = method), alpha = .5) + 
  geom_line(data = roc_plot_data[imputation == 2, ], aes(x = fp_rate, y = tp_rate, color = method), alpha = .5) + 
  geom_line(data = roc_plot_data[imputation == 3, ], aes(x = fp_rate, y = tp_rate, color = method), alpha = .5) + 
  geom_line(data = roc_plot_data[imputation == 4, ], aes(x = fp_rate, y = tp_rate, color = method), alpha = .5) + 
  geom_line(data = roc_plot_data[imputation == 5, ], aes(x = fp_rate, y = tp_rate, color = method), alpha = .5) + 
  geom_abline(intercept = 0, slope = 1, color = '#888888', lwd = 1L, linetype = 2) + 
  labs(x = 'False positive rate', y = 'True positive rate', color = 'Method') +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(aspect.ratio = 1)
png(file.path(viz_dir, 'roc_curves.png'), height = 6, width = 12, units = 'in', res = 300)
print(roc_plot)
dev.off()


## Confusion matrices by country and method --------------------------------------------->

risk_cutoffs <- c(0.1, 0.05)

for(risk_cutoff in risk_cutoffs){
  message(glue::glue("Risk cutoff at {scales::percent(risk_cutoff)}"))
  cm <- (indiv_results
    [, est := (oos >= risk_cutoff)]
    [, .(
        tp = round(sum(died & est) / 5),
        fp = round(sum(!died & est) / 5),
        fn = round(sum(died & !est) / 5),
        tn = round(sum(!died & !est) / 5)
      ), by = .(method, country)
    ]
    [, `:=` (
      precision = scales::percent(tp / (tp + fp), accuracy = 1),
      recall = scales::percent(tp / (tp + fn), accuracy = 1)
    )]
    [order(method, country)]
  )
  knitr::kable(cm)
  fwrite(cm, file = glue::glue("{viz_dir}/confusion_matrix_{risk_cutoff * 100}.csv"))
}

## Map recall by admin1 unit for each country ------------------------------------------->

ADM1_RECALL_CUTOFF <- 0.05

# Load admin1 shapefile
adm1_sf <- sf::st_read(file.path(
  "/mnt/c/Users/Lenovo/OneDrive - Henry Spatial Analysis LLC/Clients/DHS/Sharing",
  "Updated GAUL shapefile/updated_gaul_v2/g2015_2014_1.shp"
))
adm1_sf[adm1_sf$ADM0_CODE == 66, 'ADM0_NAME'] <- "Cote d'Ivoire"
adm1_sub <- adm1_sf[adm1_sf$ADM0_NAME %in% cluster_locations$country, ]

# Merge on admin1 location codes for each cluster
cluster_locations <- (indiv_results
  [, .(country, survey_id, cluster, x, y)]
  [, lapply(.SD, mean), by = .(country, survey_id, cluster)]
) |> sf::st_as_sf(coords = c('x', 'y'), crs = sf::st_crs('EPSG:4326'))
adm1_by_cluster <- sf::st_join(
  x = cluster_locations, y = adm1_sub[, c('ADM0_NAME', 'ADM1_CODE')]
) |> as.data.table()
adm1_by_cluster <- adm1_by_cluster[ADM0_NAME == country, .(country, cluster, ADM1_CODE)]
indiv_results[adm1_by_cluster, ADM1_CODE := i.ADM1_CODE, on = c('country', 'cluster')]

# Calculate recall by admin1
recall_adm1 <- (indiv_results
  [method %in% c('rf', 'treebag'), ]
  [, est := (oos >= ADM1_RECALL_CUTOFF) ]
  [, .(
      tp = round(sum(died & est) / 5),
      fn = round(sum(died & !est) / 5)
    ),
    by = .(country, ADM1_CODE, method)
  ]
  [, recall := tp / (tp + fn)]
)
adm1_sub$dummy <- 1L
method_merge <- data.table::data.table(method = c('rf', 'treebag'), dummy = 1L)
recall_sf <- adm1_sub |> 
  merge(y = method_merge, by = 'dummy') |>
  merge(y = recall_adm1, by = c('ADM1_CODE', 'method'), all.x = TRUE)

# Function to create a faceted recall plot
recall_colors <- RColorBrewer::brewer.pal(name = 'YlGnBu', n = 9)[2:9]
build_recall_plot <- function(data){
  index <- data.table::CJ(
    countries = sort(unique(data$ADM0_NAME)),
    methods = c("rf", "treebag")
  )[order(countries, methods)]
  knitr::kable(index)
  grobs_list <- vector('list', length = nrow(index))
  for(ii in index[, .I]){
    data_sub <- data[data$ADM0_NAME == index$countries[ii] & data$method == index$methods[ii], ]
    fig <- ggplot() + 
      geom_sf(data = data_sub, aes(fill = recall), lwd = 0.25, color = '#222222') +
      scale_fill_gradientn(
        colors = recall_colors, limits = c(.3, .7), oob = scales::squish,
        labels = scales::percent, guide = 'none'
      ) + 
      labs(x = '', y = '') + 
      theme_minimal() + 
      theme(axis.text = element_blank(), panel.grid = element_blank())
    grobs_list[[ii]] <- ggplot2::ggplotGrob(fig)
  }
  gridExtra::arrangeGrob(
    grobs = grobs_list,
    layout_matrix = matrix(seq_len(nrow(index)), nrow = 2, byrow = F)
  )
}

png(file.path(viz_dir, 'recall_p1.png'), height = 7, width = 10, units = 'in', res = 300)
fig <- build_recall_plot(recall_sf[recall_sf$ADM0_NAME %in% c('Philippines', 'Ghana', 'Kenya'), ])
plot(fig)
dev.off()

png(file.path(viz_dir, 'recall_p2.png'), height = 7, width = 10, units = 'in', res = 300)
fig <- build_recall_plot(recall_sf[!recall_sf$ADM0_NAME %in% c('Philippines', 'Ghana', 'Kenya'), ])
plot(fig)
dev.off()




# Create plot
recall_plot <- ggplot() + 
  facet_grid(method ~ ADM0_NAME) + 
  geom_sf(data = recall_sf, aes(fill = recall), lwd = 0.25, color = '#222222') + 
  scale_fill_gradientn(
    colors = recall_colors,
    na.value = '#aaaaaa',
    limits = c(.3, .7001),
    breaks = seq(.3, .7, by = .1),
    oob = scales::squish,
    labels = scales::percent
  ) + 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) + 
  labs(fill = 'Recall')
png(file.path(viz_dir, 'recall_maps.png'), height = 8, width = 20, units = 'in', res = 300)
plot(recall_plot)
dev.off()