## #######################################################################################
## 
## VISUALIZE SHAPLEY DECOMPOSITION RESULTS
## 
## #######################################################################################

library(data.table); library(ggplot2)

working_dir <- '~/temp_data/usaid-mch-ml'
dataset <- 'GH2022DHS'
prepared_data_version <- '20240312'
results_version <- '20240502'

## Load all results from this model version --------------------------------------------->

model_dir <- file.path(working_dir, 'model_results', results_version)
fns <- list.files(model_dir)
fns <- fns[which(startsWith(fns, dataset))]

# Create a table of normalized shapley values by model type and imputation
fns_shapley <- fns[!grepl("loss", fns)]
shapley_data <- lapply(file.path(model_dir, fns_shapley), data.table::fread) |> 
  data.table::rbindlist()
model_types <- shapley_data[, sort(unique(method))]
# Average across imputations
shapley_averages <- shapley_data[
  , .(shapley_value_norm = mean(shapley_value_norm)), by = .(survey_id, feature, method)
]

# Get baseline loss by imputation
prepared_data <- data.table::fread(
  glue::glue("{working_dir}/prepared_data/{prepared_data_version}/analysis_dataset_{dataset}.csv")
)
prepared_data[, bl_est_by_age := mean(died), by = age_group ]
baseline_loss_by_age <- prepared_data[, sum((died - bl_est_by_age)**2)]

# Calculate improvement from the baseline loss for each model and imputation
fns_loss <- fns[grepl('loss', fns)]
loss_data <- lapply(fns_loss, function(fn){
  subset_data <- data.table::fread(file.path(model_dir, fn))
  meta <- strsplit(fn, split = '_')[[1]]
  subset_data[, `:=` (survey_id = meta[1], method = meta[2], imputation = as.integer(meta[3]))]
  return(subset_data[model_type == 'Full'])
}) |> data.table::rbindlist()
loss_data[, improvement := baseline_loss_by_age - loss ]
improvement_average <- (loss_data
  [, .(improvement = mean(improvement)), by = .(survey_id, method)]
  [order(-improvement)]
)

# Scale shapley values by average improvement
(shapley_averages
  [improvement_average, total_improvement := i.improvement, on = 'method' ]
  [, shapley_value := shapley_value_norm * total_improvement ]
)


## Determine relative feature importance ------------------------------------------------>

# Load feature metadata
feature_meta <- data.table::fread(file.path(working_dir, 'model_results/feature_metadata.csv'))
feature_styling <- data.table::fread(file.path(working_dir, 'model_results/feature_styling.csv'))
shapley_meta <- merge(x = shapley_averages, y = feature_meta, by = 'feature', all.x = TRUE)

# Get relative importance of themes
theme_sort <- (shapley_meta
  [, .(importance = sum(shapley_value)), by = theme ]
  [order(-importance)]
  [, theme_rank := .I ]
)
# Get relative importance within themes
feature_sort <- (shapley_meta
  [, .(importance = sum(shapley_value)), by = .(feature, theme)]
  [order(theme, -importance)]
  [, feature_rank := seq_len(nrow(.SD)), by = theme ]
)
# Sort features by theme importance, then feature importance within theme
feature_meta <- (feature_meta
  [theme_sort, theme_rank := i.theme_rank, on = 'theme']
  [feature_sort, feature_rank := i.feature_rank, on = 'feature']
  [order(theme_rank, feature_rank)]
  [, feature_plot_order := .I ]
  [feature_styling, color := i.color, on = c('theme', 'feature_rank')]
)
# Use this ranking to set plotting order
shapley_meta$feature_label <- factor(
  shapley_meta$feature_label,
  levels = feature_meta$feature_label
)

# Set plotting order by method
shapley_meta$method <- factor(shapley_meta$method, levels = improvement_average$method)

# Set color palette by feature
color_palette <- feature_meta$color
names(color_palette) <- feature_meta$feature_label


## Create figure ------------------------------------------------------------------------>

shapley_fig <- ggplot(data = shapley_meta) +
  geom_bar(
    aes(x = method, y = shapley_value, fill = feature_label),
    position = 'stack', stat = 'identity'
  ) + 
  labs(
    title = 'Shapley decomposition: Ghana',
    x = 'Model',
    y = "Shapley absolute feature importance",
    fill = "Feature"
  ) +
  scale_fill_manual(values = color_palette) + 
  scale_y_continuous(labels = scales::comma) +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw()

pdf(
  glue::glue("{working_dir}/model_results/{results_version}/shapley_graph_{dataset}.pdf"),
  height = 8, width = 12
)
print(shapley_fig)
dev.off()
