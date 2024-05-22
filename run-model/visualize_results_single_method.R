## #######################################################################################
## 
## VISUALIZE SHAPLEY DECOMPOSITION RESULTS
## 
## #######################################################################################

library(data.table); library(ggplot2)

working_dir <- '~/temp_data/usaid-mch-ml'
method_name <- 'rf'
title <- 'Relative feature importance: Random forest'
prepared_data_version <- '20240516'
results_version <- '20240521'

all_surveys <- data.table::data.table(
  survey_id = c("CI2021DHS", "GH2022DHS", "KE2022DHS", "MD2021DHS", "PH2022DHS", "SN2019DHS", "AVERAGE"),
  country = c("Cote d'Ivoire", "Ghana", "Kenya", "Madagascar", "Philippines", "Senegal", "AVERAGE")
)
all_surveys$country <- factor(all_surveys$country, levels = all_surveys$country)

## Load all results from this model version --------------------------------------------->

model_dir <- file.path(working_dir, 'model_results', results_version)
viz_dir <- file.path(working_dir, 'viz', results_version)
if(!dir.exists(viz_dir)) dir.create(viz_dir)

shapley_vals <- list.files(model_dir, pattern = 'shapley_vals', full.names = TRUE) |>
  grep(pattern = method_name, value = TRUE) |>
  grep(pattern = paste0(all_surveys$survey_id, collapse = '|'), value = TRUE) |>
  lapply(data.table::fread) |>
  data.table::rbindlist()
# Ignore old features not used in the current model set
old_features <- c('w_hg_adjusted', 'w_edu_years', 's_vax_pneumo3')
shapley_vals <- shapley_vals[!feature %in% old_features, ]

shapley_vals <- shapley_vals[
  , .(shapley_value_norm = mean(shapley_value_norm)), by = .(survey_id, method, feature)
][, shapley_value_norm := shapley_value_norm / sum(shapley_value_norm), by = survey_id]
average_vals <- shapley_vals[
  , .(shapley_value_norm = mean(shapley_value_norm)), by = .(method, feature)
][, shapley_value_norm := shapley_value_norm / sum(shapley_value_norm)][, survey_id := "AVERAGE"]

shapley_full <- data.table::rbindlist(list(shapley_vals, average_vals), use.names = TRUE)


## Determine relative feature importance ------------------------------------------------>

# Load feature metadata
feature_meta <- data.table::fread(file.path(working_dir, 'model_results/feature_metadata.csv'))
feature_styling <- data.table::fread(file.path(working_dir, 'model_results/feature_styling.csv'))
shapley_meta <- merge(x = shapley_full, y = feature_meta, by = 'feature', all.x = TRUE)

# Check for missing feature metadata
missing_features <- setdiff(shapley_full$feature, feature_meta$feature)
if(length(missing_features) > 0){
  stop("Missing feature metadata for: ", paste(missing_features, collapse = ", "))
}

# Get relative importance of themes
theme_sort <- (shapley_meta
  [, .(importance = sum(shapley_value_norm)), by = theme ]
  [order(-importance)]
  [, theme_rank := .I ]
)
# Get relative importance within themes
feature_sort <- (shapley_meta
  [, .(importance = sum(shapley_value_norm)), by = .(feature, theme)]
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

# Set plotting order by country
shapley_meta[all_surveys, country := i.country, on = 'survey_id' ]

# Set color palette by feature
color_palette <- feature_meta$color
names(color_palette) <- feature_meta$feature_label


## Create figure ------------------------------------------------------------------------>

shapley_fig <- ggplot(data = shapley_meta) +
  geom_bar(
    aes(x = country, y = shapley_value_norm, fill = feature_label),
    position = 'stack', stat = 'identity'
  ) + 
  labs(
    title = title,
    x = 'Country',
    y = "Shapley relative feature importance",
    fill = "Feature"
  ) +
  scale_fill_manual(values = color_palette) + 
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw()

png(
  glue::glue("{viz_dir}/shapley_graph_relative_{method_name}.png"),
  height = 7.5, width = 13, units = 'in', res = 300)
print(shapley_fig)
dev.off()
