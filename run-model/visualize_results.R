## #######################################################################################
## 
## VISUALIZE SHAPLEY DECOMPOSITION RESULTS
## 
## #######################################################################################

library(data.table); library(ggplot2)

model_results_dir <- '~/temp_data/usaid-mch-ml/model_results/20240419/'

# Load data 
combined <- fread(file.path(model_results_dir, "Ghana_combined.csv"))

combined$feature_label <- factor(combined$feature_label, levels = combined$feature_label)

color_palette <- combined$color
names(color_palette) <- combined$feature_label

# Create figure
shapley_fig <- ggplot(data = combined) +
  geom_bar(
    aes(fill = feature_label, x = method_label, y = shapley_value_norm),
    position = 'stack', stat = 'identity'
  ) + 
  labs(
    title = 'Shapley decomposition: Ghana',
    x = 'Model',
    y = "Shapley normalized feature importance",
    fill = "Feature"
  ) +
  scale_fill_manual(values = color_palette) + 
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw()

pdf(file.path(model_results_dir, 'example_results_ghana_rf.pdf'), height = 8, width = 10)
print(shapley_fig)
dev.off()
