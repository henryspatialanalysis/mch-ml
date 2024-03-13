# Map completeness of mortality data and predictors

CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'


## SETUP -------------------------------------------------------------------------------->

# Load packages
load_packages <- c('versioning', 'ggplot2', 'data.table')
lapply(load_packages, library, character.only = T) |> invisible()

# Load helper R package
config <- versioning::Config$new(CONFIG_PATH)

file_paths <- list.files(config$get_dir_path('prepared_data'))
countries <- substr(file_paths, 18, 19)

input_data_list <- file.path(config$get_dir_path('prepared_data'), file_paths) |>
  lapply(data.table::fread)
lapply(seq_along(full_data), function(ii) input_data_list[[ii]]$iso2 <<- countries[ii])
full_data <- data.table::rbindlist(input_data_list, use.names = T, fill = T) |>
  data.table::melt(id.vars = 'iso2') |>
  _[, .(completeness = 1 - mean(is.na(value))), by = .(variable, iso2)]

full_data$variable <- factor(full_data$variable, levels = rev(unique(full_data$variable)))
full_data$completeness_pct <- scales::percent(full_data$completeness, accuracy = 1)
full_data <- full_data[!grepl('_19', variable), ]

custom_fill <- c('#888888', viridisLite::magma(n = 99))
custom_color <- c('white', 'black') |> rep(each = 50)

# Plot completeness
completeness_fig <- ggplot(data = full_data, aes(x = iso2, y = variable)) + 
  geom_raster(aes(fill = completeness)) + 
  geom_text(aes(label = completeness_pct, color = completeness)) +
  scale_fill_gradientn(colors = custom_fill, guide = "none") + 
  scale_color_gradientn(colors = custom_color, guide = "none") + 
  scale_x_discrete(position = 'top') +
  labs(title = '', x = '', y = '') +
  theme_minimal()

png("~/temp_data/usaid-mch-ml/viz/variable_completeness_20240312.png", height = 24, width = 6, units = 'in', res = 300)
print(completeness_fig)
dev.off()
