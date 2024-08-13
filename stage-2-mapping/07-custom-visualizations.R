## #######################################################################################
##
## 05) MODEL VISUALIZATION
##
## AUTHOR: Nathaniel Henry, nat@henryspatialanalysis.com
## CREATED: 20 July 2024
## PURPOSE: Demonstrate many possible visualizations for MBG results
##
## #######################################################################################

DEFAULT_CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'

## Set globals
## These will ultimately be set by CLI
COUNTRY <- 'Nigeria'
ISO3 <- 'NGA'
YEAR <- 2018
INDICATOR <- 'stunting'
RUN_SET <- '20240805'
ADM2_TITLE <- 'LGA'
CONFIG_PATH <- DEFAULT_CONFIG_PATH

message(glue::glue("Visualizations for {INDICATOR} in {COUNTRY}, run set {RUN_SET}"))


## SETUP -------------------------------------------------------------------------------->

# Load packages
load_packages <- c('data.table', 'pixel2poly', 'versioning', 'ggplot2')
lapply(load_packages, library, character.only = TRUE) |> invisible()

# Load config
config <- versioning::Config$new(CONFIG_PATH)

# Load mch.ml
config$get_dir_path('r_package') |> devtools::load_all()
config$get_dir_path('mbg_repo') |> devtools::load_all()

# Get model directory
model_dir <- file.path(
  config$get_dir_path('mbg_model_results'), RUN_SET, paste0(INDICATOR, '_', ISO3)
)
if(!dir.exists(model_dir)) stop("Model directory ", model_dir, " does not exist.")
message("Using model directory ", model_dir)
model_config <- versioning::Config$new(file.path(model_dir, 'config.yaml'))

# Create viz directory as a subfolder
viz_dir <- file.path(model_dir, 'viz')
dir.create(viz_dir, showWarnings = FALSE)


## Prepare all visualization inputs ----------------------------------------------------->

# Cross-country results
results_base_dir <- dirname(model_dir)
all_result_dirs <- list.files(results_base_dir, pattern = paste0('^', INDICATOR), full.names = T)
adm2_all <- file.path(all_result_dirs, 'adm2_summaries.csv') |>
  lapply(data.table::fread) |>
  data.table::rbindlist()
adm0_all <- file.path(all_result_dirs, 'adm0_summaries.csv') |>
  lapply(data.table::fread) |>
  data.table::rbindlist()

# Results for this country only
adm2_country <- adm2_all[ADM0_NAME == COUNTRY, ]
mean_raster <- versioning::autoread(file.path(model_dir, 'mean_raster.tif'))

# Boundaries
adm2_sf <- config$read("mbg_shapefile", "adm2") |> dplyr::filter(ADM0_NAME == COUNTRY)
adm1_sf <- config$read("mbg_shapefile", "adm1") |> dplyr::filter(ADM0_NAME == COUNTRY)

# Population raster
id_raster <- versioning::autoread(file.path(model_dir, 'id_raster.tif'))
pop_covariate_table <- model_config$get('mbg_settings', 'pop_covariate_settings') |>
  data.table::as.data.table()
population_raster <- mbg::load_covariates(
  directory = config$get_dir_path('mbg_covariates'),
  covariates_table = pop_covariate_table,
  id_raster = id_raster,
  year = YEAR,
  file_format = config$get('mbg_settings', 'covariate_settings', 'file_format'),
  add_intercept = FALSE
)[[1]]

# Wealth index estimates
wealth_index <- glue::glue("{results_base_dir}/hhwi_cont_{ISO3}/mean_raster.tif") |>
  versioning::autoread()

# Travel time estimates
tt_covariate_table <- config$read('metadata', 'mbg_covariates_table') |>
  _[covariate == 'tt_hcf', ] |>
  _[, normalize := FALSE ]
tt_raster <- mbg::load_covariates(
  directory = config$get_dir_path('mbg_covariates'),
  covariates_table = tt_covariate_table,
  id_raster = id_raster,
  year = YEAR,
  file_format = config$get('mbg_settings', 'covariate_settings', 'file_format'),
  add_intercept = FALSE
)[[1]]

# Some common ggplot objects
ind_title <- stringr::str_to_title(INDICATOR)
eb <- ggplot2::element_blank()
map_theme <- ggplot2::theme_minimal() + 
  ggplot2::theme(axis.ticks = eb, axis.text = eb, panel.grid = eb)


## Custom visualization: Counts --------------------------------------------------------->

counts_pal <- RColorBrewer::brewer.pal(name = 'Purples', n = 9)
adm2_counts <- adm2_sf |>
  merge(
    y = adm2_country[, .(ADM1_CODE, ADM2_CODE, ADM2_NAME, count_mean)],
    by = c('ADM1_CODE', 'ADM2_CODE', 'ADM2_NAME')
  )

largest_pow_10 <- adm2_country$count_mean |> max() |> log10() |> floor()
count_breaks <- 10**seq(2, largest_pow_10)
max_break_ratio <- round(quantile(adm2_country$count_mean, .95) / as.integer(max(count_breaks)))
if(max_break_ratio > 2) count_breaks <- c(count_breaks, max(count_breaks) * max_break_ratio)

counts_fig <- ggplot() + 
    geom_sf(data = adm2_counts, aes(fill = count_mean), linewidth = 0.05, color = '#444444') + 
    geom_sf(data = adm1_sf, fill = NA, linewidth = 0.25, color = '#222222') + 
    scale_fill_gradientn(
      colors = counts_pal, limits = range(count_breaks), breaks = count_breaks,
      oob = scales::squish, labels = scales::comma, trans = 'log'
    ) +
    labs(title = "", x = '', y = '', fill = glue::glue("{ind_title}\n(Count by {ADM2_TITLE})")) +
    map_theme

png(file.path(viz_dir, 'counts_map.png'), height = 8, width = 8, units = 'in', res = 250)
plot(counts_fig)
dev.off()


## Custom visualization: Plots by travel time ------------------------------------------->

## Travel time plot
tt_pal <- viridisLite::rocket(100)

tt_dt <- as.data.frame(tt_raster, xy = TRUE)
colnames(tt_dt)[3] <- 'tt'

max_plot_val <- 150
plot_breaks <- seq(0, max_plot_val, length.out = 4)
plot_labs <- as.character(plot_breaks)
plot_labs[length(plot_labs)] <- paste0(plot_labs[length(plot_labs)], '+')

tt_fig <- ggplot() +
  geom_raster(data = tt_dt, aes(x = x, y = y, fill = tt)) +
  geom_sf(data = adm2_sf, fill = NA, linewidth = 0.05, color = '#888888') + 
  geom_sf(data = adm1_sf, fill = NA, linewidth = 0.25, color = '#aaaaaa') + 
  scale_fill_gradientn(
    colors = tt_pal, breaks = plot_breaks, limits = range(plot_breaks),
    labels = plot_labs, oob = scales::squish
  ) +
  labs(title = '', x = '', y = '', fill = 'Motorized travel\ntime to nearest\nhospital (minutes)') +
  map_theme

png(file.path(viz_dir, 'tt_map.png'), height = 8, width = 8, units = 'in', res = 250)
plot(tt_fig)
dev.off()

## Travel time versus stunting rates
tt_summary <- (
  data.table::data.table(
    pop = terra::values(population_raster)[, 1],
    stunting = terra::values(mean_raster)[, 1],
    tt = terra::values(tt_raster)[, 1]
  ) |>
    na.omit() |>
    _[, .(
      s_mean = weighted.mean(stunting, w = pop),
      s_lower = Hmisc::wtd.quantile(stunting, w = pop, probs = 0.2),
      s_upper = Hmisc::wtd.quantile(stunting, w = pop, probs = 0.8)
      ), 
      by = .(tt = pmin(round(tt), 30))
    ]
)[order(tt)]

tt_summary_breaks <- seq(0, 30, by = 10)
tt_summary_labels <- as.character(tt_summary_breaks)
tt_summary_labels[length(tt_summary_labels)] <- paste0(
  tt_summary_labels[length(tt_summary_labels)], '+'
)

tt_summary_fig <- ggplot(data = tt_summary, aes(x = tt)) + 
  geom_errorbar(aes(ymin = s_lower, ymax = s_upper)) +
  geom_point(aes(y = s_mean), size = 1.5) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = tt_summary_breaks, labels = tt_summary_labels) + 
  labs(
    x = 'Travel time to healthcare (minutes)',
    y = 'Child stunting rates',
    title = 'Child stunting summary by travel time',
    subtitle = 'Bars show 20th-80th percentiles in bin'
  ) +
  theme_bw()
png(file.path(viz_dir, 'tt_summary.png'), height = 4, width = 6, units ='in', res = 300)
plot(tt_summary_fig)
dev.off()


## Custom visualization: Plots by wealth index ------------------------------------------>

hhwi_lower <- -1.5e5
hhwi_upper <- 1.5e5
hhwi_full <- data.table::data.table(
  pop = terra::values(population_raster)[, 1],
  stunting = terra::values(mean_raster)[, 1],
  hhwi = terra::values(wealth_index)[, 1]
)
hhwi_summary <- hhwi_full |>
  na.omit() |>
  _[, .(
    mean = weighted.mean(stunting, w = pop),
    lower = Hmisc::wtd.quantile(stunting, w = pop, probs = 0.2),
    upper = Hmisc::wtd.quantile(stunting, w = pop, probs = 0.8)
    ), 
    by = .(hhwi = pmax(hhwi_lower, pmin(hhwi_upper, round(hhwi/1e4) * 1e4)))
  ] |>
  _[order(hhwi)]

hhwi_breaks <- seq(hhwi_lower, hhwi_upper, length.out = 4)
hhwi_labels <- paste0(hhwi_breaks/1e3, 'k')

hhwi_summary_fig <- ggplot(data = hhwi_summary, aes(x = hhwi)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = mean), size = 1.5) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = hhwi_breaks, labels = hhwi_labels) + 
  labs(
    x = 'Household wealth index',
    y = 'Child stunting rates',
    title = 'Child stunting summary by household wealth index',
    subtitle = 'Bars show 20th-80th percentiles in bin'
  ) +
  theme_bw()
png(file.path(viz_dir, 'hhwi_summary.png'), height = 4, width = 6, units ='in', res = 300)
plot(hhwi_summary_fig)
dev.off()

low_threshold <- -5e4

hhwi_threshold_dt <- na.omit(hhwi_full)[
  , .(pop = sum(pop), stunted = sum(pop * stunting)),
  by = .(low = as.integer(hhwi <= low_threshold)) 
][, `:=` (pop = pop / sum(pop), stunted = stunted / sum(stunted))]
knitr::kable(hhwi_threshold_dt)


## Custom visualization: cross-country comparison --------------------------------------->

cross_country_colors <- viridisLite::turbo(100)

adm0_all <- adm0_all[order(mean)]
adm2_all$adm0_factor <- factor(adm2_all$ADM0_NAME, levels = adm0_all$ADM0_NAME)

cc_breaks <- seq(0, .5, by = .1)
cc_labs <- scales::percent(cc_breaks)
cc_labs[length(cc_labs)] <- paste0(cc_labs[length(cc_labs)], '+')

cross_country_fig <- ggplot(
    data = adm2_all,
    aes(x = adm0_factor, y = mean, color = mean, size = population)
  ) + 
  geom_point(alpha = 0.2) +
  theme_bw() + 
  scale_color_gradientn(
    colors = cross_country_colors, limits = range(cc_breaks), breaks = cc_breaks,
    labels = cc_labs, oob = scales::squish
  ) +
  scale_size_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = 'Country',
    y = 'Stunting by district',
    color = 'Stunting',
    size = 'Under-5\npopulation',
    title = 'Six-country comparison of child stunting'
  ) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

png(file.path(viz_dir, 'cross_country_comparison.png'), height = 5, width = 8, units = 'in', res = 250)
plot(cross_country_fig)
dev.off()
