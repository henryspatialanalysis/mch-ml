## #######################################################################################
##
## COVARIATE BOXPLOTS
##
## #######################################################################################

CONFIG_PATH <- '~/repos/usaid-mch-ml/data-prep-config.yaml'
load_packages <- c('devtools', 'versioning', 'tictoc', 'ggplot2', 'data.table', 'RColorBrewer', 'sf')
lapply(load_packages, library, character.only = T) |> invisible()

config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))

# Create viz directory
viz_dir <- file.path(config$get_dir_path('prepared_data'), 'viz')
dir.create(viz_dir, showWarnings = F)

# Load all microdata
analysis_files <- config$get_dir_path('prepared_data') |>
  list.files(pattern = 'analysis_dataset', full.names = T)
microdata_list <- lapply(analysis_files, lapply(versioning::autoread))
analysis_countries <- sort(config$get('countries'))
for(ii in seq_along(microdata_list)) microdata_list[[ii]]$country <- analysis_countries[ii]
microdata_full <- data.table::rbindlist(microdata_list, fill = T, use.names = T)
microdata_full[, died_factor := 'Survived' ][c_died_pnn == 1L, died_factor := 'Died']


# Get quantiles of each indicator by country and child mortality status
skip_cols <- c(
  'cluster', 'c_birth_year', 'hh_id', 'w_id', 'w_sample_weight', 'birth_id',
  'admin1_code', 'admin1_name', 'country'
)
viz_cols <- setdiff(colnames(microdata_full), c(skip_cols, 'c_died_pnn'))

# Run visualizations
for(viz_col in viz_cols) try({
  check_vals <- na.omit(microdata_full[[viz_col]])
  if(all(range(check_vals) == c(0L, 1L))){
    proportions <- microdata_full[, .(props = mean(get(viz_col), na.rm=T)), by = .(country, died_factor)]
    fig <- ggplot(data = proportions, aes(x = died_factor, fill = died_factor, y = props)) + 
      facet_wrap('country', ncol = 1) + 
      geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_manual(values = c(Survived = '#2683C6', Died = '#FF3399')) +
      labs(x = '', y = '', fill = 'Outcome') +
      coord_flip() + 
      theme_minimal() + 
      theme(axis.text.y = element_blank())
  } else {
    lims <- grDevices::boxplot.stats(check_vals)$stats[c(1, 5)] * 1.25
    fig <- ggplot(
        data = microdata_full[!is.na(get(viz_col)), ],
        aes(x = died_factor, fill = died_factor, y = get(viz_col))
      ) + 
      facet_wrap('country', ncol = 1) +
      geom_boxplot(outlier.color = '#888888', outlier.size = .1) +
      scale_fill_manual(values = c(Survived = '#2683C6', Died = '#FF3399')) +
      scale_y_continuous(limits = lims, oob = scales::squish) +
      labs(x = '', y = '', fill = 'Outcome') +
      coord_flip() +
      theme_minimal() + 
      theme(axis.text.y = element_blank())
  }
  png(glue::glue('{viz_dir}/boxplot_{viz_col}.png'), height = 4.5, width = 7, units = 'in', res = 200)
  print(fig)
  dev.off()
})
