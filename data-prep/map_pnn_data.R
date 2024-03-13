## #######################################################################################
##
## MAP POST-NEONATAL MORTALITY DATA
##
## #######################################################################################

## Setup
CONFIG_PATH <- '~/repos/usaid-mch-ml/config.yaml'
load_packages <- c('devtools', 'versioning', 'tictoc', 'ggplot2', 'data.table', 'RColorBrewer', 'sf')
lapply(load_packages, library, character.only = T) |> invisible()

config <- versioning::Config$new(CONFIG_PATH)
devtools::load_all(config$get_dir_path('r_package'))

# Create viz directory
viz_dir <- file.path(config$get_dir_path('prepared_data'), 'viz')
dir.create(viz_dir, showWarnings = F)

# Load global admin0 and admin1 shapefiles
adm0_sf <- config$read("shps", "adm0")
adm1_sf <- config$read("shps", "adm1")

# Set PNN color scheme
pnn_colors <- RColorBrewer::brewer.pal(n = 9, name = "Spectral") |> rev()


## Plot by country ---------------------------------------------------------------------->

all_countries <- config$get('countries')

for(country in all_countries){
  # Load prepared microdata, identified by the corresponding survey ID
  survey_id <- config$get("survey_ids")[which(all_countries == country)]
  microdata <- versioning::autoread(
    glue::glue("{config$get_dir_path('prepared_data')}/analysis_dataset_{survey_id}.csv")
  )
  # Drop "null island"
  microdata <- microdata[(latitude != 0L) | (longitude != 0L), ]

  # Collapse by cluster
  collapsed <- microdata[
    , c(.(sampsize = .N), lapply(.SD, mean, na.rm=T)),
    .SDcols = c('latitude', 'longitude', 'c_died_pnn'),
    by = cluster
  ]
  # Subset global shapefiles
  match_name <- if(country == "Cote D'Ivoire") "Côte d'Ivoire" else country
  country_adm0 <- adm0_sf[adm0_sf$ADM0_NAME == match_name, ]
  country_adm1 <- adm1_sf[adm1_sf$ADM0_NAME == match_name, ]
  if(nrow(country_adm0) == 0L) stop("Could not find a shapefile match for ", match_name)

  # Plot results
  if(country %in% c('Kenya', 'Philippines','Madagascar')){
    color_limits <- c(0, 0.15)
    color_breaks <- seq(0, 0.15, by = 0.05)
    color_labels <- c('0%','5%','10%','15%+')
  } else {
    color_limits <- c(0, 0.2)
    color_breaks <- seq(0, 0.2, by = 0.05)
    color_labels <- c('0%','5%','10%','15%','20%+')
  }

  results_map <- ggplot() + 
    geom_sf(data = country_adm0, fill = NA, color = '#222222', linewidth = .5) +
    geom_sf(data = country_adm1, fill = NA, color = '#222222', linewidth = .25) +
    geom_point(
      data = collapsed,
      aes(x = longitude, y = latitude, color = c_died_pnn, size = sampsize),
      alpha = .7
    ) +
    scale_color_gradientn(
      colors = pnn_colors, limits = color_limits, oob = scales::squish,
      breaks = color_breaks, labels = color_labels
    ) + 
    scale_size_continuous(range = c(.5, 5)) +
    labs(
      title = country, color = 'Proportion died\naged 1-59mo.',
      size = 'Sample size', x = '', y = ''
    ) +
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank()
    )
    # Save to file
    png(glue::glue('{viz_dir}/{survey_id}_pnn_mapped.png'), height = 1200, width = 1500, res = 300)
    print(results_map)
    dev.off()
}