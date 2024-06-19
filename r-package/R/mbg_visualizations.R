#' Raster to Table
#' 
#' @description Helper function to convert a terra SpatRaster into a data.frame
#' 
#' @param raster_object terra SpatRaster object
#' @return Data.frame object
raster_to_table <- function(raster_object){
  raster_table <- as.data.frame(raster_object, xy = T)
  names(raster_table) <- c('x','y','value')
  return(raster_table)
}

#' MBG visualizations
#' 
#' @description Run standard visualizations for MBG input data and results
#' 
#' @param viz_dir (`character(1)`) Directory where all visualizations will be saved
#' @param input_data (`data.table`) Table with at least the columns "x", "y", "rate", and
#'   "samplesize".
#' @param mean_raster (`spatRaster`) Mean MBG estimates by pixel
#' @param ui_raster (`spatRaster`) Raster showing width of the 95% uncertainty interval
#'   for each pixel
#' @param admin_data_for_plotting (`sf`) sf object with at least the rows 'mean', 'lower',
#'   and 'upper'. Used for both visualization and admin2 boundaries.
#' @param adm1_boundaries (`sf`) sf object containing admin1 boundaries
#' @param mesh (`INLA mesh`, default NULL) INLA mesh object.
#' @param stacked_covariates (list of `SpatRaster` objects) List containing stacked
#'   covariates, if stacked covariates should be plotted.
#' @param title_base (`character(1)`) Template for all plot titles.
#' @param color_scheme (`character(N)`) vector of color codes or names for a gradient
#'   color scheme.
#' @param indicator_name (`character(1)`) Name of the outcome being plotted, for use in
#'   some legend titles.
#' @param label_function (`function`) Function converting a numeric vector into a set of
#'   labels.
#' 
#' @import ggplot2
#' @export
mbg_visualizations <- function(
  viz_dir,
  input_data,
  mean_raster,
  ui_raster,
  admin_data_for_plotting,
  adm1_boundaries,
  mesh = NULL,
  stacked_covariates = NULL,
  plot_stackers = TRUE,
  title_base = "",
  color_scheme = RColorBrewer::brewer.pal("Spectral", n = 9),
  indicator_name = "Indicator",
  label_function = scales::comma
){
  # Helper elements
  eb <- ggplot2::element_blank()
  map_theme <- ggplot2::theme_minimal() + 
    ggplot2::theme(axis.ticks = eb, axis.text = eb, panel.grid = eb)

  ## Plot the input point data
  point_color_limits <- quantile(na.omit(input_data$rate), probs = c(0.05, 0.95))
  data_point_fig <- ggplot() + 
    geom_sf(data = admin_data_for_plotting, fill = NA, linewidth = 0.25, color = "#444444") + 
    geom_point(
      data = input_data,
      aes(x = x, y = y, fill = rate, size = samplesize),
      shape = 21, alpha = .8
    ) + 
    scale_fill_gradientn(
      colors = color_scheme, limits = point_color_limits, oob = scales::squish,
      labels = label_function
    ) + 
    labs(
      title = glue::glue("Raw data: {title_base}"), x = '', y = '',
      fill = indicator_name, size = 'Number\nsampled'
    ) +
    map_theme
  png(file.path(viz_dir, 'input_data_map.png'), height = 8, width = 8, units = 'in', res = 250)
  plot(data_point_fig)
  dev.off()

  ## Plot the INLA mesh, if available
  if(!is.null(mesh)){
    png(file.path(viz_dir, 'mesh.png'), height = 5, width = 5, units = 'in', res = 250)
    plot(mesh)
    points(input_data[, .(x, y)], col = rgb(1, 0, 0, alpha = 0.7))
    dev.off()
  }

  ## Plot the stacking submodel predictions, if available
  if(!is.null(stacked_covariates)){
    stacked_tables <- lapply(stacked_covariates, raster_to_table)
    for(ii in seq_along(stacked_tables)){
      stacked_tables[[ii]]$type <- names(stacked_covariates)[ii]
    }
    stacked_tables_full <- data.table::rbindlist(stacked_tables)
    stackers_fig <- ggplot() + 
      facet_wrap('type', ncol = floor(sqrt(length(stacked_tables)))) +
      geom_raster(data = stacked_tables_full, aes(x = x, y = y, fill = value)) + 
      geom_sf(data = admin_data_for_plotting, fill = NA, linewidth = 0.05, color = '#444444') + 
      geom_sf(data = adm1_boundaries, fill = NA, linewidth = 0.25, color = '#222222') + 
      scale_fill_gradientn(colors = color_scheme, labels = label_function) + 
      labs(
        title = glue::glue("Stacker predictions: {title_base}"), x = '', y = '',
        fill = "Stacker\nprediction"
      ) +
      map_theme
    png(file.path(viz_dir, 'stacker_predictions.png'), height = 8, width = 8, units = 'in', res = 300)
    print(stackers_fig)
    dev.off()
  }

  ## Plot the pixel-level estimates
  mean_table <- raster_to_table(mean_raster)
  ui_table <- raster_to_table(ui_raster)
  raster_mean_fig <- ggplot() + 
    geom_raster(data = mean_table, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = admin_data_for_plotting, fill = NA, linewidth = 0.05, color = '#444444') + 
    geom_sf(data = adm1_boundaries, fill = NA, linewidth = 0.25, color = '#222222') + 
    scale_fill_gradientn(colors = color_scheme, labels = label_function) + 
    labs(title = '', fill = 'Mean\nestimate', x = '', y = '') +
    map_theme
  ui_limits_rast <- c(0, quantile(na.omit(ui_table$value), 0.9))
  if((run_meta$family == 'binomial') & (ui_limits_rast[2] > 0.5)) ui_limits_rast[2] <- 0.5
  raster_ui_fig <- ggplot() + 
    geom_raster(data = ui_table, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = admin_data_for_plotting, fill = NA, linewidth = 0.05, color = '#444444') + 
    geom_sf(data = adm1_boundaries, fill = NA, linewidth = 0.25, color = '#222222') + 
    scale_fill_gradientn(
      colors = viridisLite::viridis(100), limits = ui_limits_rast, oob = scales::squish,
      labels = label_function
    ) + 
    labs(title = '', fill = 'Uncertainty', x = '', y = '') +
    map_theme
  png(file.path(viz_dir, 'summary_raster_estimates.png'), height = 10, width = 6, units = 'in', res = 250)
  gridExtra::grid.arrange(
    ggplot2::ggplotGrob(raster_mean_fig), ggplot2::ggplotGrob(raster_ui_fig), ncol = 1,
    top = glue::glue("{title_base}")
  )
  dev.off()
  
  ## Plot a summary by admin2 unit
  adm_mean_fig <- ggplot() + 
    geom_sf(data = admin_data_for_plotting, aes(fill = mean), linewidth = 0.05, color = '#444444') + 
    geom_sf(data = adm1_boundaries, fill = NA, linewidth = 0.25, color = '#222222') + 
    scale_fill_gradientn(colors = color_scheme, labels = label_function) +
    labs(title = "", x = '', y = '', fill = "Mean") +
    map_theme
  ui_limits_adm <- c(0, quantile(na.omit(admin_data_for_plotting$ui_width), 0.9))
  if((run_meta$family == 'binomial') & (ui_limits_adm[2] > 0.4)) ui_limits_adm[2] <- 0.4
  adm_ui_fig <- ggplot() + 
    geom_sf(data = admin_data_for_plotting, aes(fill = ui_width), linewidth = 0.05, color = '#444444') +
    geom_sf(data = adm1_boundaries, fill = NA, linewidth = 0.25, color = '#222222') + 
    scale_fill_gradientn(
      colors = viridisLite::viridis(100), limits = ui_limits_adm, oob = scales::squish,
      labels = label_function
    ) +
    labs(title = "", x = '', y = '', fill = "UI width") +
    map_theme
  png(file.path(viz_dir, 'summary_admin_estimates.png'), height = 10, width = 6, units = 'in', res = 250)
  gridExtra::grid.arrange(
    ggplot2::ggplotGrob(adm_mean_fig), ggplot2::ggplotGrob(adm_ui_fig), ncol = 1,
    top = glue::glue("{title_base}")
  )
  dev.off()
  # Finished with plots
  invisible(NULL)
}
