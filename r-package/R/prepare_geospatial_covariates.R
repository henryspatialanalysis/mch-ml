#' Prepare geospatial covariates for analysis
#' 
#' @description Reshape geospatial covariates wide and fill missing years
#' 
#' @param covs_table Table of geospatial covariates returned from DHS API
#' @param measure_years All years needed for this analysis.
#' @param id_col Cluster ID as listed in the covariates table
#' @param drop_cols Columns that are not either covariates or the cluster ID
#' @param keep_covariates (`character(N)`, default NULL) Which covariates should be kept
#'   in the final dataset? If NULL, the default, keep all available covariates
#' 
#' @return Data.table reshaped long by year, with missing years filled
#' 
#' @import data.table
#' @export
reshape_dhs_covariates <- function(
  covs_table,
  measure_years,
  id_col = 'DHSCLUST',
  drop_cols = c('DHSID','GPS_Dataset','DHSCC','DHSYEAR','SurveyID'),
  keep_covariates = NULL
){
  # Set some empty variables to pass R checks
  variable <- id <- value <- i.value <- val_p <- val_n <- runlength <- missings <- NULL

  # Drop unnecessary columns, then melt
  covs_table <- covs_table[, setdiff(colnames(covs_table), drop_cols), with = F]
  covs_long <- melt(covs_table, id.vars = id_col, variable.factor = F) |> suppressWarnings()
  tv_covs <- (covs_long
    [grepl('_20', variable), ]
    [, year := as.integer(substr(variable, nchar(variable) - 3, nchar(variable))) ]
    [, variable := substr(variable, 1, nchar(variable) - 5)]
    [, id := get(id_col) ]
  )
  year_range <- range(c(tv_covs$year, measure_years))

  # Subset only to kept covariates
  if(!is.null(keep_covariates)){
    tv_covs <- copy(tv_covs[variable %in% keep_covariates, ])
    covs_long <- copy(covs_long[variable %in% keep_covariates, ])
  }
  
  template_dt <- data.table::CJ(
    variable = sort(unique(tv_covs$variable)),
    id = covs_table[[id_col]],
    year = seq(year_range[1], year_range[2])
  )[tv_covs, value := i.value, on = c('variable', 'id', 'year')]
  # Fill NAs by interpolation
  (template_dt
    [, val_p := nafill(value, 'locf') ]
    [, val_n := nafill(value, 'nocb') ]
    [, runlength := rleid(value) ]
    [, missings := max(.N + 1, 2), by = runlength]
    [is.na(value) & year < min(tv_covs$year), value := val_n ] # Before first recorded year
    [is.na(value) & year > max(tv_covs$year), value := val_p ] # After last recorded year
    [is.na(value), value := val_p + .SD[,.I] * (val_n - val_p) / missings, by = runlength ]
    [is.na(value), value := val_p ]
    [is.na(value), value := val_n ]
    [, c('val_p','val_n','runlength','missings') := NULL ]
  )
  # Reshape wide by id and year
  covs_wide <- dcast(template_dt, formula = id + year ~ variable)
  setnames(covs_wide, 'id', id_col)
  # Add on non-time-varying covariates
  non_tv_covs <- grep('_20', unique(covs_long$variable), invert = T, value = T)
  covs_wide <- merge(
    x = covs_wide,
    y = covs_table[, c(id_col, non_tv_covs), with = F],
    by = id_col
  )

  # Return reshaped and filled covariate table
  return(covs_wide)
}
