#' Convert CMC to year
#'
#' @param cmc Date in CMC
#' 
#' @return Date in integer year
cmc_to_year <- function(cmc){
  return(floor(cmc / 12L) + 1900L)
}

#' Convert year to CMC
#' 
#' @param year Date in integer years
#' 
#' @return CMC corresponding to January of that year
year_to_cmc <- function(year){
  return((year - 1900L) * 12L)
}

#' Prepare birth history data for a single age group
#' 
#' @description Split out a single age group from full birth history data.
#' 
#' @param data ([data.table][data.table::data.table]) Full birth histories
#' @param id_columns (`character(N)`) Fields in `data` that uniquely identify individual
#'   records
#' @param years (`integer(N)`) Years to consider
#' @param age_group_mo_start (`integer(1)`) Months of age completed when entering the
#'   age group.
#' @param age_group_mo_end (`integer(1)`) Months of age when exiting the age group - e.g.
#'   (6, 12) correponds to an age group of 6-11 months of completed age.
#' 
#' @return Data subset to individuals who entered the age group during the specified years
#' @import data.table
bh_split_age_single_group <- function(
  data, id_columns, years, age_group_mo_start, age_group_mo_end
){
  # Create a table of all years and corresponding months
  ag_dt <- data.table::CJ(
    year = years,
    mo_int = 0:11
  )[, ag_start_cmc := year_to_cmc(year) + mo_int ][, mo_int := NULL ]
  data_subset <- (data.table::copy(data)
    [, c(id_columns, 'c_birth_cmc', 'c_alive', 'c_died_age_months', 'int_month_cmc'), with = F]
    # Drop any children who died before this age group or who would intersect the
    #  interview month in this age group
    [is.na(c_died_age_months) | c_died_age_months >= age_group_mo_start, ]
    [(c_birth_cmc + age_group_mo_end) < int_month_cmc, ]
    [, ag_start_cmc := c_birth_cmc + age_group_mo_start ]
    [ag_dt, bh_year := i.year, on = 'ag_start_cmc']
    # Drop any children who did not intersect the age group in any of the target years
    [!is.na(bh_year), ]
    # Identify those who died during the age group
    [, died := as.integer((c_alive == 0L) & (c_died_age_months < age_group_mo_end)) ]
    # Drop extra variables
    [, c('ag_start_cmc', 'c_birth_cmc', 'c_alive', 'c_died_age_months', 'int_month_cmc') := NULL ]
  )
  # Return individuals in the prepared age group
  return(data_subset)
}


#' Split full birth history data into age groups
#' 
#' @param data ([data.table][data.table::data.table]) Full birth history data containing
#'   at least the fields 'c_birth_cmc', 'c_alive', 'c_died_age_months', 'int_month_cmc'
#' @param age_groups (`list(N)`) Named list corresponding to each age group. The name of
#'   each item is the age group name, and the item itself contains two integers
#'   representing *inclusive* months of age in that group. For example `6mo = c(6, 12)`
#'   represents the age group between 6 and *11* months of completed age.
#' @param max_timediff_months (`integer(1)`) Number of months prior to the interview
#'   month to consider when splitting the data. Used to derive years of analysis.
#' @param child_id_columns (`character(N)`) Fields in `data` that uniquely identify
#'   individual children.
#' 
#' @return Dataset summarized by age group and year cohorts. In addition to the original
#'   columns, the returned dataset will contain the fields 'year', 'age_group', and
#'   'died'. Individual children not falling into any of the summary year data will be
#'   excluded from analysis.
#' 
#' @import data.table
#' @export
split_ages <- function(data, age_groups, max_timediff_months, child_id_columns){
  split_years <- seq(
    cmc_to_year(min(data$int_month_cmc - max_timediff_months)),
    cmc_to_year(max(data$int_month_cmc))
  )
  split_list <- lapply(age_groups, function(ag) bh_split_age_single_group(
    data = data,
    id_columns = child_id_columns,
    years = split_years,
    age_group_mo_start = ag[1],
    age_group_mo_end = ag[2]
  ))
  for(ag_name in names(age_groups)) split_list[[ag_name]]$age_group <- ag_name
  # Combine back into a single table and merge on all other columns
  split_data_full <- merge(
    x = data.table::rbindlist(split_list),
    y = data,
    by = child_id_columns,
    all = FALSE
  )
  return(split_data_full)
}
