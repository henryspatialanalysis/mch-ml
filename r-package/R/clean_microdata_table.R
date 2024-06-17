#' Interpret text as integers
#' 
#' @description Convert one-line character strings into numeric vectors
#' 
#' @details Used to parse semicolon-separated strings used in the codebook
#' 
#' @seealso [clean_microdata_using_codebook]
#' 
#' @param text (`character(1)`) Text string possibly encoding one or more numbers
#' 
#' @return Converted integer vector
#'  
#' @export
interpret_text_as_integers <- function(text){
  # Split on semicolons, then parse (needed for semicolons)
  # Empty strings parse to NULL, everything else is returned
  text_split <- strsplit(x = text, split = ';') |> 
    unlist() |> 
    sapply(function(x) eval(parse(text = x))) |>
    unlist() |> unname() |> unique()

  return(text_split)
}


#' Clean a microdata table based on a codebook
#' 
#' @description Rename and re-format columns from DHS microdata based on a codebook
#' 
#' @details Only columns listed in the codebook are retained. This function includes some
#'   special processing for specific columns for more complex variables.
#' 
#' @param microdata ([data.table][data.table::data.table]) Table containing raw DHS
#'   microdata
#' @param codebook ([data.table][data.table::data.table]) Codebook containing at least the
#'   following fields:
#'   - "original_var": Variable as it is named in the raw microdata
#'   - "rename": Standardized variable name that will be returned after cleaning
#'   - "missing": Column representing "unknown"/"missing"/"NA" responses for a question.
#'      This is a text column decoded by [interpret_text_as_integers()].
#'   - "include": For binary indicators, this column represents all "yes" responses. Yes
#'      responses will be coded as 1, all other non-NA responses will be coded as 0. This
#'      is a text column decoded by [interpret_text_as_integers()].
#' 
#' @return Cleaned data.table containing the cleaned fields listed in codebook$rename
#' 
#' @import data.table
#' @importFrom glue glue
#' @export
clean_microdata_using_codebook <- function(microdata, codebook){
  # Define some empty variables to pass R checks
  (original_var <- rename <- include <- c_died_pnn <- c_alive <- c_died_age_months <-
    c_age_months <- c_maternal_age <- c_birth_cmc <- w_born_cmc <- c_birth_year <- 
    int_month_cmc <- c_lbw <- p_birth_weight <- water_time <- p_sba <- p_anc1 <- p_anc4 <-
    p_anc8 <- p_anc_times <- s_haz_stdev <- s_whz_stdev <- s_waz_stdev <- b_10y_prior <- 
    w_empowered_sdg <- NULL
  )

  # Confirm that required fields are available
  cb_required_fields <- c('original_var', 'rename', 'missing', 'include')
  missing_fields <- setdiff(cb_required_fields, colnames(codebook))
  if(length(missing_fields) > 0){
    stop("Codebook missing required fields: ", paste(missing_fields, collapse = ', '))
  }

  # Warn for any missing microdata fields
  missing_md_fields <- setdiff(codebook$original_var, colnames(microdata))
  if(length(missing_md_fields) > 0){
    warning(glue::glue(
      "Microdata is missing expected fields {paste(missing_md_fields, collapse = ', ')}"
    ))
  }
  codebook <- codebook[original_var %in% colnames(microdata), ]
  # Subset microdata to the original_var fields listed in the codebook
  m_clean <- microdata[, codebook$original_var, with = FALSE]
  colnames(m_clean) <- codebook$rename

  # Update missing values
  for(ii in seq_len(nrow(codebook))){
    missing_text <- codebook[ii, missing]
    if((missing_text != '') & !is.na(missing_text)){
      missing_vals <- interpret_text_as_integers(missing_text)
      update_var <- codebook[ii, rename]
      m_clean[get(update_var) %in% missing_vals, (update_var) := NA ]
    }
  }

  # Recode standard binary variables
  for(ii in seq_len(nrow(codebook))){
    include_text <- codebook[ii, include]
    if((include_text != '') & !is.na(include_text)){
      include_vals <- interpret_text_as_integers(include_text)
      update_var <- codebook[ii, rename]
      m_clean[!is.na(get(update_var)) & (get(update_var) %in% include_vals), (update_var) := 1L ]
      m_clean[!is.na(get(update_var)) & !(get(update_var) %in% include_vals), (update_var) := 0L ]
    }
  }

  ## ** CUSTOM RECODES **
  prepared_cols <- colnames(m_clean)

  # If age in months is missing, assign it for living children
  if((!'c_age_months' %in% prepared_cols) & all(c('in_month_cmc', 'c_birth_cmc') %in% prepared_cols)){
    m_clean[, c_age_months := round(int_month_cmc - c_birth_cmc) ]
    prepared_cols <- colnames(m_clean)
  }

  # Was this death in the 1-59 months range?
  if(all(c('c_alive', 'c_age_months', 'c_died_age_months') %in% prepared_cols)){
    # Start with 0
    m_clean[, c_died_pnn := 0L ]
    # Children who died between 1 and 59 months => 1
    m_clean[
      (c_alive == 0L) & (c_died_age_months >= 1L) & (c_died_age_months <= 59L),
      c_died_pnn := 1L
    ]
    # Exclude neonatal deaths
    m_clean[(c_alive == 0L) & (c_died_age_months < 1), c_died_pnn := NA_integer_ ]
    # Exclude any NAs in child_alive or (for living children) child_age_months
    m_clean[ is.na(c_alive) | ((c_alive == 1L) & is.na(c_age_months)), c_died_pnn := NA_integer_ ]
  }

  # Mother's age at birth
  if(all(c('c_birth_cmc', 'w_born_cmc') %in% prepared_cols)){
    m_clean[, c_maternal_age := floor((c_birth_cmc - w_born_cmc) / 12)]
    m_clean[, c_birth_year := 1900L + floor(c_birth_cmc / 12)]
  }

  # Tag births greater than 10 years prior to the interview
  if(all(c('c_birth_cmc', 'int_month_cmc') %in% prepared_cols)){
    m_clean[, b_10y_prior := as.integer((int_month_cmc - c_birth_cmc) <= 120) ]
  }

  # Create binary indicator for low birth weight, if available
  if('p_birth_weight' %in% prepared_cols){
    m_clean[, c_lbw := as.integer(p_birth_weight < 2500)]
  }

  # Recode: water on premises => 0 travel time
  if('water_time' %in% prepared_cols){
    m_clean[water_time == 996L, water_time := 0L ]
  }

  # Construct women's empowerment SDG indicator
  sdg_cols <- c('w_decision_contra', 'w_decision_hc', 'w_decision_sex')
  if(all(sdg_cols %in% prepared_cols)){
    m_clean[, w_empowered_sdg := w_decision_contra * w_decision_hc * w_decision_sex ]
  }

  # Construct binary stunting and wasting indicators
  if('haz' %in% prepared_cols){
    m_clean[, stunting := as.integer(haz < -200) ]
  }
  if('whz' %in% prepared_cols){
    m_clean[, wasting := as.integer(whz < -200) ]
  }

  # Construct skilled ANC and skilled birth attendance indicators
  skilled_attendants <- c('doctor','nurse','midwife','chw','aux_midwife')
  sba_cols <- paste0('p_sba_', skilled_attendants)
  if(any(sba_cols %in% prepared_cols)){
    check_cols <- intersect(sba_cols, prepared_cols)
    m_clean[, p_sba := 0L ]
    for(check_col in check_cols){
      m_clean[get(check_col) == 1L, p_sba := 1L ]
      m_clean[is.na(p_sba) & (get(check_col) == 0L), p_sba := 0L ]
      # Drop the original columns
      m_clean[, (check_col) := NULL ]
    }
  }
  # Convert anc_times to ANC1, ANC4, ANC8
  if('p_anc_times' %in% prepared_cols){
    m_clean[, p_anc1 := as.integer(p_anc_times >= 1L) ]
    m_clean[, p_anc4 := as.integer(p_anc_times >= 4L) ]
    m_clean[, p_anc8 := as.integer(p_anc_times >= 8L) ]
    # Drop the original column
    m_clean[, p_anc_times := NULL ]
  }

  # Return cleaned table
  return(m_clean)
}
