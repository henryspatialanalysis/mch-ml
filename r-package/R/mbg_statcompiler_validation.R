#' MBG statcompiler validation
#' 
#' @description Compare MBG estimates against StatCompiler by admin1 unit
#' 
#' @param viz_dir (`character(1)`) Directory where all validation results and figures will
#'   be saved. 
#' @param input_data (`data.table`) Table containing at least the fields 'rate',
#'   'samplesize', 'admin1_code', and 'admin1_name' 
#' @param adm1_mbg_summaries (`data.table`) Table containing at least the fields
#'   'ADM1_NAME', 'ADM1_CODE', 'mean', 'lower', and 'upper'
#' @param country_name (`character(1)`) Full name of country
#' @param sc_indicator (`character(1)`) Statcompiler indicator code
#' @param survey_year (`integer(1)`) Year corresponding to DHS survey
#' @param sc_denominator (`numeric(1)`, default 1) Denominator for statcompiler estimates.
#'   Used to make statcompiler estimates compatible with MBG results
#' @param label_function (`function`) Function converting a numeric vector into a set of
#'   labels.
#' 
#' @import data.table ggplot2
#' @export
mbg_statcompiler_validation <- function(
  viz_dir,
  input_data,
  adm1_mbg_summaries,
  country_name,
  sc_indicator,
  survey_year,
  sc_denominator = 1,
  label_function = scales::comma
){
  # Load statcompiler data by subnational admin unit
  country_code <- rdhs::dhs_countries(returnFields = c("CountryName", "DHS_CountryCode")) |>
    data.table::as.data.table() |>
    _[tolower(CountryName) == tolower(country_name), DHS_CountryCode]
  if(length(country_code) != 1) stop("Did not return a unique `rdhs` country ID - check name")  
  # Pull data from DHS StatCompiler API
  sc_summaries <- rdhs::dhs_data(
    countryIds = country_code,
    indicatorIds = sc_indicator,
    surveyYearStart = survey_year,
    breakdown = 'subnational'
  ) |> data.table::as.data.table()
  sc_summaries <- (sc_summaries
    [SurveyYear <= (survey_year + 1), ]
    [, adm_name := gsub('\\.', '', CharacteristicLabel)]
    [, adm_name := iconv(adm_name, to = 'ASCII//TRANSLIT')]
    [, dhs_adm_level := (nchar(CharacteristicLabel) - nchar(adm_name))/2 + 1]
    [, Value := Value / sc_denominator ]
    # Admin name changes to match statcompiler to the standard shapefile
    [adm_name == 'FCT Abuja', adm_name := "Abuja"]
    [adm_name == 'Abidjan', adm_name := "District autonome de Abidjan"]
    [adm_name == 'Yamoussoukro', adm_name := "District autonome de Yamoussoukro"]
    [adm_name == "Anamoroni'i Mania", adm_name := "Amoron I Mania"]
    [, sc_merge := tolower(adm_name) ]
  )

  # Merge statcompiler with aggregated input data
  input_data_merged <- input_data[
    , .(data_rate = weighted.mean(rate, w = samplesize, na.rm=T)),
    by = .(admin1_code, admin1_name, id_merge = tolower(admin1_name))
  ] |> na.omit() |> fedmatch::merge_plus(
    data2 = sc_summaries[, .(adm_name, sc_merge, sc_id = .I, sc_value = Value)],
    by.x = 'id_merge', by.y = 'sc_merge', match_type = 'fuzzy',
    unique_key_1 = 'admin1_code', unique_key_2 = 'sc_id'
  )
  input_data_matches <- input_data_merged$matches
  input_data_matches[, bigdiff := as.integer(abs(sc_value - data_rate) > 0.1) ]
  # Plot it
  data_versus_sc_fig <- ggplot(data = input_data_matches, aes(x = sc_value, y = data_rate)) + 
    geom_smooth(method = 'lm', col = '#339966', fill = '#99ffcc', alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = 'grey40', linetype = 3, linewidth = 1) +
    geom_point() +
    geom_label(
      data = function(x) x[bigdiff == 1L, ],
      aes(label = admin1_name),
      nudge_x = diff(range(input_data_matches$sc_value)) / 75,
      hjust = 0, label.size = NA, fill = alpha("white", 0.8), show.legend = FALSE
    ) +
    scale_x_continuous(
      labels = label_function,
      limits = c(min(input_data_matches$sc_value) - 0.02, max(input_data_matches$sc_value) + .06)
    ) + 
    scale_y_continuous(labels = label_function) +
    labs(
      x = "StatCompiler Estimates",
      y = "Aggregated input data",
      color = "Large difference?"
    ) +
    theme_bw()
  png(file.path(viz_dir, "statcompiler_data_scatter.png"), height = 8, width = 8, units = 'in', res = 250)
  print(data_versus_sc_fig)
  dev.off()

  # Prepare MBG results
  adm1_mbg_summaries <- (adm1_mbg_summaries
    [, ADM1_NAME := iconv(ADM1_NAME, to = 'ASCII//TRANSLIT') ]
    [, results_merge := tolower(ADM1_NAME)]
  )
  # Merge statcompiler and MBG results
  fuzzy_merge_results <- fedmatch::merge_plus(
    data1 = adm1_mbg_summaries[, .(ADM1_NAME, ADM1_CODE, results_merge, mean, lower, upper)],
    data2 = sc_summaries[, .(adm_name, sc_merge, dhs_adm_level, sc_value = Value, sc_id = .I)],
    by.x = 'results_merge',
    by.y = 'sc_merge',
    match_type = 'fuzzy',
    unique_key_1 = 'ADM1_CODE',
    unique_key_2 = 'sc_id'
  )
  fuzzy_matches <- fuzzy_merge_results$matches
  fuzzy_matches[, in_ui := 'No' ][(sc_value >= lower) & (sc_value <= upper), in_ui := 'Yes']
  n_total <- nrow(adm1_mbg_summaries)
  n_matched <- nrow(fuzzy_matches)
  n_in_ui <- sum(fuzzy_matches$in_ui == "Yes")
  summary_text <- glue::glue(
    "Of {n_total} admin1 units in the MBG shapefile, {n_matched} ",
    "({scales::percent(n_matched/n_total)}) were matched to DHS StatCompiler estimates.\n",
    "Of {n_matched} matched units, {n_in_ui} ({scales::percent(n_in_ui / n_matched)}) ",
    "StatCompiler estimates fell within the MBG 95% Uncertainty Interval."
  )
  message(summary_text)
  # Save the raw StatCompiler data and merged estimates
  data.table::fwrite(fuzzy_matches, file = file.path(viz_dir, 'statcompiler_matches.csv'))
  data1_nomatch <- fuzzy_merge_results$data1_nomatch
  data2_nomatch <- fuzzy_merge_results$data2_nomatch
  if(nrow(data1_nomatch) > 0) data.table::fwrite(data1_nomatch, file = file.path(viz_dir, 'mbg_nomatch.csv'))
  if(nrow(data2_nomatch) > 0) data.table::fwrite(data2_nomatch, file = file.path(viz_dir, 'statcompiler_nomatch.csv'))

  # Run a simple OLS model to see the relationship between MBG mean predictions and
  #  StatCompiler estimates
  tryCatch({
    ols_model <<- stats::lm(mean ~ sc_value, data = fuzzy_matches)
    ols_r_squared <<- summary(ols_model)$r.squared |> round(3)
    ols_coefficients <<- summary(ols_model)$coefficients[, 1] |> round(3) |> unname()
  }, error = function(e){
    ols_coefficients <<- c(NA, NA)
    ols_r_squared <<- NA
  }) 
  ## Plot admin1 comparison
  ui_colors <- c(Yes = '#444444', No = 'red')
  subtitle_text <- glue::glue(
    '{summary_text}\nOLS regression: (MBG mean est.) ~ {ols_coefficients[1]} + ',
    '{ols_coefficients[2]} * (StatCompiler est.), R\u00b2 = {ols_r_squared}'
  )
  fig <- ggplot(data = fuzzy_matches, aes(x = sc_value, y = mean, color = in_ui)) + 
    geom_smooth(method = 'lm', col = '#339966', fill = '#99ffcc', alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = 'grey40', linetype = 3, linewidth = 1) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    geom_label(
      data = function(x) x[in_ui == "No", ],
      aes(label = adm_name),
      nudge_x = diff(range(fuzzy_matches$sc_value)) / 75,
      hjust = 0, label.size = NA, fill = alpha("white", 0.8), show.legend = FALSE
    ) +
    scale_color_manual(values = ui_colors) + 
    scale_x_continuous(
      labels = label_function,
      limits = c(min(fuzzy_matches$sc_value) - 0.02, max(fuzzy_matches$sc_value) + .06)
    ) + 
    scale_y_continuous(labels = label_function) +
    labs(
      x = "StatCompiler Estimates",
      y = "MBG Estimates (Aggregated)",
      color = "In MBG UI?",
      subtitle = subtitle_text
    ) +
    theme_bw()
  try({
    png(file.path(viz_dir, "statcompiler_results_scatter.png"), height = 8, width = 8, units = 'in', res = 250)
    print(fig)
    dev.off()
  })
  # Finished with plotting
  invisible(NULL)
}
