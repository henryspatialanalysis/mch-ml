## #######################################################################################
##
## ESTIMATE FREQUENCY OF MOTHER'S AGE AT FIRST BIRTH AND 1-59mo MORTALITY PROBABILITY
##   IN EACH GROUP
##
## #######################################################################################

load_pkgs <- c('versioning', 'data.table', 'ggplot2', 'grid', 'gridExtra', 'scales')
lapply(load_pkgs, library, character.only = T) |> invisible()

CONFIG_FP <- '~/repos/usaid-mch-ml/config.yaml'
config <- versioning::Config$new(CONFIG_FP)

prepared_data_dir <- config$get_dir_path("prepared_data")
viz_dir <- file.path(prepared_data_dir, 'viz')
if(!dir.exists(viz_dir)) dir.create(viz_dir)


## Prepare visualizations for each country ---------------------------------------------->

pdf(file.path(viz_dir, 'pnmr_by_age_at_first_birth.pdf'), height = 8, width = 10)
for(country_ii in seq_along(config$get("countries"))){
  country <- config$get("countries")[country_ii]
  survey_id <- config$get("survey_ids")[country_ii]
  child_id_cols <- c('cluster', 'hh_id', 'w_id', 'birth_id')
  microdata_dt <- glue::glue("{prepared_data_dir}/analysis_dataset_{survey_id}.csv") |>
    data.table::fread() |>
    _[, lapply(.SD, max), .SDcols = c('died', 'age_group', 'w_age_first_birth'), by = child_id_cols] |>
    _[(died == 1L) | (age_group == 'a48mo'), ] |>
    _[, age_group := NULL ] |>
    _[w_age_first_birth < 15, w_age_first_birth := 15 ] |>
    _[w_age_first_birth > 26, w_age_first_birth := 26 ]
  aggregated_dt <- (microdata_dt
    [, .(N = .N, pnmr = mean(died) * 1000), by = w_age_first_birth]
    [order(w_age_first_birth)]
  )
  histogram_fig <- ggplot(data = aggregated_dt) +
    geom_bar(aes(x = w_age_first_birth, y = N), stat = 'identity') +
    scale_x_continuous(limits = c(14.5, 26.5), breaks = 15:26, labels = c('<=15', 16:25, '>=26')) + 
    scale_y_continuous(labels = scales::comma) +
    labs(y = 'Number of surveyed children', x = 'Mother\'s age at first birth') + 
    theme_bw() + theme(panel.grid.minor = element_blank())

  overall_pnmr_mean <- aggregated_dt[, weighted.mean(pnmr, w = N)]
  mortality_fig <- ggplot(data = aggregated_dt) + 
    geom_hline(yintercept = overall_pnmr_mean, color = '#888888', linetype = 2) +
    geom_line(aes(x = w_age_first_birth, y = pnmr)) + 
    annotate(geom = 'text', label = 'Survey average', y = overall_pnmr_mean + 1.5, x = 26, color = '#888888') +
    scale_x_continuous(limits = c(14.5, 26.5), breaks = 15:26, labels = c('<=15', 16:25, '>=26')) + 
    labs(x = '', y = '1-59mo. mortality probability (per 1,000)\n') + 
    theme_bw() + theme(panel.grid.minor = element_blank())

  grid.arrange(ggplotGrob(mortality_fig), ggplotGrob(histogram_fig), top = country)
}
dev.off()
