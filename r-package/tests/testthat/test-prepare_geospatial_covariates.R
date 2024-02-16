testthat::test_that("reshape_dhs_covariates", {
  test_cov_table <- data.table::data.table(
    DHSCLUST = 1:10,
    TEST_COV_2000 = rnorm(10),
    TEST_COV_2004 = rnorm(10)
  )
  measure_years <- 2000:2007

  reshaped_covariates <- reshape_dhs_covariates(
    covs_table = test_cov_table,
    measure_years = measure_years
  )
  # The results for 2007 should be exactly the same as 2004
  testthat::expect_equal(
    reshaped_covariates[(DHSCLUST == 1) & (year == 2004), TEST_COV],
    reshaped_covariates[(DHSCLUST == 1) & (year == 2007), TEST_COV]
  )
  # The results for 2002 should be exactly halfway between 2000 and 2004
  testthat::expect_equal(
    reshaped_covariates[(DHSCLUST == 10) & (year == 2002), TEST_COV],
    reshaped_covariates[(DHSCLUST == 10) & (year %in% c(2000, 2004)), mean(TEST_COV)]
  )
  # The rows of test_results should be 8 (years) * 10 (clusters)
  testthat::expect_equal(
    nrow(reshaped_covariates),
    uniqueN(test_cov_table$DHSCLUST) * length(measure_years)
  )
})
