testthat::test_that("run_shapley_decomposition", {
  # Run a simple Shapley decomposition and confirm results are correct
  predictors <- c('a','b')
  model_performance_list <- list(';;' = 100, 'a' = 90, 'b' = 10, 'a;b' = 0)
  decomp <- run_shapley_decomposition(
    predictors = predictors,
    model_performance_list = model_performance_list
  )
  # Confirm that the sum of absolute scores == (best model - worst model)
  testthat::expect_equal(
    sum(decomp$absolute),
    model_performance_list[[1]] - model_performance_list[[length(model_performance_list)]]
  )
  # Confirm that the sum of relative errors == 1
  testthat::expect_equal(sum(decomp$normalized), 1)
  # Confirm that variable 'b' had a bigger effect than 'a'
  if(decomp$absolute['b'] <= decomp$absolute['a']){
    stop("Variable 'b' should have had a larger Shapley value than variable 'a'")
  }
})
