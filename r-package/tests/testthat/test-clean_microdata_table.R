testthat::test_that("interpret_text_as_integers", {
  testthat::expect_equal(
    interpret_text_as_integers('4;5;9'),
    c(4, 5, 9)
  )
  testthat::expect_null(interpret_text_as_integers(''))
})
