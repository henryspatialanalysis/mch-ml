testthat::test_that("load_environment_variable", {
  # Should fail for a missing environment variable
  testthat::expect_error(load_environment_variable('FAKE_ENVIRONMENT_VARIABLE___'))
  # Should return a real environment variable otherwise
  testthat::expect_equal(
    load_environment_variable('USER'),
    Sys.getenv('USER')
  )
})


testthat::test_that("generate_power_set", {
  # The power set of NULL is the empty set only
  testthat::expect_equal(generate_power_set(NULL), list(NULL))

  # Check an easy power set
  testthat::expect_equal(
    generate_power_set(c('a','b')),
    list(character(0), 'a', 'b', c('a','b'))
  )

  # Check that the length is always sum(k in 0...n, nCk)
  test_length <- 10
  testthat::expect_equal(
    generate_power_set(seq_len(test_length)) |> length(),
    seq(0, test_length) |> sapply(function(k) choose(n = test_length, k = k)) |> sum()
  )
})

testthat::test_that("vector_to_names", {
  # Empty input should yield ";;"
  testthat::expect_equal(vector_to_names(NULL), ";;")
  testthat::expect_equal(vector_to_names(character(0)), ";;")

  # Check that shuffling a vector has no effect
  testthat::expect_equal(
    vector_to_names(1:10),
    vector_to_names(sample(1:10))
  )

  # Check that duplicates have no effect
  testthat::expect_equal(
    vector_to_names(1:5),
    vector_to_names(c(1:5, 1:5))
  )
})

testthat::test_that("get_loss_function", {
  # Passing a missing loss function should yield an error
  testthat::expect_error(get_loss_function("NOT_A_REAL_LOSS_FUNCTION"))
  testthat::expect_error(get_loss_function(NULL))

  # L1 error check
  testthat::expect_equal(get_loss_function("L1")(1:5, 3:7), 10)

  # L2 error check
  testthat::expect_equal(get_loss_function("L2")(1:5, 3:7), 20)
})
