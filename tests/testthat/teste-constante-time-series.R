context("Creating time series")

test_that("constant creates a constant time series", {
  time_series <- constant(start_seed = 1.3, num_years  = 2)
  expect_equal(time_series[1], time_series[2])
})
