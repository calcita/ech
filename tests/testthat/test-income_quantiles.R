test_that("income_quantiles works", {
  expect_error(income_quantiles())
  a <- income_constant_prices()
  b <- income_quantiles(data = a)
  expect_equal(ncol(b), ncol(a) + 1)
  c <- income_quantiles(data = a, quantile = 10)
  expect_equal(ncol(c), ncol(a) + 1)
})
