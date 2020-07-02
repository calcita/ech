test_that("income_quantiles works", {
  expect_error(income_quantiles())
  a <- income_constant_prices()
  expect_warning(income_quantiles(data = a))
  expect_warning(income_quantiles(data = a, quantile = 10))
})
