test_that("income_quantiles works", {
  a <- income_constant_prices(data = ech::toy_ech_2018)
  b <- income_quantiles(a)
  expect_equal(ncol(b), ncol(a) + 1)
  c <- income_quantiles(data = a, quantile = 10)
  expect_equal(ncol(c), ncol(a) + 1)
})
