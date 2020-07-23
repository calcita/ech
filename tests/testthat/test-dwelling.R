test_that("dwelling works", {
  toy_ech_2018 <- income_constant_prices(data = ech::toy_ech_2018)
  toy_ech_2018 <- income_quantiles(data = toy_ech_2018)
  toy_ech_2018 <- housing_deprivation(data = toy_ech_2018)
  expect_equal(ncol(toy_ech_2018), 600)
  })

test_that("housing works", {
  a <- housing_situation(data = ech::toy_ech_2018)
  a <- housing_conditions(data = a)
  a <- overcrowding(data = a)
  a <- homeownership(a)
  expect_equal(ncol(a), 583)
})
