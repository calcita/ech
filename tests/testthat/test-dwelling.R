test_that("dwelling works", {
  ech18 <- ech::toy_ech_2018 %>% dplyr::rename("c4_ech"= "c4", "ht11_ech" = "ht11")
  ech18 <- income_constant_prices(data = ech18, ht11 = "ht11_ech")
  ech18 <- income_quantiles(data = ech18)
  toy_ech_2018 <- housing_deprivation(data = ech18, c4 = "c4_ech")
  expect_equal(ncol(toy_ech_2018), 599)
})

test_that("housing works", {
  a <- housing_situation(data = ech::toy_ech_2018)
  a <- housing_conditions(data = a)
  a <- overcrowding(data = a)
  a <- housing_tenure(a)
  expect_equal(ncol(a), 585)
})
