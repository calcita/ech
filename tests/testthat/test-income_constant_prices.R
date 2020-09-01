test_that("return structure", {
  a <- income_constant_prices(data = ech::toy_ech_2018_income)
  testthat::expect_equal(names(a), c("numero", "mes", "ht11", "ysvl", "ht13", "ht19", "dpto", "pesoano", "estred13", "anio",
                                     "region_4", "y_pc", "y_pc_d", "rv_d", "y_wrv_d", "y_wrv_pc_d"))
  b <- income_constant_prices(data = ech::toy_ech_2018_income, index = "IPAB")
  testthat::expect_equal(names(b), c("numero", "mes", "ht11", "ysvl", "ht13", "ht19", "dpto", "pesoano", "estred13", "anio",
                                     "region_4", "y_pc", "y_pc_d", "rv_d", "y_wrv_d", "y_wrv_pc_d"))
  c <- income_constant_prices(data = ech::toy_ech_2018_income, index = "IPAB", level = "R")
  testthat::expect_equal(names(c), c("numero", "mes", "ht11", "ysvl", "ht13", "ht19", "dpto", "pesoano", "estred13", "anio",
                                     "region_4", "y_pc", "y_pc_d_r", "rv_d_r", "y_wrv_d_r", "y_wrv_pc_d_r"))


  toy_ech_2018 <- labor_income_per_capita(data = ech::toy_ech_2018)
  expect_equal(ncol(toy_ech_2018), 587)
  df <- ech::toy_ech_2018
  toy_ech_2018 <- labor_income_per_hour(data = df, base_month = "06", base_year = "2018")
  expect_equal(ncol(toy_ech_2018), 584)
})
