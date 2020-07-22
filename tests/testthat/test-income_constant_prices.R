test_that("return structure", {
  a <- income_constant_prices()
  testthat::expect_equal(names(a), c("numero", "mes", "ht11", "ysvl", "ht13", "ht19", "dpto", "pesoano",
                                     "estred13", "anio", "region_4", "aux", "deflate", "ht11_per_capita", "ht11_deflate",
                                     "ht13_deflate", "ht11_svl_def", "ht11_svl_per_capita_deflate",
                                     "ht11_per_capita_deflate"))
  toy_ech_2018 <- labor_income_per_capita(data = ech::toy_ech_2018)
  expect_equal(ncol(toy_ech_2018), 585)
  df <- ech::toy_ech_2018
  toy_ech_2018 <- labor_income_per_hour(data = df, base_month = "06", base_year = "2018")
  expect_equal(ncol(toy_ech_2018), 585)
})
