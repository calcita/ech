test_that("return structure", {
  a <- income_constant_prices()
  testthat::expect_equal(names(a), c("numero", "mes", "ht11", "ysvl", "ht13", "ht19", "dpto", "pesoano",
                                     "estred13", "anio", "region_4", "aux", "deflate", "ht11_per_capita", "ht11_deflate",
                                     "ht13_deflate", "ht11_svl_def", "ht11_svl_per_capita_deflate",
                                     "ht11_per_capita_deflate"))
})
