test_that("add_geom works", {
  pobre_x_dpto <- get_estimation_mean(variable = "pobre06", by.x = "dpto", level = "h") %>% dplyr::filter(pobre06 == "No pobre")
  pobre_x_dpto_geo <- add_geom(data = pobre_x_dpto, unit = "Departamentos", variable = "dpto")
  testthat::expect_equal(ncol(pobre_x_dpto_geo), 6)
})
