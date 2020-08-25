test_that("multiplication works", {
  pobre_x_dpto <- get_estimation_mean(data = ech::toy_ech_2018, variable = "pobre06",
                                      by.x = "nomdpto", level = "h", name = "Pobreza")
  pobre_x_dpto <- pobre_x_dpto %>% dplyr::filter(pobre06 == "Pobre")
  pobre_x_dpto_geo <- add_geom(data = pobre_x_dpto, unit = "Departamentos", variable = "nomdpto")
  aaa <- plot_geouy(x = pobre_x_dpto_geo, col = "Pobreza", l = "%")
  testthat::expect_is(aaa, "ggplot")
})
