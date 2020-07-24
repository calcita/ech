test_that("get_estimation_mean works", {
  d = ech::toy_ech_2018
  a <- get_estimation_mean(data = d, variable = "pobre06", level = "h", name = "hogares_pobre_mean")
  expect_equal(nrow(a), 1)
  a <- get_estimation_mean(data = d, variable = "pobre06", domain = d$dpto == 1, level = "h")
  expect_equal(nrow(a), 1)
  a <- get_estimation_mean(data = d, variable = "pobre06", by.x = "mes", domain = d$dpto == 1, level = "h")
  expect_equal(nrow(a), 12)
  a <- get_estimation_mean(variable = "pobre06", by.x = "dpto", level = "h")
  expect_equal(nrow(a), 19)
  a <- get_estimation_mean(variable = "pobre06", by.x = "dpto", by.y = "mes", level = "h")
  expect_equal(nrow(a), 207)
  a <- get_estimation_mean(variable = "pobre06", by.x = "secc", by.y = "mes", domain = d$dpto == 1, level = "i")
  expect_equal(nrow(a), 175)
})

test_that("get_estimation_total works", {
  d = ech::toy_ech_2018
  a <- get_estimation_total(variable = "pobre06", level = "h", name = "hogares_pobre_total")
  expect_equal(nrow(a), 1)
  a <- get_estimation_total(data = d, variable = "pobre06", domain = d$dpto == 1, level = "h")
  expect_equal(nrow(a), 1)
  a <- get_estimation_total(data = d, variable = "pobre06", by.x = "mes", domain = d$dpto == 1, level = "h")
  expect_equal(nrow(a), 12)
  a <- get_estimation_total(variable = "pobre06", by.x = "dpto", level = "h")
  expect_equal(nrow(a), 19)
  a <- get_estimation_total(variable = "pobre06", by.x = "dpto", by.y = "mes", level = "h")
  expect_equal(nrow(a), 207)
  a <- get_estimation_total(variable = "pobre06", by.x = "secc", by.y = "mes", domain = d$dpto == 1, level = "i")
  expect_equal(nrow(a), 175)
})

test_that("get_estimation_ratio works", {
  ech_2018 <- employment(data = ech::toy_ech_2018, pobpcoac = "pobpcoac")
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", level = "i", name = "tasa_ocupacion")
  expect_equal(nrow(a), 1)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", domain = ech_2018$dpto == 1, level = "i")
  expect_equal(nrow(a), 1)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", domain = ech_2018$dpto == 1, by.x = "mes", level = "i")
  expect_equal(nrow(a), 12)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", by.x = "dpto", level = "i")
  expect_equal(nrow(a), 19)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", by.x = "dpto", by.y = "mes",level = "i")
  expect_equal(nrow(a), 207)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", by.x = "secc", by.y = "mes", domain = ech_2018$dpto == 1,level = "i")
  expect_equal(nrow(a), 175)
})

test_that("get_estimation_gini works", {
  ech_2018 <- income_constant_prices(data = ech::toy_ech_2018, ipc = "R",
                               base_month = "01", base_year = "2005")
  a <- get_estimation_gini(data = ech_2018, variable = "y_wrv_pc_d_r", level = "h", name = "indice_gini")
  expect_equal(nrow(a), 1)
})
