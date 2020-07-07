test_that("get_estimation_mean works", {
  a <- get_estimation_mean(variable = "pobre06", by.x = "dpto", level = "h")
  expect_equal(nrow(a), 19)
  a <- get_estimation_mean(variable = "pobre06", by.x = "dpto", by.y = "mes", level = "h")
  expect_equal(nrow(a), 207)
})

test_that("get_estimation_total works", {
  a <- get_estimation_total(variable = "pobre06", by.x = "dpto", level = "h")
  expect_equal(nrow(a), 19)
  a <- get_estimation_total(variable = "pobre06", by.x = "dpto", by.y = "mes", level = "h")
  expect_equal(nrow(a), 207)
})

test_that("get_estimation_ratio works", {
  ech_2018 <- employment(data = ech::toy_ech_2018, pobpcoac = "pobpcoac")
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", level = "i")
  expect_equal(nrow(a), 1)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", by.x = "dpto", level = "i")
  expect_equal(nrow(a), 19)
  a <- get_estimation_ratio(data = ech_2018, variable.x = "po", variable.y = "pea", by.x = "dpto", by.y = "mes",level = "i")
  expect_equal(nrow(a), 207)
})
