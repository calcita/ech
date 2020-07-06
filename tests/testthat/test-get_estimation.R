test_that("get_estimation works", {
  a <- get_estimation_mean(variable = "pobre06", by.x = "dpto", level = "h")
  expect_equal(nrow(a), 19)
  a <- get_estimation_total(variable = "pobre06", by.x = "dpto", level = "h")
  expect_equal(nrow(a), 19)
  # a <- get_estimation_ratio(variable = "pobre06", by.x = "dpto", level = "h")
  # expect_equal(nrow(a), 19)
})
