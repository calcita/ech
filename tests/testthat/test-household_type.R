test_that("household_type works", {
  a <- household_type(data = ech::toy_ech_2018)
  testthat::expect_true("tipo_hogar" %in% names(a))
  testthat::expect_message(household_type(a))
})
