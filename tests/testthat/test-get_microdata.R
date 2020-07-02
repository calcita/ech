context("Testing get_microdata")
library(haven)
test_that("download and read data", {
  testthat::expect_error(get_microdata(year = "2016", folder = tempdir(), toR = FALSE))
  testthat::expect_error(get_microdata(year = "2010", folder = tempdir(), toR = FALSE))
  testthat::expect_error(get_microdata(year = "2016", folder = tempdir(), toR = TRUE))
  testthat::expect_error(get_microdata(folder = tempdir(), toR = FALSE))
  testthat::expect_error(get_microdata(folder = 1, toR = FALSE))

  # aaa <- haven::read_sav(paste0(tempdir(),"\\HyP_2016_Terceros.sav"))
  # testthat::expect_is(aaa, "data.frame")
  # testthat::expect_equal(nrow(aaa), 118591L)
})

