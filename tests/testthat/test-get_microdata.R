context("Testing get_microdata")

test_that("download and read data", {
  aaa <- get_microdata(year = "2016", folder = tempdir(), toR = FALSE)
  testthat::expect_is(aaa, "data.frame")
  testthat::expect_equal(nrow(aaa), 118591L)
})

