context("Testing get_microdata")

test_that("download and read data", {
  expect_is(get_microdata(year = "2017"), "data.frame")
})

