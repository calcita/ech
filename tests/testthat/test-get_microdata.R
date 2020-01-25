context("Testing get_microdata")

test_that("download and read data", {
  if (!(isWindows())) {
  expect_is(get_microdata(year = "2017", folder = tempdir(), toR = FALSE), "data.frame")
  }
})

