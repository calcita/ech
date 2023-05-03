context("Testing get_microdata")
# library(haven)
test_that("download and read data", {

  # a <- get_microdata(year = "2016", folder = tempdir(), toR = TRUE)
  # testthat::expect_equal(ncol(a),573)
  # b <- get_microdata(year = "2016", folder = tempdir(), toR = FALSE)
  # testthat::expect_equal(ncol(b),573)

  testthat::expect_error(get_microdata(folder = 1, toR = FALSE))
  # testthat::expect_error(get_microdata(year = "2018", folder = tempdir(), toR = TRUE))
  testthat::expect_error(get_microdata(year = "2010", folder = tempdir(), toR = FALSE))
  # testthat::expect_error(get_microdata(year = "2016", folder = tempdir(), toR = FALSE))
  # testthat::expect_error(get_microdata(folder = tempdir(), toR = FALSE))

  testthat::expect_error(read_microdata(path = 1))

  # a <- get_marco(year = "2004", folder = tempdir(), toR = TRUE)
  # testthat::expect_is(a, "data.frame")
  # b <- get_marco(year = "2004", folder = tempdir(), toR = FALSE)
  # testthat::expect_is(b, "data.frame")
  # testthat::expect_error(get_marco(year = "2010", folder = tempdir(), toR = FALSE))
  # testthat::expect_error(get_marco(folder = 1, toR = FALSE))
})

# context("Testing get_dictionary")
# test_that("download dictionaries", {
#   testthat::expect_error(get_dictionary(year = 2010, folder = tempdir()))
#   get_dictionary(year = 2019, folder = tempdir())
#   archivo <- fs::dir_ls(tempdir(), regexp = "\\.xls$")
#   archivo <- archivo[which.max(file.info(archivo)$mtime)]
#   testthat::expect_equal(length(archivo), 1)
#   get_dictionary(year = NULL, folder = tempdir())
#   archivo <- fs::dir_ls(tempdir(), regexp = "\\.xls$")
#   archivo <- archivo[which.max(file.info(archivo)$mtime)]
#   testthat::expect_equal(length(archivo), 1)
#   testthat::expect_error(get_dictionary(folder = 1))
#   testthat::expect_message(get_dictionary(year = 2019, folder = tempdir()))
# })
