test_that("read_microdata works", {
  # get_microdata(year = 2018, folder = tempdir(), toR = TRUE)
  aaa <- read_microdata(path = "data/toy_ech_2018.rda")
  expect_true("aaa" %in% ls())
})
