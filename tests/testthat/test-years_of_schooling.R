test_that("years_of_schooling works", {
  toy_ech_2018 <- years_of_schooling(data = ech::toy_ech_2018)
  expect_equal(ncol(toy_ech_2018), 584)
})
