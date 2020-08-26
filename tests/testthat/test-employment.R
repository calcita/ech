test_that("emploiment works", {
  a <- employment()
  testthat::expect_equal(ncol(a), 585)
  testthat::expect_error(employment(pobpcoac = "aaa"))
  b <- branch_ciiu(data = ech::toy_ech_2018)
  testthat::expect_equal(ncol(b), 583)
  c <- branch_ciiu(data = ech::toy_ech_2018, group = FALSE, disaggregated = TRUE)
  testthat::expect_equal(ncol(c), 583)
})
