test_that("gini works", {
  aaa <- gini()
  testthat::expect_is(aaa, "gini")
  testthat::expect_equal(aaa$value, 37.9265443797023)
})
