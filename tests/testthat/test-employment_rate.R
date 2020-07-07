test_that("emploiment works", {
  a <- employment()
  testthat::expect_equal(table(a$pea), structure(c(484L, 516L), .Dim = 2L, .Dimnames = structure(list(
    c("0", "1")), .Names = ""), class = "table"))
  testthat::expect_warning(employment(a))
  testthat::expect_error(employment(pobpcoac = "aaa"))
})
