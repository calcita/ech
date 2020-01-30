test_that("multiplication works", {
  a <- employment_rate()
  testthat::expect_equal(table(a$pea), structure(c(484L, 516L), .Dim = 2L, .Dimnames = structure(list(
    c("0", "1")), .Names = ""), class = "table"))
  testthat::expect_warning(employment_rate(a))
  testthat::expect_error(employment_rate(pobpcoac = "aaa"))
})
