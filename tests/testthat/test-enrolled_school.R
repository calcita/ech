test_that("enrolled_school works", {
  a <- enrolled_school()
  testthat::expect_equal(table(a$school_enrollment), structure(c(967L, 327L), .Dim = 2L, .Dimnames = structure(list(
    c("0", "1")), .Names = ""), class = "table"))
})
