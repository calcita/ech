test_that("enrolled_school works", {
  a <- enrolled_school()
  testthat::expect_equal(table(a$enrollment), structure(c(733L, 267L), .Dim = 2L, .Dimnames = structure(list(
    c("0", "1")), .Names = ""), class = "table"))
  testthat::expect_warning(enrolled_school(a))
})
