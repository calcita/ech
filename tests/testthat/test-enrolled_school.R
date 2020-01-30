
test_that("enrolled_school works", {
  a <- enrolled_school()
  expect_equal(table(a$edu_asist), structure(c(733L, 267L), .Dim = 2L, .Dimnames = structure(list(
    c("0", "1")), .Names = ""), class = "table"))
})
