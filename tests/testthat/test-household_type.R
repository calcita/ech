test_that("household_type works", {
  a <- household_type()
  testthat::expect_equal(table(a$tipo_hogar),structure(c(2L, 8L, 324L, 32L, 52L, 2L, 8L, 214L, 358L), .Dim = 9L, .Dimnames = structure(list(
    c("biparental", "compuesto", "error", "extendido con menores",
      "extendido sin menores", "monomarental", "monoparental",
      "pareja", "unipersonal")), .Names = ""), class = "table"))
  testthat::expect_message(household_type(a))
})
