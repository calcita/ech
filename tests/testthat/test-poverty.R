test_that("ubn works", {
  toy_ech_18 <- enrolled_school(data = ech::toy_ech_2018)
  toy_ech_18 <- years_of_schooling(toy_ech_18)
  toy_ech_18a <- unsatisfied_basic_needs(toy_ech_18)
  expect_equal(ncol(toy_ech_18a), 593)
  toy_ech_18b <- unsatisfied_basic_needs(toy_ech_18, ipm = TRUE)
  expect_equal(ncol(toy_ech_18b), 594)
})
