library(dplyr)

test_that("gini works", {
  a <- gini_income()
  testthat::expect_is(a, "data.frame")
  # testthat::expect_is(a, "gini")
  # testthat::expect_equal(a$value, 37.9265443797023)
  # b1 = ech::toy_ech_2018 %>% mutate(f = e26 == 1)
  # b <- gini_income(data = b1, domain = "f")
  # testthat::expect_equal(b$valueByStratum,
  #                        structure(list(stratum = structure(1:2, .Label = c("FALSE", "TRUE"
  #                        ), class = "factor"), value = c(39.4716985328902, 36.4597355546236
  #                        )), .Names = c("stratum", "value"), row.names = c(NA, -2L), class = "data.frame"))
})
