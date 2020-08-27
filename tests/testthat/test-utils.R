test_that("utils works", {
  a <- get_ipab_region(folder = tempdir(), sheet = 1, region = "M")
  expect_equal(ncol(a), 2)
  b <- get_ipab_region(folder = tempdir(), sheet = 1, region = "I")
  expect_equal(ncol(b), 2)
  toy_ech_2018 <- age_groups(data = ech::toy_ech_2018, cut = c(0, 4, 11, 17, 24))
  expect_true("age_groups" %in% names(toy_ech_2018))
})
