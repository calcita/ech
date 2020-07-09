test_that("organize_labels works", {
  expect_is(organize_names(data = ech::toy_ech_2018, year = 2018, level = "h"), "data.frame")
  expect_is(organize_names(data = ech::toy_ech_2018, year = 2018, level = "i"), "data.frame")
  expect_is(organize_namess(data = ech::toy_ech_2018, year = 2018, level = "hyp"), "data.frame")
})
