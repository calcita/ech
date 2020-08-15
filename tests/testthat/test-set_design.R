test_that("set_design works", {
  expect_is(set_design(data = ech::toy_ech_2018, level = "h"), "survey.design")
  expect_is(set_design(data = ech::toy_ech_2018, level = "i"), "survey.design")
  expect_is(set_design(data = ech::toy_ech_2018, level = "i", ids = "numero"), "survey.design")
  expect_is(set_design(data = ech::toy_ech_2018, level = "h", ids = "numero"), "survey.design")
})
