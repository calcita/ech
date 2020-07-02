test_that("get_ipc works", {
  get_ipc(folder = tempdir())
  expect_true("ipc_base2010.rds" %in% list.files(tempdir()))
})
