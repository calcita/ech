test_that("get_ipc works", {
  ipc_base2010 <- get_ipc(folder = tempdir())
  testthat::expect_true("ipc_base2010" %in% ls())
})
