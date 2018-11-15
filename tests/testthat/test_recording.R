context("Recordings")
library(dplyr)

test_that("Can record an expression", {
  a <- emc_record(A = 1)
  expect_equal(a$A, list(1))
  expect_equal(a$A_error, "")
  expect_equal(a$A_errors, 0)
  expect_equal(a$A_warnings, 0)
})


