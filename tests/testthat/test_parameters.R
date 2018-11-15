context("Parameters")

test_that("Named vector parameter", {
  p <- emc_parameters(A = c(1,2,3))
  expect_equal(p$A, c(1,2,3))
  expect_true(tibble::is_tibble(p))
})

test_that("2 named vectors loaded", {
  p <- emc_parameters(A = c(1,2,3), B = c(2,3,4))
  expect_equal(p$A, c(1,2,3))
  expect_equal(p$B, c(2,3,4))
  expect_true(tibble::is_tibble(p))
})

test_that("Rows of data frame become parameters", {
  p <- emc_parameters(data.frame(A = c(1,2,3), B = c(2,3,4)))
  expect_equal(p$A, c(1,2,3))
  expect_equal(p$B, c(2,3,4))
  expect_true(tibble::is_tibble(p))
})

test_that("Vectors and data frames bind together", {
  p <- emc_parameters(A = c(1,2,3), data.frame(B = c(2,3,4)))
  expect_equal(p$A, c(1,2,3))
  expect_equal(p$B, c(2,3,4))
  expect_true(tibble::is_tibble(p))
})

test_that("Vectors and data frames bind together", {
  p <- emc_parameters(A = c(1,2,3), data.frame(B = c(2,3,4)))
  expect_equal(p$A, c(1,2,3))
  expect_equal(p$B, c(2,3,4))
  expect_true(tibble::is_tibble(p))
})

test_that("Default Name", {
  p <- emc_parameters(data.frame(A = c(1,2,3), B = c(2,3,4)))
  expect_equal(p$name, c("1, 2", "2, 3", "3, 4"))
})

test_that("Name override", {
  p <- emc_parameters(data.frame(A = c(1,2,3), B = c(2,3,4)), name = sprintf("Test A: %s B: %s", A, B))
  expect_equal(p$name, c("Test A: 1 B: 2", "Test A: 2 B: 3", "Test A: 3 B: 4"))
})


