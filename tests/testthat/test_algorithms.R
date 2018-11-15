context("Algorithms")
library(dplyr)

test_that("Can load the algorithm list", {
  algorithms <- emc_algorithms() %>% dplyr::filter(algorithm == "lm")
  expect_gt(nrow(algorithms), 0)
})

test_that("Show only installed algorithms", {
  algorithms <- emc_algorithms(uninstalled = FALSE)
  expect_equal(algorithms %>% filter(!installed) %>% nrow(), 0)
})

test_that("Include uninstalled algorithms", {
  algorithms <- emc_algorithms(uninstalled = TRUE)
  expect_gt(algorithms %>% filter(!installed) %>% nrow(), 0)
})

test_that("Can load algorithm tags", {
  algorithms_tags <- emc_algorithm_tags(emc_algorithms()$algorithm)
  expect_gt(nrow(algorithms_tags), 0)
})
