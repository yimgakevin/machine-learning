library(dplyr)
source("../../module_qualite_smart.R", chdir = TRUE)

test_that("calc_digit_preference retourne un score", {
  set.seed(9182)
  x <- round(rnorm(500, 0, 1), 1)
  res <- calc_digit_preference(x, "WHZ")
  expect_true(!is.null(res))
  expect_true("score" %in% names(res) || "dps" %in% names(res) || is.numeric(res))
})

test_that("calc_completude gère les colonnes manquantes", {
  d <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 1))
  res <- calc_completude(d)
  expect_true(!is.null(res))
})
