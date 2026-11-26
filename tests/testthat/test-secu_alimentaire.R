library(dplyr)
source("../../module_secu_alimentaire.R", chdir = TRUE)

test_that("classify_fcs_precalc classifie correctement", {
  scores <- c(10, 25, 40, 60)
  res <- classify_fcs_precalc(scores)
  expect_length(res, 4)
  expect_true(all(!is.na(res)))
})

test_that("calc_hhs retourne des scores entre 0 et 6", {
  d <- data.frame(
    no_food = sample(0:2, 100, replace = TRUE),
    sleep_hungry = sample(0:2, 100, replace = TRUE),
    whole_day = sample(0:2, 100, replace = TRUE)
  )
  res <- calc_hhs(d, "no_food", "sleep_hungry", "whole_day")
  expect_true(!is.null(res))
})
