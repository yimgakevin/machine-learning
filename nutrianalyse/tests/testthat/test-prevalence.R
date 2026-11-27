library(dplyr)
library(tibble)
source("../../module_prevalence_smart.R", chdir = TRUE)

test_that("calc_prevalence_survey retrouve une prévalence non pondérée", {
  d <- data.frame(
    w         = rep(1, 10),
    indicator = c(rep(1, 3), rep(0, 7))
  )
  design <- create_survey_design(d, col_weights = "w")
  res <- calc_prevalence_survey(design, "indicator")

  expect_equal(res$prevalence, 30)
  expect_equal(res$n_unweighted, 10)
  expect_equal(res$n_cases, 3)
})

test_that("calc_prevalence_survey respecte les poids de sondage", {
  # 1 cas de poids 1 sur un total de poids 10 → 10 % pondéré,
  # alors que la prévalence non pondérée serait 20 %.
  d <- data.frame(
    w         = c(1, 1, 1, 2, 5),
    indicator = c(1, 0, 0, 0, 0)
  )
  design <- create_survey_design(d, col_weights = "w")
  res <- calc_prevalence_survey(design, "indicator")

  expect_equal(res$prevalence, 10)
  expect_equal(res$n_unweighted, 5)
  expect_equal(res$n_cases, 1)
})

test_that("calc_prevalence_manual calcule l'IC Wilson de référence", {
  d <- data.frame(
    w         = c(1, 1, 1, 2, 5),
    indicator = c(1, 0, 0, 0, 0)
  )
  res <- calc_prevalence_manual(d, "indicator")

  expect_equal(res$prevalence, 20)
  expect_equal(res$n_unweighted, 5)
  expect_equal(res$n_cases, 1)
  expect_equal(res$ci_low, 3.62)
  expect_equal(res$ci_high, 62.45)
  expect_equal(res$method, "Wilson (SRS)")
})
