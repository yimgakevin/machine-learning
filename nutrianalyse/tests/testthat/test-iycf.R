library(dplyr)
library(tibble)
source("../../module_secu_alimentaire.R", chdir = TRUE)
source("../../module_iycf.R", chdir = TRUE)

test_that("calc_iycf_indicators calcule l'EBF sur les enfants éligibles", {
  d <- data.frame(
    age = c(0, 1, 2, 3, 4, 5),
    ebf = c(1, 1, 0, 1, NA, 1)
  )
  res <- calc_iycf_indicators(
    d, col_age = "age", col_ebf = "ebf", use_design = FALSE
  )

  expect_null(res$error)
  ebf <- res$results$ebf
  expect_equal(ebf$n, 5)
  expect_equal(ebf$n_pos, 4)
  expect_equal(ebf$pct, 80)
  expect_equal(ebf$ci_lower, 37.6)
  expect_equal(ebf$ci_upper, 96.4)
})

test_that("iycf_eligible applique les bornes d'âge des indicateurs", {
  ages <- c(0, 5, 6, 11, 12, 23, 24)

  expect_equal(iycf_eligible(ages, "ebf"), c(TRUE, TRUE, rep(FALSE, 5)))
  expect_equal(iycf_eligible(ages, "cbf"), c(rep(FALSE, 4), TRUE, TRUE, FALSE))
})

test_that("iycf_binary reconnaît les modalités Oui/Non et numériques", {
  res <- iycf_binary(c("Oui", "Non", "oui ", NA, "NA", "1"))

  expect_equal(res$n_known, 4)
  expect_equal(res$n_pos, 3)
  expect_equal(iycf_norm(c("Allaitement exclusif", "Oui ")),
               c("allaitement_exclusif", "oui"))
})
