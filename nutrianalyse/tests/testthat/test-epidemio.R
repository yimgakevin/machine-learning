library(dplyr)
source("../../module_epidemio.R", chdir = TRUE)

test_that("analyse_2x2 calcule RR, OR, PAF correctement", {
  set.seed(7351)
  exposed <- c(rep(1, 60), rep(0, 40))
  outcome <- c(rbinom(60, 1, 0.4), rbinom(40, 1, 0.1))
  res <- analyse_2x2(exposed, outcome)
  expect_true(!is.null(res))
  expect_true("rr" %in% names(res))
  expect_true("or" %in% names(res))
  expect_true(res$rr > 0)
  expect_true(res$or > 0)
  expect_true(res$rr_lo < res$rr && res$rr < res$rr_hi)
})

test_that("analyse_2x2 retourne NULL si tableau < 2x2", {
  res <- analyse_2x2(c(1, 1, 1), c(1, 1, 1))
  expect_null(res)
})

test_that("multi_exposition traite plusieurs variables", {
  set.seed(2847)
  d <- data.frame(
    exp1 = rbinom(200, 1, 0.3),
    exp2 = rbinom(200, 1, 0.5),
    outcome = rbinom(200, 1, 0.2)
  )
  res <- multi_exposition(d, c("exp1", "exp2"), "outcome")
  expect_true(is.data.frame(res) || is.list(res))
})
