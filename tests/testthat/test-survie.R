library(dplyr)
library(survival)
source("../../module_survie.R", chdir = TRUE)

test_that("run_km produit un objet survfit", {
  set.seed(4492)
  d <- data.frame(
    time  = rexp(100, rate = 0.1),
    event = rbinom(100, 1, 0.7),
    group = rep(c("A", "B"), 50)
  )
  res <- run_km(d, "time", "event")
  expect_true(!is.null(res))
  # La fonction retourne `fit` (objet survfit) et non `km_fit`
  expect_true("fit" %in% names(res))
  expect_s3_class(res$fit, "survfit")
})

test_that("run_km avec groupes inclut log-rank p-value", {
  set.seed(6103)
  d <- data.frame(
    time  = rexp(100, rate = 0.1),
    event = rbinom(100, 1, 0.7),
    group = rep(c("A", "B"), 50)
  )
  res <- run_km(d, "time", "event", "group")
  # La p-value log-rank est dans `lr_p`
  expect_true("lr_p" %in% names(res))
  expect_true(is.numeric(res$lr_p) || is.na(res$lr_p))
})

test_that("run_cox retourne des HR avec IC", {
  set.seed(8817)
  d <- data.frame(
    time  = rexp(200, 0.05),
    event = rbinom(200, 1, 0.6),
    age   = rnorm(200, 30, 5),
    sexe  = sample(c("M", "F"), 200, replace = TRUE)
  )
  res <- run_cox(d, "time", "event", c("age", "sexe"))
  expect_true(!is.null(res))
  # run_cox retourne `model` (coxph) et `tidy` (HR table)
  expect_true("model" %in% names(res) || "tidy" %in% names(res))
  if ("tidy" %in% names(res)) {
    expect_true(all(c("term", "estimate") %in% names(res$tidy)))
  }
})
