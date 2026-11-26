library(dplyr)
library(caret)
source("../../module_predictif.R", chdir = TRUE)

test_that("run_predictive_pipeline fonctionne avec glm", {
  set.seed(3291)
  d <- data.frame(
    outcome = factor(rbinom(200, 1, 0.3), labels = c("Non", "Oui")),
    x1 = rnorm(200),
    x2 = rnorm(200),
    x3 = sample(c("A", "B"), 200, replace = TRUE)
  )
  res <- run_predictive_pipeline(d, "outcome", c("x1", "x2", "x3"),
                                  models = "glm", k_folds = 3)
  expect_true(!is.null(res))
  expect_true(all(c("results", "outcome", "is_binary") %in% names(res)))
})

test_that("metrics_table retourne un data.frame", {
  set.seed(5519)
  d <- data.frame(
    outcome = factor(rbinom(200, 1, 0.3), labels = c("Non", "Oui")),
    x1 = rnorm(200),
    x2 = rnorm(200)
  )
  res <- run_predictive_pipeline(d, "outcome", c("x1", "x2"),
                                  models = "glm", k_folds = 3)
  mt <- metrics_table(res)
  # metrics_table retourne toujours un data.frame (vide si aucun modèle n'a tourné)
  expect_true(is.data.frame(mt))
  expect_true("Modele" %in% names(mt))
})
