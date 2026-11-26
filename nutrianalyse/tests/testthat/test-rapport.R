library(officer)
source("../../module_rapport.R", chdir = TRUE)

test_that("interpreter_prevalence retourne du texte", {
  res <- interpreter_prevalence("GAM", 18.5)
  expect_true(is.character(res))
  expect_true(nchar(res) > 10)
})

test_that("interpreter_prevalence gère les cas limites", {
  expect_true(is.character(interpreter_prevalence("GAM", 0)))
  expect_true(is.character(interpreter_prevalence("GAM", 100)))
  expect_true(is.character(interpreter_prevalence("INCONNU", 10)))
})
