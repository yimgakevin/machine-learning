# Tests du module d'exploration : statistiques numériques et catégorielles.
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tibble)
})
source("../../mod_explor.R", chdir = TRUE)

test_that("explor_numeric_stats calcule les statistiques usuelles", {
  s <- explor_numeric_stats(c(1, 2, 2, 3, NA))
  val <- function(m) s$Valeur[s$Mesure == m]

  expect_equal(val("Moyenne (μ)"), "2.0000")
  expect_equal(val("Médiane"), "2.0000")
  expect_equal(val("Mode"), "2.0000")
  expect_equal(val("N valides"), "4")
  expect_equal(val("N manquants"), "1")
  expect_equal(val("% manquants"), "20.0")
  expect_true(as.numeric(val("IC 95% moyenne inf")) < 2)
  expect_true(as.numeric(val("IC 95% moyenne sup")) > 2)
})

test_that("explor_numeric_stats gère les séries vides et constantes", {
  vide <- explor_numeric_stats(c(NA_real_, NA_real_))
  expect_equal(vide$Valeur[vide$Mesure == "N valides"], "0")
  expect_equal(vide$Valeur[vide$Mesure == "% manquants"], "100")

  constant <- explor_numeric_stats(rep(5, 10))
  expect_equal(constant$Valeur[constant$Mesure == "Écart-type (σ)"], "0.0000")
  expect_equal(constant$Valeur[constant$Mesure == "Mode"], "5.0000")
})

test_that("explor_numeric_stats calcule les moyennes géométrique et harmonique", {
  s <- explor_numeric_stats(c(1, 2, 4))
  val <- function(m) as.numeric(s$Valeur[s$Mesure == m])
  expect_equal(val("Moyenne géométrique"), 2, tolerance = 1e-4)
  expect_equal(val("Moyenne harmonique"), 12 / 7, tolerance = 1e-4)
})

test_that("explor_categorical_stats produit effectifs, pourcentages et IC", {
  s <- explor_categorical_stats(c("A", "A", "B", NA, "B", "B"))
  expect_equal(nrow(s), 2)
  expect_equal(sum(s$N), 5)
  expect_equal(s$`%`[s$Modalité == "A"], 40)
  expect_equal(s$`%`[s$Modalité == "B"], 60)
  expect_equal(s$`% cumulé`[s$Modalité == "B"], 100)
  expect_true(all(s$`IC 95% inf` >= 0 & s$`IC 95% sup` <= 100))
  expect_true(s$`IC 95% inf`[s$Modalité == "A"] < 40)
  expect_true(s$`IC 95% sup`[s$Modalité == "A"] > 40)
})

test_that("explor_categorical_stats gère une variable vide", {
  s <- explor_categorical_stats(c(NA, NA))
  expect_equal(nrow(s), 0)
  expect_true(all(c("Modalité", "N", "IC 95% inf") %in% names(s)))
})
