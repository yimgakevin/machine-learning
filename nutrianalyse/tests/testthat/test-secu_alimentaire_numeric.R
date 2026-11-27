library(dplyr)
library(tibble)
source("../../module_secu_alimentaire.R", chdir = TRUE)

test_that("calc_fcs applique les poids PAM, le plafond à 7 et les seuils", {
  d <- data.frame(
    cereales     = c(0, 7, 7, 10),
    legumineuses = c(0, 7, 2, 0),
    legumes      = c(0, 7, 3, 0),
    fruits       = c(0, 7, 0, 0),
    viande       = c(0, 7, 0, 0),
    lait         = c(0, 7, 0, 0),
    sucre        = c(0, 7, 0, 0),
    huile        = c(0, 7, 7, 0),
    condiments   = c(0, 7, 0, 0)
  )
  res <- calc_fcs(
    d, "cereales", "legumineuses", "legumes", "fruits",
    "viande", "lait", "sucre", "huile", "condiments"
  )

  # 0 ; 7 × (2+3+1+1+4+4+0,5+0,5) = 112 ; 14+6+3+3,5 = 26,5 ;
  # 10 est plafonné à 7 → 7 × 2 = 14.
  expect_equal(res$score, c(0, 112, 26.5, 14))
  expect_equal(
    as.character(res$class),
    c("Pauvre", "Acceptable", "Limite", "Pauvre")
  )
  expect_equal(res$n_groups, 9L)
})

test_that("classify_fcs_precalc respecte les bornes 21 et 35", {
  res <- classify_fcs_precalc(c(20.9, 21, 35, 35.1, NA))
  expect_equal(
    as.character(res$class),
    c("Pauvre", "Limite", "Limite", "Acceptable", NA)
  )
})

test_that("calc_hdds compte les groupes consommés et classe la diversité", {
  d <- data.frame(
    g1 = c(0, 1, 1, 1),
    g2 = c(0, 1, 1, 1),
    g3 = c(0, 1, 1, 1),
    g4 = c(0, 1, 1, 1),
    g5 = c(0, 0, 1, 1),
    g6 = c(0, 0, 0, 1)
  )
  res <- calc_hdds(d, c("g1", "g2", "g3", "g4", "g5", "g6"))

  expect_equal(as.numeric(res$score), c(0, 4, 5, 6))
  expect_equal(
    as.character(res$class),
    c("Faible diversité", "Diversité moyenne", "Diversité moyenne", "Bonne diversité")
  )
})

test_that("calc_hhs recode les fréquences et classe la faim", {
  d <- data.frame(
    q1 = c(0, 1, 2, 3, 1, 1),
    q2 = c(0, 2, 1, 3, 1, 1),
    q3 = c(0, 1, 2, 0, 0, 1)
  )
  res <- calc_hhs(d, "q1", "q2", "q3")

  expect_equal(as.numeric(res$score), c(0, 4, 5, 4, 2, 3))
  expect_equal(
    as.character(res$class),
    c(
      "Peu ou pas de faim", "Faim sévère", "Faim sévère",
      "Faim sévère", "Faim modérée", "Faim modérée"
    )
  )
})
