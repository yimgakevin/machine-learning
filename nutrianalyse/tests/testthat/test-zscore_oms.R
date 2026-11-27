library(dplyr)
library(tibble)
source("../../module_zscore_oms.R", chdir = TRUE)

test_that("calc_zscore_lms applique la formule LMS de l'OMS", {
  # L ≠ 0 : Z = ((y/M)^L - 1) / (L × S)
  expect_equal(calc_zscore_lms(12, 1, 10, 0.1), 2)
  expect_equal(calc_zscore_lms(10 / 0.9, -1, 10, 0.1), 1, tolerance = 1e-8)

  # L = 0 : Z = ln(y/M) / S
  expect_equal(
    calc_zscore_lms(10 * exp(0.2), 0, 10, 0.1),
    2,
    tolerance = 1e-8
  )

  # Les mesures non positives ne produisent pas de z-score.
  expect_true(is.na(calc_zscore_lms(-1, 1, 10, 0.1)))
})

test_that("calc_zscore_lms applique l'ajustement SD23 au-delà de 3 écarts-types", {
  # Pour L = 0,5 ; M = 10 ; S = 0,1, la valeur 14,4 correspond à Z = 4.
  # L'OMS ajuste alors Z = 3 + (y - SD3) / (SD3 - SD2).
  sd3 <- 10 * (1 + 0.5 * 0.1 * 3)^(1 / 0.5)
  sd2 <- 10 * (1 + 0.5 * 0.1 * 2)^(1 / 0.5)
  expected <- 3 + (14.4 - sd3) / (sd3 - sd2)

  expect_equal(calc_zscore_lms(14.4, 0.5, 10, 0.1), expected, tolerance = 1e-8)
})

test_that("interp_lms interpole et renvoie NA hors plage", {
  ref <- tibble(
    key = c(1, 3),
    L = c(0, 2),
    M = c(10, 30),
    S = c(0.1, 0.3)
  )
  res <- interp_lms(ref, "key", c(1, 2, 3, 0, 4, NA))

  expect_equal(res$L, c(0, 1, 2, NA, NA, NA))
  expect_equal(res$M, c(10, 20, 30, NA, NA, NA))
  expect_equal(res$S, c(0.1, 0.2, 0.3, NA, NA, NA))
})

test_that("calcul_zscores_oms intègre LMS, flags et classification", {
  d <- data.frame(
    poids  = c(12, 6),
    taille = c(90, 80),
    age    = c(24, 24),
    sexe   = c("M", "M")
  )
  res <- calcul_zscores_oms(d, "poids", "taille", "age", "sexe")

  expect_equal(res$WHZ_calc[1], 0.6001064, tolerance = 1e-6)
  expect_false(res$flag_WHZ[1])
  expect_equal(res$class_GAM[1], "Normal")

  expect_true(res$flag_WHZ[2])
  expect_equal(res$class_GAM[2], "MAS (Malnutrition Aiguë Sévère)")
  expect_true(res$flag_WAZ[2])
  expect_equal(res$class_retard[2], "Retard modéré")
})
