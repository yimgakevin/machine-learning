library(dplyr)
source("../../config_oms.R", chdir = TRUE)
source("../../utils.R", chdir = TRUE)

test_that("detect_col trouve les colonnes par motifs", {
  noms <- c("region", "departement", "age_mois", "sexe")
  expect_equal(detect_col(noms, c("region", "reg")), "region")
  expect_equal(detect_col(noms, c("dept", "departement")), "departement")
  expect_null(detect_col(noms, c("pays", "country")))
})

test_that("detect_col est insensible à la casse", {
  noms <- c("Region", "DEPARTEMENT")
  res <- detect_col(noms, c("region"))
  expect_true(!is.null(res))
})
