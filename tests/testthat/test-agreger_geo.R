library(dplyr)
source("../../config_oms.R", chdir = TRUE)
source("../../utils.R", chdir = TRUE)
source("../../module_carto_v2.R", chdir = TRUE)

test_that("agreger_geo agrège correctement", {
  d <- data.frame(
    region = rep(c("Nord", "Sud"), each = 50),
    GAM = rbinom(100, 1, 0.15),
    SAM = rbinom(100, 1, 0.05),
    stringsAsFactors = FALSE
  )
  agg <- agreger_geo(d, "region")
  expect_equal(nrow(agg), 2)
  expect_true("Zone" %in% names(agg))
  expect_true("n" %in% names(agg))
  expect_true("GAM" %in% names(agg))
  expect_equal(sum(agg$n), 100)
})

test_that("agreger_geo gère les NA dans la colonne géo", {
  d <- data.frame(
    region = c("Nord", NA, "Sud", "", "Nord"),
    GAM = c(1, 0, 1, 0, 0),
    stringsAsFactors = FALSE
  )
  agg <- agreger_geo(d, "region")
  expect_true(all(!is.na(agg$Zone)))
  expect_true(all(agg$Zone != ""))
})

test_that("agreger_geo avec data.table et dplyr donnent le même résultat", {
  d <- data.frame(
    region = rep(c("A", "B", "C"), each = 30),
    GAM = rbinom(90, 1, 0.1),
    stringsAsFactors = FALSE
  )
  res_dt    <- agreger_geo(d, "region")
  res_dplyr <- agreger_geo_dplyr(d, "region")
  expect_equal(nrow(res_dt), nrow(res_dplyr))
  expect_equal(sort(res_dt$Zone), sort(res_dplyr$Zone))
})
