library(leaflet)
source("../../config_oms.R", chdir = TRUE)
source("../../utils.R", chdir = TRUE)
source("../../module_carto_v2.R", chdir = TRUE)

test_that("pal_oms_leaflet retourne une fonction palette", {
  pal <- pal_oms_leaflet(c(5, 10, 20), "GAM")
  expect_true(is.function(pal))
  cols <- pal(c(5, 10, 20))
  expect_true(all(grepl("^#", cols)))
})

test_that("make_popup génère du HTML", {
  row_data <- list(n = 50, GAM = 12.5, SAM = 3.2)
  html <- make_popup("TestZone", row_data, c("n", "GAM", "SAM"))
  expect_true(is.character(html))
  expect_true(grepl("TestZone", html))
  expect_true(grepl("<b", html))
})
