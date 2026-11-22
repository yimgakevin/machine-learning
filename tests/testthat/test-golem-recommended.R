# =============================================================================
# test-golem-recommended.R — Tests recommandés par golem
# NutriAnalyse Pro v2
# =============================================================================

test_that("app_ui is a function", {
  expect_type(app_ui, "closure")
})

test_that("app_server is a function", {
  expect_type(app_server, "closure")
})

test_that("app_ui returns a tagList", {
  ui <- app_ui()
  expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
})

test_that("run_app is exported", {
  expect_true("run_app" %in% getNamespaceExports("NutriAnalysePro"))
})

test_that("golem-config.yml exists", {
  config_file <- app_sys("golem-config.yml")
  expect_true(file.exists(config_file))
})

test_that("custom.css exists in www/", {
  css_file <- app_sys("app/www/custom.css")
  expect_true(file.exists(css_file))
})
