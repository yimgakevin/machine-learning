# Tests de structure de l'application (remplace les tests golem obsolètes).
root <- normalizePath("../..", mustWork = TRUE)

test_that("l'orchestrateur est présent et se parse", {
  f <- file.path(root, "app_v2_final.R")
  expect_true(file.exists(f))
  expect_silent(parse(f))
})

test_that("les parties UI et serveur sont présentes", {
  files <- c(
    "app_parts/00_setup.R", "app_parts/ui/ui.R", "app_parts/ui/ui_body.R",
    "app_parts/server/server_core.R", "app_parts/server/server_modules.R"
  )
  for (f in files) expect_true(file.exists(file.path(root, f)), info = f)
})

test_that("les ressources et le lockfile existent", {
  expect_true(file.exists(file.path(root, "app_parts/01_css.R")))
  expect_true(file.exists(file.path(root, "renv.lock")))
  expect_true(file.exists(file.path(root, ".Renviron.example")))
})
