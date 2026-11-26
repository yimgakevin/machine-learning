# Tests de la configuration d'authentification.
source("../../auth_config.R", chdir = TRUE)

test_that("les mots de passe sont hachés et jamais en clair", {
  expect_true(all(grepl("^\\$7\\$", USER_BASE$password)))
  expect_false(any(grepl("Admin2026|Analyste2026|Lecteur2026", USER_BASE$password)))
  expect_true(all(nchar(USER_BASE$password) > 50))
})

test_that("les utilisateurs n'ont plus de rôle et les identifiants sont uniques", {
  expect_false("role" %in% names(USER_BASE))
  expect_true(all(c("user", "password", "name") %in% names(USER_BASE)))
  expect_false(anyDuplicated(USER_BASE$user) > 0)
  expect_true(all(nzchar(USER_BASE$name)))
})
