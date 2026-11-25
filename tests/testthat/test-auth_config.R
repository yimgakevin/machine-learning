# Tests de la configuration d'authentification et des permissions.
source("../../auth_config.R", chdir = TRUE)

test_that("les mots de passe sont hachés et jamais en clair", {
  expect_true(all(grepl("^\\$7\\$", USER_BASE$password)))
  expect_false(any(grepl("Admin2026|Analyste2026|Lecteur2026", USER_BASE$password)))
  expect_true(all(nchar(USER_BASE$password) > 50))
})

test_that("les rôles sont valides et les identifiants uniques", {
  expect_true(all(USER_BASE$role %in% names(ROLE_PERMISSIONS)))
  expect_false(anyDuplicated(USER_BASE$user) > 0)
  expect_true(all(c("user", "password", "role", "name") %in% names(USER_BASE)))
})

test_that("les permissions par rôle sont cohérentes", {
  expect_true(user_can_access_tab("admin", "predictif"))
  expect_true(user_can_access_tab("analyste", "zscore"))
  expect_false(user_can_access_tab("lecteur", "predictif"))
  expect_false(user_has_permission("lecteur", "can_ia"))
  expect_true(user_has_permission("admin", "can_delete"))
})
