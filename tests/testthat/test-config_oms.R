source("../../config_oms.R", chdir = TRUE)

test_that("OMS_SEUILS contient les indicateurs clés", {
  expect_true(all(c("GAM", "SAM", "Stunting") %in% names(OMS_SEUILS)))
  for (ind in names(OMS_SEUILS)) {
    s <- OMS_SEUILS[[ind]]
    expect_true(all(c("acceptable", "alerte", "serieux", "critique") %in% names(s)))
    expect_true(s$acceptable < s$alerte)
    expect_true(s$alerte < s$serieux)
    expect_true(s$serieux < s$critique)
  }
})

test_that("classify_oms retourne les bons labels", {
  expect_equal(classify_oms(3, "GAM"), "Acceptable")
  expect_equal(classify_oms(7, "GAM"), "Alerte")
  expect_equal(classify_oms(12, "GAM"), "Sérieux")
  expect_equal(classify_oms(20, "GAM"), "Critique")
  expect_equal(classify_oms(35, "GAM"), "Urgence max")
})

test_that("classify_oms est vectorisé", {
  res <- classify_oms(c(3, 12, 35), "GAM")
  expect_length(res, 3)
})

test_that("color_oms retourne des couleurs hex", {
  cols <- color_oms(c(3, 12, 35), "GAM")
  expect_true(all(grepl("^#", cols)))
})

test_that("seuil_critique retourne un numérique", {
  expect_equal(seuil_critique("GAM"), 15)
  expect_equal(seuil_critique("SAM"), 5)
  expect_is(seuil_critique("INCONNU"), "numeric")
})
