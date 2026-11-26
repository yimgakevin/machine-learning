# Tests du socle expert : IC, design, qualité, effets, petits effectifs.
library(dplyr)
source("../../utils.R", chdir = TRUE)
source("../../mod_expert_helpers.R", chdir = TRUE)

test_that("expert_to_binary reconnaît les codages courants", {
  expect_equal(expert_to_binary(c("oui", "non", "Oui", NA)), c(1L, 0L, 1L, NA_integer_))
  expect_equal(expert_to_binary(c(TRUE, FALSE)), c(1L, 0L))
  expect_equal(expert_to_binary(c(1, 0, 1)), c(1L, 0L, 1L))
  expect_equal(expert_to_binary(c("positif", "négatif", "inconnu")),
               c(1L, 0L, NA_integer_))
})

test_that("expert_wilson encadre la proportion et reste dans [0, 1]", {
  ic <- expert_wilson(5, 20)
  expect_true(ic[1, "lower"] < 0.25 && ic[1, "upper"] > 0.25)
  expect_gte(ic[1, "lower"], 0)
  expect_lte(ic[1, "upper"], 1)

  ic0 <- expert_wilson(0, 20)
  expect_equal(unname(ic0[1, "lower"]), 0)
  expect_gt(ic0[1, "upper"], 0)

  ic20 <- expert_wilson(20, 20)
  expect_equal(unname(ic20[1, "upper"]), 1)
  expect_lt(ic20[1, "lower"], 1)
})

test_that("expert_adjust_p contrôle le FDR", {
  p <- c(0.01, 0.04, 0.03)
  adj <- expert_adjust_p(p, "BH")
  expect_true(all(adj >= p))
  expect_equal(expert_adjust_p(p, "none"), p)
})

test_that("expert_ari vaut 1 pour des partitions identiques", {
  expect_equal(expert_ari(c(1, 1, 2, 2), c(1, 1, 2, 2)), 1)
  expect_equal(expert_ari(c(1, 1, 2, 2), c(2, 2, 1, 1)), 1)
  expect_lt(expert_ari(c(1, 1, 2, 2), c(1, 2, 1, 2)), 1)
})

test_that("expert_design_args lit les contrôles du module", {
  input <- list(uni_use_design = TRUE, uni_cluster = "grappe",
                uni_strata = "", uni_weights = "poids")
  da <- expert_design_args(input, "uni")
  expect_equal(da$cluster, "grappe")
  expect_null(da$strata)
  expect_equal(da$weights, "poids")
  expect_null(expert_design_args(list(uni_use_design = FALSE), "uni")$cluster)
})

test_that("expert_mask_small masque les strates fragiles", {
  d <- data.frame(
    N = c(100, 20, 50, 40),
    Cas = c(10, 2, 1, 3),
    `Prév. (%)` = c(10, 10, 2, 7.5),
    `IC bas (%)` = c(5, 5, 1, 4),
    `IC haut (%)` = c(18, 20, 4, 12),
    check.names = FALSE
  )
  m <- expert_mask_small(d)
  expect_false(is.na(m$`Prév. (%)`[1]))
  expect_equal(m$Diffusion[1], "Libre")
  expect_true(is.na(m$`Prév. (%)`[2]))
  expect_true(is.na(m$Cas[2]))
  expect_match(m$Diffusion[2], "n < 30")
  expect_true(is.na(m$`Prév. (%)`[3]))
  expect_match(m$Diffusion[3], "1-4 cas")
  expect_equal(sum(is.na(m$`Prév. (%)`)), 3)
})

test_that("expert_mask_small laisse les tableaux sans N/Cas inchangés", {
  d <- data.frame(Indicateur = "Test", Valeur = 12)
  expect_identical(expert_mask_small(d), d)
})

test_that("expert_mask_small gère N cas et les statistiques dérivées", {
  d <- data.frame(
    N = c(100, 20),
    `N cas` = c(10, 8),
    `Prévalence (%)` = c(10, 40),
    Moyenne = c(5, 6),
    `% manquants` = c(1, 2),
    check.names = FALSE
  )
  m <- expert_mask_small(d)
  expect_false(is.na(m$`Prévalence (%)`[1]))
  expect_true(is.na(m$`Prévalence (%)`[2]))
  expect_true(is.na(m$Moyenne[2]))
  expect_true(is.na(m$`% manquants`[2]))
  expect_equal(m$`N cas`[2], 8)
  expect_equal(m$Diffusion[2], "n < 30")
})

test_that("expert_mask_small ne masque pas les moyennes pour 1-4 cas", {
  d <- data.frame(N = 100, Cas = 3, `Prévalence (%)` = 3, Moyenne = 5,
                  check.names = FALSE)
  m <- expert_mask_small(d)
  expect_true(is.na(m$`Prévalence (%)`))
  expect_true(is.na(m$Cas))
  expect_equal(m$Moyenne, 5)
  expect_equal(m$Diffusion, "1-4 cas")
})

test_that("expert_quality produit une ligne par variable", {
  d <- data.frame(a = c(1, 2, NA), b = c("x", NA, NA), stringsAsFactors = FALSE)
  q <- expert_quality(d, c("a", "b"))
  expect_equal(nrow(q), 2)
  expect_true(all(c("Variable", "N", "N valides", "% manquants") %in% names(q)))
  expect_equal(q$`N valides`[q$Variable == "b"], 1)
})

test_that("expert_rate calcule un taux et son IC", {
  d <- data.frame(deces = c(0, 1, 0, 1), pt = rep(1000, 4))
  r <- expert_rate(d, "deces", "pt")
  expect_equal(r$Deces, 2)
  expect_equal(r$`Temps-personne`, 4000)
  expect_gt(r$Taux, 0)
  expect_true(r$`IC bas` <= r$Taux && r$`IC haut` >= r$Taux)
})
