source("../../config_oms.R", chdir = TRUE)
source("../../measures.R", chdir = TRUE)

make_test_data <- function() {
  data.frame(
    GAM         = c(1, 1, 0, 0, NA),
    SAM         = c(0, 0, 0, 0, NA),
    Stunting    = c(1, 0, 0, 0, NA),
    Underweight = c(0, 0, 0, 0, NA),
    MUAC_GAM    = c(1, 0, 0, 0, NA),
    MUAC_SAM    = c(0, 0, 1, 0, NA),
    .whz        = c(-2.5, -1.5, -0.2, NA, NA),
    .haz        = c(-2.1, -0.8, 0.2, NA, NA),
    check.names = FALSE
  )
}

test_that("measure_binary calcule effectif, prévalence et IC Wilson", {
  ms <- measure_indicator(make_test_data(), "GAM")
  expect_equal(ms$n, 4L)
  expect_equal(ms$cases, 2L)
  expect_equal(ms$pct, 50)
  expect_equal(ms$severity, "urgence")
  expect_true(!is.na(ms$ci_low))
  expect_true(!is.na(ms$ci_high))
  expect_match(measure_ci_text(ms, include_n = TRUE), "IC 95%")
  expect_match(measure_ci_text(ms, include_n = TRUE), "n=4")
})

test_that("measure_indicator gère un indicateur de sévérité distinct", {
  ms <- measure_indicator(make_test_data(), "MUAC_SAM", indicator = "SAM")
  expect_equal(ms$pct, 25)
  expect_equal(ms$severity, "urgence")
})

test_that("measure_mean traite les z-scores", {
  ms <- measure_indicator(make_test_data(), ".whz")
  expect_equal(ms$n, 3L)
  expect_equal(ms$mean, -1.4)
  expect_equal(ms$severity, "serieux")
})

test_that("measure_global_score agrège les indicateurs disponibles", {
  score <- measure_global_score(make_test_data())
  expect_equal(score$score, 59)
  expect_equal(score$severity, "serieux")
  expect_length(score$details, 5L)
})

test_that("build_filter_context expose les effectifs filtrés et totaux", {
  d <- make_test_data()
  ctx <- build_filter_context(d[1:3, ], d)
  expect_equal(ctx$n, 3L)
  expect_equal(ctx$n_total, 5L)
  expect_equal(ctx$share, 60)
})

test_that("measure_severity respecte les seuils OMS", {
  expect_equal(measure_severity(5, "GAM"), "alerte")
  expect_equal(measure_score_severity(80), "acceptable")
  expect_equal(measure_zscore_severity(-2.4), "critique")
})

test_that("measure_card_specs expose des spécifications cohérentes", {
  cards <- measure_card_specs(make_test_data())
  expect_length(cards, 8L)
  expect_equal(cards[[1]]$id, "GAM")
  expect_equal(cards[[1]]$spec$max_pct, 30)
  muac_sam <- Filter(function(x) x$id == "MUAC_SAM", cards)[[1]]
  expect_equal(muac_sam$measure$severity, "urgence")
  whz <- Filter(function(x) x$id == ".whz", cards)[[1]]
  expect_equal(whz$measure$kind, "mean")
  expect_equal(whz$measure$mean, -1.4)
})
