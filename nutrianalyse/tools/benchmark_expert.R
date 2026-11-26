# Micro-benchmark des helpers experts (hors application).
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(purrr)
})
source("utils.R")               # var_type(), auto_test() utilisés par les helpers
source("mod_expert_helpers.R")
truncate_label <- function(x, max_chars = 22) substr(as.character(x), 1, max_chars)

set.seed(42)
n <- 5000
d <- data.frame(
  region = sample(c("Nord", "Sud", "Est", "Ouest", "Centre"), n, TRUE),
  cluster = sample(1:50, n, TRUE),
  strate = sample(c("urbain", "rural"), n, TRUE),
  poids = round(runif(n, 0.5, 3), 2),
  ind1 = sample(c("oui", "non"), n, TRUE),
  ind2 = sample(c("oui", "non"), n, TRUE),
  ind3 = sample(c(0, 1), n, TRUE),
  ind4 = sample(c(TRUE, FALSE), n, TRUE),
  ind5 = sample(c("positif", "négatif"), n, TRUE),
  ind6 = sample(c("oui", "non"), n, TRUE),
  num = rnorm(n, 50, 10),
  stringsAsFactors = FALSE
)
vars <- c(A = "ind1", B = "ind2", C = "ind3", D = "ind4", E = "ind5", F = "ind6")
tm <- function(label, expr) {
  t <- system.time(force(expr))["elapsed"]
  cat(sprintf("%-42s %7.3f s\n", label, t))
  invisible(t)
}

cat("n =", n, "| 6 indicateurs binaires\n")
tm("Wilson global (6 indicateurs)", expert_prev_multi(d, vars))
tm("Wilson par strate (6 × 5 groupes)", purrr::map_dfr(seq_along(vars), function(i)
  expert_prev_by(d, vars[[i]], "region", names(vars)[i])))
tm("Design-based global (cluster+poids)", expert_prev_multi(d, vars, cluster = "cluster", weights = "poids"))
tm("Design-based par strate (6 × 5)", purrr::map_dfr(seq_along(vars), function(i)
  expert_prev_by(d, vars[[i]], "region", names(vars)[i], cluster = "cluster", weights = "poids")))
tm("Qualité (12 variables)", expert_quality(d, names(d)))
tm("Tests multiples (11 variables)", expert_multi_tests(d, "num", setdiff(names(d), "num")))

ds <- scale(d[, c("num", "ind3", "ind4")])
cl <- kmeans(ds, 3, nstart = 10)$cluster
tm("Stabilité bootstrap (n=5000, B=30)", expert_bootstrap_stability(ds, cl, "kmeans", 3, B = 30))

cat("\nBENCHMARK_EXPERT_OK\n")
