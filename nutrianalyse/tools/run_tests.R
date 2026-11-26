# Exécute la suite testthat existante.
suppressPackageStartupMessages(library(testthat))
res <- test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat("\nTOTAL:", nrow(df), "| FAILED:", sum(df$failed), "| WARN:", sum(df$warning), "| SKIP:", sum(df$skipped), "\n")
if (any(df$failed > 0)) quit(status = 1)
