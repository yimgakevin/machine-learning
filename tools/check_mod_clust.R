# Vérifie le module clustering en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_clust_ui("clust"))
ids <- c(
  "clust-clust_method", "clust-clust_auto_k", "clust-run_clust",
  "clust-clust_summary", "clust-clust_plot2d", "clust-clust_optimal",
  "clust-clust_dendro", "clust-clust_profile", "clust-clust_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_CLUST_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_CLUST_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(42)
d <- data.frame(
  x = rnorm(24),
  y = rnorm(24),
  z = rnorm(24),
  groupe = rep(c("A", "B", "C"), 8),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_clust_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_num = shiny::reactive(c("x", "y", "z")),
      gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis"),
      gprimary = function() "#0077B6"
    ),
    {
      session$setInputs(
        clust_method = "kmeans",
        clust_auto_k = TRUE,
        clust_vars = c("x", "y", "z"),
        run_clust = 1
      )
      cat("clust_vars_ui :", class(output$clust_vars_ui)[1], "\n")
      cat("clust_plot2d  :", class(output$clust_plot2d)[1], "\n")
      cat("clust_optimal :", class(output$clust_optimal)[1], "\n")
      cat("clust_profile :", class(output$clust_profile)[1], "\n")
      cat("clust_table   :", class(output$clust_table)[1], "\n")
      cat("clust_summary :", class(output$clust_summary)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_CLUST_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_CLUST_OK\n") else quit(status = 1)
