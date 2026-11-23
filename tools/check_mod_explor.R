# Vérifie le module exploration en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_explor_ui("explor"))
ids <- c(
  "explor-explor_cols_ui", "explor-explor_n",
  "explor-dl_csv", "explor-dl_xlsx", "explor-explor_table",
  "explor-explor_var", "explor-explor_stats", "explor-explor_auto_plot"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_EXPLOR_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_EXPLOR_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(31)
n <- 50
d <- data.frame(
  WHZ = rnorm(n, -0.3, 1.1),
  age_mois = sample(6:59, n, replace = TRUE),
  poids = round(rnorm(n, 12, 3), 1),
  sexe = rep(c("M", "F"), n / 2),
  region = rep(c("Nord", "Sud"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_explor_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("sexe", "region")),
      vars_num = shiny::reactive(c("WHZ", "age_mois", "poids")),
      gpal = function(n, type = "cat") grDevices::rainbow(n),
      gprimary = function() "#0077B6",
      gsecondary = function() "#D98B14"
    ),
    {
      session$setInputs(
        explor_cols = c("WHZ", "sexe", "region"),
        explor_n = 15,
        explor_var = "WHZ"
      )
      cat("explor_cols_ui :", class(output$explor_cols_ui)[1], "\n")
      cat("explor_table   :", class(output$explor_table)[1], "\n")
      cat("explor_stats   :", class(output$explor_stats)[1], "\n")
      cat("explor_auto_plot:", class(output$explor_auto_plot)[1], "\n")

      session$setInputs(explor_var = "sexe")
      cat("stats (facteur) :", class(output$explor_stats)[1], "\n")
      cat("plot (facteur)  :", class(output$explor_auto_plot)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_EXPLOR_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_EXPLOR_OK\n") else quit(status = 1)
