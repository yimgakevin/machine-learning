# Vérifie le module descriptif en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_descoms_ui("descoms"))
ids <- c(
  "descoms-desc_by_ui", "descoms-desc_vars_ui", "descoms-desc_add_p",
  "descoms-dl_gt", "descoms-desc_gt_out", "descoms-desc_corr_plot",
  "descoms-desc_zscore_plot", "descoms-desc_expert_tabs",
  "descoms-desc_boot_var_ui", "descoms-run_desc_boot", "descoms-desc_boot_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_DESCOMS_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_DESCOMS_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(61)
n <- 50
d <- data.frame(
  WHZ = rnorm(n, -0.3, 1.1),
  HAZ = rnorm(n, -0.6, 1.2),
  WAZ = rnorm(n, -0.4, 1.0),
  age_mois = sample(6:59, n, replace = TRUE),
  sexe = rep(c("M", "F"), n / 2),
  region = rep(c("Nord", "Sud"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_descoms_server,
    args = list(
      df_f = shiny::reactive(d),
      vars_cat = shiny::reactive(c("sexe", "region")),
      vars_num = shiny::reactive(c("WHZ", "HAZ", "WAZ", "age_mois")),
      map_cols = shiny::reactive(list(whz = "WHZ", haz = "HAZ", waz = "WAZ")),
      gpal = function(n, type = "cat") grDevices::rainbow(n)
    ),
    {
      session$setInputs(
        desc_by = "none",
        desc_vars = c("WHZ", "HAZ", "age_mois", "sexe"),
        desc_add_p = FALSE,
        desc_grp = "sexe",
        desc_corr_vars = c("WHZ", "HAZ", "age_mois"),
        desc_boot_var = "WHZ",
        desc_boot_B = 500,
        desc_boot_conf = 0.95,
        run_desc_boot = 1
      )
      cat("desc_gt_out      :", class(output$desc_gt_out)[1], "\n")
      cat("desc_corr_plot   :", class(output$desc_corr_plot)[1], "\n")
      cat("desc_zscore_plot :", class(output$desc_zscore_plot)[1], "\n")
      cat("desc_boot_table  :", class(output$desc_boot_table)[1], "\n")
      cat("desc_boot_compar :", class(output$desc_boot_comparison)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_DESCOMS_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_DESCOMS_OK\n") else quit(status = 1)
