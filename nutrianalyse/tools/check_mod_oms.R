# Vérifie le module OMS en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

# 1) Vérifie que l'UI du module utilise bien les IDs namespacés.
ui_html <- as.character(mod_oms_ui("oms"))
ids <- c(
  "oms-oms_groupes", "oms-oms_table", "oms-dl_oms_csv",
  "oms-oms_forest", "oms-oms_gauge_gam", "oms-oms_gauge_stunt",
  "oms-oms_zscore_dist"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE)) {
    cat("MOD_OMS_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_OMS_UI_OK |", length(ids), "IDs namespacés\n")

d <- data.frame(
  GAM = c(0, 1, 0, 1, 0, 1),
  SAM = c(0, 0, 0, 1, 0, 1),
  MAM = c(0, 1, 0, 0, 0, 1),
  Stunting = c(0, 1, 1, 0, 1, 0),
  Sev_Stunting = c(0, 0, 0, 1, 0, 0),
  Underweight = c(0, 1, 0, 1, 0, 1),
  MUAC_GAM = c(0, 1, 0, 0, 0, 1),
  MUAC_SAM = c(0, 0, 0, 0, 0, 1),
  region = c("A", "A", "B", "B", "A", "B"),
  sexe = c("M", "F", "M", "F", "M", "F"),
  age_group5 = c("0-5", "6-11", "0-5", "6-11", "0-5", "6-11"),
  depl = c("Non", "Oui", "Non", "Oui", "Non", "Oui"),
  fcs_cat = c("Pauvre", "Moyen", "Riche", "Pauvre", "Moyen", "Riche"),
  WHZ = c(-1.2, -2.5, 0.3, -3.1, -0.5, -2.2),
  HAZ = c(-1.0, -2.1, -0.2, -2.8, -0.7, -1.9),
  WAZ = c(-1.1, -2.2, -0.1, -2.9, -0.6, -2.0),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(
    mod_oms_server,
    args = list(
      df_f = shiny::reactive(d),
      map_cols = shiny::reactive(list(
        region = "region", sexe = "sexe", depl = "depl",
        whz = "WHZ", haz = "HAZ", waz = "WAZ"
      )),
      vars_cat = shiny::reactive("region"),
      has_oms = shiny::reactive(TRUE),
      filter_context = shiny::reactive(list(data = d)),
      gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis")
    ),
    {
      session$setInputs(oms_groupes = c(
        "Global", "Région", "Sexe", "Groupe d'âge", "Déplacement", "FCS"
      ))
      cat("oms_table       :", class(output$oms_table)[1], "\n")
      cat("oms_forest      :", class(output$oms_forest)[1], "\n")
      cat("oms_gauge_gam   :", class(output$oms_gauge_gam)[1], "\n")
      cat("oms_gauge_stunt :", class(output$oms_gauge_stunt)[1], "\n")
      cat("oms_zscore_dist :", class(output$oms_zscore_dist)[1], "\n")
    }
  )
  TRUE
}, error = function(e) {
  cat("MOD_OMS_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_OMS_OK\n") else quit(status = 1)
