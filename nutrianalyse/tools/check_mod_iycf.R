# Vérifie le module IYCF en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_iycf_ui("iycf"))
ids <- c(
  "iycf-iycf_age_ui", "iycf-iycf_bfstatus_ui", "iycf-iycf_use_design",
  "iycf-iycf_ebf_ui", "iycf-iycf_mdd_ui", "iycf-run_iycf", "iycf-iycf_tabs",
  "iycf-iycf_verdict", "iycf-iycf_kpi", "iycf-iycf_gauge", "iycf-iycf_summary_table",
  "iycf-iycf_quality_summary", "iycf-iycf_child_table", "iycf-iycf_dl_xlsx",
  "iycf-iycf_export_info"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_IYCF_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_IYCF_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(17)
n <- 80
d <- data.frame(
  age_mois = sample(6:59, n, replace = TRUE),
  ebf = sample(c("oui", "non"), n, replace = TRUE),
  cbf = sample(c("oui", "non"), n, replace = TRUE),
  eibf = sample(c("oui", "non"), n, replace = TRUE),
  isf = sample(c("oui", "non"), n, replace = TRUE),
  mdd = sample(c("oui", "non"), n, replace = TRUE),
  mmf = sample(c("oui", "non"), n, replace = TRUE),
  mad = sample(c("oui", "non"), n, replace = TRUE),
  fer = sample(c("oui", "non"), n, replace = TRUE),
  vit_a = sample(c("oui", "non"), n, replace = TRUE),
  cpn4 = sample(c("oui", "non"), n, replace = TRUE),
  biberon = sample(c("oui", "non"), n, replace = TRUE),
  boisson_sucree = sample(c("oui", "non"), n, replace = TRUE),
  ultra_transforme = sample(c("oui", "non"), n, replace = TRUE),
  zero_legume_fruit = sample(c("oui", "non"), n, replace = TRUE),
  region = rep(c("Nord", "Sud", "Est"), length.out = n),
  child_sex = rep(c("M", "F"), length.out = n),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_iycf_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      iycf_age = "age_mois", iycf_ebf = "ebf", iycf_cbf = "cbf",
      iycf_eibf = "eibf", iycf_isf = "isf", iycf_mdd = "mdd", iycf_mmf = "mmf",
      iycf_mad = "mad", iycf_iron = "fer", iycf_vita = "vit_a", iycf_anc = "cpn4",
      iycf_bottle = "biberon", iycf_sweet = "boisson_sucree",
      iycf_upt = "ultra_transforme", iycf_zvf = "zero_legume_fruit",
      iycf_use_design = FALSE, iycf_stratvar = "region",
      iycf_equity_var = "region", run_iycf = 1
    )
    session$setInputs(iycf_focus = "ebf")
    cat("iycf_age_ui      :", class(output$iycf_age_ui)[1], "\n")
    cat("iycf_verdict     :", class(output$iycf_verdict)[1], "\n")
    cat("iycf_kpi         :", class(output$iycf_kpi)[1], "\n")
    cat("iycf_gauge       :", class(output$iycf_gauge)[1], "\n")
    cat("iycf_summary_tbl :", class(output$iycf_summary_table)[1], "\n")
    cat("iycf_strat_plot  :", class(output$iycf_strat_plot)[1], "\n")
    cat("iycf_equity_plot :", class(output$iycf_equity_plot)[1], "\n")
    cat("iycf_child_tbl   :", class(output$iycf_child_table)[1], "\n")
    cat("iycf_export_info :", class(output$iycf_export_info)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_IYCF_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_IYCF_OK\n") else quit(status = 1)
