# Vérifie le module WASH en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_wash_ui("wash"))
ids <- c(
  "wash-wash_source_ui", "wash-wash_time_ui", "wash-wash_qty_ui", "wash-wash_treat_ui",
  "wash-wash_latrine_ui", "wash-wash_shared_ui", "wash-wash_oda_ui", "wash-wash_hw_ui",
  "wash-wash_soap_ui", "wash-wash_hwsoap_ui", "wash-run_wash", "wash-wash_tabs",
  "wash-wash_kpi", "wash-wash_ladder_plot", "wash-wash_sources_table", "wash-wash_treatment_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_WASH_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_WASH_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(13)
n <- 40
d <- data.frame(
  source_eau = sample(c("Robinet", "Puits", "Rivière"), n, replace = TRUE),
  temps_collecte_eau = sample(5:60, n, replace = TRUE),
  quantite_eau = sample(8:25, n, replace = TRUE),
  traitement_eau = sample(c("Chloration", "Aucun"), n, replace = TRUE),
  type_latrine = sample(c("Latrine améliorée", "Latrine traditionnelle"), n, replace = TRUE),
  latrine_partagee = sample(c("oui", "non"), n, replace = TRUE),
  defecation_air_libre = sample(c("oui", "non"), n, replace = TRUE),
  lavage_mains = sample(c("oui", "non"), n, replace = TRUE),
  savon_disponible = sample(c("oui", "non"), n, replace = TRUE),
  eau_savon_lavage = sample(c("oui", "non"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_wash_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      wash_source = "source_eau", wash_time = "temps_collecte_eau", wash_qty = "quantite_eau",
      wash_treat = "traitement_eau", wash_latrine = "type_latrine", wash_shared = "latrine_partagee",
      wash_oda = "defecation_air_libre", wash_hw = "lavage_mains", wash_soap = "savon_disponible",
      wash_hwsoap = "eau_savon_lavage", run_wash = 1
    )
    cat("wash_source_ui   :", class(output$wash_source_ui)[1], "\n")
    cat("wash_kpi         :", class(output$wash_kpi)[1], "\n")
    cat("wash_ladder      :", class(output$wash_ladder_plot)[1], "\n")
    cat("wash_sources_tbl :", class(output$wash_sources_table)[1], "\n")
    cat("wash_treat_tbl   :", class(output$wash_treatment_table)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_WASH_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_WASH_OK\n") else quit(status = 1)
