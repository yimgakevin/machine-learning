# Vérifie le module Ménage & Protection en isolation avec des données synthétiques.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

html <- as.character(mod_menage_ui("menage"))
ids <- c(
  "menage-hh_size_ui", "menage-hh_u5_ui", "menage-hh_5_17_ui", "menage-hh_adults_ui",
  "menage-hh_head_sex_ui", "menage-hh_head_age_ui", "menage-hh_education_ui",
  "menage-hh_income_ui", "menage-hh_income_src_ui", "menage-hh_expenditure_ui",
  "menage-hh_food_exp_ui", "menage-hh_livelihood_ui", "menage-hh_shelter_ui",
  "menage-hh_tenure_ui", "menage-hh_registration_ui", "menage-hh_documentation_ui",
  "menage-hh_incident_ui", "menage-hh_child_labor_ui", "menage-hh_marriage_ui",
  "menage-run_menage", "menage-menage_tabs", "menage-hh_kpi", "menage-hh_composition_table",
  "menage-hh_income_plot", "menage-hh_livelihood_table", "menage-hh_shelter_table",
  "menage-hh_tenure_table", "menage-hh_protection_plot", "menage-hh_protection_table"
)
for (id in ids) {
  if (!grepl(paste0('id="', id, '"'), html, fixed = TRUE)) {
    cat("MOD_MENAGE_UI_FAIL: ID manquant", id, "\n")
    quit(status = 1)
  }
}
cat("MOD_MENAGE_UI_OK |", length(ids), "IDs namespacés\n")

set.seed(16)
n <- 40
d <- data.frame(
  taille_menage = sample(1:8, n, replace = TRUE),
  taille_menage_0_5 = sample(0:3, n, replace = TRUE),
  taille_menage_5_17 = sample(0:4, n, replace = TRUE),
  taille_menage_adultes = sample(1:5, n, replace = TRUE),
  chef_menage_sexe = sample(c("M", "F"), n, replace = TRUE),
  chef_menage_age = sample(20:75, n, replace = TRUE),
  education_cm = sample(c("Primaire", "Secondaire", "Supérieur"), n, replace = TRUE),
 revenus = sample(0:500, n, replace = TRUE),
  source_revenus = sample(c("Commerce", "Salariat", "Agriculture"), n, replace = TRUE),
  depenses = sample(50:400, n, replace = TRUE),
  depenses_alim = sample(20:200, n, replace = TRUE),
  moyens_subsistance = sample(c("Commerce", "Salariat", "Agriculture"), n, replace = TRUE),
  type_abri = sample(c("Maison", "Tente", "Abri précaire"), n, replace = TRUE),
  statut_propriete = sample(c("Propriétaire", "Locataire"), n, replace = TRUE),
  enregistrement = sample(c("oui", "non"), n, replace = TRUE),
  documentation = sample(c("oui", "non"), n, replace = TRUE),
  incident_protection = sample(c("oui", "non"), n, replace = TRUE),
  travail_enfant = sample(c("oui", "non"), n, replace = TRUE),
  mariage_precoce = sample(c("oui", "non"), n, replace = TRUE),
  stringsAsFactors = FALSE
)

ok <- tryCatch({
  shiny::testServer(mod_menage_server, args = list(df_f = shiny::reactive(d)), {
    session$setInputs(
      hh_size = "taille_menage", hh_u5 = "taille_menage_0_5", hh_5_17 = "taille_menage_5_17",
      hh_adults = "taille_menage_adultes", hh_head_sex = "chef_menage_sexe",
      hh_head_age = "chef_menage_age", hh_education = "education_cm",
      hh_income = "revenus", hh_income_src = "source_revenus",
      hh_expenditure = "depenses", hh_food_exp = "depenses_alim",
      hh_livelihood = "moyens_subsistance", hh_shelter = "type_abri",
      hh_tenure = "statut_propriete", hh_registration = "enregistrement",
      hh_documentation = "documentation", hh_incident = "incident_protection",
      hh_child_labor = "travail_enfant", hh_marriage = "mariage_precoce", run_menage = 1
    )
    cat("hh_size_ui      :", class(output$hh_size_ui)[1], "\n")
    cat("hh_kpi          :", class(output$hh_kpi)[1], "\n")
    cat("hh_composition  :", class(output$hh_composition_table)[1], "\n")
    cat("hh_income_plot  :", class(output$hh_income_plot)[1], "\n")
    cat("hh_livelihood   :", class(output$hh_livelihood_table)[1], "\n")
    cat("hh_shelter_tbl  :", class(output$hh_shelter_table)[1], "\n")
    cat("hh_tenure_tbl   :", class(output$hh_tenure_table)[1], "\n")
    cat("hh_protection   :", class(output$hh_protection_plot)[1], "\n")
    cat("hh_prot_tbl     :", class(output$hh_protection_table)[1], "\n")
  })
  TRUE
}, error = function(e) {
  cat("MOD_MENAGE_FAIL:", conditionMessage(e), "\n")
  FALSE
})

if (ok) cat("CHECK_MOD_MENAGE_OK\n") else quit(status = 1)
