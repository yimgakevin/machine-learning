# Vérifie les onglets experts ajoutés aux modules (IC, strates, qualité, exports).
# Source l'application une fois puis exécute chaque module en isolation.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg) > 0) dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1])))) else getwd()
setwd(root)

source("app_v2_final.R", local = FALSE)

set.seed(2026)
n <- 120
mk <- function() {
  data.frame(
    region = sample(c("Nord", "Sud", "Est"), n, TRUE),
    sexe = sample(c("M", "F"), n, TRUE),
    age = round(runif(n, 6, 59)),
    poids = round(runif(n, 7, 25), 1),
    taille = round(runif(n, 65, 120), 1),
    hemoglobine = round(rnorm(n, 10.5, 1.8), 1),
    altitude = sample(c(0, 500, 1500, 2500), n, TRUE),
    fumeur = sample(c("oui", "non"), n, TRUE),
    enceinte = sample(c("oui", "non"), n, TRUE),
    llin_possession = sample(c("oui", "non"), n, TRUE),
    llin_utilisation = sample(c("oui", "non"), n, TRUE),
    llin_acces = sample(c("oui", "non"), n, TRUE),
    nb_moustiquaires = sample(0:4, n, TRUE),
    taille_menage = sample(3:12, n, TRUE),
    tdr_malaria = sample(c("positif", "négatif"), n, TRUE, prob = c(.3, .7)),
    fievre = sample(c("oui", "non"), n, TRUE),
    water_source = sample(c("forage", "puits protégé", "rivière", "robinet"), n, TRUE),
    water_time = sample(c(5, 15, 30, 45, 60), n, TRUE),
    water_quantity = sample(c(5, 10, 15, 20, 30), n, TRUE),
    water_treatment = sample(c("aucun", "chlore", "ébullition"), n, TRUE),
    latrine_type = sample(c("latrine améliorée ventilée", "aucune", "toilette à fosse"), n, TRUE),
    open_defecation = sample(c("oui", "non"), n, TRUE),
    handwashing_facility = sample(c("oui", "non"), n, TRUE),
    soap_available = sample(c("oui", "non"), n, TRUE),
    handwash_soap = sample(c("oui", "non"), n, TRUE),
    chef_menage_sexe = sample(c("M", "F"), n, TRUE),
    chef_menage_age = sample(18:80, n, TRUE),
    revenus = round(runif(n, 0, 500), 0),
    depenses = round(runif(n, 50, 600), 0),
    depenses_alim = round(runif(n, 20, 400), 0),
    source_revenus = sample(c("salaire", "commerce", "aide"), n, TRUE),
    moyens_subsistance = sample(c("agriculture", "élevage", "commerce"), n, TRUE),
    type_abri = sample(c("tente", "maison", "abri de fortune"), n, TRUE),
    statut_propriete = sample(c("propriétaire", "locataire", "hébergé"), n, TRUE),
    enregistrement = sample(c("oui", "non"), n, TRUE),
    documentation = sample(c("oui", "non"), n, TRUE),
    incident_protection = sample(c("oui", "non"), n, TRUE),
    travail_enfant = sample(c("oui", "non"), n, TRUE),
    mariage_precoce = sample(c("oui", "non"), n, TRUE),
    nb_deces = sample(0:2, n, TRUE, prob = c(.8, .15, .05)),
    pop_debut = sample(3:10, n, TRUE),
    pop_fin = sample(3:10, n, TRUE),
    taille_menage_0_5 = sample(0:3, n, TRUE),
    age_decede = sample(c(0, 1, 3, 8, 20), n, TRUE),
    cause_deces = sample(c("paludisme", "diarrhée", "inconnue"), n, TRUE),
    sexe_decede = sample(c("M", "F"), n, TRUE),
    recall_period = rep(90, n),
    handicap_voir = sample(1:4, n, TRUE),
    handicap_entendre = sample(1:4, n, TRUE),
    handicap_marcher = sample(1:4, n, TRUE),
    handicap_memoire = sample(1:4, n, TRUE),
    handicap_soins_personnels = sample(1:4, n, TRUE),
    handicap_communication = sample(1:4, n, TRUE),
    GAM = sample(c(TRUE, FALSE), n, TRUE, prob = c(.1, .9)),
    SAM = sample(c(TRUE, FALSE), n, TRUE, prob = c(.03, .97)),
    Stunting = sample(c(TRUE, FALSE), n, TRUE, prob = c(.25, .75)),
    Underweight = sample(c(TRUE, FALSE), n, TRUE, prob = c(.15, .85)),
    MUAC_GAM = sample(c(TRUE, FALSE), n, TRUE, prob = c(.08, .92)),
    age_group5 = sample(c("6-23", "24-59"), n, TRUE),
    fcs_cat = sample(c("Pauvre", "Limite", "Acceptable"), n, TRUE),
    .whz = rnorm(n), .haz = rnorm(n), .waz = rnorm(n),
    stringsAsFactors = FALSE
  )
}
d <- mk()
fail <- character()

run_case <- function(label, expr) {
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) {
    fail <<- c(fail, paste0(label, " : ", conditionMessage(e))); FALSE
  })
  cat(if (ok) "OK   " else "FAIL ", label, "\n")
}

# ── Anémie ──────────────────────────────────────────────────────────────────
run_case("anemie", shiny::testServer(mod_anemie_server, args = list(df_f = shiny::reactive(d)), {
  session$setInputs(anem_hb = "hemoglobine", anem_alt = "altitude", anem_smoke = "fumeur",
                    anem_preg = "enceinte", anem_pop = "enfant_6_59",
                    anem_strat = "region", run_anem = 1)
  invisible(output$anem_expert_table); invisible(output$anem_expert_plot)
  invisible(output$anem_test_table); invisible(output$anem_quality_table)
}))

# ── Paludisme ───────────────────────────────────────────────────────────────
run_case("paludisme", shiny::testServer(mod_paludisme_server, args = list(df_f = shiny::reactive(d)), {
  session$setInputs(palu_poss = "llin_possession", palu_use = "llin_utilisation",
                    palu_access = "llin_acces", palu_nb_nets = "nb_moustiquaires",
                    palu_hh_size = "taille_menage", palu_tdr = "tdr_malaria",
                    palu_fever = "fievre", palu_strat = "region", run_palu = 1)
  invisible(output$palu_expert_table); invisible(output$palu_expert_plot)
  invisible(output$palu_test_table); invisible(output$palu_quality_table)
}))

# ── WASH ────────────────────────────────────────────────────────────────────
run_case("wash", shiny::testServer(mod_wash_server, args = list(df_f = shiny::reactive(d)), {
  session$setInputs(wash_source = "water_source", wash_time = "water_time",
                    wash_qty = "water_quantity", wash_treat = "water_treatment",
                    wash_latrine = "latrine_type", wash_oda = "open_defecation",
                    wash_hw = "handwashing_facility", wash_soap = "soap_available",
                    wash_hwsoap = "handwash_soap", wash_strat = "region", run_wash = 1)
  invisible(output$wash_expert_table); invisible(output$wash_expert_plot)
  invisible(output$wash_test_table); invisible(output$wash_quality_table)
}))

# ── Ménage ──────────────────────────────────────────────────────────────────
run_case("menage", shiny::testServer(mod_menage_server, args = list(df_f = shiny::reactive(d)), {
  session$setInputs(hh_size = "taille_menage", hh_head_sex = "chef_menage_sexe",
                    hh_head_age = "chef_menage_age", hh_income = "revenus",
                    hh_expenditure = "depenses", hh_food_exp = "depenses_alim",
                    hh_income_src = "source_revenus", hh_livelihood = "moyens_subsistance",
                    hh_shelter = "type_abri", hh_tenure = "statut_propriete",
                    hh_registration = "enregistrement", hh_documentation = "documentation",
                    hh_incident = "incident_protection", hh_child_labor = "travail_enfant",
                    hh_marriage = "mariage_precoce", hh_strat = "region", run_menage = 1)
  invisible(output$hh_expert_table); invisible(output$hh_expert_plot)
  invisible(output$hh_test_table); invisible(output$hh_quality_table)
}))

# ── Mortalité ───────────────────────────────────────────────────────────────
run_case("mortalite", shiny::testServer(mod_mortalite_server, args = list(df_f = shiny::reactive(d)), {
  session$setInputs(mort_deaths = "nb_deces", mort_pop_start = "pop_debut",
                    mort_pop_end = "pop_fin", mort_u5_pop = "taille_menage_0_5",
                    mort_age_dec = "age_decede", mort_cause = "cause_deces",
                    mort_sex_dec = "sexe_decede", mort_recall = "recall_period",
                    mort_strat = "region", run_mort = 1)
  invisible(output$mort_expert_table); invisible(output$mort_expert_plot)
  invisible(output$mort_test_table); invisible(output$mort_quality_table)
}))

# ── Handicap ────────────────────────────────────────────────────────────────
run_case("handicap", shiny::testServer(mod_handicap_server, args = list(df_f = shiny::reactive(d)), {
  session$setInputs(wg_voir = "handicap_voir", wg_entendre = "handicap_entendre",
                    wg_marcher = "handicap_marcher", wg_memoire = "handicap_memoire",
                    wg_soins = "handicap_soins_personnels", wg_comm = "handicap_communication",
                    wg_threshold = 3, wg_strat = "region", run_wg = 1)
  invisible(output$wg_expert_table); invisible(output$wg_expert_plot)
  invisible(output$wg_test_table); invisible(output$wg_quality_table)
}))

# ── Univarié ────────────────────────────────────────────────────────────────
run_case("uni", shiny::testServer(mod_uni_server,
  args = list(df_f = shiny::reactive(d),
              vars_cat = shiny::reactive(c("region", "sexe")),
              vars_cat_pie = shiny::reactive(c("region", "sexe")),
              vars_num = shiny::reactive(c("age", "poids", "taille")),
              gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis"),
              gprimary = function() "#0077B6", gsecondary = function() "#C0392B",
              global_palette = shiny::reactive("Auto (Global)")), {
  session$setInputs(uni_type = "histo", uni_var = "age", uni_pal = "Auto (Global)")
  invisible(output$uni_expert_tbl); invisible(output$uni_mod_tbl)
  invisible(output$uni_quality_tbl)
}))

# ── Bivarié ─────────────────────────────────────────────────────────────────
run_case("biv", shiny::testServer(mod_biv_server,
  args = list(df_f = shiny::reactive(d),
              vars_cat = shiny::reactive(c("region", "sexe")),
              vars_num = shiny::reactive(c("age", "poids", "taille")),
              gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis"),
              global_palette = shiny::reactive("Auto (Global)")), {
  session$setInputs(biv_type = "dodge", biv_x = "region", biv_y = "sexe")
  invisible(output$biv_effect_tbl); invisible(output$biv_multi_tbl)
}))

# ── Clustering ──────────────────────────────────────────────────────────────
run_case("clust", shiny::testServer(mod_clust_server,
  args = list(df_f = shiny::reactive(d),
              vars_num = shiny::reactive(c("age", "poids", "taille")),
              gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis"),
              gprimary = function() "#0077B6"), {
  session$setInputs(clust_vars = c("age", "poids", "taille"), clust_method = "kmeans",
                    clust_auto_k = TRUE, run_clust = 1)
  invisible(output$clust_compare_tbl); invisible(output$clust_quality_tbl)
  session$setInputs(run_stability = 1)
  invisible(output$clust_stab_tbl); invisible(output$clust_stab_plot)
}))

# ── OMS ─────────────────────────────────────────────────────────────────────
run_case("oms", shiny::testServer(mod_oms_server,
  args = list(df_f = shiny::reactive(d),
              map_cols = shiny::reactive(list(region = "region", sexe = "sexe", depl = NULL,
                                              whz = ".whz", haz = ".haz", waz = ".waz")),
              vars_cat = shiny::reactive(c("region", "sexe")),
              has_oms = shiny::reactive(TRUE),
              filter_context = shiny::reactive(list(data = d, n = nrow(d), n_total = nrow(d))),
              gpal = function(n, type = "cat") grDevices::hcl.colors(n, "viridis")), {
  session$setInputs(oms_groupes = c("Global", "Région"))
  invisible(output$oms_table); invisible(output$oms_design_note)
  invisible(output$oms_quality_tbl); invisible(output$oms_quality_flags)
}))

cat("\nTOTAL_CASES:", 10, "| FAILED:", length(fail), "\n")
if (length(fail) > 0) {
  cat(paste(fail, collapse = "\n"), "\n")
  quit(status = 1)
}
cat("CHECK_EXPERT_MODULES_OK\n")
