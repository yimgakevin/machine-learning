# Plan : Tests unitaires, Modules Shiny formels, Integration des 4 modules

## Contexte

Le server() actuel fait 5 600 lignes monolithiques, aucun test n'existe, et 4 modules (epidemio, predictif, survie, rapport) existent mais ne sont pas integres.

## Partie 1 : Tests unitaires (testthat)

Structure :
```
tests/
  testthat.R
  testthat/
    test-config_oms.R         -> classify_oms, color_oms, seuil_critique
    test-utils.R              -> detect_col, fonctions utilitaires
    test-agreger_geo.R        -> data.table + fallback dplyr
    test-epidemio.R           -> analyse_2x2, multi_exposition
    test-predictif.R          -> run_predictive_pipeline, metrics_table
    test-survie.R             -> run_km, run_cox
    test-qualite_smart.R      -> detect_smart_flags, calc_plausibility_score
    test-prevalence_smart.R   -> classify_prevalence_oms
    test-secu_alimentaire.R   -> calc_fcs, calc_rcsi, calc_hdds, calc_hhs
    test-rapport.R            -> generate_word_report
    test-carto_functions.R    -> pal_oms_leaflet, make_popup, agreger_geo
```

Approche : tester les fonctions pures (pas les reactifs Shiny), donnees synthetiques, gestion NA et cas limites.

## Partie 2 : Modules Shiny formels (moduleServer)

Chaque nouveau module suit la structure :
```r
xxxUI <- function(id) {
  ns <- NS(id)
  tagList(...)
}
xxxServer <- function(id, df_f, map_cols) {
  moduleServer(id, function(input, output, session) { ... })
}
```

Modules a creer :
1. mod_epidemio.R  (Nouveau - analyse 2x2, RR, OR, PAF)
2. mod_survie.R    (Nouveau - Kaplan-Meier, Cox, log-rank)
3. mod_predictif.R (Nouveau - ROC, calibration, k-fold)
4. mod_rapport.R   (Extraction du server inline + integration module_rapport.R)

Les modules existants (carto, qualite, etc.) restent inline - les convertir tous risquerait de casser l'app.

## Partie 3 : Integration dans app_v2_final.R

Pour chaque module :
1. source("mod_xxx.R") en scope global
2. menuItem dans le sidebar
3. tabItem avec xxxUI("xxx_id")
4. xxxServer("xxx_id", df_f, map_cols) dans server()

Ajouts sidebar :
- menuItem("Epidemiologie", tabName="epidemio", icon=icon("virus"))
- menuItem("Analyse de survie", tabName="survie", icon=icon("hourglass-half"))
- menuItem("Modele predictif", tabName="predictif", icon=icon("brain"))
- Le rapport (deja present) sera extrait en moduleServer

## Ordre d'execution

1. Creer tests/testthat/ avec fichiers de test
2. Creer mod_epidemio.R (UI + Server moduleServer)
3. Creer mod_survie.R
4. Creer mod_predictif.R
5. Creer mod_rapport.R (extraction du server inline)
6. Modifier app_v2_final.R (sources, menuItems, tabItems, server calls)
7. Verifier syntaxe et lancer

## Livrables

| Fichier | Action |
|---------|--------|
| tests/testthat.R | Nouveau |
| tests/testthat/test-*.R (11 fichiers) | Nouveau |
| mod_epidemio.R | Nouveau |
| mod_survie.R | Nouveau |
| mod_predictif.R | Nouveau |
| mod_rapport.R | Nouveau (extraction + amelioration) |
| app_v2_final.R | Modifie |
