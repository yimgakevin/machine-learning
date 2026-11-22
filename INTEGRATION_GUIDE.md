# Guide d'intégration — NutriAnalyse Pro v2 + Modules avancés

## Structure finale du projet

```
mon_projet/
├── app_v2.R                ← Application principale (lancer avec runApp)
├── utils.R                 ← Fonctions de base (détection, stats, palettes)
├── inferentiel_v3.R        ← Module tests avec H₀/H₁ et tailles d'effet
├── module_epidemio.R       ← RR, OR, PAF, tableau 2×2, Cochran-Armitage
├── module_predictif.R      ← ROC comparée, calibration, validation croisée
├── module_survie.R         ← Kaplan-Meier, Log-rank, Cox, Schoenfeld
└── module_rapport.R        ← Rapport Word/PDF style OMS automatisé
```

## Installation des packages supplémentaires

```r
install.packages(c(
  # Épidémiologie
  "epitools", "epiR", "PropCIs",

  # Modélisation prédictive
  "pROC", "caret", "ROSE", "ResourceSelection",
  "randomForest",   # pour rf
  "xgboost",        # pour xgb

  # Analyse de survie
  "survival", "survminer", "broom",

  # Rapport Word
  "officer", "flextable",

  # Déjà dans app_v2.R
  "car"   # pour leveneTest
))
```

## Intégration dans app_v2.R

Ajoutez ces lignes au **début du fichier app_v2.R**, après `source("utils.R")` :

```r
source("inferentiel_v3.R")
source("module_epidemio.R")
source("module_predictif.R")
source("module_survie.R")
source("module_rapport.R")
```

---

## Ce que chaque module apporte

### module_epidemio.R — Analyses épidémiologiques

**Fonction principale : `analyse_2x2(exposed, outcome)`**

Calcule automatiquement pour un tableau 2×2 :
- Risque Relatif (RR) + IC 95%
- Odds Ratio (OR) + IC 95% (méthode exacte de Woolf)
- Fraction Attribuable dans la Population (PAF/FAP) + IC 95%
- Fraction Attribuable chez les Exposés (FAE)
- Risque Attribuable absolu (RAR = différence de risques)
- Nombre Nécessaire à Traiter (NNT)
- Test Chi² et Fisher

**Fonction `multi_exposition(d, expositions, outcome)`** : lance le tableau 2×2
sur plusieurs facteurs d'exposition simultanément et retourne un tableau synthèse.

**Fonction `cochran_armitage_test(d, var_ord, outcome)`** : teste la tendance
linéaire dose-réponse entre une variable ordinale et un outcome binaire.

**Comment l'utiliser dans le server() :**
```r
res_2x2 <- analyse_2x2(
  exposed = df_f()[["diarrhea_last_2w"]],
  outcome = df_f()[["GAM"]]
)
output$tableau_2x2 <- renderUI({ render_2x2_html(res_2x2) })

res_multi <- multi_exposition(df_f(),
  expositions = c("diarrhea_last_2w","fever_last_2w","vaccination_status"),
  outcome     = "GAM"
)
output$multi_exp_tbl <- renderDT({ datatable(res_multi) })
```

---

### module_predictif.R — Modélisation prédictive

**Fonction principale : `run_predictive_pipeline(d, outcome, predictors)`**

Pipeline complet en une seule fonction :
1. Partition stratifiée 70/30
2. Détection du déséquilibre → rééchantillonnage automatique si ratio > 2
3. Validation croisée k-fold (défaut: 5)
4. Entraînement Logistique + Random Forest + XGBoost en parallèle
5. Évaluation sur jeu de test indépendant

**Fonctions de visualisation :**
- `plot_roc_compare(pipeline_res)` → courbes ROC superposées avec AUC
- `plot_calibration(pipeline_res, model_name)` → courbe de calibration + Hosmer-Lemeshow
- `metrics_table(pipeline_res)` → tableau AUC/Sensibilité/Spécificité/F1/Kappa
- `plot_importance(pipeline_res)` → importance des variables Top-15

**Comment l'utiliser :**
```r
# Ceci prend 2-5 minutes selon la taille des données
pipeline <- run_predictive_pipeline(
  d          = df_f(),
  outcome    = "GAM",
  predictors = c("muac_mm","food_consumption_score",
                 "household_hunger_scale","child_age_months"),
  k_folds    = 5,
  models     = c("glm","rf")   # retirez "xgb" si trop lent
)
output$roc_plot     <- renderPlotly({ plot_roc_compare(pipeline) })
output$calib_plot   <- renderPlotly({ plot_calibration(pipeline) })
output$metrics_tbl  <- renderDT({ datatable(metrics_table(pipeline)) })
```

---

### module_survie.R — Analyse de survie

**Fonction `run_km(d, time_var, event_var, group_var)`** :
- Courbe de Kaplan-Meier (globale ou par groupe)
- Test Log-rank automatique si group_var fourni
- Hypothèses : H₀ = survies identiques dans tous les groupes

**Fonction `run_cox(d, time_var, event_var, covariates)`** :
- Modèle de Cox semi-paramétrique
- HR + IC 95% pour chaque covariable
- Test de Schoenfeld (proportionnalité des risques)
- C-index (concordance ≈ AUC)
- Tests globaux : LRT, Wald, Score

**Fonctions de visualisation :**
- `plot_km(km_res)` → courbe KM interactive avec IC 95%
- `plot_cox_forest(cox_res)` → forest plot des HR
- `plot_ph_test(cox_res)` → diagnostic de l'hypothèse PH

**Exemple d'utilisation :**
```r
# Si vous avez une variable temps (ex: durée de séjour, temps de récupération)
km_res <- run_km(df_f(),
  time_var  = "duree_hospitalisation",
  event_var = "guerison",
  group_var = "region"
)
output$km_plot <- renderPlotly({ plot_km(km_res) })

cox_res <- run_cox(df_f(),
  time_var   = "duree_hospitalisation",
  event_var  = "guerison",
  covariates = c("child_age_months","muac_mm","region","displacement_status")
)
output$cox_forest <- renderPlotly({ plot_cox_forest(cox_res) })
output$ph_diag    <- renderPlotly({ plot_ph_test(cox_res) })
```

---

### module_rapport.R — Rapport Word automatisé

**Fonction principale : `generate_word_report(d, map_cols_res, ...)`**

Génère un document Word (.docx) complet avec :
- Page de titre avec logo et informations
- Résumé exécutif avec interprétations rédigées automatiquement
- Tableau descriptif formaté style OMS (couleurs, police Calibri)
- Tableau des prévalences avec IC 95% et code couleur d'alerte
- Graphique des indicateurs intégré dans le document
- Conclusion avec recommandations basées sur les seuils OMS
- Références méthodologiques (OMS 2006, SMART, PAM)

**Sections disponibles :**
```r
sections = c("resume", "descriptif", "indicateurs", "inferentiel", "conclusion")
```

**Dans le downloadHandler de l'onglet Rapport :**
```r
output$dl_word <- downloadHandler(
  filename = function() paste0("rapport_", Sys.Date(), ".docx"),
  content  = function(file) {
    doc <- generate_word_report(
      d            = df_f(),
      map_cols_res = map_cols(),
      titre        = input$rpt_title,
      auteur       = input$rpt_auteur,
      organisation = "Enquête nutritionnelle SMART",
      zone         = input$rpt_zone,
      periode      = input$rpt_date,
      sections     = input$rpt_sections
    )
    print(doc, target = file)
  }
)
```

---

## Packages requis — liste complète

```r
install.packages(c(
  # Application principale
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "tidyverse", "scales", "forcats",
  "DT", "gtsummary", "gt", "flextable",
  "plotly", "ggiraph", "ggdist", "corrplot", "viridis",
  "RColorBrewer", "e1071",
  "leaflet", "leaflet.extras", "sf",
  "rnaturalearth", "rnaturalearthdata",
  "binom", "FactoMineR", "factoextra",
  "cluster", "NbClust", "dbscan",
  "readxl", "haven", "arrow", "writexl",
  "rmarkdown", "officer",

  # Modules avancés
  "epitools", "epiR", "PropCIs",        # épidémiologie
  "pROC", "caret", "ROSE",              # prédiction
  "ResourceSelection",                   # Hosmer-Lemeshow
  "randomForest", "xgboost",            # ML
  "survival", "survminer", "broom",     # survie
  "car"                                  # Levene test
))
```

## Lancement

```r
# Depuis RStudio, ouvrez app_v2.R
shiny::runApp("app_v2.R")
```

Tous les `source()` en tête du fichier chargeront automatiquement les modules.
