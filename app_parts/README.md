# app_parts/ — code découpé de NutriAnalyse Pro v2

Le point d'entrée reste `app.R` → `app_v2_final.R` (orchestrateur de 49 lignes).
Tout le code applicatif est ici.

```
app_parts/
├── 00_setup.R          # options, packages, source() des modules métier, moteurs
├── 01_css.R            # feuille de style globale (objet CSS)
├── ui/
│   ├── ui.R            # assemblage : dashboardPage(ui_header(), ui_sidebar(), ui_body())
│   ├── ui_header.R     # dashboardHeader()
│   ├── ui_sidebar.R    # dashboardSidebar()
│   └── ui_body.R       # dashboardBody() + login + chatbot + tabItems()
└── server/
    ├── README.md       # liste et rôle des 20 fichiers
    └── server_<domaine>.R

mod_<nom>.R             # 23 modules Shiny (UI + serveur namespacés)
mod_expert_helpers.R    # socle expert : IC, design, qualité, effets, exports
mod_sens_helpers.R      # sélecteurs dynamiques namespacés des modules SENS
```

## Niveau expert

Les modules SENS (`anemie`, `paludisme`, `wash`, `menage`, `mortalite`,
`handicap`) ainsi que `uni`, `biv`, `clust` et `oms` disposent d'onglets
experts : IC 95 % Wilson ou design-based (cluster / strates / poids), DEFF,
stratification, contrôles qualité et exports CSV/XLSX. Le socle commun est
`mod_expert_helpers.R`, chargé dans `app_parts/00_setup.R` avant les modules.

## Règles importantes

1. **Ne pas créer de dossier `R/`** à la racine : Shiny le source automatiquement,
   ce qui provoquerait des doubles chargements.
2. Les fichiers `app_parts/server/*.R` sont sourcés **dans** `server()` avec
   `local = TRUE`. Ils partagent donc `input`, `output`, `session` et tous les
   réactifs locaux (`df_f`, `map_cols`, `lang`, `mapped_data`, `audit_log_rv`…).
   Ne jamais les sourcer au niveau global.
3. Les onglets sont des modules `mod_<nom>.R` : `mod_<nom>_ui("<nom>")` est
   appelée dans `ui_body.R` et `mod_<nom>_server(...)` dans un fichier serveur
   (`server_<domaine>.R` ou un pont dédié).
4. L'ordre des `source()` du serveur est l'ordre d'origine du monolithe.
   Le préserver évite de déplacer des `moduleServer()` ou des `observeEvent()`
   qui dépendent de réactifs définis plus haut.

## Vérifications

Depuis la racine du projet :

```bash
Rscript tools/check_app.R      # setup + UI + définition du serveur
Rscript tools/check_server.R   # exécution du serveur en session mock
Rscript tools/check_expert_modules.R  # onglets experts des 10 modules remontés
Rscript tools/run_tests.R      # suite testthat
Rscript tools/launch_test.R    # démarrage HTTP réel sur le port 8765
```

Voir `../ARCHITECTURE.md` pour la stratégie de conversion en vrais modules Shiny.
