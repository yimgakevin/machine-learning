# app_parts/ — code découpé de NutriAnalyse Pro v2

Le point d'entrée reste `app.R` → `app_v2_final.R` (orchestrateur de ~60 lignes).
Tout le code applicatif est ici.

```
app_parts/
├── 00_setup.R          # options, packages, source() des modules métier, moteurs
├── 01_css.R            # feuille de style globale (objet CSS)
├── ui/
│   ├── ui.R            # assemblage : dashboardPage(ui_header(), ui_sidebar(), ui_body())
│   ├── ui_header.R     # dashboardHeader()
│   ├── ui_sidebar.R    # dashboardSidebar()
│   ├── ui_body.R       # dashboardBody() + login + chatbot + tabItems()
│   ├── ui_tabs_index.R # source tous les onglets de tabs/
│   └── tabs/
│       └── tab_<nom>.R # ui_tab_<nom>() <- function() tabItem(...)
└── server/
    ├── README.md       # liste et rôle des 32 fichiers
    └── server_<domaine>.R
```

## Règles importantes

1. **Ne pas créer de dossier `R/`** à la racine : Shiny le source automatiquement,
   ce qui provoquerait des doubles chargements.
2. Les fichiers `app_parts/server/*.R` sont sourcés **dans** `server()` avec
   `local = TRUE`. Ils partagent donc `input`, `output`, `session` et tous les
   réactifs locaux (`df_f`, `map_cols`, `lang`, `mapped_data`, `audit_log_rv`…).
   Ne jamais les sourcer au niveau global.
3. Les fichiers `app_parts/ui/tabs/*.R` définissent des fonctions
   `ui_tab_<nom>()` ; elles sont appelées dans `ui_body.R`.
4. L'ordre des `source()` du serveur est l'ordre d'origine du monolithe.
   Le préserver évite de déplacer des `moduleServer()` ou des `observeEvent()`
   qui dépendent de réactifs définis plus haut.

## Vérifications

Depuis la racine du projet :

```bash
Rscript tools/check_app.R      # setup + UI + définition du serveur
Rscript tools/check_server.R   # exécution du serveur en session mock
Rscript tools/run_tests.R      # suite testthat
Rscript tools/launch_test.R    # démarrage HTTP réel sur le port 8765
```

Voir `../ARCHITECTURE.md` pour la stratégie de conversion en vrais modules Shiny.
