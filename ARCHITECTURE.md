# Architecture — NutriAnalyse Pro v2

## Avant / après

| Élément | Avant | Après |
|---|---|---|
| `app_v2_final.R` | **15 754 lignes** | **61 lignes** (orchestrateur) |
| UI | 1 bloc `dashboardPage` de ~3 030 lignes | `app_parts/ui/` : coquille + 23 onglets |
| Serveur | 1 fonction de ~11 750 lignes | 32 fichiers dans `app_parts/server/` |
| Onglets UI inline | 23 | 0 (tous dans `app_parts/ui/tabs/`) |

Le code n'a **pas** été réécrit : chaque fichier contient une tranche exacte de
l'ancien monolithe. Le comportement est identique, ce qui a été vérifié par
parse, chargement complet, session serveur mock et démarrage HTTP réel.

## Arborescence

```
app.R                      # point d'entrée (inchangé)
app_v2_final.R             # orchestrateur : setup + ui + server + shinyApp
app_parts/
├── 00_setup.R             # options, packages, source() des modules métier
├── 01_css.R               # CSS global
├── ui/
│   ├── ui.R               # dashboardPage(ui_header(), ui_sidebar(), ui_body())
│   ├── ui_header.R
│   ├── ui_sidebar.R
│   ├── ui_body.R          # login, chatbot, tabItems()
│   ├── ui_tabs_index.R
│   └── tabs/              # 23 fichiers tab_<nom>.R
└── server/                # 32 fichiers server_<domaine>.R
    └── README.md          # description de chaque fichier
```

## Comment ça fonctionne

### UI
Chaque ancien `tabItem(...)` est devenu une fonction :

```r
# app_parts/ui/tabs/tab_accueil.R
ui_tab_accueil <- function() {
  tabItem(tabName = "accueil", ...)
}
```

`ui_body.R` appelle simplement :

```r
tabItems(
  ui_tab_accueil(),
  ui_tab_import(),
  ...
)
```

### Serveur
`app_v2_final.R` contient :

```r
server <- function(input, output, session) {
  source("app_parts/server/server_core.R", local = TRUE)
  source("app_parts/server/server_import.R", local = TRUE)
  ...
}
```

`local = TRUE` est essentiel : chaque fichier est évalué **dans** l'environnement
de `server()`. Tous partagent donc `input`, `output`, `session`, ainsi que les
réactifs locaux existants (`df_f`, `map_cols`, `lang`, `mapped_data`,
`audit_log_rv`, `filter_context`…). C'est ce qui permet de découper 527 `output$`
et 76 observers sans réécrire une seule reactive.

### Pourquoi pas un dossier `R/` ?
Shiny source automatiquement le dossier `R/` d'une application au démarrage.
Nos fichiers `app_parts/server/*.R` ont besoin du scope de `server()` et ne
doivent **jamais** être chargés globalement. Le nom `app_parts/` évite ce piège.

## Vérifications

```bash
Rscript tools/check_app.R      # setup + construction de l'UI + serveur défini
Rscript tools/check_server.R   # exécution de server() en session mock
Rscript tools/run_tests.R      # suite testthat (39 tests)
Rscript tools/launch_test.R    # démarrage HTTP réel (port 8765)
```

Résultats obtenus après le découpage :

- `PARSE OK` sur les 63 fichiers R ;
- `CHECK_OK` : 84 packages, UI construite, serveur défini ;
- `CHECK_SERVER_OK` : les 32 fichiers serveur se sourcent et s'enregistrent ;
- `HTTP_OK` : l'application répond sur `http://127.0.0.1:8765/` ;
- testthat : 0 échec (les 5 erreurs `golem-recommended` sont pré-existantes :
  elles attendent `app_ui`/`app_server`, absents de l'application actuelle).

## Conventions pour la suite

### Ajouter un onglet
1. Créer `app_parts/ui/tabs/tab_<nom>.R` avec `ui_tab_<nom> <- function() tabItem(...)`.
2. Ajouter `ui_tab_<nom>()` dans le `tabItems()` de `ui_body.R`.
3. Créer `app_parts/server/server_<nom>.R`.
4. Ajouter le `source(..., local = TRUE)` dans `app_v2_final.R`, au bon endroit
   dans l'ordre.

### Ajouter un vrai module Shiny
Le projet utilise déjà ce pattern (`mod_epidemio.R`, `mod_survie.R`,
`mod_predictif.R`, etc.) :

```r
mod_x_ui <- function(id) { ns <- NS(id); ... }
mod_x_server <- function(id, df_f, map_cols, lang) { moduleServer(id, function(input, output, session) { ... }) }
```

Le fichier est sourcé dans `00_setup.R`, l'UI est appelée dans `tabItems()`,
et le serveur dans `server_modules.R` ou `server_modules_externes.R`.

## Phase 2 — conversion en vrais modules Shiny

Le découpage actuel est **mécanique** : il réduit la taille des fichiers mais
conserve les IDs globaux. L'étape suivante consiste à convertir les onglets
inline en vrais modules `mod_<nom>_ui/server` avec `ns()`, **un onglet à la fois**.

Ordre recommandé (du plus isolé au plus imbriqué) :

1. `about`, `oms`, `uni`, `biv`, `descoms`
2. `infer`, `clust`, `qualite`, `prevalence`
3. `secu_alim`, `zscore`, `rdim`
4. `import`, `accueil` (les plus couplés aux réactifs globaux)

Pour chaque onglet :

1. Créer `mod_<nom>.R` avec `mod_<nom>_ui(id)` et
   `mod_<nom>_server(id, df_f, map_cols, lang, ...)`.
2. Remplacer `ui_tab_<nom>()` par `mod_<nom>_ui("<nom>")`.
3. Remplacer le contenu de `server_<nom>.R` par l'appel `mod_<nom>_server(...)`.
4. Préfixer tous les `input$` / `output$` avec `ns()` dans l'UI et le serveur.
5. Vérifier les sélecteurs JS/CSS qui ciblent des IDs (`#dashboard_filters`,
   `#ai_float_btn`, etc.).
6. Lancer les quatre vérifications ci-dessus, puis committer.

Ne pas convertir plusieurs onglets dans le même commit : un onglet = un commit,
pour pouvoir revenir en arrière précisément.
