# Architecture — NutriAnalyse Pro v2

## Avant / après

| Élément | Avant | Après |
|---|---|---|
| `app_v2_final.R` | **15 754 lignes** | **54 lignes** (orchestrateur) |
| UI | 1 bloc `dashboardPage` de ~3 030 lignes | `app_parts/ui/` : coquille + 23 modules `mod_*.R` |
| Serveur | 1 fonction de ~11 750 lignes | 20 fichiers dans `app_parts/server/` + modules |
| Onglets UI inline | 23 | 0 (tous convertis en modules namespacés) |

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
│   └── ui_body.R          # login, chatbot, tabItems(mod_*_ui("<nom>"))
└── server/                # 20 fichiers server_<domaine>.R
    └── README.md          # description de chaque fichier
mod_*.R                   # 23 modules Shiny (UI + serveur namespacés)
```

Quelques ponts racine restent volontairement dans `app_parts/server/` :
`server_import.R` (store `raw_data`/`mapped_data`/`df_f`), `server_accueil.R`
(`event_data` plotly + `update*()` des filtres `f_*`) et
`server_zscore_setup.R` (réactif `zs_data` partagé).

## Comment ça fonctionne

### UI
Chaque ancien `tabItem(...)` est devenu un module namespacé :

```r
# mod_accueil.R
mod_accueil_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(tabName = "accueil", ...)   # tabName reste global (lien du menu)
}
```

`ui_body.R` appelle simplement :

```r
tabItems(
  mod_accueil_ui("accueil"),
  mod_import_ui("import"),
  ...
)
```

### Serveur
`app_v2_final.R` contient :

```r
server <- function(input, output, session) {
  source("app_parts/server/server_core.R", local = TRUE)     # réactifs racine
  source("app_parts/server/server_import.R", local = TRUE)   # store + df_f
  ...
  source("app_parts/server/server_zscore_setup.R", local = TRUE)  # zs_data + module
}
```

`local = TRUE` est essentiel : chaque fichier est évalué **dans** l'environnement
de `server()`. Les réactifs racine (`df_f`, `map_cols`, `lang`, `mapped_data`,
`audit_log_rv`, `filter_context`, `zs_data`…) sont ensuite passés explicitement
aux modules (`mod_<nom>_server(id, ...)`). C'est ce qui permet de découper 527
`output$` et 76 observers sans réécrire une seule reactive.

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

- `PARSE OK` sur l'ensemble des fichiers R de l'application ;
- `CHECK_OK` : 84 packages, UI construite, serveur défini ;
- `CHECK_SERVER_OK` : les 20 fichiers serveur se sourcent et s'enregistrent ;
- `HTTP_OK` : l'application répond sur `http://127.0.0.1:8765/` ;
- testthat : 0 échec (les 5 erreurs `golem-recommended` sont pré-existantes :
  elles attendent `app_ui`/`app_server`, absents de l'application actuelle).

## Conventions pour la suite

### Ajouter un onglet / un module
1. Créer `mod_<nom>.R` avec `mod_<nom>_ui(id)` et `mod_<nom>_server(id, ...)`.
2. Ajouter `source("mod_<nom>.R")` dans `00_setup.R`.
3. Ajouter `mod_<nom>_ui("<nom>")` dans le `tabItems()` de `ui_body.R`.
4. Ajouter l'appel `mod_<nom>_server("<nom>", ...)` dans un fichier
   `app_parts/server/server_<domaine>.R` sourcé par l'orchestrateur, au bon
   endroit dans l'ordre.

```r
mod_x_ui <- function(id) { ns <- NS(id); ... }
mod_x_server <- function(id, df_f, map_cols, lang) {
  moduleServer(id, function(input, output, session) { ... })
}
```

## Phase 2 — conversion en vrais modules Shiny

Le découpage mécanique et la conversion en modules namespacés sont terminés.
État :

| Onglet | Statut |
|---|---|
| `oms` | ✅ converti (`mod_oms.R`) — sert de modèle |
| `uni` | ✅ converti (`mod_uni.R`) |
| `biv` | ✅ converti (`mod_biv.R`) |
| `clust` | ✅ converti (`mod_clust.R`) |
| `qualite` | ✅ converti (`mod_qualite.R`) |
| `prevalence` | ✅ converti (`mod_prevalence.R`) |
| `secu_alim` | ✅ converti (`mod_secu_alim.R`) |
| `explor` | ✅ converti (`mod_explor.R`) |
| `dplyr` | ✅ converti (`mod_dplyr.R`) |
| `about` | ✅ converti (`mod_about.R`) — UI statique |
| `descoms` | ✅ converti (`mod_descoms.R`) |
| `infer` | ✅ converti (`mod_infer.R`) |
| `rdim` | ✅ converti (`mod_rdim.R`) |
| `mortalite` | ✅ converti (`mod_mortalite.R`, helper `mod_sens_helpers.R`) |
| `paludisme` | ✅ converti (`mod_paludisme.R`) |
| `wash` | ✅ converti (`mod_wash.R`) |
| `handicap` | ✅ converti (`mod_handicap.R`) |
| `anemie` | ✅ converti (`mod_anemie.R`) |
| `menage` | ✅ converti (`mod_menage.R`) |
| `iycf` | ✅ converti (`mod_iycf.R`) — bloc SENS 71-424 |
| `import` | ✅ converti (`mod_import.R`) — store racine conservé dans `server_import.R` |
| `accueil` | ✅ converti (`mod_accueil.R`) — filtres `f_*` globaux, pont dans `server_accueil.R` |
| `zscore` | ✅ converti (`mod_zscore.R`) — `zs_data` racine partagé, pont dans `server_zscore_setup.R` |

Les 23 onglets sont convertis. Aucun fichier `app_parts/ui/tabs/` ne subsiste.

### Pattern validé par le pilote `oms`

```r
# mod_<nom>.R — UI
mod_<nom>_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(
    tabName = "<nom>",                       # PAS namespacé (lien du menu)
    checkboxGroupInput(ns("groupe"), ...),   # IDs namespacés
    DTOutput(ns("table")),
    ...
  )
}

# mod_<nom>.R — serveur
mod_<nom>_server <- function(id, df_f, map_cols, lang, ...) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({ ... })        # input$/output$ inchangés :
  })                                         # moduleServer les namespacé
}
```

Points d'attention :

1. **UI uniquement** : les IDs sont préfixés avec `ns()`. Côté serveur, on garde
   `input$` / `output$` tels quels — `moduleServer` applique le namespace.
2. **Réactifs partagés** : les passer en arguments (`df_f`, `map_cols`, `lang`,
   `vars_cat`, `has_oms`, `filter_context`…).
3. **Helpers locaux au serveur** : `gpal()` dépend de `input$global_palette` et
   n'est pas global. Il doit être passé en argument, comme dans `mod_oms.R`.
4. **IDs JS/CSS** : vérifier les sélecteurs qui ciblent des IDs globaux.
5. **Un onglet = un commit**, avec les vérifications ci-dessus.

### Procédure

1. Créer `mod_<nom>.R` avec `mod_<nom>_ui(id)` et `mod_<nom>_server(id, ...)`.
2. Ajouter `source("mod_<nom>.R")` dans `00_setup.R`.
3. Ajouter `mod_<nom>_ui("<nom>")` dans `ui_body.R`.
4. Créer ou adapter `app_parts/server/server_<domaine>.R` pour appeler
   `mod_<nom>_server(...)` avec les réactifs racine nécessaires.
5. Lancer `check_app`, `check_server`, `launch_test`, `run_tests`, puis un test
   d'isolation du module comme `tools/check_mod_oms.R`.
