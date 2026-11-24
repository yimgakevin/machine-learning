# tools/

## Vérifications opérationnelles

| Script | Rôle |
|---|---|
| `check_app.R` | Charge le setup, construit l'UI et définit `server()` sans lancer l'app. |
| `check_server.R` | Exécute `server()` dans une session mock `shiny::testServer`. |
| `check_expert_modules.R` | Exécute les onglets experts des 10 modules remontés sur un jeu synthétique. |
| `benchmark_expert.R` | Micro-benchmark des helpers experts (IC, design, qualité, tests, bootstrap). |
| `run_tests.R` | Lance la suite `tests/testthat`. |
| `launch_test.R` | Démarre l'app sur le port 8765, vérifie la réponse HTTP, puis l'arrête. |

Tous se lancent depuis la racine du projet :

```bash
Rscript tools/check_app.R
Rscript tools/check_server.R
Rscript tools/check_expert_modules.R
Rscript tools/benchmark_expert.R
Rscript tools/run_tests.R
Rscript tools/launch_test.R
```

## Historique du refactor

`refactor_2026-11/` contient les scripts one-shot qui ont découpé
`app_v2_final.R` (setup, UI, onglets, serveur). Ils référencent les numéros de
lignes de l'ancien monolithe et ne sont **pas** idempotents : ne pas les
relancer sur l'arborescence actuelle. Ils sont conservés pour traçabilité.
