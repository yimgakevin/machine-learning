# NutriAnalyse Pro

Application Shiny d'analyse nutritionnelle et épidémiologique (enquêtes SMART,
standards OMS/UNICEF) : import et contrôle qualité des données, indicateurs
nutritionnels, cartographie, rapports et agents IA.

## Démarrage rapide

```bash
Rscript tools/run_app.R
```

Ou ouvrir `app.R` dans RStudio.

### Configuration locale

1. Copier le modèle :

   ```bash
   cp .Renviron.example .Renviron
   ```

2. Renseigner éventuellement `GEMINI_API_KEY` dans `.Renviron`.
   Sans clé, les agents IA sont désactivés mais l'application reste utilisable.

3. Ne jamais committer `.Renviron` ni `config/users.rds`.

## Documentation

- [README_v2.md](README_v2.md) — guide complet de l'application
- [ARCHITECTURE.md](ARCHITECTURE.md) — architecture, conventions, vérifications
- [SECURITY_GUIDE.md](SECURITY_GUIDE.md) — sécurité, comptes, clés API
- [INSTRUCTIONS_LANCEMENT.md](INSTRUCTIONS_LANCEMENT.md) — lancement détaillé

## Vérifications

```bash
Rscript tools/run_tests.R
Rscript tools/check_app.R
Rscript tools/check_server.R
Rscript tools/check_expert_modules.R
```

## Historique du dépôt

Ce dépôt contient également l'ancien portfolio Machine Learning, conservé dans
[README_MACHINE_LEARNING.md](README_MACHINE_LEARNING.md). Le dossier `data/` de
ce portfolio a été renommé `ml_data/` pour éviter un conflit de casse avec le
dossier `DATA/` de l'application.

Les dossiers de l'ancienne application NutriAnalyse (`shiny_application/`,
`NutritionalAnalysis/`) ont été retirés de la branche `main` ; leur historique
reste consultable dans les commits antérieurs.
