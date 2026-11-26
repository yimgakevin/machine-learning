# Serveur découpé

Chaque fichier est sourcé dans `server()` avec `local = TRUE`.

- `server_core.R` — i18n, palette, authentification, réactifs de base, géo, chatbot, audit trail (265 lignes)
- `server_import.R` — import fichier, KoboToolbox / ODK, store racine `df_f` (180 lignes)
- `server_dashboard_shell.R` — statut sidebar, déclencheur des filtres, thème (53 lignes)
- `server_accueil.R` — pont racine vers le module Accueil : `event_data` plotly et `update*()` des filtres `f_*` (13 lignes)
- `server_exploration.R` — exploration et statistiques complètes (10 lignes)
- `server_univarie.R` — analyses univariées (12 lignes)
- `server_bivarie.R` — analyses bivariées (9 lignes)
- `server_descriptive.R` — descriptive OMS (9 lignes)
- `server_inferentiel.R` — inférentiel unifié (7 lignes)
- `server_qualite_smart.R` — contrôle qualité SMART / plausibility check (7 lignes)
- `server_prevalence.R` — prévalences SMART pondérées (7 lignes)
- `server_secu_alim.R` — sécurité alimentaire — calculs FCS/rCSI/HDDS/HHS (7 lignes)
- `server_rdim.R` — réduction de dimension — helpers et UI (10 lignes)
- `server_clustering.R` — clustering (8 lignes)
- `server_zscore_setup.R` — z-scores — réactif racine `zs_data` et appel du module (8 lignes)
- `server_sens.R` — modules SENS — mortalité, IYCF, anémie, paludisme, WASH, ménage, handicap (28 lignes)
- `server_oms.R` — indicateurs OMS / PAM (10 lignes)
- `server_modules.R` — cartographie v2 et modules Shiny formels (45 lignes)
- `server_dplyr.R` — manipulation dplyr (9 lignes)
- `server_modules_externes.R` — modules de recherche avancée et bases de données externes (93 lignes)
