# Plan : Vérification et correction de tous les modules avec le dataset test

## Contexte
Le dataset `DATA/nutrianalyse_test_dataset.csv` (1200 lignes × 75 colonnes) est prêt. Il faut tester chaque module en simulant le pipeline de données de l'app (`auto_detect → calc_indicateurs → df_f`) puis en appelant les fonctions clés de chaque module pour vérifier qu'elles fonctionnent sans erreur et produisent des résultats cohérents.

**Modules exclus** : les modules de connexion externe (SQL, DHIS2, ODK, REDCap, KoboToolbox) ne sont pas testables avec un CSV.

---

## Étapes

### Phase 1 — Préparer l'environnement de test
- Sourcer tous les fichiers nécessaires (`utils.R`, `config_oms.R`, modules)
- Charger le dataset, exécuter `auto_detect()` + `calc_indicateurs()` pour obtenir `df_test` (équivalent de `df_f()`)
- Préparer `map_cols` (équivalent de `map_cols()`)

### Phase 2 — Modules nutritionnels SMART (cœur de l'app)
1. **module_zscore_oms.R** : tester `calc_zscore_wfl`, `calc_zscore_hfa`, `calc_zscore_wfa` avec poids/taille/âge/sexe du dataset
2. **module_qualite_smart.R** : tester le contrôle qualité (flags, digit preference, plausibilité)
3. **module_prevalence_smart.R** : tester le calcul de prévalences SMART avec IC et DEFF
4. **module_secu_alimentaire.R** : tester calcul FCS, rCSI, HHS, HDDS + classifications

### Phase 3 — Analyses statistiques (serveur principal app_v2_final.R)
5. **Descriptif** : tester les outputs descriptifs (tableau résumé, distributions)
6. **Inférentiel** : tester les tests bivariés (t-test, chi², corrélation)
7. **Réduction de dimension** : tester PCA/MCA avec les variables numériques
8. **Clustering** : tester k-means/CAH

### Phase 4 — Modules expert (module_*_expert.R + server_*_expert.R)
9. **module_desc_expert.R** : tester bootstrap IC, stats robustes, batterie normalité, outliers, distributions
10. **module_infer_expert.R** : tester tests appariés, puissance, Bayes Factor, TOST, régression diagnostics
11. **module_temporel_expert.R** : tester comparaison continues, DID, tendances, CUSUM (par survey_round)

### Phase 5 — Modules formels (mod_*.R avec moduleServer)
12. **mod_epidemio.R** : tester table 2×2, RR/OR, Cochran-Armitage
13. **mod_survie.R** : tester Kaplan-Meier, Cox (en créant time/event si besoin)
14. **mod_predictif.R** : tester pipeline ML (GLM + RandomForest au minimum)
15. **mod_dashboard_exec.R** : vérifier que les KPIs se calculent
16. **mod_qualite_auto.R** : tester digit preference, cohérence
17. **mod_temporal_tests.R** : tester comparaisons entre rounds

### Phase 6 — Modules avancés
18. **mod_advanced_geolocation.R** : tester geocoded_data + K-means spatial
19. **mod_advanced_benchmarking.R** : tester comparaison par groupe
20. **mod_advanced_forecasting.R** : tester agrégation temporelle + ARIMA/ETS
21. **mod_advanced_nlp.R** : vérifier le chargement (pas de colonnes texte longues dans ce dataset)

### Phase 7 — Corrections et rapport final
- Corriger tout code qui échoue ou produit des résultats aberrants
- Documenter les résultats attendus pour chaque module

---

## Approche pour chaque module
Pour chaque module, la procédure est :
1. **Source** le fichier
2. **Appeler** la/les fonction(s) principale(s) avec les données test
3. **Vérifier** que le résultat n'est pas NULL/erreur et est cohérent
4. **Corriger** si nécessaire (dans le fichier source)
5. **Consigner** le résultat attendu

## Livrables
- Tous les modules testés et corrigés
- Résumé des résultats de référence pour chaque module
