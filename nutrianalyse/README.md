# NutriAnalyse Pro v2

[![CI](https://github.com/yimgakevin/machine-learning/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/yimgakevin/machine-learning/actions/workflows/ci.yml)
![R](https://img.shields.io/badge/R-4.5.2-276DC3)
![Shiny](https://img.shields.io/badge/Shiny-application-1f65b8)
![Licence](https://img.shields.io/badge/licence-AGPL--3.0-2d8e47)

> **Plateforme d'analyse nutritionnelle, épidémiologique et de sécurité alimentaire**
> pour les enquêtes SMART/SENS et les données de santé publique. Application Shiny
> reproductible (R 4.5.2, `renv`) couvrant l'import multi-source, le contrôle
> qualité, les indicateurs OMS/UNICEF/PAM, la statistique inférentielle et
> multivariée, l'épidémiologie, la survie, la modélisation prédictive, la
> cartographie GADM et la production de rapports.

| | |
|---|---|
| **Version** | 2.0.0 |
| **Auteur** | YIMGA WANDJI KEVIN — DSAMID Data Science & Analysis |
| **ORCID** | [0009-0006-1000-7339](https://orcid.org/0009-0006-1000-7339) |
| **Licence** | AGPL-3.0 |
| **Dépôt** | [yimgakevin/machine-learning](https://github.com/yimgakevin/machine-learning) — dossier `nutrianalyse/` |
| **CI** | GitHub Actions, Windows Server, R 4.5.2, `renv` |

> [!IMPORTANT]
> **Les données de `DATA/` sont synthétiques.** Elles ont été générées pour
> illustrer les fonctionnalités et ne correspondent à aucune enquête réelle.
> Voir la section [Données de démonstration](#donnees-demonstration).

---

## Sommaire

1. [Positionnement et cas d'usage](#positionnement)
2. [Vue d'ensemble fonctionnelle](#vue-ensemble)
3. [Parc analytique : 40 onglets](#parc-analytique)
4. [Méthodologie statistique](#methodologie)
5. [Architecture logicielle](#architecture)
6. [Installation et lancement](#installation)
7. [Sécurité et gouvernance](#securite)
8. [Données de démonstration](#donnees-demonstration)
9. [Vérification, tests et CI](#verification)
10. [Flux de travail type](#flux-travail)
11. [Documentation](#documentation)
12. [Limites, bonnes pratiques et pistes](#limites)
13. [Licence, citation et contact](#licence)

---

<a id="positionnement"></a>
## 1. Positionnement et cas d'usage

NutriAnalyse Pro v2 est une application d'analyse **de bout en bout** pensée pour
les équipes qui produisent des résultats de nutrition et de santé publique à
partir d'enquêtes transversales, répétées ou à base communautaire.

**Public cible**

- épidémiologistes et statisticiens de programmes nutrition ;
- équipes MEAL/suivi-évaluation d'ONG et d'agences des Nations Unies ;
- ministères de la santé et instituts nationaux de statistique ;
- chercheurs et doctorants travaillant sur les enquêtes SMART, SENS, MICS/EDS ;
- analystes de sécurité alimentaire et de moyens d'existence.

**Cas d'usage couverts**

- contrôle qualité et plausibilité d'une enquête anthropométrique ;
- calcul des z-scores OMS 2006 et des prévalences GAM/SAM/MAM, retard de
  croissance, insuffisance pondérale et MUAC ;
- analyse descriptive, inférentielle, multivariée et par grappes ;
- estimation de prévalences pondérées avec intervalle de confiance et effet de
  plan (DEFF) ;
- analyses épidémiologiques (RR, OR, PAF, Mantel-Haenszel), de survie
  (Kaplan-Meier, Cox) et de modélisation prédictive ;
- cartographie GADM multi-niveaux, heatmaps, détection de grappes spatiales et
  priorisation géographique ;
- production de rapports Word/PDF/Excel/HTML reproductibles ;
- connexion à des sources de données externes (SQL, DHIS2, ODK/Kobo, REDCap).

**Ce que la plateforme ne fait pas**

- elle ne collecte pas les données de terrain (elle s'y connecte) ;
- elle ne remplace ni WHO Anthro, ni un logiciel d'enquête certifié, ni la revue
  d'un statisticien ou d'un comité d'éthique ;
- ce n'est pas un dispositif médical et elle ne produit pas de diagnostic
  individuel.

**Choix de conception**

- **reproductibilité** : environnement `renv` verrouillé, R 4.5.2, CI Windows ;
- **traçabilité** : audit trail, exports horodatés, versionnage Git ;
- **sécurité** : aucun secret dans le code, comptes hachés, mode production ;
- **confidentialité** : masquage des petits effectifs dans les exports ;
- **sobriété des dépendances** : dégradation gracieuse quand un package optionnel
  est absent (analyses signalées comme indisponibles, pas de crash global).

---

<a id="vue-ensemble"></a>
## 2. Vue d'ensemble fonctionnelle

| Domaine | Capacités principales |
|---|---|
| **Données** | Import CSV/TSV/Excel/SPSS/Stata/Parquet, Kobo/ODK, mapping automatique et manuel, dictionnaire OMS, exploration, manipulation `dplyr` sans code |
| **Qualité** | Flags SMART, plausibilité, préférence de chiffres, age heaping, complétude, score global, anomalies par grappe, audit trail |
| **Anthropométrie** | Z-scores OMS 2006 par LMS, GAM/SAM/MAM, stunting, insuffisance pondérale, MUAC, œdèmes, prévalences et IC |
| **Nutrition et santé** | IYCF/ANJE, anémie, paludisme/LLIN, WASH/JMP, ménage et protection, handicap (Washington Group), mortalité CMR/U5MR |
| **Sécurité alimentaire** | FCS, HHS, rCSI, HDDS, profils de consommation et de stratégies d'adaptation |
| **Statistiques** | Descriptif, inférentiel, tests multiples, tailles d'effet, ACP/ACM/FAMD/AFC, k-means/CAH/DBSCAN, bootstrap |
| **Épidémiologie** | Tableaux 2×2, RR, OR, PAF, multi-exposition, Cochran-Armitage, stratification, Mantel-Haenszel, forest plots |
| **Survie** | Kaplan-Meier, log-rank, Cox multivariable, hypothèse PH, table de vie, survie médiane |
| **Prédiction** | GLM, Random Forest, XGBoost, SVM RBF, k-NN, ROC/AUC, calibration, lift, PDP, importance, export du modèle |
| **Temporel** | Comparaisons inter-vagues, DID, tendances, CUSUM, bootstrap, heatmaps, trajectoires, ARIMA/ETS |
| **Spatial** | GADM L1–L3, choroplèthes, cercles, GPS, heatmaps, Jenks, LISA, clustering spatial, priorisation |
| **Rapports** | Word OMS, PDF, Excel multi-feuilles, HTML, sections configurables, interprétations automatiques |
| **IA** | 4 agents Gemini spécialisés + orchestrateur, RAG OMS/UNICEF/WFP/Lancet, clé optionnelle |
| **Connecteurs** | SQLite, PostgreSQL, MySQL/MariaDB, SQL Server, DHIS2, ODK Central, KoboToolbox, REDCap |

---

<a id="parc-analytique"></a>
## 3. Parc analytique : 40 onglets

L'application expose **40 onglets** regroupés en quatre familles : le cœur
SMART/nutrition, les statistiques et la science des données, l'épidémiologie et
la prédiction, puis le niveau expert, la recherche avancée et les connecteurs.

### 3.1 Données, qualité et exploration

| Onglet | Ce qu'il apporte |
|---|---|
| **Tableau de bord** | Vue exécutive : KPI, alertes OMS, jauges, distributions de z-scores, synthèse de l'enquête |
| **Import & Mappeur** | Fichier local (CSV, TSV, Excel, SPSS, Stata, Parquet), Kobo/ODK, détection automatique des colonnes, dictionnaire OMS, correction manuelle, aperçu |
| **Exploration** | Tableau filtrable, statistiques numériques et catégorielles, valeurs manquantes, graphiques automatiques |
| **Manipulation dplyr** | Interface sans code : `filter`, `select`, `mutate`, `arrange`, `summarise`, `slice`, `across`, aperçu du code R généré |
| **Qualité SMART** | Flags WHZ/HAZ/WAZ/MUAC, plausibilité, préférence de chiffres, age heaping, écart-type par équipe, complétude, score global |
| **Qualité automatisée** | Score de qualité consolidé, contrôles de cohérence, anomalies par grappe, diagnostics exportables |
| **Audit Trail** | Journal horodaté des actions utilisateur, traçabilité des analyses et exports |

### 3.2 Anthropométrie et indicateurs OMS

| Onglet | Ce qu'il apporte |
|---|---|
| **Z-scores OMS 2006** | Calcul LMS (WHZ, HAZ, WAZ), MUAC, âge, flags SMART, moteur expert (scores de qualité, anomalies, benchmarking), exports |
| **Indicateurs OMS/PAM** | GAM, SAM, MAM, stunting, insuffisance pondérale, MUAC, œdèmes, IC Wilson, forest plots, jauges, FCS/HHS |
| **Prévalences SMART** | Prévalences pondérées, IC 95 % design-based, DEFF, résultats par strate, exports |
| **Mortalité SMART** | CMR et U5MR, IC Poisson ou `svyratio`, tests quasi-Poisson, stratification |
| **IYCF / ANJE** | Indicateurs OMS/UNICEF d'allaitement et d'alimentation du nourrisson et du jeune enfant |
| **Anémie SENS** | Hémoglobine, classification OMS, prévalences, IC, stratification |
| **Paludisme / LLIN** | Possession et utilisation de moustiquaires, TDR, fièvre, ratio LLIN/personne |
| **WASH** | Indicateurs JMP : eau, assainissement, hygiène, services de base |
| **Ménage & Protection** | Chef de ménage féminin, part alimentaire, protection, indicateurs binaires |
| **Handicap (WG)** | Prévalences par domaine du Washington Group, IC, stratification |
| **Sécurité alimentaire** | FCS, HHS, rCSI, HDDS, catégorisation et profils |

### 3.3 Statistiques et science des données

| Onglet | Ce qu'il apporte |
|---|---|
| **Univariée** | Histogramme, densité, boxplot, violin, barres, secteurs, statistiques complètes, export haute définition |
| **Bivariée** | Nuages de points, boxplots par groupe, barres, corrélations, test statistique automatique et interprétation |
| **Tableau descriptif** | `gtsummary`, p-values, `corrplot` de Spearman, z-scores par groupe, tableau publication |
| **Inférentielle** | Chi²/Fisher, t-test/Mann-Whitney, ANOVA/Kruskal-Wallis + BH, corrélations, Shapiro, régression logistique |
| **Réduction de dimension** | ACP, ACM, FAMD, AFC (FactoMineR), éboulis, cercle des corrélations, biplot, contributions, cos² |
| **Clustering** | K-means, CAH Ward, DBSCAN, silhouette, dendrogramme, profils, stabilité bootstrap (ARI) |
| **Modèle prédictif** | GLM, Random Forest, XGBoost, SVM RBF, k-NN ; ROC/AUC avec IC, calibration, lift, matrice de confusion, importance, PDP, permutation, courbe d'apprentissage, seuil optimal, export `.rds`, scoring |

### 3.4 Épidémiologie, temps, espace et rapports

| Onglet | Ce qu'il apporte |
|---|---|
| **Épidémiologie** | Tableau 2×2, RR, OR, PAF, multi-exposition, Cochran-Armitage, stratification, Mantel-Haenszel, forest plot |
| **Analyse de survie** | Kaplan-Meier, log-rank, Cox multivariable, test PH, table de vie, survie médiane, comparaisons multiples |
| **Tests Temporels** | Comparaison de proportions entre vagues (chi²/z/Fisher, taille d'effet) et 7 techniques expertes : moyennes continues, différence-in-differences, tendances multi-vagues, CUSUM, bootstrap percentile/BCa, heatmap temporelle, trajectoires par zone |
| **Prévisions temporelles** | ARIMA/SARIMA (`auto.arima`), lissage ETS, test ADF, intervalles de confiance à 95 % |
| **Cartographie** | GADM L1/L2/L3, choroplèthe, cercles proportionnels, GPS, heatmap, Jenks, LISA, clustering spatial, priorisation, couverture mondiale (~180 pays) |
| **Rapport exportable** | Word OMS, PDF, Excel multi-feuilles, HTML, sections configurables, graphiques et interprétations |
| **Dashboard exécutif** | KPI OMS, alertes, synthèse décisionnelle de style Power BI |

### 3.5 Niveau expert, recherche avancée et connecteurs

| Onglet | Ce qu'il apporte |
|---|---|
| **Géolocalisation** | Détection dynamique des colonnes GPS, clustering spatial k-means, heatmaps, matrice de distances, rapport spatial |
| **Analyse textes (NLP)** | Sentiments, thèmes LDA, nuages de mots, fréquences, n-grams, classification de commentaires libres |
| **Benchmarking** | Référence personnalisée, percentiles, classements, comparaison d'indicateurs |
| **SQL / Base de données** | SQLite, PostgreSQL, MySQL/MariaDB, SQL Server (ODBC), exploration et import de tables |
| **DHIS2** | API REST v2.x : analytics, data values, metadata, unités d'organisation |
| **ODK / KoboToolbox** | ODK Central v1 et KoboToolbox v2 : formulaires, soumissions, mapping |
| **REDCap** | API REST v13+ : records, instruments, dictionnaire de données, événements |
| **À propos** | Version, crédits, documentation, état de configuration |

> Les onglets **Niveau Expert** (Dashboard exécutif, Qualité automatisée, Audit
> Trail, Tests Temporels), **Recherche Avancée** (Prévisions, Géolocalisation,
> NLP, Benchmarking) et **Données & APIs** (SQL, DHIS2, ODK, REDCap) sont
> accessibles depuis les menus repliables correspondants.

---

<a id="methodologie"></a>
## 4. Méthodologie statistique

### 4.1 Z-scores OMS 2006 et anthropométrie

Le moteur `module_zscore_oms.R` applique la **méthode LMS** des normes OMS 2006
(WHO Child Growth Standards) avec interpolation des tables de référence :

$$
Z = \frac{(Y/M)^L - 1}{L S} \quad (L \neq 0),
\qquad
Z = \frac{\ln(Y/M)}{S} \quad (L = 0)
$$

où $Y$ est la mesure (poids, taille, MUAC), et $L$, $M$, $S$ les paramètres de
la distribution de référence pour l'âge ou la taille et le sexe.

Fonctions couvertes :

- **WHZ** (weight-for-height z-score) à partir du poids et de la taille ;
- **HAZ** (height-for-age z-score) à partir de la taille et de l'âge ;
- **WAZ** (weight-for-age z-score) à partir du poids et de l'âge ;
- **MUAC** et âge, avec détection des valeurs hors plage ;
- **flags SMART** : WHZ ∈ [-5, 5], HAZ ∈ [-6, 6], WAZ ∈ [-6, 5],
  MUAC ∈ [80, 220] mm ;
- **œdèmes bilatéraux** pris en compte dans la classification GAM/SAM.

Les tables LMS sont intégrées et interpolées sur les plages usuelles. Pour un
usage de publication ou de certification, les résultats doivent être
**contre-vérifiés avec WHO Anthro** ou le package R `anthro` ; cette
recommandation est explicite dans la section [Limites](#limites).

### 4.2 Prévalences, intervalles de confiance et plan d'enquête

- **IC Wilson** pour une proportion, adapté aux prévalences faibles ou aux
  effectifs modérés :

$$
\frac{\hat p + \frac{z^2}{2n} \pm z\sqrt{\frac{\hat p(1-\hat p)}{n} + \frac{z^2}{4n^2}}}{1 + \frac{z^2}{n}}
$$

- **Estimation design-based** via le package `survey` (`svydesign`,
  `svymean`, `svyciprop`) lorsque les variables de grappe, strate et poids sont
  disponibles ;
- **DEFF** (design effect) calculé comme le rapport de la variance design à la
  variance d'un échantillonnage aléatoire simple de même taille ;
- **Stratification** : résultats par strate, avec contrôle des effectifs et
  masquage des petits effectifs ;
- **Pondération** : poids de sondage ou poids calibrés, avec `nest = TRUE` pour
  les grappes imbriquées et `survey.lonely.psu = "adjust"` pour les strates à
  une seule grappe.

### 4.3 Contrôle qualité SMART

Le module `module_qualite_smart.R` reproduit les contrôles attendus dans une
enquête SMART :

- **flags anthropométriques** et valeurs biologiquement implausibles ;
- **score de plausibilité** par grappe et par équipe ;
- **préférence de chiffres** (digit preference) sur les mesures ;
- **age heaping** sur les âges déclarés ;
- **écart-type** des z-scores par équipe, avec détection des enquêteurs
  atypiques ;
- **complétude** et cohérence des variables clés ;
- **score global de qualité** et export des diagnostics.

### 4.4 Inférence, tailles d'effet et tests multiples

- tests paramétriques et non paramétriques : t-test, Welch, Mann-Whitney,
  ANOVA, Kruskal-Wallis, chi², Fisher, tests appariés, Friedman ;
- **tailles d'effet** : V de Cramér, d de Cohen (et dz apparié), r de Pearson,
  ρ de Spearman, rank-biserial ;
- **correction de tests multiples** de Benjamini-Hochberg :

$$
p_{(i)} \leq \frac{i}{m}\alpha
$$

- **bootstrap** : IC percentile et BCa pour moyennes, médianes, proportions et
  différences ;
- **puissance statistique** et taille d'échantillon requise (`pwr`) ;
- **facteur de Bayes** pour une lecture bayésienne de l'évidence ;
- **équivalence TOST** (two one-sided tests) pour démontrer l'absence de
  différence pertinente ;
- **diagnostics de régression** : résidus, influence, hétéroscédasticité,
  multicolinéarité.

### 4.5 Analyse multivariée et clustering

- **ACP** (variables quantitatives), **ACM** (qualitatives), **FAMD**
  (mixtes), **AFC** (tables de contingence) via FactoMineR ;
- lecture des **valeurs propres**, **éboulis**, **cercle des corrélations**,
  **biplot**, **contributions** et **cos²** ;
- **k-means** avec initialisation multiple, **CAH Ward**, **DBSCAN** pour les
  formes non convexes ;
- **silhouette**, **dendrogramme**, profils moyens par cluster ;
- **stabilité bootstrap** : ré-échantillonnage, recalcul des partitions et
  **Adjusted Rand Index (ARI)** pour quantifier la robustesse.

### 4.6 Épidémiologie

- tableaux 2×2 et mesures d'association : **RR**, **OR**, **PAF** ;
- **multi-exposition** et ajustement ;
- **Cochran-Armitage** pour la tendance sur une variable ordinale ;
- **stratification** et **Mantel-Haenszel** pour le contrôle d'un facteur de
  confusion ;
- **forest plots** et intervalles de confiance ;
- analyse de **survie** : Kaplan-Meier, log-rank, Cox multivariable, test de
  proportionnalité des risques, table de vie et survie médiane.

### 4.7 Modélisation prédictive

Le module `mod_predictif.R` entraîne et compare plusieurs familles de modèles :

- **GLM** (logistique binaire ou multinomiale) ;
- **Random Forest** ;
- **XGBoost** ;
- **SVM à noyau RBF** ;
- **k-NN**.

Évaluation et interprétation :

- **ROC/AUC** avec intervalle de confiance et **PR-AUC** ;
- **matrice de confusion**, sensibilité, spécificité, valeurs prédictives,
  **accuracy**, **Kappa**, **F1**, **MCC** ;
- **Brier score**, **log-loss**, **calibration** et courbes **gain/lift** ;
- **importance** des variables, **PDP** (partial dependence) et **permutation
  importance** ;
- **courbe d'apprentissage** et optimisation du **seuil de décision** ;
- export du modèle en `.rds` et **scoring** d'un jeu de données.

La séparation entraînement/test et la validation croisée sont explicites dans
l'interface ; les résultats doivent être interprétés avec les précautions
habituelles (surapprentissage, fuite de données, déséquilibre de classes).

### 4.8 Analyses temporelles et spatiales

- **différence-in-differences** : modèle
  $y = \beta_0 + \beta_1 post + \beta_2 treat + \beta_3 post \times treat + \varepsilon$,
  où $\beta_3$ est l'effet DID ;
- **tendances multi-vagues**, **CUSUM** pour la détection précoce de dérive,
  **bootstrap** des différences inter-vagues, **heatmaps** et **trajectoires**
  par zone ;
- **ARIMA/SARIMA** via `auto.arima`, **ETS**, test ADF de stationnarité,
  prévisions avec IC 95 % ;
- **cartographie** : choroplèthes GADM, cercles proportionnels, heatmaps,
  classification de Jenks, **LISA** (local indicators of spatial association)
  si `spdep` est disponible, clustering spatial k-means et priorisation.

### 4.9 Petits effectifs et confidentialité

Les exports appliquent une règle de diffusion :

- strate de **moins de 30 observations** → statistiques dérivées (prévalences,
  IC, moyennes, pourcentages) remplacées par `NA` ;
- strate comportant **1 à 4 cas** → cas, prévalences et IC masqués ;
- une colonne **`Diffusion`** documente chaque ligne (`Libre`, `n < 30`,
  `1-4 cas`).

Cette règle est activée par défaut dans les exports des 10 modules experts, les
prévalences SMART, les exports IYCF, le rapport Word et le résumé des z-scores.

---

<a id="architecture"></a>
## 5. Architecture logicielle

### 5.1 Arborescence active

```text
nutrianalyse/
├── app.R                      # point d'entrée Shiny (runApp / bouton RStudio)
├── app_v2_final.R             # orchestrateur : setup + UI + server + shinyApp
├── app_parts/
│   ├── 00_setup.R             # options, packages, source() des modules et moteurs
│   ├── 01_css.R               # feuille de style globale
│   ├── ui/
│   │   ├── ui.R               # dashboardPage(header, sidebar, body)
│   │   ├── ui_header.R
│   │   ├── ui_sidebar.R
│   │   └── ui_body.R          # login, chatbot, tabItems()
│   └── server/                # 20 fichiers server_<domaine>.R, sourcés local = TRUE
├── mod_*.R                    # modules Shiny namespacés (UI + serveur)
├── module_*.R                 # moteurs métier et helpers experts
├── tools/                     # vérifications, tests, benchmarks, gestion des comptes
├── tests/testthat/            # 54 tests unitaires et d'intégration
├── DATA/                      # jeux de démonstration synthétiques
├── renv.lock                  # environnement R verrouillé (R 4.5.2)
├── DESCRIPTION                # métadonnées du projet
├── .Renviron.example          # modèle de configuration locale (sans secret)
├── SECURITY_GUIDE.md
├── ARCHITECTURE.md
└── README.md
```

Les dossiers `_archive/`, `R_backup/` et les fichiers `*_backup*`,
`app_v2_final_*.R` sont **historiques** : ils sont conservés pour traçabilité et
ne sont pas chargés par l'application active.

### 5.2 Orchestrateur et découpage

`app_v2_final.R` ne contient plus que **49 lignes** : il source le setup, le CSS,
l'UI et découpe le serveur en fichiers spécialisés. Le monolithe initial de
15 754 lignes a été transformé sans réécriture fonctionnelle :

| Élément | Avant | Après |
|---|---|---|
| `app_v2_final.R` | 15 754 lignes | 49 lignes |
| UI | un bloc `dashboardPage` de ~3 030 lignes | coquille + 23 modules namespacés + onglets experts |
| Serveur | une fonction de ~11 750 lignes | 20 fichiers `app_parts/server/` + modules |
| Onglets de la v2 initiale | 23 onglets inline | 23 modules namespacés |

Le découpage est **mécanique** : chaque fichier contient une tranche exacte de
l'ancien monolithe, ce qui garantit un comportement identique. La CI vérifie le
parse, le chargement complet, la session serveur mock et le démarrage HTTP réel.

### 5.3 Pourquoi `app_parts/` et pas `R/` ?

Shiny source automatiquement le dossier `R/` au démarrage. Or les fichiers
`app_parts/server/*.R` ont besoin du **scope de `server()`** et ne doivent jamais
être chargés globalement. Le nom `app_parts/` évite ce piège. Chaque fichier est
évalué avec `local = TRUE` :

```r
server <- function(input, output, session) {
  source("app_parts/server/server_core.R", local = TRUE)
  source("app_parts/server/server_import.R", local = TRUE)
  # ...
}
```

Les réactifs racine (`df_f`, `map_cols`, `lang`, `mapped_data`, `audit_log_rv`,
`filter_context`, `zs_data`, etc.) sont créés dans ces fichiers puis passés
**explicitement** aux modules (`mod_<nom>_server(id, ...)`). C'est ce qui permet
de découper 527 `output$` et 76 observers sans réécrire une seule reactive.

### 5.4 Contrat d'un module

```r
# UI
mod_x_ui <- function(id) {
  ns <- shiny::NS(id)
  tabItem(
    tabName = "x",                 # nom global du menu, non namespacé
    DTOutput(ns("table"))
  )
}

# Serveur
mod_x_server <- function(id, df_f, map_cols, lang) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({ ... })   # input$/output$ restent inchangés
  })
}
```

Règles :

1. Les **IDs UI** sont préfixés par `ns()` ; côté serveur, `moduleServer` applique
   le namespace automatiquement.
2. Les **réactifs partagés** sont passés en arguments, jamais lus dans le global.
3. Les **helpers locaux** qui dépendent de `input$` (palette, thème) sont passés
   en argument.
4. Les **sélecteurs JS/CSS** qui ciblent des IDs globaux doivent être vérifiés.
5. Un onglet = un commit, avec `check_app`, `check_server`, `launch_test` et les
   tests.

### 5.5 Couches et moteurs

| Couche | Fichiers / rôle |
|---|---|
| **i18n** | `i18n.R` — interface FR/EN, fonction `tr()` |
| **Sécurité** | `auth_config.R`, `safe_eval.R`, `config_api_secure_v2.R` |
| **Configuration OMS** | `config_oms.R`, `measures.R` — seuils, classifications, indicateurs |
| **Anthropométrie** | `module_zscore_oms.R`, `zscore_expert_engine.R` — LMS, flags, qualité, anomalies |
| **Qualité** | `module_qualite_smart.R` — flags, plausibilité, digit preference, age heaping |
| **Prévalences** | `module_prevalence_smart.R` — `survey`, DEFF, IC design-based |
| **Modules SENS** | `module_mortalite.R`, `module_iycf.R`, `module_anemie.R`, `module_paludisme.R`, `module_wash.R`, `module_menage_protection.R`, `module_handicap_wg.R` |
| **Inférence** | `inferentiel_v4.R`, `module_desc_expert.R`, `module_infer_expert.R` |
| **Temporel** | `module_temporel_expert.R`, `mod_temporal_tests.R` |
| **Cartographie** | `module_carto_v2.R`, `carto_expert_engine.R`, `carto_intelligence_engine.R`, `carto_predictive_engine.R`, `spatial_analysis_engine.R`, `gadm_loader.R` |
| **Rapports** | `module_rapport.R` — Word, PDF, Excel, HTML |
| **IA** | `context_manager_v3.R`, `gemini_api_v3.R`, `chat_agent.R`, `analysis_agent.R`, `report_agent.R`, `diagnose_agent.R`, `master_agent.R`, `rag_nutrition_kb.R` |
| **Connecteurs** | `mod_database_sql.R`, `mod_dhis2.R`, `mod_odk_central.R`, `mod_redcap.R` (chargement différé) |

Les connecteurs bases de données sont **lazy-loadés** : ils ne sont sourcés
qu'à la première visite de leur onglet, ce qui réduit le temps de démarrage et
isole les dépendances optionnelles (`DBI`, `RSQLite`, `RPostgres`, `RMariaDB`,
`odbc`).

### 5.6 Performance

Mesures locales documentées (R 4.5.2, 84 packages chargés ; dépend du matériel) :

| Mesure | Temps |
|---|---|
| Chargement complet de l'application | 18,4 s |
| Construction UI Accueil / Z-scores | 0,05 s / 0,21 s |
| Calcul Z-scores (500 / 2 000 lignes) | 0,05 s / 0,07 s |
| IC Wilson, 6 indicateurs, n = 5 000 | 0,12 s |
| IC design-based (grappes + poids), n = 5 000 | 0,68 s |
| Tests multiples BH, 11 variables, n = 5 000 | 0,20 s |
| Stabilité bootstrap (B = 30), n = 5 000 | 0,92 s |

`Rscript tools/benchmark_expert.R` reproduit les quatre dernières lignes ;
`Rscript tools/benchmark_app.R` mesure le chargement et les opérations
courantes.

### 5.7 Ajouter un onglet ou un module

1. Créer `mod_<nom>.R` avec `mod_<nom>_ui(id)` et `mod_<nom>_server(id, ...)`.
2. Ajouter `source("mod_<nom>.R")` dans `app_parts/00_setup.R`.
3. Ajouter `mod_<nom>_ui("<nom>")` dans le `tabItems()` de
   `app_parts/ui/ui_body.R`.
4. Ajouter l'appel `mod_<nom>_server("<nom>", ...)` dans un fichier
   `app_parts/server/server_<domaine>.R` sourcé par l'orchestrateur, au bon
   endroit dans l'ordre.
5. Écrire un test dans `tests/testthat/` et, si pertinent, un
   `tools/check_mod_<nom>.R`.
6. Lancer `check_app`, `check_server`, `run_tests`, `launch_test`, puis committer.

---

<a id="installation"></a>
## 6. Installation et lancement

### 6.1 Prérequis

| Élément | Version / remarque |
|---|---|
| **R** | 4.5.2 (version verrouillée dans `renv.lock`) |
| **RStudio / Positron** | Recommandé pour le bouton *Run App* ; sinon `Rscript` |
| **Pandoc** | Fourni par RStudio/Quarto ; nécessaire aux exports Word/PDF/HTML |
| **Chrome ou Chromium** | Nécessaire à `pagedown::chrome_print()` pour l'export PDF |
| **Système** | Windows (CI), Linux/macOS possibles ; `sf`, `arrow` et `leaflet` peuvent nécessiter GDAL/GEOS/PROJ et Arrow selon la plateforme |
| **Mémoire** | 4 Go minimum recommandés ; davantage pour les jeux de données volumineux |

### 6.2 Installation

```bash
git clone https://github.com/yimgakevin/machine-learning.git
cd machine-learning/nutrianalyse

# Restaure l'environnement R verrouillé (R 4.5.2)
Rscript -e 'renv::restore()'

# Configuration locale (sans secret par défaut)
cp .Renviron.example .Renviron
# Windows PowerShell :
# Copy-Item .Renviron.example .Renviron

# Lancement
Rscript tools/run_app.R
```

L'application démarre sur **http://127.0.0.1:8765**. Le port est modifiable :

```bash
NUTRI_PORT=3838 Rscript tools/run_app.R
```

Sous Windows, si `Rscript` n'est pas dans le `PATH`, utiliser le chemin complet
(`C:\Program Files\R\R-4.5.2\bin\Rscript.exe`) ou passer par RStudio.

### 6.3 Lancement dans RStudio

1. Ouvrir `nutrianalyse/Shiny_entreprise.Rproj` ou `nutrianalyse/app.R`.
2. Vérifier que le répertoire de travail est bien `nutrianalyse/`.
3. Cliquer sur **Run App** (ou `shiny::runApp(".")`).

Le point d'entrée `app.R` source `app_v2_final.R` et renvoie l'objet
`shiny.appobj` attendu par `runApp()`.

### 6.4 Configuration locale

`.Renviron` est **ignoré par Git** et ne doit jamais être commité.

| Variable | Rôle | Défaut |
|---|---|---|
| `GEMINI_API_KEY` | Clé Google AI Studio pour les agents Gemini | vide → agents désactivés |
| `NUTRI_ENV` | `development` ou `production` | `development` |
| `NUTRI_USERS_FILE` | Chemin du fichier de comptes haché | `config/users.rds` |
| `NUTRI_PORT` | Port du serveur Shiny | `8765` |
| `NUTRI_NEW_PASSWORD` | Mot de passe non interactif pour `manage_users.R` | — |

Sans `GEMINI_API_KEY`, l'application démarre normalement : seuls les agents IA
sont désactivés. Les autres analyses restent disponibles.

### 6.5 Déploiement serveur

L'application est déployable sur **Shiny Server Open Source**, **Posit
Connect** ou un conteneur R maîtrisé. Recommandations :

- définir `NUTRI_ENV=production` et `GEMINI_API_KEY` dans l'environnement du
  serveur, jamais dans l'image ni dans le dépôt ;
- monter `config/users.rds` sur un volume persistant accessible en lecture ;
- prévoir un volume pour le cache GADM (`gadm_cache/`) et, si utilisé, le cache
  Gemini (`.gemini_cache/`) ;
- servir l'application derrière un reverse proxy HTTPS ;
- dimensionner la mémoire selon la taille des fichiers importés ;
- ne pas exposer les exports ou les données brutes sans contrôle d'accès ;
- vérifier `renv::status()` avant de construire l'image de déploiement.

La CI ne déploie pas : elle vérifie le chargement, la session serveur, les tests
et le démarrage HTTP. Le déploiement reste une décision d'exploitation.

---

<a id="securite"></a>
## 7. Sécurité et gouvernance

### 7.1 Secrets et clés API

- `GEMINI_API_KEY` est lue via `Sys.getenv()` ; elle n'est **jamais** écrite dans
  le code, les données ou la documentation ;
- `config_api_secure_v2.R` masque les clés dans les logs
  (`****[GOOGLE_API_KEY_MASKED]****`) ;
- `.Renviron` et `config/users.rds` sont ignorés par Git ;
- la CI refuse toute chaîne `AIzaSy…` dans les fichiers versionnés ;
- en cas de compromission : révoquer la clé dans Google AI Studio, en générer
  une nouvelle, la placer dans `.Renviron` ou l'environnement serveur, puis
  vérifier l'historique Git et les logs.

### 7.2 Comptes et authentification

Les utilisateurs sont gérés par `shinyauthr` et `sodium` :

```bash
Rscript tools/manage_users.R list
Rscript tools/manage_users.R add alice "Alice D."
Rscript tools/manage_users.R passwd alice
Rscript tools/manage_users.R remove alice
```

- mots de passe **hachés argon2** via `sodium::password_store()` ;
- mot de passe en clair **jamais** stocké ;
- 12 caractères minimum lors de la création interactive ;
- tous les comptes authentifiés disposent des **mêmes droits** : il n'y a plus
  de rôles ni de permissions différenciées (choix de conception) ;
- sans `config/users.rds`, trois comptes de démonstration sont utilisés ; ils
  sont **refusés** quand `NUTRI_ENV=production` et le démarrage échoue avec un
  message explicite.

### 7.3 Mode production

Pour un déploiement réel :

1. générer `config/users.rds` avec `tools/manage_users.R` ;
2. définir `NUTRI_ENV=production` ;
3. vérifier que les comptes de démonstration ne sont plus actifs ;
4. configurer HTTPS, journaux et sauvegardes ;
5. restreindre l'accès réseau et les exports.

### 7.4 Diffusion des résultats

La règle de petits effectifs (section 4.9) s'applique aux exports. La case
« Masquer les petits effectifs » est activée par défaut ; elle ne doit être
désactivée que pour un usage interne documenté.

### 7.5 Audit et traçabilité

L'onglet **Audit Trail** journalise les actions utilisateur avec horodatage. Les
exports sont datés et les analyses sont reproductibles à partir des données, du
code et de la version de l'environnement (`renv.lock`).

### 7.6 Checklist avant commit

```text
☐ .Renviron absent de Git                 → git check-ignore -v .Renviron
☐ config/users.rds absent de Git          → git check-ignore -v config/users.rds
☐ Aucune clé AIzaSy dans le code/docs     → grep -rn "AIzaSy" --include="*.R" --include="*.md"
☐ Aucun mot de passe en clair             → grep -rn 'password_store("' --include="*.R"
☐ Tests et vérifications passent          → Rscript tools/run_tests.R
☐ renv synchronisé                        → Rscript -e 'renv::status()'
```

Voir [SECURITY_GUIDE.md](SECURITY_GUIDE.md) et
[SECURITY_CHECKLIST.md](SECURITY_CHECKLIST.md) pour la procédure complète.

---

<a id="donnees-demonstration"></a>
## 8. Données de démonstration

Les fichiers de `DATA/` sont **synthétiques**. Ils servent à tester l'import, le
mapping, les contrôles qualité et les analyses, sans exposer de données réelles.

| Fichier | Taille approximative | Contenu |
|---|---|---|
| `dataset_SMART_SENS_179vars.csv` | ≈ 1 200 lignes × 179 colonnes | Jeu SMART/SENS complet : ménage, anthropométrie, mortalité, IYCF, sécurité alimentaire |
| `nutrianalyse_test_complet.csv` | ≈ 1 500 lignes × 84 colonnes | Jeu de test transversal avec strates, grappes, GPS, FCS/HHS/rCSI, indicateurs enfant |
| `nutrianalyse_test_dataset.csv` | ≈ 1 200 lignes × 75 colonnes | Jeu de test standard pour l'import et les analyses courantes |
| `world_class_smart_sens_dataset.csv` | ≈ 3 300 lignes × 75 colonnes | Jeu anthropométrique enfant plus volumineux, avec poids/taille/MUAC et z-scores |
| `table-1774774807035.csv` | 3 lignes × 3 colonnes | Index de fichiers, non destiné à l'analyse |

**Précautions**

- ne pas utiliser ces données pour des décisions opérationnelles ou des
  publications ;
- pour des données réelles, déployer l'application dans un environnement privé,
  avec authentification, chiffrement et politique de conservation ;
- vérifier la conformité éthique et réglementaire (consentement, anonymisation,
  accès) avant tout traitement de données personnelles.

---

<a id="verification"></a>
## 9. Vérification, tests et CI

### 9.1 Commandes locales

Depuis `nutrianalyse/` :

```bash
Rscript tools/check_app.R             # setup + UI + définition de server()
Rscript tools/check_server.R          # exécution de server() en session mock
Rscript tools/run_tests.R             # suite testthat (54 tests)
Rscript tools/check_expert_modules.R  # 10 modules experts en isolation
Rscript tools/launch_test.R           # démarrage HTTP réel sur le port 8765
Rscript tools/benchmark_expert.R      # micro-benchmarks des helpers experts
```

Résultats attendus :

- `CHECK_OK` : 84 packages chargés, UI construite, `server()` défini ;
- `CHECK_SERVER_OK` : les 20 fichiers serveur se sourcent et s'enregistrent ;
- `testthat` : **54 tests, 0 échec** ;
- `CHECK_EXPERT_MODULES_OK` : **10 modules, 0 échec** ;
- `HTTP_OK` : l'application répond sur `http://127.0.0.1:8765/`.

Des vérifications ciblées existent pour chaque module :
`tools/check_mod_<nom>.R` (import, accueil, explor, uni/biv, infer, rdim,
clust, qualite, prevalence, secu_alim, OMS, SENS, etc.).

### 9.2 Suite `testthat`

Les 54 tests couvrent notamment :

| Fichier | Domaine |
|---|---|
| `test-agreger_geo.R` | Agrégation géographique |
| `test-app-structure.R` | Structure de l'application et contrats de modules |
| `test-auth_config.R` | Authentification et comptes |
| `test-carto_functions.R` | Fonctions cartographiques |
| `test-config_oms.R` | Seuils et classifications OMS |
| `test-epidemio.R` | Mesures épidémiologiques |
| `test-expert_helpers.R` | IC, design, qualité, effets, exports |
| `test-explor.R` | Statistiques exploratoires |
| `test-measures.R` | Indicateurs et mesures |
| `test-predictif.R` | Modélisation prédictive |
| `test-qualite_smart.R` | Flags et plausibilité SMART |
| `test-rapport.R` | Génération de rapports |
| `test-secu_alimentaire.R` | FCS, HHS, rCSI, HDDS |
| `test-survie.R` | Kaplan-Meier, Cox, log-rank |
| `test-utils.R` | Utilitaires et détection de colonnes |

### 9.3 Intégration continue

Le workflow se trouve à la racine du monorepo :
[`.github/workflows/ci.yml`](../.github/workflows/ci.yml).

| Étape | Contenu |
|---|---|
| Runner | `windows-latest` |
| R | 4.5.2 via `r-lib/actions/setup-r` |
| Environnement | `renv` restauré depuis `nutrianalyse/renv.lock` |
| Synchronisation | `renv::status()` doit être `TRUE` |
| Sécurité | scan des clés `AIzaSy…` dans les fichiers versionnés |
| Application | `check_app.R`, `check_server.R` |
| Tests | `run_tests.R`, `check_expert_modules.R` |
| Portée | `push` et `pull_request`, avec annulation des runs obsolètes |

Le badge CI en tête de ce README reflète l'état du workflow sur `main`.

### 9.4 Benchmarks

```bash
Rscript tools/benchmark_app.R
Rscript tools/benchmark_expert.R
```

Les mesures de référence sont données en section 5.6. Elles dépendent du
matériel, de la version de R et du cache `renv`.

### 9.5 Dépannage

| Symptôme | Cause probable | Solution |
|---|---|---|
| `Rscript: command not found` | R absent du `PATH` | Utiliser le chemin complet ou RStudio |
| `There is no package called …` | Environnement `renv` non restauré | `Rscript -e 'renv::restore()'` |
| `renv::status()` non synchronisé | Bibliothèque ou lockfile incohérent | `renv::restore()` puis `renv::status()` |
| Export PDF en échec | Chrome/Chromium absent | Installer Chrome ou utiliser Word/HTML |
| Pandoc introuvable | `RSTUDIO_PANDOC` non défini | Installer RStudio/Quarto ou définir `RSTUDIO_PANDOC` |
| Agents IA indisponibles | `GEMINI_API_KEY` vide ou invalide | Renseigner `.Renviron` ou l'environnement serveur |
| Démarrage refusé en production | `config/users.rds` absent | `Rscript tools/manage_users.R add …` |
| Carte vide | GPS absents ou GADM non chargé | Vérifier les colonnes GPS et le cache GADM |
| Conflit de casse Windows | `data/` vs `DATA/` | Le dossier ML a été renommé `ml_data/` à la racine du monorepo |

---

<a id="flux-travail"></a>
## 10. Flux de travail type

### 10.1 Import et mapping

1. Onglet **Import & Mappeur** → *Fichier local* ou *Kobo/ODK*.
2. Charger le fichier (CSV, Excel, SPSS, Stata, Parquet).
3. Vérifier la détection automatique des colonnes (vert = détecté, jaune = à
   vérifier, rouge = non trouvé).
4. Corriger le mapping avec le dictionnaire OMS si nécessaire.
5. Appliquer le mappeur : les données deviennent `mapped_data` puis `df_f`.

### 10.2 Contrôle qualité

1. Onglet **Qualité SMART** : flags, plausibilité, digit preference, age heaping,
   complétude.
2. Onglet **Qualité automatisée** : score global et anomalies par grappe.
3. Décider des exclusions ou corrections **avant** les analyses ; documenter
   chaque décision.

### 10.3 Analyse

1. **Z-scores OMS 2006** pour calculer WHZ/HAZ/WAZ et les flags.
2. **Prévalences SMART** pour les estimations pondérées et les IC design-based.
3. **Indicateurs OMS/PAM** pour GAM/SAM/MAM, stunting, MUAC et FCS/HHS.
4. **Statistiques** (univarié, bivarié, descriptif, inférentiel) pour les
   comparaisons.
5. **Épidémiologie**, **survie** ou **modèle prédictif** selon la question.
6. **Cartographie** pour la dimension géographique.

### 10.4 Rapport

1. Onglet **Rapport exportable** : titre, période, sections, format.
2. Choisir Word, PDF, Excel ou HTML.
3. Vérifier les tableaux, graphiques et interprétations.
4. Archiver le rapport avec la version des données et le commit Git.

### 10.5 Conventions de données

| Variable | Convention attendue |
|---|---|
| Âge enfant | mois (`child_age_months`, `age_m`) |
| Poids | kg |
| Taille | cm |
| MUAC | mm |
| Sexe | `M`/`F` ou codage documenté |
| Z-scores | `whz`, `haz`, `waz` ou recalcul par LMS |
| Grappe / strate | colonnes explicites pour le design-based |
| Poids de sondage | colonne de poids optionnelle |
| GPS | latitude/longitude en degrés décimaux |
| Administration | région, département, district, village |
| Valeurs manquantes | `NA` ou code documenté |

Le dictionnaire de variables (`NutriAnalyse_Dictionnaire_Variables.qmd` /
`.docx`) et les fichiers `Variables_OMS*.docx` détaillent les alias reconnus.

---

<a id="documentation"></a>
## 11. Documentation

| Document | Contenu |
|---|---|
| **README.md** | Ce guide : vue d'ensemble, méthodologie, installation, sécurité |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Découpage, conventions, contrat des modules, performances |
| [SECURITY_GUIDE.md](SECURITY_GUIDE.md) | Clés API, comptes, production, diffusion des résultats |
| [SECURITY_CHECKLIST.md](SECURITY_CHECKLIST.md) | Checklist opérationnelle avant commit/déploiement |
| [INSTRUCTIONS_LANCEMENT.md](INSTRUCTIONS_LANCEMENT.md) | Lancement détaillé et dépannage |
| [tools/README.md](tools/README.md) | Rôle de chaque script de vérification |
| [app_parts/server/README.md](app_parts/server/README.md) | Description des 20 fichiers serveur |
| [README_v2.md](README_v2.md) | Documentation historique de la v2 initiale (archive) |
| [ADVANCED_MODULES_README.md](ADVANCED_MODULES_README.md) | Modules avancés et manifeste |
| [ZSCORE_EXPERT_GUIDE.md](ZSCORE_EXPERT_GUIDE.md) | Guide expert des z-scores |
| [ZSCORE_USER_MANUAL.md](ZSCORE_USER_MANUAL.md) | Manuel utilisateur des z-scores |
| [DOCUMENTATION_COMPLETE_NUTRIANALYSE_PRO_V2.md](DOCUMENTATION_COMPLETE_NUTRIANALYSE_PRO_V2.md) | Documentation fonctionnelle complète |
| [API_SETUP_INSTRUCTIONS.md](API_SETUP_INSTRUCTIONS.md) | Configuration de l'API Gemini |
| [INTEGRATION_GADM_WORLDWIDE.md](INTEGRATION_GADM_WORLDWIDE.md) | Cartographie GADM mondiale |
| [NutriAnalyse_Dictionnaire_Variables.qmd](NutriAnalyse_Dictionnaire_Variables.qmd) | Dictionnaire des variables |
| [../.github/workflows/ci.yml](../.github/workflows/ci.yml) | Workflow CI du monorepo |

---

<a id="limites"></a>
## 12. Limites, bonnes pratiques et pistes

### 12.1 Limites assumées

- **Données de démonstration synthétiques** : aucun résultat réel ne doit être
  inféré des CSV fournis.
- **Tables LMS intégrées et interpolées** : pour une publication ou une
  certification, contre-vérifier avec WHO Anthro / `anthro`.
- **Packages optionnels** : certaines analyses (spatial, NLP, connecteurs)
  dépendent de packages ou de services externes ; l'interface signale les
  indisponibilités.
- **Export PDF** : dépend de Chrome/Chromium et de Pandoc.
- **Agents IA** : dépendent de Gemini et de la clé ; les données envoyées à l'API
  doivent être compatibles avec la politique de confidentialité du projet.
- **Pas de rôles** : tous les comptes authentifiés ont les mêmes droits ; pour
  une équipe hétérogène, isoler l'application par instance ou par déploiement.
- **Pas un dispositif médical** : les résultats ne constituent pas un diagnostic
  individuel et doivent être validés par un expert.
- **Performance** : le chargement initial est d'environ 18 s ; les jeux de
  données très volumineux doivent être filtrés ou traités en amont.

### 12.2 Bonnes pratiques

- verrouiller `renv.lock` et documenter la version de R ;
- versionner le code et les rapports, pas les secrets ;
- vérifier la qualité des données avant toute analyse ;
- documenter les hypothèses statistiques et les exclusions ;
- activer le masquage des petits effectifs pour toute diffusion ;
- tester les modules après chaque modification structurelle ;
- conserver les rapports avec le commit et la version des données.

### 12.3 Pistes d'évolution

- étendre la couverture de tests aux modules experts, avancés et connecteurs ;
- ajouter des tests de non-régression sur les sorties statistiques ;
- renforcer le déploiement reproductible (image conteneur, documentation
  d'exploitation) ;
- compléter l'internationalisation au-delà de FR/EN si nécessaire ;
- automatiser la comparaison des z-scores avec WHO Anthro sur des jeux de
  référence ;
- ajouter des contrôles de qualité de code statique (lintr) à la CI.

---

<a id="licence"></a>
## 13. Licence, citation et contact

**Licence** : [AGPL-3.0](https://www.gnu.org/licenses/agpl-3.0.html). L'usage en
réseau implique les obligations de l'AGPL (mise à disposition du code source
correspondant aux utilisateurs du service).

**Citation**

```text
YIMGA WANDJI, K. (2026). NutriAnalyse Pro v2 (Version 2.0.0)
[Computer software]. DSAMID Data Science & Analysis.
https://github.com/yimgakevin/machine-learning/tree/main/nutrianalyse
```

**Auteur** : YIMGA WANDJI KEVIN — ORCID
[0009-0006-1000-7339](https://orcid.org/0009-0006-1000-7339) —
contact@dsamid.com

**Dépôt** : [yimgakevin/machine-learning](https://github.com/yimgakevin/machine-learning)

---

*Dernière mise à jour : novembre 2026.*
