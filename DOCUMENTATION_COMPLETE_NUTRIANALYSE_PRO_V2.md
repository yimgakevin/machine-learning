---
output:
  word_document: default
  html_document: default
---
# DOCUMENTATION COMPLÈTE — NutriAnalyse Pro v2

**Plateforme d'Analyse Nutritionnelle et Épidémiologique Universelle**
**Standards OMS / UNICEF / PAM — Niveau Publication Scientifique**
**Version:** 2.0 — **Date:** 25 mars 2026
**Auteur technique:** Kevin Yimga — DSAMID Data Science & Analysis
**Technologie:** R Shiny (shinydashboard) · R 4.5.2+

---

# TABLE DES MATIÈRES

1. [Vue d'ensemble de l'application](#1-vue-densemble)
2. [Architecture technique](#2-architecture-technique)
3. [Installation et lancement](#3-installation-et-lancement)
4. [Structure des fichiers](#4-structure-des-fichiers)
5. [Module 1 — Tableau de bord (accueil)](#5-module-1--tableau-de-bord)
6. [Module 2 — Import & Mappeur intelligent](#6-module-2--import--mappeur-intelligent)
7. [Module 3 — KoboToolbox / ODK](#7-module-3--kobotoolbox--odk)
8. [Module 4 — Exploration interactive](#8-module-4--exploration-interactive)
9. [Module 5 — Manipulation dplyr](#9-module-5--manipulation-dplyr)
10. [Module 6 — Graphiques & Stats : Analyse univariée](#10-module-6--analyse-univariée)
11. [Module 7 — Graphiques & Stats : Analyse bivariée](#11-module-7--analyse-bivariée)
12. [Module 8 — Graphiques & Stats : Tableau descriptif OMS](#12-module-8--tableau-descriptif-oms)
13. [Module 9 — Statistiques inférentielles](#13-module-9--statistiques-inférentielles)
14. [Module 10 — Réduction de dimension](#14-module-10--réduction-de-dimension)
15. [Module 11 — Clustering](#15-module-11--clustering)
16. [Module 12 — Qualité SMART](#16-module-12--qualité-smart)
17. [Module 13 — Prévalences SMART](#17-module-13--prévalences-smart)
18. [Module 14 — Sécurité Alimentaire](#18-module-14--sécurité-alimentaire)
19. [Module 15 — Z-Scores OMS 2006](#19-module-15--z-scores-oms-2006)
20. [Module 16 — Indicateurs OMS / PAM / UNICEF](#20-module-16--indicateurs-oms--pam--unicef)
21. [Module 17 — Cartographie](#21-module-17--cartographie)
22. [Module 18 — Épidémiologie](#22-module-18--épidémiologie)
23. [Module 19 — Analyse de survie](#23-module-19--analyse-de-survie)
24. [Module 20 — Modèle prédictif](#24-module-20--modèle-prédictif)
25. [Module 21 — Rapport exportable](#25-module-21--rapport-exportable)
26. [Niveau Expert — Dashboard Exécutif](#26-niveau-expert--dashboard-exécutif)
27. [Niveau Expert — Qualité Automatisée](#27-niveau-expert--qualité-automatisée)
28. [Niveau Expert — Audit Trail & Traçabilité](#28-niveau-expert--audit-trail--traçabilité)
29. [Niveau Expert — Tests Statistiques Temporels](#29-niveau-expert--tests-statistiques-temporels)
30. [Recherche Avancée — Prévisions temporelles](#30-recherche-avancée--prévisions-temporelles)
31. [Recherche Avancée — Géolocalisation](#31-recherche-avancée--géolocalisation)
32. [Recherche Avancée — Analyse textes (NLP)](#32-recherche-avancée--analyse-textes-nlp)
33. [Recherche Avancée — Benchmarking](#33-recherche-avancée--benchmarking)
34. [Page À propos](#34-page-à-propos)
35. [Système de filtres globaux](#35-système-de-filtres-globaux)
36. [Design System — CSS & Palette OMS](#36-design-system)
37. [Référence des fonctions par fichier](#37-référence-des-fonctions)
38. [Standards et seuils OMS/PAM/UNICEF](#38-standards-et-seuils)
39. [Dépendances R](#39-dépendances-r)
40. [Dépannage et erreurs courantes](#40-dépannage)
41. [Annexe A — Compteur de lignes de code](#annexe-a)
42. [Annexe B — Glossaire](#annexe-b)

---

# 1. VUE D'ENSEMBLE

## 1.1 Qu'est-ce que NutriAnalyse Pro v2 ?

NutriAnalyse Pro v2 est une application Shiny complète de niveau publication scientifique et thèse de doctorat, conçue pour l'analyse nutritionnelle, épidémiologique et statistique dans les contextes humanitaires et de recherche. Elle intègre les standards officiels de l'OMS, de l'UNICEF et du PAM pour :

- **Importer** des données depuis de multiples sources (CSV, Excel, SPSS, Stata, Parquet, KoboToolbox/ODK)
- **Explorer** les données de manière interactive avec statistiques automatiques et graphiques adaptatifs
- **Manipuler** les données via une interface visuelle dplyr complète (14 opérations)
- **Visualiser** avec 26+ types de graphiques (univariés, bivariés, interactifs)
- **Tester** des hypothèses avec 15+ tests statistiques et vérification automatique des conditions
- **Réduire** la dimensionnalité (ACP, ACM, FAMD, AFC) et **Clusteriser** (K-means, CAH, DBSCAN)
- **Calculer** les Z-scores selon les tables OMS 2006 (WHZ, HAZ, WAZ) avec méthode LMS Box-Cox
- **Évaluer** la qualité des données selon la méthodologie SMART/ENA (10 critères)
- **Calculer** les prévalences pondérées avec DEFF et IC 95% design-based
- **Analyser** la sécurité alimentaire (FCS, rCSI, HDDS, HHS — standards PAM/FAO)
- **Cartographier** avec 15 sous-onglets et 4 tiers d'analyse spatiale (LISA, Moran's I, priorisation, prédiction)
- **Analyser** les associations épidémiologiques (OR, RR, PAF, Mantel-Haenszel, tendances)
- **Modéliser** la survie (Kaplan-Meier, Cox PH, tables de vie, diagnostics Schoenfeld)
- **Prédire** avec Machine Learning (GLM, Random Forest, XGBoost — binaire et multiclasse)
- **Prévoir** des séries temporelles (ARIMA, ETS, diagnostics ACF/PACF)
- **Analyser** les textes par NLP (sentiment, LDA, word clouds)
- **Comparer** les performances par benchmarking (interne, externe, gap analysis)
- **Auditer** chaque action avec un audit trail complet et hashing MD5
- **Générer** des rapports exportables en Word, PDF, HTML et Excel (13 sections configurables)

## 1.2 Public cible

| Profil | Usage principal |
|--------|----------------|
| Nutritionniste terrain | Import données, calcul prévalences, sécurité alimentaire, rapport |
| Coordinateur enquête SMART | Contrôle qualité, plausibilité, flags, Z-scores |
| Analyste PAM/UNICEF | Sécurité alimentaire, cartographie, priorisation, indicateurs |
| Épidémiologiste | Associations (OR/RR), analyse de survie, tests statistiques |
| Chercheur / Doctorant | Statistiques inférentielles, modélisation prédictive, réduction de dimension |
| Data Scientist humanitaire | NLP, forecasting, benchmarking, Machine Learning |
| Responsable programme | Dashboard exécutif, audit trail, tests temporels entre vagues |

## 1.3 Vue d'ensemble des modules (28+ onglets, 11 sections)

| # | Section | Onglet (tabName) | Fonction principale |
|---|---------|------------------|---------------------|
| 1 | — | Tableau de bord (`accueil`) | KPIs, jauges OMS, alertes, seuils de référence |
| 2 | — | Import & Mappeur (`import`) | CSV, Excel, SPSS, Stata, Parquet, KoboToolbox |
| 3 | — | Exploration (`explor`) | Table interactive, statistiques, graphique auto |
| 4 | — | Manipulation dplyr (`dplyr`) | 14 opérations visuelles dplyr |
| 5 | Graphiques & Stats | Univariée (`uni`) | 13 types de graphiques |
| 6 | Graphiques & Stats | Bivariée (`biv`) | 13 combinaisons quali/quanti |
| 7 | Graphiques & Stats | Tableau descriptif (`descoms`) | gtsummary, Spearman, Z-scores par groupe |
| 8 | — | Inférentielle (`infer`) | 15+ tests + régression logistique multivariée |
| 9 | — | Réduction de dimension (`rdim`) | ACP, ACM, FAMD, AFC (FactoMineR) — 7 onglets |
| 10 | — | Clustering (`clust`) | K-means, CAH Ward.D2, DBSCAN |
| 11 | — | Qualité SMART (`qualite`) | 10 critères de plausibilité |
| 12 | — | Prévalences SMART (`prevalence`) | Pondération + DEFF + IC 95% design-based |
| 13 | — | Sécurité Alimentaire (`secu_alim`) | FCS, rCSI, HDDS, HHS (PAM/FAO) |
| 14 | — | Z-scores OMS 2006 (`zscore`) | Calcul LMS + 10 critères expert + 7 onglets |
| 15 | — | Indicateurs OMS/PAM (`oms`) | Forest plots, jauges, distributions |
| 16 | — | Cartographie (`carto`) | 15 sous-onglets, 4 tiers spatial |
| 17 | — | Épidémiologie (`epidemio`) | 2×2, multi-exposition, MH, tendances |
| 18 | — | Analyse de survie (`survie`) | Kaplan-Meier, Cox PH, tables de vie |
| 19 | — | Modèle prédictif (`predictif`) | GLM, RF, XGBoost (binaire + multiclasse) |
| 20 | — | Rapport exportable (`rapport`) | Word, PDF, HTML, Excel — 13 sections |
| 21 | Niveau Expert | Dashboard Exécutif (`exec_dash`) | Score global 0-100, h

# 5. MODULE 1 — TABLEAU DE BORD

**Onglet :** `accueil` · **Icône :** 🏠 · **Sélectionné par défaut au lancement**

## 5.1 Description

Le tableau de bord est la page d'accueil de NutriAnalyse Pro. Il offre une vue synthétique et immédiate de la situation nutritionnelle dès qu'un jeu de données est importé. Conçu selon les standards visuels OMS/UNICEF, il agrège automatiquement les indicateurs clés pour permettre une prise de décision rapide.

## 5.2 Composants de l'interface

### Bandeau héro
- Gradient bleu-vert aux couleurs OMS (#00426A → #008DC9 → #1D9E75)
- Titre de l'application, version et description
- Badge dynamique indiquant l'état des données (nombre d'observations, colonnes détectées)

### Alertes OMS automatiques
- Génération automatique d'alertes colorées selon les seuils OMS
- Code couleur : 🟢 Acceptable · 🟡 Alerte · 🟠 Sérieux · 🔴 Critique
- Chaque alerte indique l'indicateur concerné, la valeur observée et le seuil franchi

### KPI Cards (2 rangées)
- **Rangée 1** : Indicateurs anthropométriques principaux (GAM, SAM, Stunting, Underweight)
- **Rangée 2** : Indicateurs complémentaires (MUAC_GAM, FCS moyen, HHS moyen, N total)
- Chaque card affiche la valeur, l'IC 95%, le code couleur OMS et une icône contextuelle

### Graphique des prévalences par groupe (IC 95% Wilson)
- Diagramme en barres interactif (plotly) montrant les prévalences avec barres d'erreur
- Intervalles de confiance calculés par la méthode de Wilson
- Survol affichant les détails (N, cas, IC bas/haut)

### Jauge GAM — Seuil OMS
- Jauge circulaire indiquant le niveau de GAM par rapport aux seuils OMS
- Zones colorées : vert (< 5%), jaune (5-10%), orange (10-15%), rouge (≥ 15%)
- Badge de sévérité dynamique sous la jauge

### Distribution des scores Z vs référence OMS N(0,1)
- Graphique de densité superposant la distribution observée des Z-scores (WHZ)
- Courbe de référence N(0,1) en pointillés
- Zones ombrées indiquant les seuils de malnutrition (-3 SD, -2 SD)

### Tableau de référence seuils OMS/SMART
- Tableau statique à 5 lignes présentant les seuils de classification
- Codes couleur : 🟢 < 5% → 🟡 5-9.9% → 🟠 10-14.9% → 🔴 15-29.9% → 🟥 ≥ 30%
- Colonnes : Couleur, GAM, Sévérité, Action requise

---

# 6. MODULE 2 — IMPORT & MAPPEUR INTELLIGENT

**Onglet :** `import` · **Icône :** ⚙️ · **Fichiers :** `utils.R`, section app_v2_final.R

## 6.1 Description

Ce module constitue la porte d'entrée de l'application. Il gère l'importation des données depuis de multiples formats et effectue une détection automatique intelligente des colonnes SMART/OMS grâce au dictionnaire `OMS_DICT`.

## 6.2 Import multi-format

### Zone d'upload dans la sidebar
- Zone drag-and-drop stylisée avec bordure pointillée
- Accepte : `.csv`, `.tsv`, `.xlsx`, `.xls`, `.sav`, `.dta`, `.parquet`
- Indicateur de statut après import (nombre de lignes × colonnes)

### Formats supportés

| Format | Extension | Bibliothèque R | Notes |
|--------|-----------|----------------|-------|
| CSV / TSV | `.csv`, `.tsv` | `readr` | Auto-détection séparateur (`;` ou `,`) et décimal |
| Excel | `.xlsx`, `.xls` | `readxl` | Première feuille par défaut |
| SPSS | `.sav` | `haven` | Conversion labels → factors automatique |
| Stata | `.dta` | `haven` | Support des formats Stata 12-17 |
| Parquet | `.parquet` | `arrow` | Format columnar haute performance |

### Nettoyage automatique à l'import
- Noms de colonnes convertis en minuscules
- Espaces remplacés par des underscores
- Détection automatique du séparateur décimal
- Suppression des espaces en tête/queue des valeurs texte

## 6.3 Détection automatique des colonnes (OMS_DICT)

Le dictionnaire `OMS_DICT` contient **17 variables SMART** avec **50+ alias** en anglais, français et espagnol :

| Variable | Exemples d'alias détectés |
|----------|---------------------------|
| `whz` | whz_score, wfh, weight_for_height_z, peso_talla_z, zscore_whz |
| `haz` | haz_score, hfa, height_for_age_z, talla_edad_z, zscore_haz |
| `waz` | waz_score, wfa, weight_for_age_z, peso_edad_z, zscore_waz |
| `muac` | muac_mm, pb, pbm, mid_upper_arm_circumference, perimeter_brachial |
| `oedema` | oedema, oedeme, bilateral_oedema, edema |
| `age_m` | child_age_months, age_mois, agemos, age_en_mois |
| `sexe` | child_sex, sex, gender, genre, sexe |
| `region` | region, admin1, wilaya, gouvernorat, province |
| `lat` | gps_latitude, latitude, coord_lat, _gps_latitude |
| `lon` | gps_longitude, longitude, coord_lon, _gps_longitude |
| `poids` | weight_kg, weight, poids, peso |
| `taille` | height_cm, height, taille, talla |
| `fcs` | food_consumption_score, fcs, score_conso |
| `hhs` | household_hunger_scale, hhs, score_faim |
| `csi` | coping_strategy_index, csi, rcsi |
| `depl` | displacement_status, statut_deplacement, idp_status |
| `grappe` | cluster, grappe, cluster_id, psu |

La fonction `auto_detect(df)` applique une détection en **3 niveaux** :
1. **Correspondance exacte** : nom de colonne identique à un alias
2. **Pattern regex** : recherche par expressions régulières
3. **Sous-chaîne** : détection d'une partie du nom de colonne

### Interface du mappeur
- **Deux colonnes** affichant toutes les variables SMART à mapper
- **Badges de statut** : 🟢 Détecté automatiquement · 🟡 À vérifier · 🔴 Non trouvé
- **Sélecteurs manuels** pour corriger ou ajuster chaque mapping
- **Bouton "Appliquer le mapping"** pour valider et calculer les indicateurs

## 6.4 Calcul des indicateurs (calc_indicateurs)

Après le mapping, la fonction `calc_indicateurs(df, mapping)` ajoute **25+ colonnes calculées** :

**Malnutrition aiguë (WHZ) :**
- `GAM` : WHZ < -2 OU œdèmes bilatéraux (Malnutrition Aiguë Globale)
- `SAM` : WHZ < -3 OU œdèmes bilatéraux (Sévère)
- `MAM` : -3 ≤ WHZ < -2, sans œdèmes (Modérée)

**Retard de croissance (HAZ) :**
- `Stunting` : HAZ < -2
- `Sev_Stunting` : HAZ < -3

**Insuffisance pondérale (WAZ) :**
- `Underweight` : WAZ < -2
- `Sev_Underweight` : WAZ < -3

**MUAC (Périmètre brachial) :**
- `MUAC_GAM` : MUAC < 125 mm
- `MUAC_SAM` : MUAC < 115 mm

**Classifications catégorielles :**
- `nutr_cat` : Normal / MAM / SAM (basé sur WHZ + œdèmes)
- `muac_cat` : Normal / MAM / SAM (basé sur MUAC)
- `age_group` : 0-5m, 6-11m, 12-23m, 24-35m, 36-47m, 48-59m, 60+m
- `fcs_cat` : Pauvre / Limite / Acceptable (basé sur FCS)
- `hhs_cat` : Aucune faim / Légère / Modérée / Sévère (basé sur HHS)
- `ICV` : Indice Composite de Vulnérabilité (0-100)

---

# 7. MODULE 3 — KOBOTOOLBOX / ODK

**Onglet :** `import` (sous-onglet "KoboToolbox / ODK") · **Fichier :** `module_kobo_import.R` (339 lignes, 10 fonctions)

## 7.1 Description

Ce sous-module permet de se connecter directement à un compte KoboToolbox ou ODK pour importer les données d'enquête sans export manuel. Il supporte les serveurs OCHA (humanitarianresponse.info) et KoboToolbox.org, ainsi que les serveurs personnalisés.

## 7.2 Serveurs pré-configurés

| Serveur | URL | Usage |
|---------|-----|-------|
| OCHA (humanitaire) | `https://kobo.humanitarianresponse.info` | Enquêtes humanitaires UN |
| KoboToolbox (global) | `https://kf.kobotoolbox.org` | Usage général |
| Personnalisé | URL libre | Serveurs organisationnels |

## 7.3 Pipeline API complet

```
1. kobo_test_connection(url, token)
   → Valider les identifiants API (test GET /api/v2/assets)
        ↓
2. kobo_list_forms(url, token)
   → Lister les formulaires déployés avec nombre de soumissions
        ↓
3. kobo_download_data(url, token, uid)
   → Télécharger les données (max 30 000 soumissions, pagination auto)
        ↓
4. kobo_json_to_df(results)
   → Convertir JSON brut → data.frame R structuré
        ↓
5. kobo_clean_columns(df)
   → Supprimer 14 colonnes système, nettoyer les noms
        ↓
6. kobo_auto_map(col_names)
   → Détecter 14+ variables SMART (50+ alias multilingues)
```

## 7.4 Interface utilisateur

- **Connexion** : Champ serveur (dropdown), champ token API, bouton "Tester la connexion"
- **Formulaires** : Liste des formulaires avec nom, date de déploiement, nombre de soumissions
- **Téléchargement** : Sélection du formulaire, bouton "Télécharger les 

# 10. MODULE 6 — ANALYSE UNIVARIÉE

**Onglet :** `uni` · **Icône :** 📊 · **Sous-menu :** Graphiques & Stats

## 10.1 Description

Ce module permet d'analyser une variable à la fois avec 13 types de graphiques différents, des statistiques descriptives complètes et un mode interactif ggiraph. Il adapte automatiquement les options selon que la variable est qualitative ou quantitative.

## 10.2 Les 13 types de graphiques

### Variables qualitatives
| Type | Description |
|------|-------------|
| **Barplot (effectifs)** | Diagramme en barres classique avec comptage |
| **Barplot (proportions %)** | Diagramme en barres avec pourcentages |
| **Camembert / Donut** | Diagramme circulaire ou en anneau |

### Variables quantitatives
| Type | Description |
|------|-------------|
| **Histogramme + Gauss** | Histogramme avec courbe normale superposée |
| **Boxplot annoté** | Boîte à moustaches avec valeurs des quartiles |
| **Violin + Boxplot** | Forme de distribution + boîte à moustaches combinées |
| **Courbe de densité** | Estimation par noyau de la densité |
| **Courbe ridgeline** | Densité de style ridge (pour groupes) |
| **QQ-plot (normalité)** | Graphique quantile-quantile pour vérifier la normalité |
| **Densité vs OMS N(0,1)** | Distribution observée superposée à la référence OMS |

### Combinés
| Type | Description |
|------|-------------|
| **Histogramme + densité** | Histogramme avec courbe de densité superposée |
| **Boxplot + points (jitter)** | Boîte à moustaches avec les points individuels |

## 10.3 Options

- **Sélection de variable** : dropdown dynamique selon les données chargées
- **Palette de couleurs** : OMS officiel, UNICEF bleu, Viridis, Spectral, RdYlGn, Gradient bleu/rouge
- **Afficher effectifs/valeurs** : annotations sur le graphique (checkbox)
- **Mode ggiraph** : interactivité hover+ avec survol détaillé (checkbox)
- **Télécharger HD** : export haute définition du graphique courant

## 10.4 Statistiques complètes

Le panneau latéral affiche les 19 statistiques calculées automatiquement (cf. section Exploration) pour la variable sélectionnée, défilables si nombreuses.

---

# 11. MODULE 7 — ANALYSE BIVARIÉE

**Onglet :** `biv` · **Icône :** 📊 · **Sous-menu :** Graphiques & Stats

## 11.1 Description

Ce module étudie la relation entre deux variables avec 13 types de graphiques adaptés à toutes les combinaisons possibles (quali × quali, quali × quanti, quanti × quanti). Un test statistique automatique est exécuté en parallèle pour quantifier l'association.

## 11.2 Les 13 combinaisons de graphiques

### Quali × Quali
| Type | Description |
|------|-------------|
| **Barplot groupé (dodge)** | Barres côte à côte par modalité |
| **Barplot empilé (%)** | Barres empilées normalisées en pourcentage |
| **Heatmap fréquences** | Matrice colorée des fréquences croisées |
| **Mosaïque (proportions)** | Graphique mosaïque proportionnel |

### Quali × Quanti
| Type | Description |
|------|-------------|
| **Boxplot par groupe** | Boîtes à moustaches côte à côte par catégorie |
| **Violin par groupe** | Distributions en violon par catégorie |
| **Densités superposées** | Courbes de densité empilées par groupe |
| **Histogrammes facettes** | Histogrammes en sous-panneaux (facet_wrap) |
| **ggdist (halfeye)** | Distribution demi-œil combinant densité et intervalle |

### Quanti × Quanti
| Type | Description |
|------|-------------|
| **Nuage de points + LOESS** | Scatter plot avec courbe de tendance locale |
| **Hexbin (densité 2D)** | Bins hexagonaux pour visualiser la densité |
| **Heatmap corrélations** | Matrice de corrélation pour plusieurs variables |

## 11.3 Test automatique

Le panneau latéral affiche automatiquement le test statistique approprié à la combinaison de variables :

| Combinaison | Test exécuté | Mesure d'association |
|-------------|-------------|---------------------|
| Quali × Quali | Chi² de Pearson | V de Cramér |
| Quali (2 gr.) × Quanti | Mann-Whitney / t-test | d de Cohen |
| Quali (≥3 gr.) × Quanti | Kruskal-Wallis / ANOVA | η² |
| Quanti × Quanti | Corrélation Spearman | ρ |

Le résultat inclut :
- La statistique du test et la p-value
- L'interprétation en langage clair (significatif ou non)
- La taille d'effet avec sa classification (faible, moyen, fort)

---

# 12. MODULE 8 — TABLEAU DESCRIPTIF OMS

**Onglet :** `descoms` · **Icône :** 📊 · **Sous-menu :** Graphiques & Stats

## 12.1 Description

Ce module génère un tableau récapitulatif professionnel de type publication scientifique selon les standards OMS, en utilisant le package `gtsummary`. Il permet de stratifier les résultats par un groupe et d'ajouter automatiquement les p-values des tests de comparaison.

## 12.2 Composants

### Tableau gtsummary
- **Variable de stratification** : dropdown pour choisir le groupe (sexe, région, etc.)
- **Variables à inclure** : multi-sélection des colonnes à résumer
- **Option p-values** : ajout automatique des tests (t-test, Chi², etc.)
- **Export Word** : téléchargement direct en format `.docx` stylé

Le tableau affiche pour chaque variable :
- **Quantitatives** : Moyenne ± ET ou Médiane [Q1-Q3] selon la distribution
- **Qualitatives** : N (%) par modalité
- **P-value** : test approprié auto-sélectionné (t-test, Wilcoxon, Chi², Fisher)

### Matrice de corrélation Spearman
- Sélection des variables numériques à inclure
- Heatmap colorée (corrplot) avec coefficients
- Croix sur les corrélations non-significatives (p > 0.05)

### Scores Z par groupe vs seuils OMS
- Graphique interactif (plotly) montrant les Z-scores moyens par groupe
- Lignes horizontales indiquant les seuils OMS (-2 SD, -3 SD)
- Sélecteur de variable de groupage

---

# 13. MODULE 9 — STATISTIQUES INFÉRENTIELLES

**Onglet :** `infer` · **Icône :** 🧪 · **Fichier :** `inferentiel_v4.R` (~1 000 lignes, 15 fonctions)

## 13.1 Description

Ce module exécute automatiquement tous les tests statistiques appropriés pour une paire de variables. Il vérifie les hypothèses de chaque test (normalité, homoscédasticité), calcule les tailles d'effet et fournit des interprétations en langage clair. Deux sous-onglets : tests bivariés et régression logistique multivariée.

## 13.2 Sous-onglet "Tests bivariés"

### Sélection automatique des tests

La fonction `run_all_tests(d, var_x, var_y, alpha)` détecte les types de variables et exécute simultanément tous les tests pertinents :

| Combinaison | Tests exécutés | Taille d'effet |
|-------------|---------------|----------------|
| **Quali × Quali** | Chi² de Pearson, Fisher exact | V de Cramér |
| **Quali (2 gr.) × Quanti** | t-test Student, t-test Welch, Mann-Whitney U | d de Cohen, r-rank biserial |
| **Quali (≥3 gr.) × Quanti** | ANOVA Fisher, ANOVA Welch, Kruskal-Wallis | η², ε² |
| **Quanti × Quanti** | Pearson r, Spearman ρ, Kendall τ | r, ρ, τ avec IC 95% |

### Vérification des hypothèses

| Hypothèse | Test utilisé | Seuil | Interprétation |
|-----------|-------------|-------|----------------|
| Normalité | Shapiro-Wilk (n ≤ 5000) | p > 0.05 | ✅ Distribution normale |
| Homoscédasticité | Levene (via `car`) | p > 0.05 | ✅ Variances homogènes |
| Indépendance | Vérifié structurellement | — | ✅ Observations indépendantes |

### Interprétation des tailles d'effet

| Mesure | Très faible | Faible | Moyen | Fort |
|--------|-----------|--------|-------|------|
| V de Cramér | < 0.10 | < 0.20 | < 0.40 | ≥ 0.40 |
| d de Cohen | < 0.20 | < 0.50 | < 0.80 | ≥ 0.80 |
| η² (ANOVA) | < 0.01 | < 0.06 | < 0.14 | ≥ 0.14 |
| ε² (K-W) | < 0.01 | < 0.06 | < 0.14 | ≥ 0.14 |
| Corrélation r | < 0.10 | < 0.30 | < 0.50 | ≥ 0.70 |

### Interface des résultats

- **Badge type** : identification automatique de la combinaison (Quali×Quali, etc.)
- **Tableau récapitulatif** (DT) : tous les tests avec test, statistique, p-value, effet, verdict
- **Cartes détaillées** : pour chaque test, une carte HTML avec statut ✅/❌, conditions vérifiées, interprétation
- **Graphique adaptatif** (plotly) : visualisation automatique selon le type de variables
- **Seuil α configurable** : slider de 0.01 à 0.10 (défaut 0.05)

### Lég

# 14. MODULE 10 — RÉDUCTION DE DIMENSION

**Onglet :** `rdim` · **Icône :** 🔬 · **Bibliothèque :** FactoMineR + factoextra

## 14.1 Description

Ce module simplifie des données multivariées complexes en identifiant les axes principaux de variation. Il supporte 4 méthodes factorielles avec détection automatique de la méthode optimale selon le type de données, et offre 7 onglets de résultats interactifs.

## 14.2 Les 4 méthodes factorielles

| Méthode | Acronyme | Type de données | Fonction FactoMineR |
|---------|----------|----------------|---------------------|
| **Analyse en Composantes Principales** | ACP / PCA | Variables numériques | `PCA()` |
| **Analyse des Correspondances Multiples** | ACM / MCA | Variables catégorielles | `MCA()` |
| **Analyse Factorielle de Données Mixtes** | FAMD | Variables numériques + catégorielles | `FAMD()` |
| **Analyse Factorielle des Correspondances** | AFC / CA | Tableau de contingence | `CA()` |

### Mode Auto
Lorsque "Auto" est sélectionné, l'application détecte automatiquement :
- **100% numériques** → ACP
- **100% catégorielles** → ACM
- **Mélange** → FAMD
- **Tableau de contingence** → AFC

## 14.3 Paramètres

- **Variables à inclure** : multi-sélection des colonnes
- **Dimensions à garder** : 2 à 15 (défaut : 5)
- **Centrer-réduire** : option pour l'ACP (recommandé si échelles différentes)
- **Variable de groupage** : pour colorer le biplot par catégorie
- **Résumé méthode** : affichage du choix de méthode et justification

## 14.4 Les 7 onglets de résultats

| # | Onglet | Contenu |
|---|--------|---------|
| 1 | 📊 **Éboulis des valeurs propres** | Scree plot interactif montrant le % de variance expliquée par axe. Aide à choisir le nombre d'axes à retenir (règle de Kaiser, coude). |
| 2 | 🎯 **Sélection automatique des axes** | Recommandation automatique du nombre d'axes optimal selon Kaiser (eigenvalue > 1), pourcentage cumulé de variance, et règle du coude. Badge Kaiser affiché. |
| 3 | 🔵 **Cercle des corrélations** | Projection des variables sur le plan factoriel. Coloration par Cos² (qualité de représentation), Contribution, ou uniforme. Choix des axes X/Y. Export PNG. |
| 4 | 👥 **Carte des individus** | Projection des observations sur le plan factoriel. Coloration par variable groupe, Cos², ou densité. Ellipses de concentration. Export PNG. |
| 5 | 📈 **Contributions variables** | Barplot horizontal des contributions de chaque variable à un axe choisi. Seuil de contribution moyenne (1/p × 100) affiché optionnellement. Export PNG. |
| 6 | 🎨 **Cos² (qualité)** | Heatmap des Cos² montrant la qualité de représentation de chaque variable sur chaque axe. Plus le Cos² est élevé, mieux la variable est représentée. |
| 7 | 📋 **Tableau des résultats** | Table DT complète avec coordonnées, contributions et Cos² pour toutes les variables/individus. |

## 14.5 Fonctionnalité FactoShiny

Bouton "🌐 Ouvrir FactoShiny" permettant de lancer l'interface interactive FactoShiny pour une exploration avancée dans une fenêtre séparée.

---

# 15. MODULE 11 — CLUSTERING

**Onglet :** `clust` · **Icône :** 🔗

## 15.1 Description

Ce module regroupe automatiquement les observations en profils similaires par classification non supervisée. Trois méthodes sont disponibles avec détection automatique du nombre optimal de clusters. Utile pour identifier des sous-populations, des typologies nutritionnelles ou des profils de vulnérabilité.

## 15.2 Les 3 méthodes de clustering

| Méthode | Description | Paramètre clé |
|---------|-------------|---------------|
| **K-means** | Partitionnement en K groupes minimisant l'inertie intra-classe. Rapide, adapté aux grands jeux de données. | K (nombre de clusters) |
| **CAH (Ward.D2)** | Classification Ascendante Hiérarchique avec critère de Ward. Produit un dendrogramme. Adapté pour visualiser la structure hiérarchique. | K (coupure du dendrogramme) |
| **DBSCAN** | Clustering basé sur la densité. Détecte des clusters de forme arbitraire et identifie les points aberrants. | eps (rayon), minPts (points min.) |

## 15.3 Paramètres

- **Variables** : multi-sélection des colonnes numériques à utiliser
- **Nombre de clusters K** : slider (2-20) ou détection automatique
- **Nombre optimal automatique** : calcul par la méthode de la silhouette moyenne
- **Paramètres DBSCAN** : eps (epsilon, rayon de voisinage) si DBSCAN sélectionné

## 15.4 Onglets de résultats

| # | Onglet | Contenu |
|---|--------|---------|
| 1 | **Visualisation 2D (PCA)** | Projection des clusters sur les 2 premières composantes principales (plotly interactif). Chaque cluster est coloré et entouré d'une ellipse. |
| 2 | **Nombre optimal** | Graphique de la silhouette moyenne par nombre de clusters (2-15). Le K optimal est celui qui maximise la silhouette. |
| 3 | **Dendrogramme (CAH)** | Arbre hiérarchique avec coupure au niveau K choisi. Uniquement pour la méthode CAH. Branches colorées par cluster. |
| 4 | **Profil des clusters** | Barplot des moyennes de chaque variable par cluster. Permet d'interpréter les caractéristiques de chaque profil. |
| 5 | **Tableau des clusters** | Table DT avec chaque observation et son cluster assigné. Exportable. |

## 15.5 Résumé textuel

Le panneau latéral affiche un résumé incluant :
- Le nombre de clusters trouvés
- La taille de chaque cluster (N et %)
- La silhouette moyenne (qualité globale du clustering)
- L'inertie intra-classe (pour K-means)

---

# 16. MODULE 12 — QUALITÉ SMART

**Onglet :** `qualite` · **Icône :** ✅ · **Fichier :** `module_qualite_smart.R` (699 lignes, 13 fonctions)

## 16.1 Description

Ce module implémente l'évaluation de la qualité des données anthropométriques selon la méthodologie **SMART/ENA** (Standardized Monitoring and Assessment of Relief and Transitions). Il vérifie automatiquement 10 critères de plausibilité et génère un score global pour évaluer la fiabilité des données de l'enquête nutritionnelle.

## 16.2 Les 10 critères de plausibilité

| # | Critère | Mesure | 🟢 Excellent | 🟡 Acceptable | 🔴 Problématique |
|---|---------|--------|-------------|--------------|-----------------|
| 1 | **Flags SMART** | % WHZ hors [-5;+5] | ≤ 1.25% | ≤ 2.5% | > 3.75% |
| 2 | **Écart-type WHZ** | SD attendu ~1.0 | 0.8-1.2 | 0.7-1.3 | Hors limites |
| 3 | **Moyenne WHZ** | Moyenne des Z-scores | ±0.25 | ±0.5 | > ±1.0 |
| 4 | **Digit Preference** | DPS (score 0-100) | ≤ 2 | ≤ 6 | > 12 |
| 5 | **Age Heaping** | Ratio multiples de 6 mois | ≤ 1.3 | ≤ 2.0 | > 3.0 |
| 6 | **Sex Ratio** | Rapport M/F | 0.85-1.15 | 0.75-1.25 | Hors limites |
| 7 | **Complétude** | % variables non-NA | ≥ 95% | ≥ 80% | < 60% |
| 8 | **Asymétrie** | Skewness WHZ | < 0.5 | < 1.0 | > 1.0 |
| 9 | **Aplatissement** | Kurtosis WHZ | < 1.0 | < 2.0 | > 2.0 |
| 10 | **SD par équipe** | CV inter-équipes | < 0.2 | 0.2-0.35 | > 0.35 |

### Score de plausibilité global
- **0-9 points** : Excellent 🟢
- **10-14 points** : Bon 🟢
- **15-24 points** : Acceptable 🟡
- **≥ 25 points** : Problématique 🔴

## 16.3 Paramètres d'entrée

### Variables anthropométriques
- WHZ (Poids-pour-Taille Z-score)
- HAZ (Taille-pour-Âge Z-score)
- WAZ (Poids-pour-Âge Z-score)
- MUAC (Périmètre brachial en mm)

### Variables contextuelles
- Sexe, Âge (mois), Poids (kg), Taille (cm), Équipe/Enquêteur

## 16.4 Les 6 onglets de résultats

| # | Onglet | Contenu |
|---|--------|---------|
| 1 | 🏆 **Score de plausibilité** | Tableau HTML détaillé des 10 critères avec score, emoji de qualité, et classification globale |
| 2 | 🚩 **SMART Flags** | Table DT des observations flaggées + histogramme de distribution des WHZ avec zones de flags |
| 3 | **#** **Digit Preference** | Sélecteur de variable, table des fréquences par dernier chiffre, barplot de distribution |
| 4 | 👶 **Age Heaping** | Résumé de l'indice d'accumulation des âges + histogramme de la distribution des âges |
| 5 | 👥 **SD par équipe** | Table DT des SD par équipe/enquêteur + barplot comparatif |
| 6 | 📋 **Complétude** | Table DT du % de valeurs non-manquantes par variable clé |

## 16.5 Fonctions principales

| Fonction | But |
|----------|-----|
| `

# 18. MODULE 14 — SÉCURITÉ ALIMENTAIRE

**Onglet :** `secu_alim` · **Icône :** 🍽️ · **Fichier :** `module_secu_alimentaire.R` (524 lignes, 11 fonctions)

## 18.1 Description

Ce module calcule les 4 indicateurs clés de sécurité alimentaire selon les standards officiels du PAM et de la FAO : Food Consumption Score (FCS), reduced Coping Strategy Index (rCSI), Household Dietary Diversity Score (HDDS) et Household Hunger Scale (HHS). Chaque indicateur est documenté avec ses formules, pondérations et seuils de classification.

## 18.2 Modes de calcul

| Mode | Description |
|------|-------------|
| **FCS pré-calculé** | Utilise une colonne FCS existante dans les données |
| **Calculer FCS** | Calcule le FCS depuis les 8 groupes alimentaires |
| **rCSI** | Calcule l'indice des stratégies de survie |
| **HDDS** | Calcule le score de diversité alimentaire |
| **HHS** | Calcule l'échelle de la faim des ménages |
| **Tous les indicateurs** | Calcule tous les indicateurs disponibles simultanément |

## 18.3 Les 4 indicateurs en détail

### FCS — Food Consumption Score (PAM)

**Principe :** Somme pondérée du nombre de jours de consommation (0-7) par groupe alimentaire sur les 7 derniers jours.

**Pondérations PAM officielles :**

| Groupe alimentaire | Poids | Justification |
|-------------------|-------|---------------|
| Céréales / Tubercules | 2 | Énergie de base |
| Légumineuses / Noix | 3 | Protéines végétales |
| Légumes | 1 | Micronutriments |
| Fruits | 1 | Vitamines |
| Viande / Poisson / Œufs | 4 | Protéines animales (haute valeur) |
| Produits laitiers | 4 | Protéines + calcium |
| Sucre | 0.5 | Énergie vide |
| Huile / Graisse | 0.5 | Lipides |

**Formule :** `FCS = Σ (jours_consommation × poids)` pour chaque groupe

**Classification :**
| Catégorie | Score FCS | Interprétation |
|-----------|-----------|----------------|
| 🔴 Pauvre | < 21 | Consommation alimentaire inadéquate |
| 🟡 Limite | 21-35 | Consommation alimentaire borderline |
| 🟢 Acceptable | > 35 | Consommation alimentaire acceptable |

**Minimum requis :** 4 groupes alimentaires mappés.

### rCSI — Reduced Coping Strategy Index (PAM)

**Principe :** Somme pondérée de la fréquence (jours/semaine) d'utilisation de 5 stratégies de survie au cours des 7 derniers jours.

**Pondérations de sévérité :**

| Stratégie de survie | Poids | Sévérité |
|---------------------|-------|----------|
| Manger des aliments moins préférés | 1 | Légère |
| Emprunter de la nourriture | 2 | Moyenne |
| Réduire le nombre de repas | 1 | Légère |
| Réduire la taille des portions | 1 | Légère |
| Restreindre la consommation des adultes | 3 | Sévère |

**Formule :** `rCSI = Σ (jours_stratégie × poids)` — Score de 0 à 56.

**Classification :**
| Catégorie | Score rCSI | Interprétation |
|-----------|------------|----------------|
| 🟢 Pas/peu de coping | 0-3 | Stratégies rarement utilisées |
| 🟡 Moyen | 4-18 | Recours modéré aux stratégies |
| 🔴 Élevé | ≥ 19 | Recours fréquent (situation critique) |

**Minimum requis :** 3 stratégies mappées.

### HDDS — Household Dietary Diversity Score (FAO)

**Principe :** Nombre de groupes alimentaires (sur 12) consommés par le ménage dans les dernières 24 heures. Chaque groupe est codé en binaire (0/1).

**Les 12 groupes FAO :**
1. Céréales 2. Racines/Tubercules 3. Légumes 4. Fruits 5. Viande 6. Œufs
7. Poisson 8. Légumineuses 9. Lait 10. Huile/Graisse 11. Sucre 12. Condiments

**Formule :** `HDDS = Σ (groupe_consommé)` — Score de 0 à 12.

**Classification :**
| Catégorie | Score HDDS | Interprétation |
|-----------|------------|----------------|
| 🔴 Faible | 0-3 | Diversité très limitée |
| 🟡 Moyen | 4-5 | Diversité insuffisante |
| 🟢 Élevé | ≥ 6 | Diversité alimentaire acceptable |

**Minimum requis :** 4 colonnes binaires.

### HHS — Household Hunger Scale (FAO/USAID)

**Principe :** 3 questions sur l'expérience de la faim au cours des 30 derniers jours. Chaque réponse est recodée : 0 = Jamais, 1 = Rarement/Parfois, 2 = Souvent.

**Les 3 questions :**
1. Y a-t-il eu un moment où il n'y avait pas de nourriture du tout dans la maison ?
2. Un membre du ménage est-il allé dormir en ayant faim ?
3. Un membre du ménage est-il resté toute une journée sans manger ?

**Formule :** `HHS = Σ (réponse_recodée)` — Score de 0 à 6.

**Classification :**
| Catégorie | Score HHS | Interprétation |
|-----------|-----------|----------------|
| 🟢 Peu ou pas de faim | 0-1 | Situation acceptable |
| 🟡 Faim modérée | 2-3 | Insécurité alimentaire modérée |
| 🔴 Faim sévère | 4-6 | Crise alimentaire |

**Minimum requis :** 2 questions mappées.

## 18.4 Onglets de résultats (6 onglets)

| # | Onglet | Contenu |
|---|--------|---------|
| 1 | 📊 **Synthèse** | Cards récapitulatives de tous les indicateurs + détails de la synthèse |
| 2 | 🥧 **FCS** | Donut des catégories + histogramme de distribution + statistiques |
| 3 | ⚠️ **rCSI** | Donut des catégories + histogramme + statistiques |
| 4 | 🌱 **HDDS** | Donut + histogramme + fréquences par groupe alimentaire |
| 5 | ❤️ **HHS** | Donut + histogramme + statistiques |
| 6 | 📊 **Par strate** | Graphique empilé par strate + table DT stratifiée |

---

# 19. MODULE 15 — Z-SCORES OMS 2006

**Onglet :** `zscore` · **Icône :** 📐 · **Fichiers :** `module_zscore_oms.R` + `zscore_expert_engine.R` + `zscore_expert_integration.R`

## 19.1 Description

Ce module calcule les Z-scores anthropométriques selon les Standards de Croissance OMS 2006 pour les enfants de 0 à 60 mois, en utilisant la méthode LMS (Box-Cox). Il intègre les tables de référence complètes, un moteur expert avec 10 critères de qualité, et un système de narratifs cliniques automatiques.

## 19.2 Tables LMS intégrées

Les tables de référence OMS 2006 sont embarquées directement dans le code :

| Table | Sexe | Dimensions | Clé | Plage |
|-------|------|-----------|-----|-------|
| WFL_BOYS | Garçons | 133 × 4 | Taille (cm) | 45.0-110.0 cm |
| WFL_GIRLS | Filles | 133 × 4 | Taille (cm) | 45.0-110.0 cm |
| HFA_BOYS | Garçons | 61 × 4 | Âge (mois) | 0-60 mois |
| HFA_GIRLS | Filles | 61 × 4 | Âge (mois) | 0-60 mois |
| WFA_BOYS | Garçons | 61 × 4 | Âge (mois) | 0-60 mois |
| WFA_GIRLS | Filles | 61 × 4 | Âge (mois) | 0-60 mois |

**Colonnes LMS :** L (Lambda, forme Box-Cox), M (Médiane), S (Sigma, coefficient de variation)

## 19.3 Algorithme de calcul

```
Formule principale (L ≠ 0) :
    Z = [(y/M)^L - 1] / (L × S)

Formule alternative (L ≈ 0) :
    Z = ln(y/M) / S

Ajustement SD23 pour |Z| > 3 (méthode officielle OMS) :
    Pour Z > 3 : Z_adj = 3 + (y - SD3pos) / (SD3pos - SD2pos)
    Pour Z < -3 : Z_adj = -3 + (y - SD3neg) / (SD2neg - SD3neg)
```

Les paramètres L, M, S sont obtenus par **interpolation linéaire** entre les valeurs tabulées lorsque la clé (taille ou âge) n'est pas exacte.

## 19.4 Paramètres d'entrée

- **Poids** (kg), **Taille** (cm), **Âge** (mois), **Sexe**
- **Codage du sexe** : configurable (défaut M/F, modifiable pour 1/2, garçon/fille, etc.)
- Les colonnes `WHZ_calc`, `HAZ_calc`, `WAZ_calc` sont injectées dans les données et disponibles dans **tous les modules**.

## 19.5 Les 7 onglets de résultats

| # | Onglet | Contenu détaillé |
|---|--------|-----------------|
| 1 | 📊 **Tableau de bord** | KPI cards expert OMS/UNICEF avec sparklines, narratifs cliniques automatiques, décomposition sévère/modéré, score de qualité. Table de synthèse statistique. |
| 2 | 📈 **Distributions** | Histogrammes WHZ/HAZ/WAZ avec courbe N(0,1) superposée. Stratification par sexe optionnelle. Zones ombrées des seuils OMS. |
| 3 | 🟩 **Classification** | Donuts GAM/Retard/Insuffisance + barres empilées Normal/Modéré/Sévère. Table de classification détaillée. |
| 4 | ♂♀ **Par sexe** | Boxplots WHZ/HAZ/WAZ par sexe + table de comparaison avec tests statistiques. |
| 5 | 💾 **Données** | Table DT de toutes les observations avec Z-scores calculés. Export CSV et Excel. |
| 6 | 🛡️ **Qualité** | Dashboard expert des 10 critères qualité. Résumé des flags SMART. Table des observations flaggées. |
| 7 | 📄 **Rapport Expert** | Rapport Markdown complet exportable. Narratifs cliniques
, statistiques, recommandations. Export Markdown (.md) téléchargeable. |

## 19.6 Moteur expert — 10 critères de qualité

La fonction `calc_quality_score_expert()` évalue la qualité de la collecte avec un scoring transparent (0-100) :

| # | Critère | Méthode d'évaluation | Score max |
|---|---------|---------------------|-----------|
| 1 | Écart-type WHZ | Distance à 1.0 attendu (optimal) | 100 |
| 2 | Digit Preference | Entropie de la distribution des décimales | 100 |
| 3 | Age Heaping | % d'âges multiples de 6 mois | 100 |
| 4 | Sex Ratio | % garçons (attendu 45-55%) | 100 |
| 5 | Corrélations Z-scores | Corrélation moyenne WHZ/HAZ/WAZ | 100 |
| 6 | Flags SMART | % observations hors limites SMART | 100 |
| 7 | Complétude | % de Z-scores non-NA | 100 |
| 8 | Normalité | p-value du test de Shapiro-Wilk | 100 |
| 9 | Skewness / Kurtosis | Forme de la distribution | 100 |
| 10 | SD par équipe | CV inter-équipes (variabilité mesure) | 100 |

**Score global = moyenne pondérée des 10 critères**

Classification : Excellent (≥ 90) · Bon (75-89) · Acceptable (60-74) · Problématique (< 60)

## 19.7 Narratifs cliniques automatiques

La fonction `gen_narrative_kpi()` produit un texte d'interprétation structuré automatiquement pour chaque indicateur, par exemple :

> *Malnutrition aiguë = 15.3 % — CRITIQUE. Situation critique dépassant le seuil d'urgence OMS (≥ 15 %). Composition : 4.2 % sévère (MAS) + 11.1 % modéré (MAM). Distribution observée : µ = −0.82 SD, σ = 1.12. Score de qualité des données : 95/100 (Excellent). Aucune anomalie majeure détectée. Recommandation : intervention nutritionnelle thérapeutique et supplémentaire immédiate.*

---

# 20. MODULE 16 — INDICATEURS OMS / PAM / UNICEF

**Onglet :** `oms` · **Icône :** 📋

## 20.1 Description

Ce module calcule les prévalences nutritionnelles standardisées avec intervalles de confiance à 95 % (méthode Wilson) pour l'ensemble des indicateurs OMS, PAM et UNICEF. Il propose des forest plots interactifs, des jauges de sévérité et des distributions de Z-scores, avec possibilité de stratification simultanée sur plusieurs axes.

## 20.2 Stratifications disponibles

Sélection multiple parmi :
- **Global** — Prévalences sur l'ensemble des données
- **Région** — Par zone géographique administrative
- **Sexe** — Garçons vs Filles
- **Groupe d'âge** — Par tranche (0-5m, 6-11m, 12-23m, 24-35m, 36-47m, 48-59m)
- **Déplacement** — Déplacés vs Résidents vs Retournés
- **FCS** — Par catégorie de consommation alimentaire (Pauvre / Limite / Acceptable)

## 20.3 Indicateurs calculés

Pour chaque stratification, avec IC 95 % Wilson :

| Indicateur | Définition | Seuil de classification |
|------------|-----------|-------------------------|
| **GAM** | WHZ < −2 ou œdèmes bilatéraux | OMS : < 5 % / 5-9 % / 10-14 % / ≥ 15 % |
| **SAM** | WHZ < −3 ou œdèmes | OMS : < 1 % / 1-2 % / 2-5 % / ≥ 5 % |
| **Stunting** | HAZ < −2 | OMS : < 20 % / 20-29 % / 30-39 % / ≥ 40 % |
| **Sev_Stunting** | HAZ < −3 | — |
| **Underweight** | WAZ < −2 | OMS : < 10 % / 10-19 % / 20-29 % / ≥ 30 % |
| **MUAC_GAM** | MUAC < 125 mm | — |
| **MUAC_SAM** | MUAC < 115 mm | — |

## 20.4 Composants de l'interface

- **Table interactive (DT)** — Indicateur, Stratification, N, Cas, Prévalence %, IC bas/haut, badge sévérité OMS. Export CSV.
- **Forest plot interactif (plotly)** — Points + barres d'erreur IC 95 % pour toutes les stratifications. Lignes verticales des seuils OMS.
- **Jauge GAM** et **Jauge Stunting** — Jauges circulaires avec zones colorées selon sévérité.
- **Distribution Z-scores vs N(0,1)** — Histogramme observé + courbe OMS de référence en pointillés.

---

# 21. MODULE 17 — CARTOGRAPHIE

**Onglet :** `carto` · **Icône :** 🌍 · **6 fichiers · 4 tiers · 15 sous-onglets**

## 21.1 Description

Le module cartographie est le plus complexe de l'application. Il offre 15 sous-onglets organisés en 4 niveaux d'analyse spatiale, du simple rendu choroplèthe à la prédiction de tendances par Machine Learning, en passant par l'analyse d'autocorrélation spatiale (LISA/Moran's I) et l'algorithme de priorisation d'interventions.

## 21.2 Architecture en 4 tiers

### TIER 1 — Rendu intelligent (`carto_expert_engine.R` — 8 fonctions)

Gère le rendu cartographique professionnel avec classification optimale et tooltips riches.

| Fonction | Rôle |
|----------|------|
| `calc_jenks_breaks()` | Classification par ruptures naturelles de Jenks (vs linéaire) |
| `gen_region_annotations()` | Labels enrichis avec population, IC 95 %, N |
| `pal_oms_expert()` | Palettes contextuelles selon l'indicateur (GAM, SAM, Stunting…) |
| `calc_optimal_bounds()` | Auto-zoom et centrage sur la zone d'intérêt |
| `gen_rich_tooltip()` | Tooltips HTML avec IC 95 % et comparaison nationale |
| `prepare_choropleth_data()` | Pipeline complet de préparation des données choroplèthes |
| `optimize_view()` | Optimisation de la vue avec prise en compte des zones adjacentes |
| `gen_legend_html()` | Légende dynamique avec seuils OMS intégrés |

### TIER 2 — Analyse spatiale (`spatial_analysis_engine.R` — 8 fonctions)

Fournit les outils d'analyse spatiale avancée (nécessite le package `spdep`).

| Fonction | Rôle |
|----------|------|
| `calc_lisa()` | Indicateurs Locaux d'Association Spatiale (High-High, Low-Low, etc.) |
| `calc_morans_i()` | Test d'autocorrélation spatiale globale de Moran (statistique I, p-value) |
| `rank_hotspots()` | Classification des hotspots par niveau d'urgence |
| `spatial_clustering()` | K-means spatial sur variables nutritionnelles |
| `spatial_anova()` | ANOVA entre clusters spatiaux |
| `prepare_radar_data()` | Données pour profils radar par région |
| `calc_correlation_matrix()` | Matrice de corrélation inter-indicateurs |
| `calc_distance_matrix()` | Distances géographiques entre zones |

### TIER 3 — Intelligence géographique (`carto_intelligence_engine.R` — 7 fonctions)

Fournit les outils de priorisation et de génération de rapports géographiques.

| Fonction | Rôle |
|----------|------|
| `calc_prevalence_ic()` | IC 95 % Clopper-Pearson (binomial exact) par zone |
| `add_ic_to_sf()` | Ajout des IC au shapefile (objet `sf`) |
| `gen_sparkline_html()` | Mini-graphiques tendance en SVG inline pour les tooltips |
| `calc_intervention_priority()` | Score d'urgence composite 0-100 |
| `gen_region_report()` | Rapport narratif automatique par région |
| `export_spatial_data()` | Export GeoJSON, CSV, Shapefile (.zip) |
| `summary_geographic()` | Résumé géographique compact |

**Algorithme de priorisation (score 0-100) :**
- Prévalence observée : **35 %**
- Urgence selon seuil OMS franchi : **30 %**
- Population affectée (N cas) : **20 %**
- Accessibilité géographique : **15 %**

### TIER 4 — Prédiction & optimisation (`carto_predictive_engine.R` — 6 fonctions)

| Fonction | Rôle |
|----------|------|
| `predict_spatial_trend()` | Prédiction des tendances de malnutrition (régression, ML) |
| `calc_regional_benchmark()` | Comparaison vs moyenne nationale et régions similaires |
| `calc_accessibility()` | Score d'accessibilité géographique par zone |
| `allocate_resources()` | Allocation optimale de budget par priorité |
| `calc_performance_trend()` | Analyse de la progression historique entre vagues |
| `calc_synergy_opportunities()` | Identification d'opportunités d'interventions intégrées |

## 21.3 Les 15 sous-onglets

| # | Onglet | Tier | Contenu |
|---|--------|------|---------|
| 1 | 🗺️ **Carte interactive** | Base | Carte Leaflet avec choroplèthe, cercles proportionnels, heatmap densité. Filtres par indicateur et zone. |
| 2 | 📊 **Carte statique** | Base | Carte ggplot2 exportable en PNG haute définition (publication-ready). |
| 3 | 🌍 **Afrique centrale** | Base | Contexte régional — carte de l'Afrique Centrale avec positionnement du pays. |
| 4 | 📋 **Tableau prévalences** | Base | Table DT des prévalences par zone + forest plot. |
| 5 | 📍 **GPS détaillée** | Base | Points GPS individuels (jusqu'à 50 000) colorés par indicateur. |
| 6 | 🔴 **Hotspots LISA** | Tier 2 | Carte LISA (High-High, Low-Low, High-Low, Low-High). Clusters d'

# 22. MODULE 18 — ÉPIDÉMIOLOGIE

**Onglet :** `epidemio` · **Icône :** 🦠 · **Fichier :** `mod_epidemio.R`

Analyse 2×2 (OR, RR, PAF, NNT avec CI 95%), multi-exposition (5-10 expositions simultanées), test de tendance Cochran-Armitage, stratification Mantel-Haenszel (OR ajusté, test Breslow-Day). Exports CSV, PNG (forest plots), Excel.

---

# 23. MODULE 19 — ANALYSE DE SURVIE

**Onglet :** `survie` · **Icône :** ⏱️ · **Fichier :** `mod_survie.R`

Kaplan-Meier (survie médiane, bandes IC 95%, log-rank test si groupes), tables de vie actuarielles, modèle de Cox (HR, C-index, Nagelkerke R², LRT/Wald/Score), vérification proportionnalité (Schoenfeld), diagnostics résidus (Martingale, déviance).

---

# 24. MODULE 20 — MODÈLE PRÉDICTIF

**Onglet :** `predictif` · **Icône :** 🤖 · **Fichier :** `mod_predictif.R`

GLM (binaire) / Multinomial (3-10 classes), Random Forest, XGBoost. Détection automatique type problème. SMOTE rééchantillonnage, train/test 60-85%, K-folds CV. Binaire: AUC-ROC, Sensibilité, Spécificité, Youden, Hosmer-Lemeshow. Multiclasse: Accuracy, Macro F1, Weighted metrics, Macro AUC. Importance variables (coefficients / permutation / gain).

---

# 25. MODULE 21 — RAPPORT EXPORTABLE

**Onglet :** `rapport` · **Icône :** 📄 · **Fichier :** `mod_rapport.R`

Formats: Word (.docx), PDF, HTML, Excel (.xlsx). 13 sections configurables: Résumé exécutif, Statistiques descriptives, Contrôle qualité SMART, Prévalences pondérées, Sécurité alimentaire, Indicateurs OMS/PAM, Z-scores, Analyses inférentielles, Épidémiologie, Survie, Modèle prédictif, Cartographie, Conclusions & recommandations.

---

# 26. NIVEAU EXPERT — DASHBOARD EXÉCUTIF

**Onglet :** `exec_dash` · **Icône :** ⭐ · **Fichier :** `mod_dashboard_exec.R`

Score global 0-100 (vert 80-100, jaune 60-79, orange 40-59, rouge 20-39, rouge foncé 0-19). KPI cards primaires (GAM, SAM, Stunting, Underweight, MUAC_GAM). Graphique prévalences vs seuils OMS. Jauges criticité. Heatmap zones × indicateurs. Table priorité zones. Distribution Z-scores vs N(0,1). Alertes automatiques proactives.

---

# 27. NIVEAU EXPERT — QUALITÉ AUTOMATISÉE

**Onglet :** `qualite_auto` · **Icône :** 🛡️ · **Fichier :** `mod_qualite_auto.R`

10 critères automatisés: Digit Preference, Age Heaping, Cohérence âge/taille, Cohérence poids/taille, Complétude, Plausibilité Z-scores, Anomalies cluster, Écart-type WHZ, Sex Ratio, Performance équipe. Score composite 0-100. Dashboard HTML interactif avec recommandations spécifiques par critère.

---

# 28. NIVEAU EXPERT — AUDIT TRAIL & TRAÇABILITÉ

**Onglet :** `audit_trail` · **Icône :** 📋 · **Fichier :** `mod_audit_trail.R`

Enregistrement automatique: timestamp, module, action, détails. Hachage MD5 des données à chaque version. Dashboard statistiques session (total actions, imports, analyses, durée). Timeline chronologique. Table DT filtrée par module/type/période. Export CSV log complet. Historique hashs.

---

# 29. NIVEAU EXPERT — TESTS STATISTIQUES TEMPORELS

**Onglet :** `temporal_tests` · **Icône :** 📈 · **Fichier :** `mod_temporal_tests.R`

Comparaison proportions entre 2 vagues (Chi², Z-test, Fisher exact). Différence (pp) avec IC 95% Wald. Cohen's h taille d'effet. Direction: ↗ augmentation / ↘ diminution / → stable. Stratification multi-zones avec correction Holm. Forest plot différences. Graphique Cohen's h. Étoiles: *** p<0.001, ** p<0.01, * p<0.05, † p<0.10, ns p≥0.10.

---

# 30. RECHERCHE AVANCÉE — PRÉVISIONS TEMPORELLES

**Onglet :** `advanced_forecasting` · **Icône :** 🔮 · **Fichier :** `mod_advanced_forecasting.R`

Auto-détection colonne date. Agrégation configurable (quotidien/hebdo/mensuel/trimestriel). ARIMA (Auto.arima) ou ETS (Error-Trend-Seasonal). Horizon 3-36 périodes. Graphique série + prédictions avec bandes IC 80%/95%. ACF/PACF plots. Test Ljung-Box (résidus indépendants). Résumé modèle.

---

# 31. RECHERCHE AVANCÉE — GÉOLOCALISATION

**Onglet :** `advanced_geolocation` · **Icône :** 🌍 · **Fichier :** `mod_advanced_geolocation.R`

Sources: GPS (lat/lon direct), adresses (géocodage API), divisions admin (centroïdes). K-means spatial (2-20 clusters). Heatmap densité. Distance matrix (km). Accessibilité (rayon couverture). Exports: GeoJSON, Shapefile (.zip), KML, CSV cluster assignment.

---

# 32. RECHERCHE AVANCÉE — ANALYSE TEXTES (NLP)

**Onglet :** `advanced_nlp` · **Icône :** 💬 · **Fichier :** `mod_advanced_nlp.R`

Auto-détection colonnes texte (notes, comments, remarks). Sentiment analysis: -1 (très négatif) à +1 (très positif). Classification: très négatif/négatif/neutre/positif/très positif. Topic modeling LDA (2-15 topics). Top mots par topic. Word cloud & fréquences. Bigrammes. Exemples textes par sentiment. Exports: sentiment CSV, topics CSV, keywords CSV, rapport HTML.

---

# 33. RECHERCHE AVANCÉE — BENCHMARKING

**Onglet :** `advanced_benchmarking` · **Icône :** 📊 · **Fichier :** `mod_advanced_benchmarking.R`

Benchmarking interne (agrégats vos données) vs externe (fichier CSV/Excel). Comparaison absolue (valeur vs référence). Gap analysis (différence et % écart). Percentile ranking. Z-score standardisé. Visualisations: barplot comparatif, heatmap gaps, percentiles cumulatifs, rankings.

---

# 34. PAGE À PROPOS

**Onglet :** `about` · **Icône :** ℹ️

Logo DSAMID, mission (démocratiser data science pour santé publique), vision (outils identiques partout), 4 piliers expertise (Data Visualization, Statistique Avancée, Machine Learning, Impact Terrain). NutriAnalyse Pro v2: 15+ modules, 20K lignes R, 100% standards OMS. Stack: R/Shiny, tidyverse, ggplot2, Leaflet, plotly, DT, gtsummary, survey, FactoMineR, epitools, survival, pROC, xgboost, KoboToolbox API. Contact & tagline.

---

# 35. SYSTÈME DE FILTRES GLOBAUX

22 filtres dynamiques applicables à tous les modules, organisés en 5 sections:

**Géographie (5):** Région, Département, Zone type (Urbain/Rural/Camp), Cluster/Grappe, Strate.

**Démographie (4):** Sexe (Garçons/Filles), Déplacement (Déplacé/Résident/Retourné), Âge (slider 0-60m), Groupe d'âge (6 tranches).

**Indicateurs OMS (7):** Statut nutritionnel, GAM, SAM, MAM, Stunting, Underweight, Œdèmes.

**Anthropométrie (4):** MUAC (80-220mm), WHZ (-6 à +6), HAZ (-6 à +6), WAZ (-6 à +5).

**Qualité (2):** Exclure NA sur colonnes sélectionnées, Exclure outliers |z|>3.

---

# 36. DESIGN SYSTEM — CSS & PALETTE OMS

Palette: Bleu primaire #008DC9, Bleu foncé #00426A, Vert acceptable #4dac26, Jaune alerte #f9c734, Orange sérieux #e88400, Rouge critique #c9161d, Rouge foncé #880000, Teal #1D9E75, UNICEF bleu #009fda, Violet #7b2d8b.

Typographie: Source Sans Pro (300/400/600/700/800), tailles 11-32px, spacing 8-28px, border-radius 4-20px.

Ombres: légère 0 2px 10px, moyen 0 3px 12px, profonde 0 6px 20px. Transition 0.2s ease.

Classes CSS: .box-primary/.box-danger/.box-success, .badge-ok/.badge-err, .alerte-box, .stat-card, .zs-kpi-card, .section-header, .wizard-step.

---

# 37. RÉFÉRENCE FONCTIONS

~20 000 lignes · ~200+ fonctions · ~500 KB code total.

Fichiers clés: app_v2_final.R (8100L, 50 outputs), utils.R (484L, 21fn), inferentiel_v4.R (1000L, 15fn), module_qualite_smart.R (699L, 13fn), module_prevalence_smart.R (579L, 11fn), module_secu_alimentaire.R (524L, 11fn), module_zscore_oms.R (700+L, 5fn), zscore_expert_engine.R (600+L, 5fn), module_carto_v2.R (785L, 15fn), carto engines (4 fichiers × 265-402L), mod_epidemio/survie/predictif/rapport/exec_dash/qualite_auto/audit_trail/temporal_tests (300-600L chacun), mod_advanced_* (350-400L chacun).

---

# 38. STANDARDS OMS/PAM/UNICEF

**Anthropométrie OMS 2006:**
WHZ: Normal ≥-2SD, MAM -3≤Z<-2SD, MAS Z<-3SD ou œdèmes, GAM Z<-2 ou œdèmes.
HAZ: Normal ≥-2SD, Modéré -3≤Z<-2SD, Sévère Z<-3SD.
WAZ: Normal ≥-2SD, Modéré -3≤Z<-2SD, Sévère Z<-3SD.

**Alerte prévalences OMS:**
GAM: <5% acceptable, 5-9% pauvre, 10-14% sérieux, ≥15% critique.
SAM: <1% acceptable, 1-2% pauvre, 2-5% sérieux, ≥5% critique.
Stunting: <20% acceptable, 20-29% pauvre, 30-39% sérieux, ≥40% critique.
Underweight: <10% acceptable, 10-19% pauvre, 20-29% sérieux, ≥30% critique.

**Sécurité alimentaire
