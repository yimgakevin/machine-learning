
# English version ou version anglaise
<div align="center">

# Pan-Cancer RNA-Seq Transcriptomic Classifier

### A Publication-Grade Machine Learning Pipeline for Multi-Class Tumor Classification

**Kevin Yimga Wandji**

[![R](https://img.shields.io/badge/R-≥4.5-276DC3?style=flat-square&logo=r)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-Document-blue?style=flat-square)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=flat-square)](LICENSE)
[![Dataset: UCI](https://img.shields.io/badge/Dataset-UCI%20ML%20Repository-orange?style=flat-square)](https://doi.org/10.24432/C5R88H)
[![TCGA PANCAN](https://img.shields.io/badge/Data-TCGA%20PANCAN%20HiSeq-green?style=flat-square)](https://doi.org/10.24432/C5R88H)
[![Reproducible](https://img.shields.io/badge/Seed-2026%20(reproducible)-lightgrey?style=flat-square)]()

</div>

---

## Overview

This repository contains a **14-phase reproducible pipeline** for pan-cancer classification from bulk RNA-seq data (TCGA PANCAN HiSeq, UCI ML Repository). The pipeline classifies tumor samples into five cancer types — **BRCA, KIRC, COAD, LUAD, PRAD** — using a rigorously validated multi-criteria biomarker selection strategy followed by an ensemble of eight machine learning classifiers.

The pipeline is implemented as a single self-contained [Quarto](https://quarto.org/) document (`PANCAN_ML_PhD_corrected.qmd`) combining narrative, code, and publication-ready figures. Every methodological decision is justified by reference to the genomics and machine learning literature.

> **Strict data leakage prevention is enforced throughout**: the train/test split is the very first operation, all feature selection and scaling are fitted exclusively on the training set, and the test set is accessed only once for final evaluation.

---

## Scientific Context

| Cancer type | Acronym | Key molecular features |
|---|---|---|
| Breast invasive carcinoma | **BRCA** | ER/PR pathways, HER2 amplification |
| Kidney renal clear cell carcinoma | **KIRC** | HIF/mTOR pathway, VHL loss |
| Colon adenocarcinoma | **COAD** | APC and KRAS mutations |
| Lung adenocarcinoma | **LUAD** | KRAS and EGFR mutations |
| Prostate adenocarcinoma | **PRAD** | Androgen receptor signalling |

The dataset contains **801 samples × 20,531 genes**, pre-normalised (log-transformed) by TCGA. Labels are available from the [UCI ML Repository](https://doi.org/10.24432/C5R88H) (DOI: 10.24432/C5R88H).

---

## Pipeline Architecture

```
Dataset (801 × 20,531)
│
├─ Phase 1  ─── Package installation & environment setup
├─ Phase 2  ─── Data loading, integrity checks (6 controls)
│
├─ Phase 3  ─── Exploratory Data Analysis (EDA)
│               KDE distributions · PCA · UMAP · t-SNE · Inter-class heatmap
│
├─ Phase 4  ─── Stratified 60/20/20 split  ← FIRST OPERATION (anti-leakage)
│               χ² stratification test
│
├─ Phase 5  ─── Multi-criteria biomarker selection (train set only)
│   ├─ [1] MAD filter (remove strictly constant genes)
│   ├─ [2] ANOVA F-test (BH FDR < 0.1%)
│   ├─ [3] Mutual Information (permutation test, 1,000 iterations)
│   ├─ [4] Borda count consensus ranking (ANOVA + MI)
│   ├─ [5] Kneedle algorithm → optimal k* (MCC-vs-n_genes curve)
│   ├─ [6] Boruta (ranger RF wrapper, maxRuns = 150)
│   └─ [7] Stability Selection (100 bootstrap iterations, π ≥ 0.50)
│           → Final panel: 50–600 genes (literature-consistent)
│
├─ Phase 6  ─── Biomarker characterisation
│               Heatmap (η²) · Violin plots · ANOVA-MI concordance (Spearman ρ)
│
├─ Phase 7  ─── ML training (8 classifiers, RandomizedSearchCV, 50 iterations)
│   ├─ Logistic Regression (ElasticNet, glmnet)
│   ├─ SVM Linear (svmLinear2)
│   ├─ SVM RBF (svmRadial)
│   ├─ K-Nearest Neighbors (kknn)
│   ├─ Random Forest (ranger, 500 trees)
│   ├─ XGBoost (xgb.train native API, ≥ v2.0)
│   ├─ LightGBM (native API)
│   └─ MLP (nnet, multi-layer perceptron)
│
├─ Phase 8  ─── Confusion matrices & per-class metrics
├─ Phase 9  ─── ROC/PR curves (pROC, PRROC)
├─ Phase 10 ─── Model comparison (lollipop, radar, ECE scatter, summary table)
├─ Phase 11 ─── Bootstrap BCa 95% CI (B = 2,000; Efron 1987)
├─ Phase 12 ─── Nested CV (5×5) + permutation test (n = 1,000)
├─ Phase 13 ─── TreeSHAP interpretability (shapviz)
├─ Phase 14 ─── Final report + publication-ready Methods section
└─ Phase 15 ─── Comparison with published literature
```

---

## Key Methodological Choices

### Feature Selection — Why not intersection?

The original pipeline intersected five strict filters, yielding ~38 genes — biologically implausible and inconsistent with the literature (published panels range from 50 to 600 genes). This pipeline uses a **hierarchical strategy with Kneedle-driven panel size**:

1. The Kneedle algorithm identifies the inflection point of the MCC-vs-n_genes validation curve, providing a **data-driven k\*** without arbitrary thresholds.
2. Boruta and Stability Selection serve as biological validation layers, not as primary filters.
3. The union of all confirmed genes provides a conservative upper bound.

References: Lyu & Haque (2018) *Sci Rep*; Guo et al. (2019) *Front Genet*; Chen et al. (2022) *Bioinformatics*; Satopää et al. (2011) Kneedle algorithm.

### Primary Evaluation Metric — MCC

The Matthews Correlation Coefficient is used as the primary metric following Chicco & Jurman (2020) *BMC Genomics*. MCC uses all cells of the confusion matrix and is unbiased by class prevalence — critical for the moderate imbalance in this dataset (BRCA n=300 vs COAD n=78, ratio ≈ 3.8×).

### Hyperparameter Optimisation — RandomizedSearchCV

Randomized search (Bergstra & Bengio, 2012) with 50 iterations per model replaces grid search. Inner loop: 5-fold CV repeated 3 times, optimising macro AUC. Model selection: best MCC on the validation set.

### Statistical Validation

| Method | Purpose | Parameters |
|---|---|---|
| Nested CV (5×5) | Unbiased generalisation estimate | 5 outer × 5 inner folds |
| Permutation test | H₀: performance = chance | n = 1,000 permutations |
| Bootstrap BCa | Confidence intervals for key metrics | B = 2,000; Efron (1987) |

---

## Evaluation Metrics (12 metrics)

| Category | Metrics |
|---|---|
| **Discrimination** | Accuracy, Balanced Accuracy, MCC, F1-macro, Cohen's κ, G-mean |
| **Probabilistic** | AUC-ROC (macro OvR), PR-AUC (macro), Log-Loss, Brier Score, BSS |
| **Calibration** | ECE — Expected Calibration Error (10 equal-width bins) |

---

## Repository Structure

```
.
├── PANCAN_ML_PhD_corrected.qmd   # Main pipeline (Quarto document)
├── README.md
│
├── data_raw/
│   ├── data.csv                  # Expression matrix (801 × 20,531)
│   └── labels.csv                # Class labels (801 × 1)
│
└── outputs/
    ├── figures/
    │   ├── Fig1_distributions.pdf
    │   ├── Fig2_PCA.pdf
    │   ├── Fig3_UMAP_tSNE.pdf
    │   ├── Fig_Heatmap_biomarkers.pdf
    │   ├── Fig_Concordance_ANOVA_MI.pdf
    │   ├── Fig_Panel_Comparison.pdf
    │   ├── Fig_Stability_Distribution.pdf
    │   ├── Fig_Confusion_Matrices.pdf
    │   ├── Fig_ROC_PR_curves.pdf
    │   ├── Fig_Model_Comparison.pdf
    │   ├── Fig_Bootstrap_BCa.pdf
    │   ├── Fig_NestedCV.pdf
    │   ├── Fig_Permutation_Test.pdf
    │   └── Fig_SHAP_Tree.pdf
    │
    ├── metrics/
    │   ├── final_panel_genes.csv         # Biomarker gene list with scores
    │   ├── final_panel_indices.rds
    │   ├── metrics_val_all_models.csv    # Validation metrics, all models
    │   ├── test_final_results.rds        # Final test set evaluation
    │   ├── nested_cv_results.rds
    │   ├── permutation_test_results.rds
    │   ├── bootstrap_bca_results.rds
    │   ├── bootstrap_bca_summary.csv
    │   ├── calibration_results.rds
    │   ├── final_report.rds
    │   └── methods_section.txt           # Ready-to-paste Methods text
    │
    ├── models/
    │   ├── models_checkpoint.rds         # All trained models
    │   ├── best_model.rds
    │   ├── xgboost_final.model
    │   └── lightgbm_final.model
    │
    └── shap/
        └── shap_object.rds
```

---

## Requirements

### R ≥ 4.5.2 — Required packages

```r
install.packages(c(
  # Data manipulation & visualisation
  "tidyverse", "here", "scales", "patchwork", "ggrepel",
  "ggplot2", "pheatmap", "RColorBrewer", "viridis",
  "ggpubr", "cowplot", "factoextra", "FactoMineR",

  # Dimensionality reduction
  "umap", "Rtsne",

  # ML frameworks
  "tidymodels", "caret",

  # Classifiers
  "glmnet",    # Logistic Regression ElasticNet
  "ranger",    # Random Forest
  "e1071",     # SVM (Linear + RBF)
  "xgboost",   # XGBoost (≥ 2.0)
  "lightgbm",  # LightGBM
  "kknn",      # K-Nearest Neighbors
  "nnet",      # MLP

  # Feature selection
  "Boruta", "infotheo",

  # Evaluation metrics
  "yardstick", "mltools", "pROC", "PRROC",

  # Interpretability
  "shapviz", "vip",

  # Statistical validation
  "boot",

  # Utilities
  "gridExtra", "gtable", "sessioninfo"
))
```

> **Note on XGBoost ≥ 2.0**: `cb.evaluation.log` is not exported from the namespace in all builds. The pipeline uses a custom R closure callback to capture evaluation scores. See the XGBoost module for details.

---

## Quickstart

### 1. Download the data

```bash
# Option A — Manual download from UCI
# https://archive.ics.uci.edu/dataset/401/gene+expression+cancer+rna+seq
# Extract TCGA-PANCAN-HiSeq-801x20531.tar.gz → place data.csv and labels.csv in data_raw/

# Option B — Automated download (uncomment in Phase 2 of the .qmd)
```

### 2. Create output directories

```r
dir.create("outputs/figures", recursive = TRUE)
dir.create("outputs/metrics", recursive = TRUE)
dir.create("outputs/models",  recursive = TRUE)
dir.create("outputs/shap",    recursive = TRUE)
```

### 3. Render the full pipeline

```bash
quarto render PANCAN_ML_PhD_corrected.qmd
```

Or, from within R:

```r
quarto::quarto_render("PANCAN_ML_PhD_corrected.qmd")
```

The output is a self-contained HTML report (`PANCAN_ML_PhD_corrected.html`) with interactive table-of-contents, collapsible code blocks, and all figures embedded.

### 4. Run individual phases

Each phase is an independent `{r}` chunk labelled by name. Execute them sequentially in RStudio or via `knitr::knit_exit()` checkpointing.

---

## Reproducibility

| Parameter | Value |
|---|---|
| Random seed | `SEED = 2026L` |
| R version tested | R 4.5.2 |
| Quarto version | ≥ 1.5 |
| OS | Linux / macOS / Windows |
| Parallelism | `parallel::detectCores() - 1` |
| Cache | Quarto chunk caching enabled (`cache: true`) |

The seed `2026` was chosen as the year of the analysis, set **before** examining results, consistent with open-science pre-registration practice (not chosen to optimise results).

---

## Literature Benchmark

| Reference | Method | Accuracy | Validation |
|---|---|---|---|
| Hossain et al. (2025) | SVM, 8 classifiers | 99.87% | 5-fold CV |
| Ludwig & Siludwig (2021) | RF, SVM | ~99.0% | Train/test |
| Lyu & Haque (2018) | Deep AE + classifier | 99.07% | 80/20 split |
| Guo et al. (2019) | mRMR + classifiers | 98.5% | CV |
| **This pipeline** | **8 classifiers + 5-filter selection** | **see outputs/** | **Nested CV + permutation + BCa** |

> Direct accuracy comparison requires identical protocols. This pipeline reports MCC, AUC-ROC, and BCa confidence intervals alongside accuracy — a more complete and statistically rigorous characterisation.

---

## Output Highlights

**Phase 10 — Model Comparison Figure**
Four-panel publication figure: MCC lollipop, normalised radar chart (7 metrics), ECE vs MCC calibration scatter, and a formatted comparison table.

**Phase 11 — Bootstrap BCa**
Forest plot of 95% BCa confidence intervals for MCC, AUC-ROC, F1-macro, and BSS. Two-phase threshold sweep (coarse + fine) per metric.

**Phase 13 — TreeSHAP**
Beeswarm and bar importance plots for the best tree-based model, identifying the most influential biomarkers at the individual prediction level.

**Phase 14 — Methods Text**
`outputs/metrics/methods_section.txt` contains a complete, parameterised Methods section ready for manuscript submission — values are injected from the actual pipeline results.

---

## Citation

If you use this pipeline or adapt it for your research, please cite the dataset:

```bibtex
@dataset{tcga_pancan_2016,
  title  = {Gene Expression Cancer RNA-Seq},
  author = {Weinstein, John N. and others},
  year   = {2013},
  doi    = {10.24432/C5R88H},
  url    = {https://archive.ics.uci.edu/dataset/401},
  note   = {UCI Machine Learning Repository}
}
```

And the key methodological references used in this pipeline:

```bibtex
@article{chicco2020mcc,
  title   = {The advantages of the Matthews correlation coefficient (MCC)
             over F1 score and accuracy in binary classification evaluation},
  author  = {Chicco, Davide and Jurman, Giuseppe},
  journal = {BMC Genomics},
  year    = {2020},
  volume  = {21},
  doi     = {10.1186/s12864-019-6413-7}
}

@article{efron1987bca,
  title   = {Better Bootstrap Confidence Intervals},
  author  = {Efron, Bradley},
  journal = {Journal of the American Statistical Association},
  year    = {1987},
  volume  = {82},
  pages   = {171--185}
}

@article{kursa2010boruta,
  title   = {Feature Selection with the Boruta Package},
  author  = {Kursa, Miron B. and Rudnicki, Witold R.},
  journal = {Journal of Statistical Software},
  year    = {2010},
  volume  = {36},
  doi     = {10.18637/jss.v036.i11}
}
```

---

## License

This project is released under the [MIT License](LICENSE).
The TCGA PANCAN HiSeq dataset is publicly available under the [UCI ML Repository terms of use](https://archive.ics.uci.edu/dataset/401).

---

<div align="center">
<sub>Built with R · Quarto · ggplot2 · caret · XGBoost · LightGBM · shapviz</sub>
</div>




# Version Française

<div align="center">

# Classificateur Transcriptomique Pan-Cancer RNA-Seq

### Un pipeline de machine learning publication-grade pour la classification multi-classes de tumeurs

**Kevin Yimga Wandji**

[![R](https://img.shields.io/badge/R-≥4.5-276DC3?style=flat-square&logo=r)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-Document-blue?style=flat-square)](https://quarto.org/)
[![Licence: MIT](https://img.shields.io/badge/Licence-MIT-yellow.svg?style=flat-square)](LICENSE)
[![Dataset: UCI](https://img.shields.io/badge/Dataset-UCI%20ML%20Repository-orange?style=flat-square)](https://doi.org/10.24432/C5R88H)
[![TCGA PANCAN](https://img.shields.io/badge/Données-TCGA%20PANCAN%20HiSeq-green?style=flat-square)](https://doi.org/10.24432/C5R88H)
[![Reproductible](https://img.shields.io/badge/Graine-2026%20(reproductible)-lightgrey?style=flat-square)]()

</div>

---

## Présentation

Ce dépôt contient un **pipeline reproductible en 14 phases** pour la classification pan-cancer à partir de données RNA-seq en vrac (TCGA PANCAN HiSeq, UCI ML Repository). Le pipeline classifie des échantillons tumoraux en cinq types — **BRCA, KIRC, COAD, LUAD, PRAD** — grâce à une stratégie rigoureusement validée de sélection multi-critères de biomarqueurs, suivie d'un ensemble de huit classificateurs de machine learning.

Le pipeline est implémenté sous forme d'un unique document [Quarto](https://quarto.org/) auto-contenu (`PANCAN_ML_PhD_corrected.qmd`), combinant narration scientifique, code R et figures prêtes à la publication. Chaque choix méthodologique est justifié par des références issues de la littérature en génomique et en apprentissage automatique.

> **La prévention stricte des fuites de données (data leakage) est appliquée tout au long du pipeline** : le split train/test est la toute première opération, toutes les étapes de sélection de variables et de normalisation sont ajustées exclusivement sur le jeu d'entraînement, et le jeu de test n'est accédé qu'une seule fois pour l'évaluation finale.

---

## Contexte scientifique

| Type de cancer | Acronyme | Caractéristiques moléculaires clés |
|---|---|---|
| Carcinome invasif du sein | **BRCA** | Voies ER/PR, amplification HER2 |
| Carcinome rénal à cellules claires | **KIRC** | Voie HIF/mTOR, perte du gène VHL |
| Adénocarcinome du côlon | **COAD** | Mutations APC et KRAS |
| Adénocarcinome pulmonaire | **LUAD** | Mutations KRAS et EGFR |
| Adénocarcinome de la prostate | **PRAD** | Signalisation par le récepteur des androgènes |

Le jeu de données contient **801 échantillons × 20 531 gènes**, pré-normalisés (log-transformés) par le TCGA. Les étiquettes sont disponibles depuis l'[UCI ML Repository](https://doi.org/10.24432/C5R88H) (DOI : 10.24432/C5R88H).

---

## Architecture du pipeline

```
Jeu de données (801 × 20 531)
│
├─ Phase 1  ─── Installation des packages et configuration de l'environnement
├─ Phase 2  ─── Chargement des données, vérifications d'intégrité (6 contrôles)
│
├─ Phase 3  ─── Analyse Exploratoire des Données (EDA)
│               Distributions KDE · PCA · UMAP · t-SNE · Heatmap inter-classes
│
├─ Phase 4  ─── Split stratifié 60/20/20  ← PREMIÈRE OPÉRATION (anti-leakage)
│               Test de stratification chi²
│
├─ Phase 5  ─── Sélection multi-critères des biomarqueurs (jeu d'entraînement uniquement)
│   ├─ [1] Filtre MAD (suppression des gènes strictement constants)
│   ├─ [2] ANOVA F-test (FDR Benjamini-Hochberg < 0,1 %)
│   ├─ [3] Information Mutuelle (test de permutation, 1 000 itérations)
│   ├─ [4] Classement consensus Borda (ANOVA + MI)
│   ├─ [5] Algorithme Kneedle → k* optimal (courbe MCC vs n_gènes)
│   ├─ [6] Boruta (wrapper Random Forest ranger, maxRuns = 150)
│   └─ [7] Stability Selection (100 itérations bootstrap, π ≥ 0,50)
│           → Panel final : 50–600 gènes (cohérent avec la littérature)
│
├─ Phase 6  ─── Caractérisation des biomarqueurs
│               Heatmap (η²) · Violin plots · Concordance ANOVA-MI (Spearman ρ)
│
├─ Phase 7  ─── Entraînement ML (8 classificateurs, RandomizedSearchCV, 50 itérations)
│   ├─ Régression Logistique (ElasticNet, glmnet)
│   ├─ SVM Linéaire (svmLinear2)
│   ├─ SVM RBF (svmRadial)
│   ├─ K Plus Proches Voisins (kknn)
│   ├─ Forêt Aléatoire (ranger, 500 arbres)
│   ├─ XGBoost (API native xgb.train, ≥ v2.0)
│   ├─ LightGBM (API native)
│   └─ MLP (nnet, perceptron multi-couches)
│
├─ Phase 8  ─── Matrices de confusion et métriques par classe
├─ Phase 9  ─── Courbes ROC et PR (pROC, PRROC)
├─ Phase 10 ─── Comparaison des modèles (lollipop, radar, scatter ECE, tableau)
├─ Phase 11 ─── Bootstrap BCa IC 95 % (B = 2 000 ; Efron 1987)
├─ Phase 12 ─── CV imbriquée (5×5) + test de permutation (n = 1 000)
├─ Phase 13 ─── Interprétabilité TreeSHAP (shapviz)
├─ Phase 14 ─── Rapport final + section Methods prête à la publication
└─ Phase 15 ─── Comparaison avec la littérature publiée
```

---

## Choix méthodologiques clés

### Sélection de variables — Pourquoi pas l'intersection ?

Le pipeline original intersectait cinq filtres stricts, produisant ~38 gènes — biologiquement invraisemblable et incohérent avec la littérature (les panels publiés comportent de 50 à 600 gènes). Ce pipeline adopte une **stratégie hiérarchique pilotée par l'algorithme Kneedle** :

1. L'algorithme Kneedle identifie le point d'inflexion de la courbe MCC-vs-n_gènes sur le jeu de validation, fournissant un **k\* déterminé par les données** sans seuil arbitraire.
2. Boruta et la Stability Selection servent de couches de validation biologique, non de filtres primaires.
3. L'union de tous les gènes confirmés constitue une borne supérieure conservative.

R�férences : Lyu & Haque (2018) *Sci Rep* ; Guo et al. (2019) *Front Genet* ; Chen et al. (2022) *Bioinformatics* ; Satopää et al. (2011) algorithme Kneedle.

### Métrique principale — MCC

Le Coefficient de Corrélation de Matthews est utilisé comme métrique principale, en accord avec Chicco & Jurman (2020) *BMC Genomics*. Le MCC exploite toutes les cellules de la matrice de confusion et n'est pas biaisé par la prévalence des classes — critique au vu du déséquilibre modéré de ce jeu de données (BRCA n = 300 vs COAD n = 78, ratio ≈ 3,8×).

### Optimisation des hyperparamètres — RandomizedSearchCV

La recherche aléatoire (Bergstra & Bengio, 2012) avec 50 itérations par modèle remplace la recherche sur grille exhaustive. Boucle interne : validation croisée 5-fold répétée 3 fois, maximisant l'AUC macro. Sélection du modèle : meilleur MCC sur le jeu de validation.

### Validation statistique

| Méthode | Objectif | Paramètres |
|---|---|---|
| CV imbriquée (5×5) | Estimation non biaisée de la généralisation | 5 folds externes × 5 folds internes |
| Test de permutation | H₀ : performance = hasard | n = 1 000 permutations |
| Bootstrap BCa | Intervalles de confiance pour les métriques clés | B = 2 000 ; Efron (1987) |

---

## Métriques d'évaluation (12 métriques)

| Catégorie | Métriques |
|---|---|
| **Discrimination** | Accuracy, Balanced Accuracy, MCC, F1-macro, Cohen's κ, G-mean |
| **Probabilistes** | AUC-ROC (macro OvR), PR-AUC (macro), Log-Loss, Brier Score, BSS |
| **Calibration** | ECE — Erreur de Calibration Attendue (10 intervalles de largeur égale) |

---

## Structure du dépôt

```
.
├── PANCAN_ML_PhD_corrected.qmd   # Pipeline principal (document Quarto)
├── README.md                     # README en anglais
├── README_FR.md                  # Ce fichier
│
├── data_raw/
│   ├── data.csv                  # Matrice d'expression (801 × 20 531)
│   └── labels.csv                # Étiquettes de classe (801 × 1)
│
└── outputs/
    ├── figures/
    │   ├── Fig1_distributions.pdf
    │   ├── Fig2_PCA.pdf
    │   ├── Fig3_UMAP_tSNE.pdf
    │   ├── Fig_Heatmap_biomarqueurs.pdf
    │   ├── Fig_Concordance_ANOVA_MI.pdf
    │   ├── Fig_Comparaison_Panels.pdf
    │   ├── Fig_Stabilite_Distribution.pdf
    │   ├── Fig_Matrices_Confusion.pdf
    │   ├── Fig_Courbes_ROC_PR.pdf
    │   ├── Fig_Comparaison_Modeles.pdf
    │   ├── Fig_Bootstrap_BCa.pdf
    │   ├── Fig_CV_Imbriquee.pdf
    │   ├── Fig_Test_Permutation.pdf
    │   └── Fig_SHAP_Tree.pdf
    │
    ├── metrics/
    │   ├── final_panel_genes.csv         # Liste des gènes biomarqueurs avec scores
    │   ├── final_panel_indices.rds
    │   ├── metrics_val_all_models.csv    # Métriques de validation, tous les modèles
    │   ├── test_final_results.rds        # Évaluation finale sur le jeu de test
    │   ├── nested_cv_results.rds
    │   ├── permutation_test_results.rds
    │   ├── bootstrap_bca_results.rds
    │   ├── bootstrap_bca_summary.csv
    │   ├── calibration_results.rds
    │   ├── final_report.rds
    │   └── methods_section.txt           # Section Methods prête pour le manuscrit
    │
    ├── models/
    │   ├── models_checkpoint.rds         # Tous les modèles entraînés
    │   ├── best_model.rds
    │   ├── xgboost_final.model
    │   └── lightgbm_final.model
    │
    └── shap/
        └── shap_object.rds
```

---

## Prérequis

### R ≥ 4.5.2 — Packages requis

```r
install.packages(c(
  # Manipulation de données et visualisation
  "tidyverse", "here", "scales", "patchwork", "ggrepel",
  "ggplot2", "pheatmap", "RColorBrewer", "viridis",
  "ggpubr", "cowplot", "factoextra", "FactoMineR",

  # Réduction de dimension
  "umap", "Rtsne",

  # Frameworks ML
  "tidymodels", "caret",

  # Classificateurs
  "glmnet",    # Régression Logistique ElasticNet
  "ranger",    # Forêt Aléatoire
  "e1071",     # SVM (Linéaire + RBF)
  "xgboost",   # XGBoost (≥ 2.0)
  "lightgbm",  # LightGBM
  "kknn",      # K Plus Proches Voisins
  "nnet",      # MLP (perceptron multi-couches)

  # Sélection de variables
  "Boruta", "infotheo",

  # Métriques d'évaluation
  "yardstick", "mltools", "pROC", "PRROC",

  # Interprétabilité
  "shapviz", "vip",

  # Validation statistique
  "boot",

  # Utilitaires
  "gridExtra", "gtable", "sessioninfo"
))
```

> **Note sur XGBoost ≥ 2.0** : `cb.evaluation.log` n'est pas exporté depuis le namespace dans toutes les versions compilées. Le pipeline utilise un callback R personnalisé (closure sur un environnement partagé) pour capturer les scores d'évaluation à chaque itération. Cette approche est compatible avec toutes les versions ≥ 1.x. Voir la section XGBoost du pipeline pour les détails.

---

## Démarrage rapide

### 1. Télécharger les données

```bash
# Option A — Téléchargement manuel depuis UCI
# https://archive.ics.uci.edu/dataset/401/gene+expression+cancer+rna+seq
# Extraire TCGA-PANCAN-HiSeq-801x20531.tar.gz
# → placer data.csv et labels.csv dans data_raw/

# Option B — Téléchargement automatique
# Décommenter les lignes correspondantes dans la Phase 2 du fichier .qmd
```

### 2. Créer les répertoires de sortie

```r
dir.create("outputs/figures", recursive = TRUE)
dir.create("outputs/metrics", recursive = TRUE)
dir.create("outputs/models",  recursive = TRUE)
dir.create("outputs/shap",    recursive = TRUE)
```

### 3. Compiler le pipeline complet

```bash
quarto render PANCAN_ML_PhD_corrected.qmd
```

Ou depuis R :

```r
quarto::quarto_render("PANCAN_ML_PhD_corrected.qmd")
```

Le résultat est un rapport HTML auto-contenu (`PANCAN_ML_PhD_corrected.html`) avec une table des matières interactive, des blocs de code repliables et toutes les figures intégrées.

### 4. Exécuter les phases individuellement

Chaque phase est un chunk `{r}` indépendant, identifié par son nom. Les exécuter séquentiellement dans RStudio. Des checkpoints intermédiaires sont sauvegardés automatiquement dans `outputs/models/models_checkpoint.rds` après chaque modèle entraîné.

---

## Reproductibilité

| Paramètre | Valeur |
|---|---|
| Graine aléatoire | `SEED = 2026L` |
| Version R testée | R 4.5.2 |
| Version Quarto | ≥ 1.5 |
| Système d'exploitation | Linux / macOS / Windows |
| Parallélisme | `parallel::detectCores() - 1` |
| Cache Quarto | Activé (`cache: true`) |

La graine `2026` a été choisie comme l'année de l'analyse, fixée **avant** tout examen des résultats, conformément aux pratiques de science ouverte (aucun data dredging).

---

## Comparaison avec la littérature

| Référence | Méthode | Accuracy | Protocole de validation |
|---|---|---|---|
| Hossain et al. (2025) | SVM, 8 classificateurs | 99,87 % | CV 5-fold |
| Ludwig & Siludwig (2021) | RF, SVM | ~99,0 % | Train/test |
| Lyu & Haque (2018) | Auto-encodeur profond | 99,07 % | Split 80/20 |
| Guo et al. (2019) | mRMR + classificateurs | 98,5 % | CV |
| **Ce pipeline** | **8 classificateurs + sélection 5 filtres** | **voir outputs/** | **CV imbriquée + permutation + BCa** |

> Une comparaison directe de l'accuracy nécessite des protocoles identiques. Ce pipeline rapporte le MCC, l'AUC-ROC et les intervalles de confiance BCa en complément de l'accuracy — une caractérisation plus complète et statistiquement rigoureuse.

---

## Points forts des sorties

**Phase 10 — Figure de comparaison des modèles**  
Figure à quatre panneaux destinée à la publication : lollipop MCC, radar normalisé (7 métriques), nuage de points ECE vs MCC et tableau comparatif formaté.

**Phase 11 — Bootstrap BCa**  
Forest plot des intervalles de confiance BCa à 95 % pour le MCC, l'AUC-ROC, le F1-macro et le BSS. Balayage du seuil en deux phases (grossier puis fin) pour chaque métrique.

**Phase 12 — CV imbriquée et test de permutation**  
Estimation non biaisée des performances par CV 5×5. Test de permutation (n = 1 000) pour vérifier que les performances observées dépassent le niveau du hasard (p < 0,001 attendu).

**Phase 13 — TreeSHAP**  
Beeswarm plot et bar plot d'importance pour le meilleur modèle arborescent, identifiant les biomarqueurs les plus influents au niveau de la prédiction individuelle.

**Phase 14 — Texte Methods**  
`outputs/metrics/methods_section.txt` contient une section Methods complète et paramétrée, prête pour la soumission à un journal — les valeurs numériques (taille du panel, métriques, IC) sont injectées automatiquement depuis les résultats réels du pipeline.

---

## Références bibliographiques clés

| Référence | Usage dans le pipeline |
|---|---|
| Chicco & Jurman (2020) *BMC Genomics* | Justification du MCC comme métrique principale |
| Efron (1987) *JASA* | Bootstrap BCa pour les intervalles de confiance |
| Kursa & Rudnicki (2010) *J Stat Softw* | Algorithme Boruta |
| Bergstra & Bengio (2012) *JMLR* | RandomizedSearchCV |
| Satopää et al. (2011) *IEEE* | Algorithme Kneedle (détection du coude) |
| Benjamini & Hochberg (1995) *JRSS-B* | Correction FDR pour les tests multiples |
| Gorodkin (2004) *Comput Biol Chem* | MCC multiclasse |
| Meinshausen & Bühlmann (2010) *JRSS-B* | Stability Selection |
| Naeini et al. (2015) *AAAI* | Expected Calibration Error (ECE) |
| Lundberg et al. (2020) *Nat Mach Intell* | TreeSHAP |

---

## Citation

Si vous utilisez ce pipeline ou l'adaptez pour vos recherches, merci de citer le jeu de données :

```bibtex
@dataset{tcga_pancan_uci,
  title  = {Gene Expression Cancer RNA-Seq},
  author = {Weinstein, John N. and others},
  year   = {2013},
  doi    = {10.24432/C5R88H},
  url    = {https://archive.ics.uci.edu/dataset/401},
  note   = {UCI Machine Learning Repository}
}
```

Et les références méthodologiques clés :

```bibtex
@article{chicco2020mcc,
  title   = {The advantages of the Matthews correlation coefficient (MCC)
             over F1 score and accuracy in binary classification evaluation},
  author  = {Chicco, Davide and Jurman, Giuseppe},
  journal = {BMC Genomics},
  year    = {2020},
  volume  = {21},
  doi     = {10.1186/s12864-019-6413-7}
}

@article{efron1987bca,
  title   = {Better Bootstrap Confidence Intervals},
  author  = {Efron, Bradley},
  journal = {Journal of the American Statistical Association},
  year    = {1987},
  volume  = {82},
  pages   = {171--185}
}

@article{kursa2010boruta,
  title   = {Feature Selection with the Boruta Package},
  author  = {Kursa, Miron B. and Rudnicki, Witold R.},
  journal = {Journal of Statistical Software},
  year    = {2010},
  volume  = {36},
  doi     = {10.18637/jss.v036.i11}
}
```

---

## Licence

Ce projet est distribué sous licence [MIT](LICENSE).  
Le jeu de données TCGA PANCAN HiSeq est disponible publiquement selon les [conditions d'utilisation de l'UCI ML Repository](https://archive.ics.uci.edu/dataset/401).

---

<div align="center">
<sub>Construit avec R · Quarto · ggplot2 · caret · XGBoost · LightGBM · shapviz</sub>
</div>
