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



