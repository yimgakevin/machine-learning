**Description**

Ce dépôt regroupe les analyses réalisées dans le cadre de plusieurs projets  visant à caractériser les profils transcriptomiques dans différents contextes biologiques (pathologies, traitements, développement). Pour chaque projet, l'ensemble du pipeline d'analyse (contrôle qualité, analyse différentielle, exploration fonctionnelle et validation statistique) est implémenté dans un document Quarto autonome, garantissant la reproductibilité et la traçabilité des résultats.

Chaque projet est indépendant et contient :

- Un fichier *.qmd (Quarto) qui intègre le code R, les visualisations et les commentaires narratifs.

- Un dossier data/ avec les fichiers de comptage (matrice gènes × échantillons) et les métadonnées (conditions expérimentales, covariables).

- Un dossier results/ où sont sauvegardés les tables (listes de gènes différentiellement exprimés, résultats d'enrichissement) et les figures (ACP, heatmaps, volcano plots, etc.).

**Prérequis**
- R (version ≥ 4.1)

- Quarto (≥ 1.3) ou RStudio avec prise en charge de Quarto

- Packages R : l'installation des dépendances est détaillée ci-dessous.

Dépendances de base (analyses toujours disponibles)
Ces packages sont utilisés dans tous les projets :

install.packages(c("tidyverse", "ggplot2", "pheatmap", "RColorBrewer", "VennDiagram", "BiocManager"))
BiocManager::install(c("limma", "edgeR", "DESeq2", "clusterProfiler", "org.Hs.eg.db", "pathview"))

Dépendances optionnelles (analyses avancées)  

Certaines analyses (GSEA, WGCNA, enrichissement TF, tests de normalité multivariés) nécessitent des packages supplémentaires. Les documents Quarto incluent des blocs conditionnels (eval = require(package, quietly = TRUE)) pour les exécuter uniquement si le package est installé.

# Pour GSEA (Hallmarks) et enrichissement TF
BiocManager::install("fgsea")
install.packages("msigdbr")

# Pour WGCNA
install.packages("WGCNA")

# Pour tests de normalité multivariés
install.packages("MVN")


***Analyses réalisées (pour chaque projet)****

**1. Contrôle qualité et prétraitement**
Filtrage des gènes peu exprimés (CPM > 1 dans au moins X échantillons).

Normalisation TMM (edgeR) ou rlog/vst (DESeq2) selon l'outil utilisé.

Évaluation de la distribution des comptages (boxplots, densité).

Détection de outliers par analyse de distance inter-échantillons.

**2. Analyse exploratoire**
ACP (Analyse en Composantes Principales) sur les données normalisées.

Classification hiérarchique (heatmap des distances euclidiennes).

Identification des sources de variation (batch, conditions) par corrélation avec les composantes principales.

**3. Analyse différentielle**
Méthode principale : limma-voom (robuste et rapide, toujours disponible).
Alternatives : DESeq2 ou edgeR peuvent être utilisés pour comparaison.

Construction de la matrice de design et des contrastes d'intérêt.

Ajustement des p-values pour tests multiples (FDR, méthode de Benjamini-Hochberg).

Seuils de significativité : |log2FC| ≥ 1 et FDR < 0.05 (ajustables).

**4. Visualisation des résultats**
Volcano plots : mise en évidence des gènes différentiellement exprimés.

MA plots : visualisation du rapport d'expression en fonction de l'abondance.

Boxplots par gène : expression normalisée pour quelques gènes d'intérêt (top DE, gènes candidats).

**5. Annotation et enrichissement fonctionnel**
Conversion des identifiants de gènes (Ensembl → Entrez/Symbol) via clusterProfiler ou org.Hs.eg.db.

Analyse GO (Gene Ontology) : termes biologiques, moléculaires et cellulaires surreprésentés.

**Analyse KEGG : voies métaboliques enrichies.**

Visualisation : dotplots, barplots, réseaux GO (cnetplot, emapplot).

**6. Comparaisons multiples et diagrammes de Venn**  
Identification des gènes communs et spécifiques entre plusieurs comparaisons (par ex. condition A vs B, A vs C).

Diagrammes de Venn avec VennDiagram ou ggvenn.

**7. Comparaisons entre sous-types**  
Si les échantillons sont stratifiés en sous-groupes (ex : grades tumoraux, sous-types moléculaires), analyse différentielle intra-groupe et inter-groupes.

**8. Bootstrap validation**
Rééchantillonnage des échantillons (bootstrap) pour évaluer la stabilité des gènes différentiellement exprimés.

Calcul du Jaccard index ou du pourcentage de chevauchement entre les listes de gènes issues de multiples bootstraps.

**9. Signature scoring**
Calcul de scores d'expression pour des signatures géniques prédéfinies (ex : signatures immunitaires, signatures de prolifération).

Méthode : moyenne des expressions normalisées (z-score) ou méthode GSVA.

**10. Analyses avancées (optionnelles)**
Ces analyses sont incluses dans les documents Quarto, mais exécutées uniquement si les packages requis sont installés (messages d'avertissement explicites).

⚠️ **GSEA (Hallmarks)** 
Utilisation de fgsea et msigdbr pour l'enrichissement de jeux de gènes prédéfinis (hallmarks, C2, etc.).

Préparation du classement des gènes (ranking selon logFC et p-value).

Visualisation des voies enrichies (barplots, network plots).

⚠️ **WGCNA (Weighted Gene Co-expression Network Analysis)**  
Construction de réseaux de co-expression à partir des données normalisées.

Identification de modules de gènes corrélés à des traits phénotypiques.

Visualisation : dendrogramme des modules, heatmap des corrélations module-trait, réseau d'intramodularité.

⚠️ **Enrichissement des facteurs de transcription (TF)**  
Utilisation de msigdbr pour extraire les jeux de gènes cibles de TFs (collection C3).

Analyse d'enrichissement par hypergéométrique ou GSEA ciblée sur les TFs.

⚠️ **Tests de normalité avancés**  
Évaluation de la normalité multivariée des données transformées avec le package MVN (test de Mardia, Henze-Zirkler, etc.). Utile pour valider les hypothèses de certains modèles linéaires.

**Résultats**

Pour chaque projet, les fichiers générés sont stockés dans projetX/results/ :

- tables/ : listes de gènes DE (avec logFC, p-values, FDR), résultats d'enrichissement (GO, KEGG).

- figures/ : ACP, heatmaps, volcano plots, boxplots, diagrammes de Venn, etc.

- éventuellement un rapport HTML/PDF produit par Quarto, synthétisant l'ensemble de l'analyse.

Références
- Law, C. W., et al. (2014). voom: precision weights unlock linear model analysis tools for RNA-seq read counts. Genome Biology.

- Ritchie, M. E., et al. (2015). limma powers differential expression analyses for RNA-sequencing and microarray studies. Nucleic Acids Research.

- Love, M. I., et al. (2014). *Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2*. Genome Biology.

- Yu, G., et al. (2012). clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS.

- Subramanian, A., et al. (2005). Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. PNAS.

- Langfelder, P., & Horvath, S. (2008). WGCNA: an R package for weighted correlation network analysis. BMC Bioinformatics.

**Organisation du dépôt**  

├── projet1/                     # Premier projet (ex : "Cancer du sein")
│   ├── projet1.qmd              # Document Quarto principal (analyse complète)
│   ├── data/                     # Données spécifiques au projet (comptages, metadata)
│   ├── results/                   # Sorties générées (tables, figures)
│   └── README.md                  # Informations spécifiques au projet (optionnel)
├── projet2/                     # Deuxième projet
│   ├── projet2.qmd
│   ├── data/
│   └── results/
├── projet3/                     # Troisième projet
│   ├── projet3.qmd
│   ├── data/
│   └── results/
├── utils/                        # Scripts R partagés (fonctions personnalisées)
├── _quarto.yml                   # Configuration globale pour Quarto (optionnel)
├── LICENSE
└── README.md                     # Ce fichier


