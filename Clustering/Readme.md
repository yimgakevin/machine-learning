Analyse des Données d'Ozone et Détection de Pics de Pollution

Introduction  
Ce notebook explore un jeu de données lié à la prévision de la concentration d'ozone, avec l'objectif principal d'améliorer la prévision déterministe (MOCAGE) et de déterminer la meilleure stratégie pour anticiper l'occurrence d'un pic de pollution. Il aborde des techniques d'analyse exploratoire des données, de transformation de variables, d'Analyse en Composantes Principales (ACP) et d'Analyse Factorielle Discriminante (AFD).

Description des Données  
Les données ont été fournies par Météo France et contiennent les variables suivantes :

JOUR : Type de jour (férié=1, non férié=0).  
O3obs : Concentration d'ozone observée le lendemain à 17h (cible principale).  
MOCAGE : Prévision de la concentration d'ozone par un modèle déterministe.  
TEMPE : Température prévue pour le lendemain 17h.  
RMH2O : Rapport d'humidité.  
NO2 : Concentration en dioxyde d'azote.  
NO : Concentration en monoxyde d'azote.  
STATION : Lieu de l'observation (Aix-en-Provence, Rambouillet, Munchhausen, Cadarache, Plan de Cuques).  
VentMOD : Force du vent.  
VentANG : Orientation du vent.  
Prise en charge et Préparation des Données

Chargement des données : Les données sont chargées à partir d'un fichier CSV distant.  
Vérification et conversion des types : Les colonnes JOUR et STATION sont converties en type Categorical, et O3obs en float.  
Analyse descriptive : Affichage des premières lignes (.head()), des informations (.info()) et des statistiques descriptives (.describe()).  
Feature Engineering  
Des transformations sont appliquées à certaines variables pour améliorer leur distribution et leur pertinence pour l'analyse :

SRMH2O : Racine carrée de RMH2O.  
LNO2 : Logarithme de NO2.  
LNO : Logarithme de NO. Les colonnes originales RMH2O, NO2, NO sont ensuite supprimées.  
Une variable binaire DepSeuil est créée, indiquant si la concentration d'ozone observée (O3obs) dépasse le seuil de 150 (True si O3obs > 150, False sinon).  

Analyse Exploratoire des Données (EDA)
Analyse Univariée
Variables quantitatives : Utilisation de box plots et d'histograms (sns.histplot, plt.show()) pour visualiser la distribution de chaque variable numérique.
Variables qualitatives : Analyse des comptages de valeurs (.value_counts()), puis visualisation avec des pie charts et des bar plots pour JOUR et STATION.
Analyse Bivariée
Variables quantitatives : Utilisation de scatter plots (ozone.plot(kind="scatter")) et d'une matrice de scatter plots (scatter_matrix) pour explorer les relations par paires entre les variables numériques.
Variables quantitative et qualitative : Box plots (ozone.boxplot(column="O3obs", by="JOUR"), ozone.boxplot(column="O3obs", by="STATION")) pour comparer les distributions de O3obs selon les catégories de JOUR et STATION.
Variables qualitatives : Tableau de contingence (pd.crosstab) et mosaic plot (mosaic) pour étudier la relation entre DepSeuil et STATION.
Analyse en Composantes Principales (ACP)
L'ACP est appliquée aux variables numériques standardisées du jeu de données ozone pour réduire la dimensionalité et visualiser les structures sous-jacentes.

Standardisation des données : Les variables sont mises à l'échelle (scale).
Calcul des composantes principales : PCA est utilisée pour ajuster et transformer les données.
Variance expliquée : Un graphique montre la décroissance de la variance expliquée par chaque composante.
Distribution des composantes : Des box plots des composantes sont utilisés pour identifier d'éventuels outliers.
Représentation des individus : Les individus sont projetés sur le plan formé par les deux premières composantes, colorés selon la variable DepSeuil.
Cercle des Corrélations : Un cercle des corrélations est tracé pour visualiser les relations entre les variables originales et les deux premiers axes factoriels, incluant le pourcentage de variance expliquée par chaque axe.
Interprétation: L'Axe 1 est fortement corrélé avec O3obs, MOCAGE, TEMPE, LNO2, et SRMH2O, suggérant une dimension liée à la concentration d'ozone et aux conditions météorologiques/chimiques. L'Axe 2 est plus lié aux variables de vent (VentMOD, VentANG).
Analyse Factorielle Discriminante (AFD)
L'AFD est appliquée sur un jeu de données insect (chargé séparément à partir de lubisch.txt) pour illustrer sa capacité à discriminer des classes.

Chargement des données insect : Six variables numériques (X1 à X6) et une variable de classe (Y) sont utilisées.

ACP sur insect : Une ACP initiale est réalisée pour une visualisation préliminaire, montrant une certaine séparation des classes.
Application de l'AFD : LinearDiscriminantAnalysis est utilisée pour trouver les fonctions discriminantes qui maximisent la séparation des classes.
Visualisation des individus AFD : Les individus sont projetés sur le plan formé par les deux premières fonctions discriminantes, colorés par leur classe d'insecte.

