Apprentissage Semi-Supervisé pour la Classification d’Images Médicales

Présentation

Ce projet porte sur l’étude et l’implémentation de différentes techniques d’apprentissage semi-supervisé appliquées à la classification d’images médicales. L’apprentissage semi-supervisé vise à tirer profit à la fois des données étiquetées et des données non étiquetées afin d’améliorer la performance des modèles, notamment dans les contextes où l’annotation manuelle est coûteuse ou limitée.

Méthodes explorées

Ce travail comprend l’expérimentation de plusieurs approches majeures du semi-supervisé, parmi lesquelles :

1. Label Propagation : méthode classique qui diffuse les labels des données étiquetées vers les données non-étiquetées via un graphe de similarité.

2. sGAN (Semi-supervised Generative Adversarial Networks) : exploitation des GANs pour générer des exemples synthétiques et apprendre à partir de données partiellement étiquetées.

3. FixMatch : combinaison de pseudo-labellisation et de régularisation par cohérence avec des augmentations faibles et fortes.

4. FlexMatch : amélioration dynamique de FixMatch qui ajuste le seuil de confiance pour la pseudo-étiquetage en fonction des classes.

5. MixMatch : mélange de techniques incluant la pseudo-étiquetage, les augmentations, et la régularisation via la combinaison de données.

6. Régularisation par cohérence : encourager le modèle à faire des prédictions stables face à des perturbations (ex : augmentations, bruit).

7. Pseudo-labelling : utilisation des prédictions du modèle comme labels artificiels pour les données non-étiquetées.

Datasets utilisés

DermaMNIST : dataset d’images dermatologiques annotées, utilisé pour la classification des maladies de la peau.

Kevasev : dataset spécifique d’images médicales, comprenant un grand nombre d’exemples non étiquetés pour tester les méthodes semi-supervisées.
