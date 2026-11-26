# Plan : Vérification des variables avec le dictionnaire OMS

## Objectif
Après la détection automatique des variables, permettre à l'utilisateur de vérifier ses colonnes contre les alias du dictionnaire OMS (`OMS_DICT`) pour mieux faire les équivalences.

## Modifications dans `app_v2_final.R`

### 1. UI — Onglet "Fichier local" (après les mapper rows, ~ligne 1393)
Ajouter une section **"Vérification avec le dictionnaire"** sous les deux colonnes du mappeur :
- Un tableau DT interactif montrant pour chaque variable OMS :
  - **Variable OMS** (clé + label)
  - **Colonne détectée** dans les données
  - **Statut** (Détecté / Non trouvé)
  - **Tous les alias reconnus** du dictionnaire `OMS_DICT` (pour que l'utilisateur compare)
  - **Aperçu** des valeurs échantillon de la colonne détectée

### 2. UI — Onglet "Conformité OMS" (après le tableau, ~ligne 1533)
Ajouter un panneau de détail `uiOutput("dict_selected_aliases")` qui s'affiche quand l'utilisateur clique sur une ligne du tableau de conformité :
- Affiche les **valeurs échantillon** de la colonne sélectionnée
- Affiche tous les **alias du dictionnaire** pour la variable OMS détectée (sous forme de badges, avec surbrillance de l'alias qui a matché)
- Permet de visualiser rapidement si la correspondance est correcte

### 3. Serveur — Nouveau `output$dict_verification_table` (~après ligne 7688)
- Parcourt `OMS_DICT` et `map_cols()` pour construire un data.frame de vérification
- Affiche un DT avec formatStyle conditionnel (vert=détecté, rouge=non trouvé)
- Les alias sont affichés en texte concaténé pour comparaison rapide

### 4. Serveur — Nouveau `output$dict_selected_aliases` (~après ligne 7907)
- Réagit au clic sur `input$oms_conform_table_rows_selected`
- Affiche les alias sous forme de badges cliquables
- Met en surbrillance l'alias qui a correspondu à la colonne

## Résumé des fichiers modifiés
- **`app_v2_final.R`** : UI (~2 insertions) + Serveur (~2 nouveaux outputs)
- Aucun autre fichier modifié
