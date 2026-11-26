# Plan : Sélecteur de palette global dans toute l'application

## Problème
Seuls 3 modules ont un sélecteur de palette (univarié, bivarié, carto statique). Les autres sections (exploration, clustering, z-scores, sécurité alimentaire, réduction de dimension, dplyr, accueil, etc.) utilisent des couleurs codées en dur. L'utilisateur veut pouvoir choisir sa palette PARTOUT.

## Stratégie
Créer un **sélecteur de palette GLOBAL** dans la sidebar, visible partout, avec les thèmes institutionnels (OMS, UNICEF, FAO, PAM) + palettes populaires. Ce sélecteur unique contrôle toutes les couleurs de tous les graphiques.

## Étape 1 : Définir les palettes institutionnelles (dans `utils.R`)

Créer `PALETTES_GLOBAL` — un dictionnaire nommé avec :

| Nom | Source | Couleurs |
|-----|--------|----------|
| OMS officiel | WHO | vert → jaune → orange → rouge → rouge foncé |
| UNICEF | UNICEF | bleu clair → bleu UNICEF → bleu foncé |
| FAO | FAO | vert olive → jaune → brun terre |
| PAM / WFP | WFP | bleu WFP → bleu clair → blanc |
| Viridis | R | viridis classique |
| Spectral | ColorBrewer | arc-en-ciel divergent |
| Set2 | ColorBrewer | catégoriel doux |
| Inferno | R | jaune → rouge → noir |
| Plasma | R | jaune → violet |
| Terre (Nature) | Custom | vert nature → brun → beige |

Chaque palette doit fournir :
- `seq` : version séquentielle (pour heatmaps, choroplèthes)
- `cat` : version catégorielle (pour barplots, groupes)
- `div` : version divergente (pour corrélations)

## Étape 2 : Ajouter le sélecteur global dans la sidebar

Dans la sidebar (sous les filtres ou en haut), ajouter :
```r
selectInput("global_palette", "🎨 Thème couleurs :",
  choices = names(PALETTES_GLOBAL))
```

## Étape 3 : Créer `get_global_pal(n, type)` dans `utils.R`

Fonction centralisée qui remplace tous les appels directs :
- Lit `input$global_palette` (passé en paramètre)
- Retourne exactement `n` couleurs du type demandé (seq/cat/div)

## Étape 4 : Mettre à jour les fonctions de palette existantes

- `get_pal()` → ajouter paramètre `palette_name` optionnel
- `pal_fn_uni()` → utiliser le choix global si aucun choix local
- `gpalf()` (bivarié) → idem

## Étape 5 : Appliquer dans chaque section

### Sections à modifier :
1. **Accueil** : `accueil_prev_chart`, `accueil_zscore` → utiliser palette globale
2. **Exploration** : `explor_auto_plot` → utiliser palette globale
3. **Univarié** : garder le sélecteur local MAIS pré-remplir avec le choix global
4. **Bivarié** : idem, garder local mais synchro avec global
5. **Réduction de dimension** : `rdim_contrib`, `rdim_cos2`, `rdim_biplot`, `rdim_circle`
6. **Clustering** : `clust_plot2d`, `clust_profile`, `clust_optimal`
7. **Z-scores** : `zs_dist_*`, `zs_donut_*`, `zs_sex_*`, `zs_stacked_bar`
8. **Sécurité alimentaire** : `sa_fcs_*`, `sa_rcsi_*`, `sa_hdds_*`, `sa_hhs_*`
9. **Qualité SMART** : `sq_flags_plot`, `sq_dp_plot`, `sq_age_plot`
10. **Dplyr quick-viz** : `dplyr_plot_out`
11. **Cartographie** : `carto_pal_static` → enrichir choix + synchro global
12. **Inférentielle** : `infer_plot`, `multi_forest`

## Étape 6 : Synchro sidebar palette → palettes locales uni/biv/carto

Quand le global change, mettre à jour automatiquement :
- `uni_pal` → sélection correspondante
- `biv_pal` → sélection correspondante
- `carto_pal_static` → sélection correspondante

## Étape 7 : Enrichir carto_pal_static

Ajouter les mêmes choix que le global dans le sélecteur de la cartographie.

## Étape 8 : Validation syntaxique
