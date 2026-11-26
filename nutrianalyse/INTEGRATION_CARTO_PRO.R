# ========================================================
# INTEGRATION_CARTO_PRO.R
# Guide pour integrer le module cartographique Pro
# ========================================================

# ETAPE 1: Dans app_v2_final.R, remplacer:
#   source("ui_carto_v2.R")
# par:
#   source("ui_carto_pro.R")

# ETAPE 2: Dans app_v2_final.R, dans le body de dashboardBody, remplacer:
#   ui_carto_v2
# par:
#   ui_carto_pro

# ETAPE 3: Dans le server de app_v2_final.R, ajouter :
#   source("carto_server_pro.R", local = TRUE)
# APRES la ligne source("carto_server_outputs.R", ...)
# Ou remplacer cette ligne par:
#   source("carto_server_pro.R", local = TRUE)

# ETAPE 4 (optionnel): Commenter les anciennes sources:
#   # source("carto_server_outputs.R", local = TRUE)
#   # source("ui_carto_v2.R")

# ========================================================
# FICHIERS CREES:
# 1. carto_pro_engine.R     - Moteur cartographique (fonctions)
# 2. ui_carto_pro.R         - Interface utilisateur
# 3. carto_server_pro.R     - Logique server
# ========================================================

# NOUVELLES FONCTIONNALITES:
# - Carte BIVARIATE (croisement 2 indicateurs, ex: GAM x Stunting)
# - Carte MULTI-INDICATEURS (small multiples, jusqu a 6)
# - POPUPS professionnels avec mini-barres de progression
# - KPIs en overlay (prevalence, effectif, zones en alerte)
# - Legende HTML professionnelle avec seuils OMS
# - Cartes par DEPARTEMENT et ARRONDISSEMENT
# - Export PNG haute resolution (300 dpi) et PDF
# - Classification OMS avec couleurs normees
# - Carte statique ggplot2 de qualite publication
# - Labels intelligents proportionnels a la prevalence
# ========================================================
