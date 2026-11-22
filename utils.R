# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  utils.R — Fonctions utilitaires NutriAnalyse Pro v2                       ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ── Opérateur null-coalescing ─────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ── PALETTE INSTITUTIONNELLE (OMS/UNHCR/PAM) ─────────────────────────────────
C <- list(
  bleu="#0077B6", bleu_fonce="#0B3954", bleu_moyen="#0C4A6E",
  vert="#2E7D52", vert_fonce="#1B5E3B", jaune="#D4A843",
  orange="#D98B14", rouge="#BF3636", rouge_f="#5C1A1A",
  violet="#6B3FA0", gris="#6B7B8D", unicef="#009fda",
  teal="#00838F", rose="#D4537E", fond="#F4F6FB",
  sidebar="#0B3954", header="#0C4A6E", or="#D4A843"
)
pal_gam  <- c(C$vert, C$jaune, C$orange, C$rouge, C$rouge_f)
pal_div  <- c(C$rouge_f, C$rouge, C$orange, "#FFF9E6", C$teal, C$bleu, C$sidebar)

seuils_gam <- list(
  acceptable=5, alerte=10, serieux=15, critique=30
)

couleur_prevalence <- function(p) {
  if (is.na(p)) return(C$gris)
  if (p < seuils_gam$acceptable) return(C$vert)
  if (p < seuils_gam$alerte)     return(C$jaune)
  if (p < seuils_gam$serieux)    return(C$orange)
  if (p < seuils_gam$critique)   return(C$rouge)
  return(C$rouge_f)
}

alerte_label <- function(p) {
  if (is.na(p)) return("N/A")
  if (p < seuils_gam$acceptable) return("🟢 Acceptable")
  if (p < seuils_gam$alerte)     return("🟡 Alerte")
  if (p < seuils_gam$serieux)    return("🟠 Sérieux")
  if (p < seuils_gam$critique)   return("🔴 Critique")
  return("🟥 Urgence maximale")
}

# ── COLONNES OMS — dictionnaire exhaustif ────────────────────────────────────
# Sources : DHIS2 Tracker/Aggregate, UNICEF NutriDash, PAM SCOPE/VAM,
#            ENA/SMART, WHO Anthro, DHS, MICS, IPC + noms terrain multi-langues
OMS_DICT <- list(
  # ═══ ANTHROPOMÉTRIE — Z-scores ═══
  whz = c("whz","whz_score","wfh","wfhz","weight_for_height_z","peso_talla_z",
          "whz_score_final","whzscore","zscoreWH","wh_zscore","zscore_wh",
          "wasting_zscore","indice_pt","indice_poids_taille","rapport_poids_taille",
          "z_score_poids_taille","wfl","wfh_zscore","nut_whz","weight_height_zscore",
          "wfhz_score","poids_taille_z","score_z_pt"),
  haz = c("haz","haz_score","hfa","hfaz","height_for_age_z","talla_edad_z",
          "hazscore","haz_score_final","zscoreHA","ha_zscore","zscore_ha",
          "stunting_zscore","indice_ta","indice_taille_age","rapport_taille_age",
          "z_score_taille_age","lhfa","hfa_zscore","nut_haz","height_age_zscore",
          "hfaz_score","taille_age_z","score_z_ta"),
  waz = c("waz","waz_score","wfa","wfaz","weight_for_age_z","peso_edad_z",
          "wazscore","waz_score_final","zscoreWA","wa_zscore","zscore_wa",
          "underweight_zscore","indice_pa","indice_poids_age","rapport_poids_age",
          "z_score_poids_age","wfa_zscore","nut_waz","weight_age_zscore",
          "wfaz_score","poids_age_z","score_z_pa"),

  # ═══ ANTHROPOMÉTRIE — Mesures brutes ═══
  muac = c("muac","muac_mm","muac_cm","pb","pbm","mid_upper_arm_circumference",
           "muac_enfant","brachial","muac_child","arm_circumference",
           "perimeter_brachial","pb_mm","pb_cm","pbm_mm","mid_arm",
           "brachial_perimeter","muac_measurement","circ_bras","tour_de_bras",
           "perimetre_brachial","arm_circ","upper_arm"),
  poids = c("weight","weight_kg","poids","poids_kg","peso","pesos","body_weight",
            "child_weight","wt","wgt","masse","poids_enfant","poids_mesure",
            "child_wt","body_mass","kg","weight_child","wgt_kg"),
  taille = c("height","height_cm","taille","taille_cm","altura","talla",
             "body_height","child_height","ht","hgt","longueur","taille_enfant",
             "taille_mesure","length_cm","body_length","recumbent_length",
             "standing_height","lying_length","debout","couche","height_length"),
  oedema = c("oedema","oedeme","edema","bilateral_oedema","oedemes","oedema_yn",
             "odemas","oedema_bilateral","bilateral_pitting_edema","oedeme_bilateral",
             "nutritional_oedema","oedeme_nutritionnel","edema_yn"),

  # ═══ DÉMOGRAPHIE ═══
  age_m = c("age_months","child_age_months","age_mois","age_en_mois","agemos",
            "agem","age_month","mois","age_child_months","child_months",
            "age_in_months","age_mths","age_mo","months_old","age_enfant_mois"),
  sexe = c("sex","sexe","child_sex","gender","genre","child_gender","sex_child",
           "sexe_enfant","sex_of_child","child_sex_code","sexo","geschlecht"),
  date_naissance = c("dob","date_of_birth","birthdate","date_naissance","naissance",
                     "birth_date","ddn","date_birth","child_dob","child_birthdate",
                     "fecha_nacimiento","data_nascimento"),
  date_mesure = c("measurement_date","date_mesure","date_visite","visit_date",
                  "survey_date","date_enquete","date_collecte","collection_date",
                  "data_collection_date","interview_date","fecha_encuesta"),

  # ═══ GÉOGRAPHIE ═══
  region = c("region","regions","zone","area","admin1","wilaya","gouvernorat",
             "province","state","adm1_name","admin1_name","region_name",
             "nom_region","estado","provincia","oblast","county"),
  dept = c("department","departement","dept","district","admin2","commune",
           "cercle","prefecture","adm2_name","admin2_name","district_name",
           "nom_departement","nom_district","sous_prefecture","lga",
           "woreda","kebele","municipio","canton"),
  village = c("village","village_name","localite","locality","site","camp",
              "camp_name","settlement","nom_village","nom_localite",
              "community","communaute","aldea","pueblo"),
  cluster = c("cluster","grappe","psu","cluster_id","cluster_no",
              "primary_sampling_unit","unite_echantillonnage","cluster_number",
              "grappe_id","numero_grappe","sampling_unit"),
  lat = c("latitude","gps_latitude","lat","gps_lat","coord_lat","y_coord",
          "latitud","gps_y","lat_deg","latitude_deg"),
  lon = c("longitude","gps_longitude","lon","lng","gps_lon","coord_lon",
          "x_coord","longitud","gps_x","lon_deg","longitude_deg"),

  # ═══ SÉCURITÉ ALIMENTAIRE — Scores composites ═══
  fcs = c("fcs","food_consumption_score","fcs_score","score_conso",
          "score_consommation","food_conso_score","sca","score_conso_alim"),
  rcsi = c("rcsi","rcsi_score","csi","reduced_csi","reduced_coping_strategy",
           "coping_strategy_index","indice_strategies","indice_strategie_reduit",
           "reduced_coping_strategy_index","score_rcsi","rcsiscore"),
  hdds = c("hdds","hdds_score","dietary_diversity_score","score_diversite",
           "household_dietary_diversity","dds","score_diversite_alimentaire",
           "dietary_diversity","sdam","diversite_alimentaire"),
  hhs = c("hhs","hhs_score","household_hunger_scale","echelle_faim",
          "hunger_scale","hhs_total","score_faim","hunger_score",
          "household_hunger_score"),

  # ═══ SÉCURITÉ ALIMENTAIRE — Composantes FCS (PAM) ═══
  fcs_cereales = c("fcs_staples","fcs_cereals","fcs1","cereales","staples",
                   "fcs_cereal","cereales_tubercules","fcs_stap",
                   "fcsstap","fcscereales","fcscereals","fcstaples"),
  fcs_legumineuses = c("fcs_pulses","fcs_legumes_sec","fcs2","legumineuses",
                       "pulses","fcs_pulse","haricots","fcs_leg",
                       "fcspulse","fcspulses","fcslegumineuses","fcslegum"),
  fcs_lait = c("fcs_dairy","fcs_milk","fcs3","lait","dairy","produits_laitiers",
               "fcs_dai","milk_products",
               "fcsdairy","fcslait","fcsmilk"),
  fcs_proteines = c("fcs_protein","fcs_meat","fcs4","proteines","viande",
                    "protein","meat_fish","fcs_prot","fcs_meat_fish",
                    "fcspr","fcsprot","fcsprotein","fcsviande","fcsmeat",
                    "fcs_pr","fcsanimal"),
  fcs_legumes = c("fcs_vegetables","fcs_veg","fcs5","legumes","legumes_feuilles",
                  "vegetables","fcs_vegetable","green_leafy",
                  "fcsveg","fcsveget","fcslegumes","fcsvegetables"),
  fcs_fruits = c("fcs_fruit","fcs_fruits","fcs6","fruits","fruit","fcs_frt",
                 "fcsfruit","fcsfruits","fcsfrt"),
  fcs_huile = c("fcs_oil","fcs_fat","fcs7","huile","graisse","oil","fat",
                "oil_fat","fcs_oil_fat",
                "fcsfat","fcsoil","fcshuile","fcsgraisse"),
  fcs_sucre = c("fcs_sugar","fcs_sweet","fcs8","sucre","sugar","condiments",
                "fcs_condiments","sweet_condiments",
                "fcssugar","fcssucre","fcssweet","fcscondiments"),

  # ═══ SÉCURITÉ ALIMENTAIRE — Composantes HHS (FANTA) ═══
  hhs_nofood = c("hhs_nofood","hhs1","hhs_no_food","hhs_nofood_freq",
                 "pas_nourriture","no_food_eat","hhs_q1",
                 "hhsnofood","hhsnofoodfreq","hhsq1","hhs_nofoodfrq",
                 "HHSNoFood","nofoodhome","nofood_freq",
                 "hhs_nourriture"),
  hhs_sleep = c("hhs_sleep","hhs2","hhs_bed_hungry","hhs_sleep_freq",
                "dormi_faim","sleep_hungry","hhs_q2",
                "hhsbedhungry","hhssleep","hhsq2","hhssleephungry",
                "HHSBedHungry","bedhungry","bed_hungry_freq",
                "hhs_sommeil"),
  hhs_wholeday = c("hhs_wholeday","hhs3","hhs_whole_day","hhs_wholeday_freq",
                   "journee_sans_manger","whole_day_no_food","hhs_q3",
                   "hhsnomealday","hhswholeday","hhsq3","hhs_nomealday",
                   "HHSNoMealDay","nomealday","wholedaynofood",
                   "hhs_journee"),

  # ═══ COMPOSANTES rCSI (WFP/PAM) — 5 stratégies d'adaptation ═══
  rcsi_lessqlty = c("rcsi_less_qlty","rcsilessqlty","rcsi_lesspref","rcsi_lessqlty",
                    "rCSILessQlty","rcsi1","less_preferred","alim_moins_pref",
                    "moinsqualite","less_quality","coping_lessqlty","pref_alim_moins",
                    "rcsi_qualite"),
  rcsi_borrow = c("rcsi_borrow","rcsiborrow","rcsi_emprunter","rcsi2",
                  "rCSIBorrow","borrow_food","emprunter_nourriture","coping_borrow",
                  "emprunter","borrow","aide_alimentaire","borrow_rely",
                  "rcsi_emprunt"),
  rcsi_meal_nb = c("rcsi_meal_nb","rcsimealnb","rcsi_repas","rcsi3",
                   "rCSIMealNb","reduce_meals","reduire_repas","fewer_meals",
                   "nombre_repas","reduce_nb_meals","mealnb","coping_mealnb"),
  rcsi_meal_size = c("rcsi_meal_size","rcsimealsize","rcsi_portions","rcsi4",
                     "rCSIMealSize","reduce_portions","reduire_portions","smaller_portions",
                     "portions_reduites","coping_mealsize","mealsize","limit_portion",
                     "rcsi_taille_repas"),
  rcsi_meal_adult = c("rcsi_meal_adult","rcsimealadult","rcsi_adultes","rcsi5",
                      "rCSIMealAdult","restrict_adults","reduire_adultes","adult_restrict",
                      "adultes_reduire","coping_mealadult","mealadult","adult_priority",
                      "rcsi_adulte"),

  # ═══ COMPOSANTES HDDS (12 groupes alimentaires FAO/FANTA) ═══
  hdds_cereales = c("hdds_cereales","hdds_cereal","hdds_cereals","hdds1",
                    "dd_cereales","dd_cereal","grp_cereal","cereale_hdds",
                    "hdds_grp1","hdds_staples","hdds_grain"),
  hdds_tubercules = c("hdds_tubercules","hdds_tuber","hdds_roots","hdds2",
                      "dd_tubercules","dd_tuber","grp_tubercule","tubercule_hdds",
                      "hdds_grp2","hdds_roottuber","hdds_root_tuber",
                      "hdds_racines"),
  hdds_legumes_hdds = c("hdds_legumes","hdds_veg","hdds_vegetables","hdds3",
                        "dd_legumes","dd_veg","grp_legume","legume_hdds",
                        "hdds_grp3","hdds_vegetable","hdds_greens"),
  hdds_fruits_hdds = c("hdds_fruits","hdds_fruit","hdds_frt","hdds4",
                       "dd_fruits","dd_fruit","grp_fruit","fruit_hdds",
                       "hdds_grp4","hdds_frts"),
  hdds_viande = c("hdds_viande","hdds_meat","hdds_viandes","hdds5",
                  "dd_viande","dd_meat","grp_viande","viande_hdds",
                  "hdds_grp5","hdds_organ_meat","hdds_orgviande"),
  hdds_oeufs = c("hdds_oeufs","hdds_eggs","hdds_egg","hdds6",
                 "dd_oeufs","dd_eggs","grp_oeuf","oeuf_hdds",
                 "hdds_grp6","hdds_egg"),
  hdds_poisson = c("hdds_poisson","hdds_fish","hdds_seafood","hdds7",
                   "dd_poisson","dd_fish","grp_poisson","poisson_hdds",
                   "hdds_grp7","hdds_fishseafood"),
  hdds_legumineuses_hdds = c("hdds_legumineuses","hdds_pulses","hdds_legum","hdds8",
                             "dd_legumineuses","dd_pulses","grp_legumineuse",
                             "legumineuse_hdds","hdds_grp8","hdds_bean","hdds_nuts",
                             "hdds_noix"),
  hdds_lait = c("hdds_lait","hdds_dairy","hdds_milk","hdds9",
                "dd_lait","dd_dairy","grp_lait","lait_hdds",
                "hdds_grp9","hdds_milk_dairy"),
  hdds_huile = c("hdds_huile","hdds_oil","hdds_fat","hdds10",
                 "dd_huile","dd_oil","grp_huile","huile_hdds",
                 "hdds_grp10","hdds_oil_fat"),
  hdds_sucre = c("hdds_sucre","hdds_sugar","hdds_sweet","hdds11",
                 "dd_sucre","dd_sugar","grp_sucre","sucre_hdds",
                 "hdds_grp11","hdds_sugar_honey"),
  hdds_condiments = c("hdds_condiments","hdds_cond","hdds_spices","hdds12",
                      "dd_condiments","dd_cond","grp_condiment","condiment_hdds",
                      "hdds_grp12","hdds_spice"),

  # ═══ ENQUÊTE & MÉTADONNÉES ═══
  equipe = c("team","equipe","team_id","enqueteur","enumerator","data_collector",
             "equipe_enquete","team_number","team_name","superviseur",
             "collecteur","interviewer","agent"),
  strate = c("strata","strate","stratum","strat","sampling_strata",
             "strate_echantillonnage","survey_strata","stratification"),
  depl = c("displacement_status","statut_deplacement","statut","status",
           "population_type","pop_type","idp_status","refugee_status",
           "type_population","deplacement","displaced","idp","refugie",
           "resident","host_community",
           "statut_deplacement_enq"),
  survey = c("survey_round","round","vague","enquete_round","wave",
             "survey_wave","round_number","passage","numero_passage"),

  # ═══ SANTÉ / VACCINATION (SMART+) ═══
  rougeole = c("measles","rougeole","measles_vaccination","vac_measles",
               "vaccin_rougeole","measles_vacc","rubeola","sarampion",
               "measles_card","measles_recall"),
  vita = c("vitamin_a","vita","vitamine_a","vit_a","vit_a_supplementation",
           "vitamin_a_supp","supplementation_vita","vit_a_6months"),
  diarrhee = c("diarrhea","diarrhoea","diarrhee","has_diarrhea","diarrhea_2w",
               "diarrhee_2sem","loose_stools","diarrea","diarrea_14d"),
  ira = c("ari","ira","acute_respiratory_infection","infection_respiratoire",
          "cough_fever","toux_fievre","respiratory_infection","pneumonia",
          "ari_2weeks","ira_2sem"),

  # ═══ IMC / BMI ═══
  bmi = c("bmi","imc","body_mass_index","indice_masse_corporelle",
          "bmi_zscore","bmiz","baz","bmi_for_age_z","bmia","imc_age"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ A. MORTALITÉ (SMART CORE) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  nb_deces = c("deaths","deces","nb_deces","number_deaths","nombre_deces",
               "total_deaths","death_count","nb_morts","nbre_deces","decedes",
               "died","mort","morts","fallecidos","deceased"),
  date_deces = c("date_death","date_deces","death_date","date_mort",
                 "fecha_muerte","date_du_deces","dod","date_of_death",
                 "death_day","jour_deces"),
  cause_deces = c("cause_death","cause_deces","death_cause","cause_du_deces",
                  "cause_muerte","raison_deces","cod","cause_of_death",
                  "death_reason","motif_deces"),
  lieu_deces = c("location_death","lieu_deces","death_location","place_of_death",
                 "endroit_deces","lugar_muerte","death_place","lieu_du_deces"),
  person_time = c("person_time","personne_temps","person_days","person_years",
                  "pt","person_time_days","jours_personne","tiempo_persona",
                  "exposure_time","temps_exposition","follow_up_time"),
  cmr = c("cmr","crude_mortality_rate","taux_mortalite_brut","crude_death_rate",
          "cdr","taux_mortalite","mortality_rate","tmb","tasa_mortalidad",
          "death_rate","overall_mortality"),
  u5mr = c("u5mr","under5_mortality_rate","taux_mortalite_moins5","u5_mortality",
           "child_mortality_rate","tmm5","mortalite_moins_5ans","under_five_mr",
           "tasa_mortalidad_menores5","child_death_rate","mortality_under5"),
  naissance_recall = c("births","naissances","nb_naissances","birth_count",
                       "number_births","nombre_naissances","total_births",
                       "nacimientos","naissance","born","borns"),
  arrive_menage = c("joined","arrives","arrivees","joined_hh","arrived",
                    "nouvelles_arrivees","entrants","in_migration","immigration",
                    "llegadas","new_members","membres_arrives"),
  depart_menage = c("left","departs","partis","left_hh","departed",
                    "departs_menage","sortants","out_migration","emigration",
                    "salidas","members_left","membres_partis"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ B. IYCF — NUTRITION INFANTILE DÉTAILLÉE (OMS/UNICEF) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  allaitement_excl = c("exclusive_breastfeeding","allaitement_exclusif","ebf",
                       "excl_breastfeeding","allaitement_excl","breastfeeding_exclusive",
                       "allait_exclu","lactancia_exclusiva","excl_bf",
                       "exclusive_bf","allaitement_maternel_exclusif",
                       "bf_exclusive","sein_exclusif"),
  allaitement_cont = c("continued_breastfeeding","allaitement_continue","cbf",
                       "still_breastfeeding","allaitement_en_cours","breastfeeding_continued",
                       "lactancia_continuada","cont_bf","breastfed_currently",
                       "currently_breastfed","breastfeeding_current"),
  initiation_allaitement = c("early_initiation_bf","initiation_precoce",
                             "breastfeeding_initiation","first_hour_bf",
                             "mise_au_sein_precoce","inicio_temprano_lactancia",
                             "bf_first_hour","debut_allaitement","early_bf",
                             "allaitement_premiere_heure","initiation_allaitement"),
  freq_repas_min = c("minimum_meal_frequency","mmf","freq_repas_min",
                     "meal_frequency","frequence_repas_minimum","min_meal_freq",
                     "frecuencia_comidas","meal_freq","nb_repas_min",
                     "min_feeding_frequency","child_meal_frequency"),
  diversite_alim_min = c("minimum_dietary_diversity","mdd","mdd_c","diversite_alim_min",
                         "min_dietary_diversity","diversite_alimentaire_min",
                         "child_dietary_diversity","diversidad_dietetica_min",
                         "min_diet_diversity","child_dds","iycf_mdd"),
  regime_acceptable_min = c("minimum_acceptable_diet","mad","regime_acceptable_min",
                            "min_acceptable_diet","regime_min_acceptable",
                            "dieta_minima_aceptable","acceptable_diet",
                            "iycf_mad","mad_score","min_accept_diet"),
  intro_aliments_solides = c("introduction_solid_foods","intro_complementary",
                             "complementary_feeding","alimentation_complementaire",
                             "intro_solides","alimentacion_complementaria",
                             "solid_food_intro","debut_alimentation_comp",
                             "first_solid_food","age_intro_solides"),
  cons_fer = c("iron_rich_foods","aliments_riches_fer","iron_consumption",
               "consommation_fer","cons_fer","hierro_alimentos","iron_food",
               "fer_alimentaire","iron_intake","apport_fer"),
  bouteille = c("bottle_feeding","alimentation_biberon","bottle_use",
                "biberon","feeding_bottle","uso_biberon","bottle_fed",
                "utilisation_biberon","bottle","bouteille_alimentation"),
  nb_groupes_alim_enfant = c("food_groups_child","groupes_alim_enfant",
                             "child_food_groups","nb_groupes_alimentaires",
                             "iycf_food_groups","grupos_alimentos_nino",
                             "child_diet_groups","grp_alim_enfant"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ C. ANÉMIE (SENS) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  hemoglobine = c("hemoglobin","hemoglobine","hb","hgb","haemoglobin",
                  "hemoglobina","hb_level","taux_hemoglobine","hb_gdl",
                  "hemoglobin_level","hb_value","hemoglobine_gdl",
                  "hb_concentration","blood_hemoglobin"),
  hb_ajustee = c("adjusted_hemoglobin","hb_adjusted","hemoglobine_ajustee",
                 "hb_adj","hb_alt_adjusted","hb_altitude_adjusted",
                 "hemoglobina_ajustada","hb_corrigee","adjusted_hb",
                 "hb_corrected","hemoglobin_adjusted",
                 "hb_ajuste"),
  statut_anemie = c("anemia_status","statut_anemie","anemia","anemie",
                    "anemia_classification","classification_anemie",
                    "estado_anemia","anemic","anemique","anemia_severity",
                    "severite_anemie","anemia_category"),
  altitude = c("altitude","altitude_m","elevation","altitude_metres",
               "altitud","alt","site_altitude","altitude_site",
               "elevation_m","hauteur_site","altitude_meters"),
  ajust_altitude = c("altitude_adjustment","ajustement_altitude",
                     "altitude_correction","correction_altitude",
                     "ajuste_altitud","hb_altitude_factor","alt_adjustment",
                     "facteur_altitude","altitude_adj","alt_correction"),
  fumeur = c("smoker","fumeur","smoking_status","statut_tabac","tabac",
             "fumador","smokes","fume","tobacco_use","usage_tabac",
             "smoking","cigarette"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ D. PALUDISME / MOUSTIQUAIRES (SENS) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  llin_possession = c("llin_possession","llin","itn_possession","moustiquaire_possession",
                      "bed_net","mosquito_net","posesion_mosquitero","has_net",
                      "possess_net","possede_moustiquaire","moustiquaire",
                      "net_ownership","itn","llin_own","insecticide_net"),
  llin_utilisation = c("llin_use","llin_utilisation","net_use","itn_use",
                       "utilisation_moustiquaire","uso_mosquitero","slept_under_net",
                       "dormi_sous_moustiquaire","net_used_last_night",
                       "moustiquaire_utilisee","use_bednet","net_previous_night"),
  llin_acces = c("llin_access","net_access","acces_moustiquaire","itn_access",
                 "acceso_mosquitero","access_net","moustiquaire_acces",
                 "sufficient_nets","assez_moustiquaires","net_coverage",
                 "couverture_moustiquaire"),
  nb_moustiquaires = c("number_nets","nb_moustiquaires","nombre_moustiquaires",
                       "total_nets","net_count","numero_mosquiteros",
                       "llin_count","nbre_moustiquaires","nets_household",
                       "moustiquaires_menage","how_many_nets"),
  tdr_malaria = c("malaria_rdt","tdr_palu","rapid_test_malaria","tdr_malaria",
                  "malaria_rapid_test","test_rapide_paludisme","rdt_result",
                  "resultat_tdr","prueba_rapida_malaria","malaria_test",
                  "palu_tdr","rdt_malaria"),
  fievre = c("fever","fievre","has_fever","fiebre","fever_2weeks",
             "fievre_2sem","fever_recent","fievre_recente","child_fever",
             "fievre_enfant","temperature_elevated","pyrexia",
             "fievre_paludisme"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ E. WASH — EAU, HYGIÈNE, ASSAINISSEMENT ═══
  # ═══════════════════════════════════════════════════════════════════════════
  source_eau = c("water_source","source_eau","drinking_water_source",
                 "source_eau_boisson","fuente_agua","main_water_source",
                 "source_principale_eau","water_type","type_eau",
                 "improved_water","eau_amelioree","water_supply"),
  temps_collecte_eau = c("water_collection_time","temps_collecte_eau",
                         "time_to_water","temps_acces_eau","water_fetch_time",
                         "tiempo_recoleccion_agua","round_trip_water",
                         "duree_collecte_eau","water_distance_time",
                         "temps_eau","water_time_minutes"),
  traitement_eau = c("water_treatment","traitement_eau","water_purification",
                     "purification_eau","tratamiento_agua","hh_water_treatment",
                     "traitement_eau_menage","water_treated","eau_traitee",
                     "chlorination","chloration","water_boiling"),
  quantite_eau = c("water_quantity","quantite_eau","liters_per_person",
                   "litres_par_personne","water_lpcd","lpcd",
                   "cantidad_agua","eau_quantite_jour","water_per_day",
                   "daily_water","eau_journaliere","water_volume"),
  type_latrine = c("latrine_type","type_latrine","toilet_type","type_toilette",
                   "tipo_letrina","sanitation_facility","installation_sanitaire",
                   "latrine","toilette","toilet_facility","latrines",
                   "improved_sanitation","assainissement_ameliore",
                   "latrine_acces"),
  latrine_partagee = c("shared_latrine","latrine_partagee","shared_toilet",
                       "toilette_partagee","letrina_compartida","latrine_shared",
                       "partage_latrine","communal_latrine","toilet_shared",
                       "latrine_commune"),
  defecation_air_libre = c("open_defecation","defecation_air_libre","dal",
                           "defecacion_aire_libre","no_toilet","pas_latrine",
                           "open_air","plein_air","no_latrine","no_facility",
                           "defecation_plein_air"),
  lavage_mains = c("handwashing","lavage_mains","hand_washing","hand_wash",
                   "lavado_manos","handwash_facility","station_lavage",
                   "lavage_des_mains","handwashing_facility","hw_facility",
                   "dispositif_lavage_mains","handwash_station",
                   "eau_lavage_mains"),
  savon_disponible = c("soap_available","savon_disponible","has_soap","soap",
                       "savon","jabon_disponible","soap_presence","presence_savon",
                       "soap_at_handwashing","savon_station","handwash_soap",
                       "soap_water"),
  eau_savon_lavage = c("water_and_soap","eau_et_savon","handwash_water_soap",
                       "agua_jabon","soap_and_water","eau_savon",
                       "handwashing_with_soap","lavage_mains_savon",
                       "hw_soap_water","proper_handwashing"),
  gestion_dechets = c("waste_management","gestion_dechets","garbage_disposal",
                      "elimination_dechets","manejo_residuos","waste_disposal",
                      "dechets","ordures","solid_waste","refuse_disposal",
                      "gestion_ordures"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ F. MÉNAGE — COMPOSITION, REVENUS, DÉPENSES ═══
  # ═══════════════════════════════════════════════════════════════════════════
  taille_menage = c("household_size","taille_menage","hh_size","nb_membres",
                    "family_size","tamano_hogar","nombre_membres","hh_members",
                    "members_count","effectif_menage","total_hh_members",
                    "nb_personnes_menage","household_members"),
  taille_menage_0_5 = c("children_under5","enfants_moins_5","hh_under5",
                        "nb_enfants_0_5","children_0_59","ninos_menores5",
                        "u5_children","enfants_u5","under5_count",
                        "nb_moins_5ans","children_under_five"),
  taille_menage_5_17 = c("children_5_17","enfants_5_17","hh_children",
                         "nb_enfants_5_17","school_age_children","ninos_5_17",
                         "children_school","enfants_scolarisables",
                         "child_count_5_17"),
  taille_menage_adultes = c("adults","adultes","hh_adults","nb_adultes",
                            "adult_members","adult_count","adultos",
                            "membres_adultes","adults_18plus","adults_count"),
  chef_menage_sexe = c("head_sex","sexe_chef_menage","hh_head_sex",
                       "head_gender","genre_chef","sexo_jefe","sex_hh_head",
                       "chef_sexe","head_of_household_sex","sexe_cm",
                       "sexe_chef"),
  chef_menage_age = c("head_age","age_chef_menage","hh_head_age",
                      "head_of_household_age","edad_jefe","age_cm",
                      "chef_age","hh_head_years","age_head",
                      "age_chef"),
  revenus = c("income","revenus","revenue","household_income","ingreso",
              "revenu_menage","hh_income","monthly_income","revenu_mensuel",
              "total_income","revenus_menage","income_total","ingresos"),
  source_revenus = c("income_source","source_revenus","livelihood_source",
                     "fuente_ingreso","source_revenu","main_income_source",
                     "principale_source_revenu","income_type","type_revenu"),
  depenses = c("expenditure","depenses","expenses","household_expenditure",
               "gasto","depenses_menage","hh_expenditure","monthly_expenditure",
               "depense_mensuelle","total_expenditure","depenses_totales",
               "spending","gastos",
               "depenses_sante"),
  depenses_alim = c("food_expenditure","depenses_alimentaires","food_expenses",
                    "gasto_alimentario","depenses_nourriture","food_spending",
                    "hh_food_expenditure","depense_alimentaire",
                    "food_cost","cout_nourriture"),
  moyens_subsistance = c("livelihood","moyens_subsistance","livelihoods",
                         "moyens_existence","medios_vida","livelihood_type",
                         "type_subsistance","main_livelihood","subsistance",
                         "livelihood_strategy","strategie_subsistance",
                         "economic_activity","activite_economique"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ G. ÉCHANTILLONNAGE & DESIGN ═══
  # ═══════════════════════════════════════════════════════════════════════════
  poids_echantillon = c("sampling_weight","poids_echantillonnage","weight_sample",
                        "survey_weight","ponderation","sample_weight","wgt_sample",
                        "peso_muestral","poids_sondage","design_weight",
                        "poids_enquete","svy_weight","probability_weight"),
  deff = c("deff","design_effect","effet_plan","design_effect_factor",
           "efecto_diseno","effet_sondage","clustering_effect",
           "deff_factor","design_effect_value","deff_value"),
  correction_cluster = c("cluster_correction","correction_grappe",
                         "cluster_adjustment","ajustement_grappe",
                         "correccion_conglomerado","finite_pop_correction",
                         "fpc","correction_population_finie",
                         "cluster_corr","grappe_correction"),
  id_menage = c("household_id","id_menage","hh_id","menage_id","hogar_id",
                "hh_number","numero_menage","household_number","hhid",
                "identifiant_menage","hh_code","code_menage"),
  consentement = c("consent","consentement","informed_consent",
                   "consentimiento","consent_given","consentement_eclaire",
                   "consent_yn","accord","consent_obtained","consented",
                   "a_consenti","consent_status"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ A2. MORTALITÉ — COMPLÉMENTS (SMART Methodology v2) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  sexe_decede = c("sex_deceased","sexe_decede","sex_dead","sexe_mort",
                  "deceased_sex","sexo_fallecido","dead_sex","genre_decede",
                  "sex_of_deceased","sex_death"),
  age_decede = c("age_deceased","age_decede","age_dead","age_mort",
                 "deceased_age","edad_fallecido","dead_age","age_du_decede",
                 "age_at_death","age_deces"),
  recall_period = c("recall_period","periode_rappel","recall_days",
                    "duree_rappel","periodo_recuerdo","recall_length",
                    "jours_rappel","recall_start","debut_rappel",
                    "reference_period","periode_reference"),
  pop_debut = c("population_start","pop_debut","pop_start","population_debut",
                "initial_population","pop_initiale","starting_pop",
                "hh_size_start","taille_menage_debut","pop_beginning"),
  pop_fin = c("population_end","pop_fin","pop_end","population_fin",
              "final_population","pop_finale","ending_pop",
              "hh_size_end","taille_menage_fin","pop_current"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ B2. IYCF — COMPLÉMENTS (Indicateurs OMS/UNICEF 2021) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  prelacteal_feeds = c("prelacteal_feeds","aliments_prelactaux","prelacteal",
                       "prelacteal_feeding","alimentation_prelacteale",
                       "alimentacion_prelactea","pre_lacteal","prelacteal_food",
                       "given_before_breastmilk","avant_lait_maternel"),
  duree_allaitement = c("breastfeeding_duration","duree_allaitement",
                        "bf_duration","duree_allait","duracion_lactancia",
                        "months_breastfed","mois_allaitement","bf_months",
                        "length_breastfeeding","combien_mois_allaitement"),
  lait_animal = c("animal_milk","lait_animal","cow_milk","lait_vache",
                  "goat_milk","lait_chevre","leche_animal","fresh_milk",
                  "lait_frais","milk_animal","infant_formula_milk",
                  "lait_artificiel"),
  boissons_sucrees = c("sugary_drinks","boissons_sucrees","sweet_beverages",
                       "bebidas_azucaradas","soft_drinks","sodas","jus_sucre",
                       "sweetened_beverages","sugar_sweetened","boisson_sucree",
                       "unhealthy_drinks"),
  aliments_sucres = c("sweet_foods","aliments_sucres","sugary_foods",
                      "alimentos_azucarados","sweets","confiseries","bonbons",
                      "candy","unhealthy_food_sweet","snack_sucre"),
  aliments_ultra_transformes = c("ultra_processed_foods","aliments_ultra_transformes",
                                 "processed_foods","aliments_transformes",
                                 "alimentos_ultraprocesados","junk_food",
                                 "unhealthy_food","snack_food","chips_biscuits",
                                 "upt","ultra_processed"),
  zero_legumes_fruits = c("zero_veg_fruit","zero_legumes_fruits",
                          "no_fruit_veg","pas_legumes_fruits",
                          "sin_frutas_verduras","zero_fv","no_veg_no_fruit",
                          "aucun_legume_fruit"),
  repas_solides_semi = c("solid_semi_solid_food","aliments_solides_semi",
                         "complementary_food_given","alimentation_solide",
                         "alimento_solido","solid_food","nourriture_solide",
                         "soft_solid_food","given_solid_food"),
  age_allaitement = c("child_age_bf","age_enfant_allaitement","bf_child_age",
                      "age_breastfeeding","edad_lactancia","age_bf_months",
                      "age_mois_allaitement","breastfed_child_age"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ C2. ANÉMIE — COMPLÉMENTS (SENS Guidelines UNHCR) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  enceinte = c("pregnant","enceinte","pregnancy","grossesse","embarazada",
               "is_pregnant","est_enceinte","pregnancy_status","statut_grossesse",
               "femme_enceinte","pregnant_woman","gestante"),
  trimestre_grossesse = c("pregnancy_trimester","trimestre_grossesse","trimester",
                          "trimestre","trimestre_embarazo","gestation_trimester",
                          "trimestre_gestation","pregnancy_month","mois_grossesse"),
  suppl_fer_folate = c("iron_folic_acid","fer_folate","ifa","iron_supplementation",
                       "supplementation_fer","iron_supplement","fer_supplement",
                       "hierro_folico","folic_acid","acide_folique",
                       "ifa_supplementation","suppl_fer","iron_folate"),
  allaitante = c("lactating","allaitante","breastfeeding_mother","mere_allaitante",
                 "lactante","is_lactating","est_allaitante","nursing_mother",
                 "femme_allaitante","currently_lactating"),
  deworming = c("deworming","deparasitage","deworm","albendazole","mebendazole",
                "desparasitacion","deparasite","vermifuge","antiparasitaire",
                "deworming_treatment","traitement_deparasitage"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ D2. PALUDISME — COMPLÉMENTS (SENS Module Malaria) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  source_llin = c("llin_source","source_llin","net_source","source_moustiquaire",
                  "fuente_mosquitero","where_got_net","ou_obtenu_moustiquaire",
                  "net_distribution","distribution_moustiquaire","origin_net"),
  age_llin = c("llin_age","age_llin","net_age","age_moustiquaire",
               "edad_mosquitero","months_since_net","mois_moustiquaire",
               "net_age_months","anciennete_moustiquaire","how_old_net"),
  traitement_palu = c("malaria_treatment","traitement_paludisme","antimalarial",
                      "antipaludeen","tratamiento_malaria","act_treatment",
                      "traitement_act","malaria_drug","medicament_palu",
                      "palu_treatment","antimalarial_treatment"),
  prophylaxie_palu = c("malaria_prophylaxis","prophylaxie_paludisme","ipt",
                       "tpi","iptp","intermittent_preventive","prevention_palu",
                       "profilaxis_malaria","malaria_prevention",
                       "chimio_prevention","smc","chimioprevention"),
  etat_llin = c("llin_condition","etat_llin","net_condition","etat_moustiquaire",
                "estado_mosquitero","net_integrity","integrite_moustiquaire",
                "net_holes","trous_moustiquaire","llin_status"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ E2. WASH — COMPLÉMENTS (WASH Cluster / Sphère) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  stockage_eau = c("water_storage","stockage_eau","water_container",
                   "recipient_eau","almacenamiento_agua","water_vessel",
                   "contenant_eau","storage_container","bidon_eau",
                   "water_tank","citerne"),
  distance_source_eau = c("water_distance","distance_eau","distance_water_source",
                          "distance_source_eau","distancia_agua","km_to_water",
                          "metres_eau","distance_point_eau","water_source_distance",
                          "how_far_water"),
  eaux_usees = c("wastewater","eaux_usees","grey_water","eaux_grises",
                 "aguas_residuales","drainage","drainage_eau","sewage",
                 "wastewater_management","gestion_eaux_usees","sullage"),
  hygiene_menstruelle = c("menstrual_hygiene","hygiene_menstruelle","mhm",
                          "menstrual_health","sante_menstruelle","higiene_menstrual",
                          "menstruation_management","gestion_menstruelle",
                          "period_hygiene","serviettes_hygieniques"),
  recurrence_eau = c("water_availability","disponibilite_eau","water_regularity",
                     "regularite_eau","water_interruption","coupure_eau",
                     "disponibilidad_agua","water_access_frequency",
                     "frequence_acces_eau","water_reliable"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ H. SANTÉ / VACCINATION DÉTAILLÉE (PEV / SMART+) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  vac_bcg = c("bcg","vac_bcg","bcg_vaccination","vaccin_bcg","bcg_scar",
              "cicatrice_bcg","bcg_vaccine","bcg_card","bcg_recall",
              "vacuna_bcg","bcg_given"),
  vac_penta = c("penta","penta3","dpt","dtc","vac_penta","penta_3",
                "pentavalent","dpt3","dtc3","vaccin_penta","pentavalente",
                "penta_vaccination","vacuna_pentavalente","dpt_3"),
  vac_polio = c("polio","opv","opv3","vac_polio","polio_vaccination",
                "vaccin_polio","polio_3","opv_3","polio_oral",
                "vacuna_polio","polio_vaccine","poliomyelite"),
  vac_complete = c("fully_vaccinated","vaccination_complete","complete_vaccination",
                   "pev_complet","fully_immunized","completement_vaccine",
                   "vacunacion_completa","all_vaccines","toutes_vaccinations",
                   "immunization_complete","vac_complete"),
  carnet_vaccination = c("vaccination_card","carnet_vaccination","health_card",
                         "carnet_sante","tarjeta_vacunacion","vac_card",
                         "card_seen","carnet_vu","immunization_card",
                         "carte_vaccination","child_health_card"),
  consultation_soins = c("health_consultation","consultation_soins",
                         "sought_care","recours_soins","consulta_salud",
                         "health_seeking","recherche_soins","visited_health",
                         "visite_sante","health_facility_visit","consultation_medicale"),
  acces_sante = c("health_access","acces_sante","access_health_facility",
                  "acces_centre_sante","acceso_salud","distance_health",
                  "distance_sante","health_facility_distance",
                  "nearest_health","centre_sante_proche"),
  fievre_traitement = c("fever_treatment","traitement_fievre","fever_care",
                        "soins_fievre","tratamiento_fiebre","fever_action",
                        "action_fievre","fever_medication","medicament_fievre",
                        "fever_health_seeking"),
  oreillons = c("mumps","oreillons","paperas","mumps_vaccination",
                "vaccin_oreillons","mumps_vaccine","vac_mumps"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ I. SÉCURITÉ ALIMENTAIRE — COMPLÉMENTS (PAM VAM / IPC) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  lcsi = c("lcsi","lcsi_score","livelihood_csi","livelihood_coping",
           "strategies_subsistance","lcsi_index","indice_moyens_existence",
           "livelihood_coping_strategy","lcsi_total","coping_livelihood"),
  lcsi_stress = c("lcsi_stress","stress_coping","coping_stress",
                  "strategies_stress","lcsi_stress_score","stress_strategy",
                  "strategie_stress","estres_estrategia"),
  lcsi_crisis = c("lcsi_crisis","crisis_coping","coping_crisis",
                  "strategies_crise","lcsi_crisis_score","crisis_strategy",
                  "strategie_crise","crisis_estrategia"),
  lcsi_emergency = c("lcsi_emergency","emergency_coping","coping_emergency",
                     "strategies_urgence","lcsi_emerg_score","emergency_strategy",
                     "strategie_urgence","emergencia_estrategia"),
  fies = c("fies","fies_score","food_insecurity_experience","echelle_insecurite",
           "food_insecurity_experience_scale","escala_inseguridad",
           "fies_raw","fies_total","insecurite_alimentaire_experience"),
  mdd_w = c("mdd_w","mddw","minimum_dietary_diversity_women",
            "diversite_alimentaire_femme","mdd_women","dds_women",
            "diversidad_dietetica_mujer","women_dietary_diversity",
            "score_diversite_femme","mdd_f"),
  source_nourriture = c("food_source","source_nourriture","food_origin",
                        "origine_nourriture","fuente_alimento","main_food_source",
                        "source_alimentaire","food_access_source",
                        "provenance_nourriture","where_food",
                        "source_alim"),
  aide_alimentaire = c("food_assistance","aide_alimentaire","food_aid",
                       "asistencia_alimentaria","received_food_aid",
                       "recu_aide_alimentaire","food_distribution",
                       "distribution_alimentaire","food_ration","ration"),
  nb_repas_jour = c("meals_per_day","repas_par_jour","number_meals",
                    "nombre_repas_jour","comidas_dia","daily_meals",
                    "meal_count","nb_repas","repas_quotidiens",
                    "meals_yesterday","repas_hier"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ J. MÉNAGE — COMPLÉMENTS (SENS / IPC) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  type_abri = c("shelter_type","type_abri","housing_type","type_logement",
                "tipo_albergue","dwelling_type","type_habitation",
                "shelter","abri","housing","logement","vivienda"),
  statut_propriete = c("ownership_status","statut_propriete","tenure_status",
                       "statut_occupation","tenure","propiedad",
                       "land_ownership","propriete_terrain","own_rent",
                       "proprietaire_locataire","housing_tenure"),
  education_cm = c("head_education","education_chef","hh_head_education",
                   "niveau_education_cm","educacion_jefe","education_level_head",
                   "chef_education","schooling_head","scolarite_chef",
                   "education_head_hh"),
  handicap_wg = c("disability","handicap","washington_group","wg_disability",
                  "discapacidad","functional_difficulty","difficulte_fonctionnelle",
                  "wg_score","disability_status","statut_handicap",
                  "limitation_fonctionnelle","wgss",
                  "handicap_score"),
  handicap_voir = c("wg_seeing","difficulte_voir","seeing_difficulty",
                    "difficulty_seeing","vision_difficulty","probleme_vision",
                    "dificultad_ver","wg_vision","vision_wg"),
  handicap_entendre = c("wg_hearing","difficulte_entendre","hearing_difficulty",
                        "difficulty_hearing","probleme_audition","dificultad_oir",
                        "wg_hearing_difficulty","audition_wg"),
  handicap_marcher = c("wg_walking","difficulte_marcher","walking_difficulty",
                       "difficulty_walking","mobility_difficulty","probleme_mobilite",
                       "dificultad_caminar","wg_mobility","mobilite_wg"),
  handicap_memoire = c("wg_remembering","difficulte_memoire","remembering_difficulty",
                       "difficulty_remembering","cognitive_difficulty",
                       "probleme_memoire","dificultad_recordar","wg_cognition",
                       "cognition_wg"),
  handicap_soins_personnels = c("wg_selfcare","difficulte_soins","selfcare_difficulty",
                                "difficulty_selfcare","probleme_autonomie",
                                "dificultad_autocuidado","wg_self_care",
                                "autonomie_wg","soins_personnels_wg"),
  handicap_communication = c("wg_communicating","difficulte_communication",
                             "communication_difficulty","difficulty_communicating",
                             "probleme_communication","dificultad_comunicar",
                             "wg_communication","communication_wg"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ K. PROTECTION (SENS Module Protection / UNHCR) ═══
  # ═══════════════════════════════════════════════════════════════════════════
  enregistrement = c("registration","enregistrement","unhcr_registration",
                     "registered","enregistre","registro","hcr_registration",
                     "registration_status","statut_enregistrement",
                     "refugee_registration","inscrit"),
  documentation = c("documentation","documents","identity_documents",
                    "documents_identite","documentacion","has_documents",
                    "possede_documents","id_documents","papiers",
                    "identity_card","carte_identite"),
  incident_protection = c("protection_incident","incident_protection",
                          "protection_concern","probleme_protection",
                          "incidente_proteccion","gbv","vbg",
                          "violence_based_gender","violence_genre",
                          "protection_issue","security_incident"),
  travail_enfant = c("child_labor","travail_enfant","child_labour",
                     "trabajo_infantil","child_work","enfant_travailleur",
                     "working_child","child_employment","emploi_enfant",
                     "child_economic_activity"),
  mariage_precoce = c("child_marriage","mariage_precoce","early_marriage",
                      "matrimonio_infantil","forced_marriage","mariage_force",
                      "underage_marriage","mariage_mineurs","early_union",
                      "union_precoce"),

  # ═══════════════════════════════════════════════════════════════════════════
  # ═══ L. VARIABLES COMPLÉMENTAIRES SMART-SENS (ajoutées pour couverture) ══
  # ═══════════════════════════════════════════════════════════════════════════

  # ── Démographie ménage ──
  enfants_6_17 = c("enfants_6_17","children_6_17","enfants_scolarisables",
                   "school_age","children_school_age","ninos_6_17"),

  # ── IYCF détaillé ──
  introduction_liq = c("introduction_liq","introduction_liquides","intro_liquids",
                       "prelacteal_liquids","liquides_introduction","early_liquids"),
  diversite_min = c("diversite_min","minimum_dietary_diversity","mdd","mdd_score",
                    "diversite_alimentaire_min","min_diet_diversity"),
  frequence_min = c("frequence_min","minimum_meal_frequency","mmf","mmf_score",
                    "frequence_repas_minimum","meal_frequency_min"),
  introduction_aliments = c("introduction_aliments","complementary_food_intro",
                            "intro_complementary","intro_aliments_complementaires",
                            "introduction_solides","intro_solid_food"),
  aliments_prelactees = c("aliments_prelactees","prelacteal_feeds","prelacteal",
                          "aliments_avant_lait","prelacteal_feeding","pre_lacteal"),
  lait_autre = c("lait_autre","other_milk","lait_animal","animal_milk",
                 "non_breast_milk","formula_milk","lait_formule"),
  repas_fer_enrichi = c("repas_fer_enrichi","iron_fortified_food","aliments_enrichis_fer",
                        "fortified_iron","fer_enrichi","iron_rich_complementary"),
  age_introduction = c("age_introduction","age_complementary_food","age_intro_food",
                       "age_diversification","age_introduction_aliments"),
  allaitement_sein = c("allaitement_sein","ever_breastfed","a_ete_allaite",
                       "breastfed_ever","allaitement_maternel","mise_au_sein"),
  allaitement_actuel = c("allaitement_actuel","currently_breastfeeding",
                         "allaitement_en_cours","breastfeeding_current",
                         "still_breastfeeding","allaite_actuellement"),
  boissons_sucrees = c("boissons_sucrees","sweet_beverages","sugary_drinks",
                       "ssb","sugar_sweetened","bebidas_azucaradas"),
  aliments_sucres = c("aliments_sucres","sweet_foods","unhealthy_foods",
                      "alimentos_dulces","snacks_sucres","sugary_foods"),
  zero_veg_fruits = c("zero_veg_fruits","zero_fruit_veg","no_fruit_veg",
                      "zero_legumes_fruits","aucun_fruit_legume"),

  # ── Anémie compléments ──
  supplement_fer = c("supplement_fer","iron_supplement","iron_supplementation",
                     "fer_supplement","suplemento_hierro","iron_folic_acid","ifa"),
  trimestre_grossesse = c("trimestre_grossesse","pregnancy_trimester","trimester",
                          "trimestre","trimestre_embarazo","gestational_age"),
  deworming = c("deworming","deparasitage","deworm","dewormed",
                "deparasite","desparasitacion","anthelminthic"),

  # ── Paludisme compléments ──
  tdr_resultat = c("tdr_resultat","rdt_result","resultat_tdr_palu",
                   "malaria_rdt_result","test_result_malaria","rdt_positive"),
  prev_paludisme = c("prev_paludisme","malaria_prevalence","prevalence_paludisme",
                     "malaria_prev","taux_paludisme"),
  source_antipaludeen = c("source_antipaludeen","antimalarial_source",
                          "source_traitement_palu","where_antimalarial",
                          "provenance_antipaludique"),
  region_llin = c("region_llin","net_region","zone_moustiquaire",
                  "distribution_llin","llin_distribution_area"),
  traitement_paludisme = c("traitement_paludisme","malaria_treatment",
                           "traitement_palu","antimalarial_treatment",
                           "tratamiento_malaria","act_treatment"),
  age_llin = c("age_llin","net_age","age_moustiquaire","llin_age",
               "duree_moustiquaire","net_duration"),
  etat_llin = c("etat_llin","net_condition","etat_moustiquaire","llin_condition",
                "condition_moustiquaire","net_integrity"),

  # ── Santé / Vaccination compléments ──
  vomissements_2sem = c("vomissements_2sem","vomiting_2weeks","vomissements",
                        "vomiting","vomitos","child_vomiting"),
  fievre_2sem_sante = c("fievre_2sem_sante","fever_care_seeking","fievre_consultation",
                        "fever_treatment_seeking","care_seeking_fever"),
  consultation_curative = c("consultation_curative","curative_consultation",
                            "health_consultation","visite_curative","curative_care"),
  acces_formation_sante = c("acces_formation_sante","health_facility_access",
                            "access_health_facility","acces_centre_sante",
                            "distance_centre_sante"),
  origine_vaccination = c("origine_vaccination","vaccination_source",
                          "source_vaccination","where_vaccinated",
                          "lieu_vaccination"),
  visites_cpn = c("visites_cpn","anc_visits","consultations_prenatales",
                  "anc","prenatal_visits","cpn","antenatal_care"),
  vac_bcg = c("vac_bcg","bcg","bcg_vaccination","vaccin_bcg","bcg_vaccine"),
  vac_penta = c("vac_penta","penta","pentavalent","dpt","penta_vaccination",
                "vaccin_penta","penta3"),
  vac_polio = c("vac_polio","polio","opv","polio_vaccination","vaccin_polio",
                "polio_vaccine","opv3"),
  vac_complete = c("vac_complete","fully_vaccinated","vaccination_complete",
                   "complete_vaccination","vacunacion_completa","fully_immunized"),
  carnet_vaccination = c("carnet_vaccination","vaccination_card","health_card",
                         "carnet_sante","tarjeta_vacunacion","immunization_card"),

  # ── WASH compléments ──
  gestion_excreta = c("gestion_excreta","excreta_management","excreta_disposal",
                      "child_feces_disposal","gestion_feces","eliminacion_excretas"),
  gestion_eau_menage = c("gestion_eau_menage","household_water_management",
                         "water_storage","stockage_eau","gestion_eau",
                         "water_management_hh"),
  qualite_eau = c("qualite_eau","water_quality","calidad_agua",
                  "quality_water","eau_qualite","water_quality_test"),
  fcs_eau = c("fcs_eau","water_fcs","fcs_water","eau_score_fcs"),
  hygiene_menstruelle = c("hygiene_menstruelle","menstrual_hygiene","mhm",
                          "menstrual_health","hygiene_menstruation",
                          "gestion_hygiene_menstruelle"),

  # ── Sécurité alimentaire compléments ──
  fcs_categorie = c("fcs_categorie","fcs_category","fcs_class","fcs_group",
                    "categorie_fcs","fcs_classification"),
  lcsi_stress = c("lcsi_stress","lcsi_stress_strategies","lcsi1",
                  "livelihood_stress","strategie_stress","coping_stress"),
  lcsi_crise = c("lcsi_crise","lcsi_crisis","lcsi_crisis_strategies","lcsi2",
                 "livelihood_crisis","strategie_crise","coping_crisis"),
  lcsi_urgence = c("lcsi_urgence","lcsi_emergency","lcsi_emergency_strategies","lcsi3",
                   "livelihood_emergency","strategie_urgence","coping_emergency"),

  # ── HDDS compléments ──
  hdds_vit_a = c("hdds_vit_a","hdds_vitamin_a","hdds_vita_fruit","hdds_orange",
                 "dd_vitamine_a","hdds_orange_veg","hdds_vit_a_fruit"),
  hdds_autres_legumes = c("hdds_autres_legumes","hdds_other_veg","hdds_other_vegetables",
                          "dd_autres_legumes","hdds_misc_veg","hdds_other_greens"),

  # ── Washington Group compléments ──
  wg_audition = c("wg_audition","wg_hearing","difficulty_hearing","difficulte_entendre",
                  "hearing_difficulty","probleme_audition","dificultad_oir"),
  wg_mobilite = c("wg_mobilite","wg_walking","difficulty_walking","difficulte_marcher",
                  "walking_difficulty","probleme_mobilite","dificultad_caminar"),
  wg_memoire = c("wg_memoire","wg_remembering","difficulty_remembering",
                 "difficulte_memoire","memory_difficulty","probleme_memoire",
                 "dificultad_recordar","wg_cognition"),
  wg_autonomie = c("wg_autonomie","wg_self_care","difficulty_self_care",
                   "difficulte_autonomie","self_care_difficulty","probleme_autonomie",
                   "dificultad_autocuidado","wg_selfcare"),

  # ── Protection compléments ──
  document_identite = c("document_identite","identity_document","id_card",
                        "carte_identite","documento_identidad","national_id",
                        "piece_identite"),

  # ── Enquête / Métadonnées ──
  poids_echantillon = c("poids_echantillon","sampling_weight","survey_weight",
                        "weight_sample","poids_sondage","peso_muestral","svy_weight"),
  correction_plan = c("correction_plan","design_effect_correction","plan_correction",
                      "fpc","finite_pop_correction","correction_plan_sondage"),

  # ── Variables supplémentaires (variantes qui entrent en conflit avec d'autres clés) ──
  handicap_menage = c("handicap_menage","household_disability","handicap_hh",
                      "disability_household","menage_handicap"),
  fievre_paludisme = c("fievre_paludisme","malaria_fever","fever_malaria",
                       "fievre_palu","paludisme_fievre"),
  latrine_acces = c("latrine_acces","latrine_access","toilet_access",
                    "acces_latrine","access_sanitation","acces_assainissement"),
  eau_lavage_mains = c("eau_lavage_mains","handwash_water","water_handwashing",
                       "eau_pour_lavage","water_at_handwash"),
  rcsi_emprunt = c("rcsi_emprunt","rcsi_emprunter_nourriture","coping_emprunt",
                   "emprunter_voisin","rcsi_borrow_food"),
  hdds_noix = c("hdds_noix","hdds_nuts","hdds_nuts_seeds","dd_noix",
                "hdds_graines","hdds_nut_seed","noix_hdds"),
  statut_deplacement_enq = c("statut_deplacement_enq","displacement_survey",
                             "deplacement_enquete","survey_displacement_status",
                             "pop_type_survey")
)

# Métadonnées OMS : catégorie, description, type attendu, unité
OMS_META <- list(
  whz    = list(cat = "Anthropom\u00e9trie", label = "Score WHZ (Poids/Taille)",
                type = "numeric", unit = "z-score", seuils = "< -2 = MAG, < -3 = MAS"),
  haz    = list(cat = "Anthropom\u00e9trie", label = "Score HAZ (Taille/\u00c2ge)",
                type = "numeric", unit = "z-score", seuils = "< -2 = Retard croissance"),
  waz    = list(cat = "Anthropom\u00e9trie", label = "Score WAZ (Poids/\u00c2ge)",
                type = "numeric", unit = "z-score", seuils = "< -2 = Insuffisance pond\u00e9rale"),
  muac   = list(cat = "Anthropom\u00e9trie", label = "P\u00e9rim\u00e8tre brachial (MUAC)",
                type = "numeric", unit = "mm", seuils = "< 125 = MAG, < 115 = MAS"),
  poids  = list(cat = "Anthropom\u00e9trie", label = "Poids de l'enfant",
                type = "numeric", unit = "kg", seuils = ""),
  taille = list(cat = "Anthropom\u00e9trie", label = "Taille/Longueur de l'enfant",
                type = "numeric", unit = "cm", seuils = ""),
  oedema = list(cat = "Anthropom\u00e9trie", label = "\u0152d\u00e8mes bilat\u00e9raux",
                type = "binary", unit = "oui/non", seuils = "Pr\u00e9sence = MAS"),
  age_m  = list(cat = "D\u00e9mographie", label = "\u00c2ge en mois",
                type = "numeric", unit = "mois", seuils = "6-59 mois (SMART)"),
  sexe   = list(cat = "D\u00e9mographie", label = "Sexe de l'enfant",
                type = "categorical", unit = "M/F", seuils = ""),
  date_naissance = list(cat = "D\u00e9mographie", label = "Date de naissance",
                type = "date", unit = "date", seuils = ""),
  date_mesure = list(cat = "D\u00e9mographie", label = "Date de mesure/visite",
                type = "date", unit = "date", seuils = ""),
  region = list(cat = "G\u00e9ographie", label = "R\u00e9gion / Admin1",
                type = "categorical", unit = "", seuils = ""),
  dept   = list(cat = "G\u00e9ographie", label = "D\u00e9partement / District / Admin2",
                type = "categorical", unit = "", seuils = ""),
  village= list(cat = "G\u00e9ographie", label = "Village / Localit\u00e9",
                type = "categorical", unit = "", seuils = ""),
  cluster= list(cat = "G\u00e9ographie", label = "Grappe / PSU",
                type = "categorical", unit = "", seuils = ""),
  lat    = list(cat = "G\u00e9ographie", label = "Latitude GPS",
                type = "numeric", unit = "degr\u00e9s", seuils = ""),
  lon    = list(cat = "G\u00e9ographie", label = "Longitude GPS",
                type = "numeric", unit = "degr\u00e9s", seuils = ""),
  fcs    = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Score de Consommation Alimentaire (FCS)",
                type = "numeric", unit = "score 0-112", seuils = "\u2264 21 pauvre, 21.5-35 limite, > 35 acceptable"),
  rcsi   = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Indice de Strat\u00e9gies d'Adaptation R\u00e9duit (rCSI)",
                type = "numeric", unit = "score 0-56", seuils = "\u2265 19 coping \u00e9lev\u00e9"),
  hdds   = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Score de Diversit\u00e9 Alimentaire (HDDS)",
                type = "numeric", unit = "score 0-12", seuils = "< 3 faible diversit\u00e9"),
  hhs    = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "\u00c9chelle de la Faim (HHS)",
                type = "numeric", unit = "score 0-6", seuils = "\u2265 4 faim s\u00e9v\u00e8re"),
  fcs_cereales     = list(cat = "Composantes FCS", label = "FCS — C\u00e9r\u00e9ales/Tubercules", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_legumineuses = list(cat = "Composantes FCS", label = "FCS — L\u00e9gumineuses", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_lait         = list(cat = "Composantes FCS", label = "FCS — Lait/Produits laitiers", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_proteines    = list(cat = "Composantes FCS", label = "FCS — Prot\u00e9ines (viande/poisson)", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_legumes      = list(cat = "Composantes FCS", label = "FCS — L\u00e9gumes", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_fruits       = list(cat = "Composantes FCS", label = "FCS — Fruits", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_huile        = list(cat = "Composantes FCS", label = "FCS — Huile/Graisse", type = "numeric", unit = "jours 0-7", seuils = ""),
  fcs_sucre        = list(cat = "Composantes FCS", label = "FCS — Sucre/Condiments", type = "numeric", unit = "jours 0-7", seuils = ""),
  hhs_nofood  = list(cat = "Composantes HHS", label = "HHS \u2014 Pas de nourriture",              type = "numeric", unit = "0-2", seuils = "0=jamais, 1=parfois, 2=souvent"),
  hhs_sleep   = list(cat = "Composantes HHS", label = "HHS \u2014 Dormi avec la faim",            type = "numeric", unit = "0-2", seuils = "0=jamais, 1=parfois, 2=souvent"),
  hhs_wholeday= list(cat = "Composantes HHS", label = "HHS \u2014 Journ\u00e9e enti\u00e8re sans manger", type = "numeric", unit = "0-2", seuils = "0=jamais, 1=parfois, 2=souvent"),
  # Composantes rCSI (5 strat\u00e9gies d'adaptation, poids PAM/WFP)
  rcsi_lessqlty   = list(cat = "Composantes rCSI", label = "rCSI \u2014 Aliments moins pr\u00e9f\u00e9r\u00e9s (\u00d71)", type = "numeric", unit = "jours 0-7", seuils = "Poids OMS = 1"),
  rcsi_borrow     = list(cat = "Composantes rCSI", label = "rCSI \u2014 Emprunter nourriture (\u00d72)",    type = "numeric", unit = "jours 0-7", seuils = "Poids OMS = 2"),
  rcsi_meal_nb    = list(cat = "Composantes rCSI", label = "rCSI \u2014 R\u00e9duire nb repas (\u00d71)",        type = "numeric", unit = "jours 0-7", seuils = "Poids OMS = 1"),
  rcsi_meal_size  = list(cat = "Composantes rCSI", label = "rCSI \u2014 R\u00e9duire portions (\u00d71)",        type = "numeric", unit = "jours 0-7", seuils = "Poids OMS = 1"),
  rcsi_meal_adult = list(cat = "Composantes rCSI", label = "rCSI \u2014 R\u00e9duire conso adultes (\u00d73)",   type = "numeric", unit = "jours 0-7", seuils = "Poids OMS = 3"),
  # Composantes HDDS (12 groupes alimentaires FAO/FANTA)
  hdds_cereales        = list(cat = "Composantes HDDS", label = "HDDS \u2014 C\u00e9r\u00e9ales",               type = "binary", unit = "0/1", seuils = ""),
  hdds_tubercules      = list(cat = "Composantes HDDS", label = "HDDS \u2014 Tubercules/Racines",    type = "binary", unit = "0/1", seuils = ""),
  hdds_legumes_hdds    = list(cat = "Composantes HDDS", label = "HDDS \u2014 L\u00e9gumes",                type = "binary", unit = "0/1", seuils = ""),
  hdds_fruits_hdds     = list(cat = "Composantes HDDS", label = "HDDS \u2014 Fruits",               type = "binary", unit = "0/1", seuils = ""),
  hdds_viande          = list(cat = "Composantes HDDS", label = "HDDS \u2014 Viande/Abats",         type = "binary", unit = "0/1", seuils = ""),
  hdds_oeufs           = list(cat = "Composantes HDDS", label = "HDDS \u2014 \u0152ufs",            type = "binary", unit = "0/1", seuils = ""),
  hdds_poisson         = list(cat = "Composantes HDDS", label = "HDDS \u2014 Poisson/Fruits de mer",type = "binary", unit = "0/1", seuils = ""),
  hdds_legumineuses_hdds = list(cat = "Composantes HDDS", label = "HDDS \u2014 L\u00e9gumineuses/Noix", type = "binary", unit = "0/1", seuils = ""),
  hdds_lait            = list(cat = "Composantes HDDS", label = "HDDS \u2014 Lait/Produits laitiers", type = "binary", unit = "0/1", seuils = ""),
  hdds_huile           = list(cat = "Composantes HDDS", label = "HDDS \u2014 Huile/Graisse",        type = "binary", unit = "0/1", seuils = ""),
  hdds_sucre           = list(cat = "Composantes HDDS", label = "HDDS \u2014 Sucre/Miel",           type = "binary", unit = "0/1", seuils = ""),
  hdds_condiments      = list(cat = "Composantes HDDS", label = "HDDS \u2014 Condiments/\u00c9pices",type = "binary", unit = "0/1", seuils = ""),
  equipe = list(cat = "Enqu\u00eate", label = "\u00c9quipe / Enqu\u00eateur",
                type = "categorical", unit = "", seuils = ""),
  strate = list(cat = "Enqu\u00eate", label = "Strate d'\u00e9chantillonnage",
                type = "categorical", unit = "", seuils = ""),
  depl   = list(cat = "Enqu\u00eate", label = "Statut de d\u00e9placement",
                type = "categorical", unit = "", seuils = ""),
  survey = list(cat = "Enqu\u00eate", label = "Vague / Round d'enqu\u00eate",
                type = "categorical", unit = "", seuils = ""),
  rougeole = list(cat = "Sant\u00e9", label = "Vaccination rougeole",
                  type = "binary", unit = "oui/non", seuils = ""),
  vita   = list(cat = "Sant\u00e9", label = "Suppl\u00e9mentation Vitamine A",
                type = "binary", unit = "oui/non", seuils = ""),
  diarrhee = list(cat = "Sant\u00e9", label = "Diarrh\u00e9e (2 derni\u00e8res semaines)",
                  type = "binary", unit = "oui/non", seuils = ""),
  ira    = list(cat = "Sant\u00e9", label = "Infection Respiratoire Aigu\u00eb (IRA)",
                type = "binary", unit = "oui/non", seuils = ""),
  bmi    = list(cat = "Anthropom\u00e9trie", label = "IMC / BMI (Indice de Masse Corporelle)",
                type = "numeric", unit = "kg/m\u00b2 ou z-score", seuils = ""),

  # ═══ A. MORTALITÉ (SMART CORE) ═══
  nb_deces   = list(cat = "Mortalit\u00e9", label = "Nombre de d\u00e9c\u00e8s dans le m\u00e9nage",
                    type = "numeric", unit = "nombre", seuils = ""),
  date_deces = list(cat = "Mortalit\u00e9", label = "Date du d\u00e9c\u00e8s",
                    type = "date", unit = "date", seuils = ""),
  cause_deces = list(cat = "Mortalit\u00e9", label = "Cause du d\u00e9c\u00e8s",
                     type = "categorical", unit = "", seuils = "Maladie / Violence / Accident"),
  lieu_deces  = list(cat = "Mortalit\u00e9", label = "Lieu du d\u00e9c\u00e8s",
                     type = "categorical", unit = "", seuils = ""),
  person_time = list(cat = "Mortalit\u00e9", label = "Personne-temps d'exposition",
                     type = "numeric", unit = "jours-personne", seuils = ""),
  cmr = list(cat = "Mortalit\u00e9", label = "Taux de Mortalit\u00e9 Brut (CMR)",
             type = "numeric", unit = "d\u00e9c\u00e8s/10 000/jour", seuils = "\u2265 1 = urgence, \u2265 2 = hors contr\u00f4le"),
  u5mr = list(cat = "Mortalit\u00e9", label = "Taux de Mortalit\u00e9 < 5 ans (U5MR)",
              type = "numeric", unit = "d\u00e9c\u00e8s/10 000/jour", seuils = "\u2265 2 = urgence, \u2265 4 = hors contr\u00f4le"),
  naissance_recall = list(cat = "Mortalit\u00e9", label = "Naissances pendant la p\u00e9riode de rappel",
                          type = "numeric", unit = "nombre", seuils = ""),
  arrive_menage = list(cat = "Mortalit\u00e9", label = "Arriv\u00e9es dans le m\u00e9nage (p\u00e9riode rappel)",
                       type = "numeric", unit = "nombre", seuils = ""),
  depart_menage = list(cat = "Mortalit\u00e9", label = "D\u00e9parts du m\u00e9nage (p\u00e9riode rappel)",
                       type = "numeric", unit = "nombre", seuils = ""),

  # ═══ B. IYCF — NUTRITION INFANTILE (OMS/UNICEF) ═══
  allaitement_excl = list(cat = "IYCF", label = "Allaitement maternel exclusif",
                          type = "binary", unit = "oui/non", seuils = "Recommand\u00e9 0-6 mois"),
  allaitement_cont = list(cat = "IYCF", label = "Allaitement continu\u00e9",
                          type = "binary", unit = "oui/non", seuils = "Recommand\u00e9 jusqu'\u00e0 24 mois"),
  initiation_allaitement = list(cat = "IYCF", label = "Mise au sein pr\u00e9coce (< 1h)",
                                type = "binary", unit = "oui/non", seuils = ""),
  freq_repas_min = list(cat = "IYCF", label = "Fr\u00e9quence minimale des repas (MMF)",
                        type = "binary", unit = "oui/non", seuils = "\u2265 2-3 repas selon \u00e2ge"),
  diversite_alim_min = list(cat = "IYCF", label = "Diversit\u00e9 alimentaire minimale (MDD)",
                            type = "binary", unit = "oui/non", seuils = "\u2265 5 groupes sur 8"),
  regime_acceptable_min = list(cat = "IYCF", label = "R\u00e9gime minimum acceptable (MAD)",
                               type = "binary", unit = "oui/non", seuils = "MMF + MDD"),
  intro_aliments_solides = list(cat = "IYCF", label = "Introduction aliments solides",
                                type = "binary", unit = "oui/non", seuils = "6-8 mois"),
  cons_fer = list(cat = "IYCF", label = "Consommation aliments riches en fer",
                  type = "binary", unit = "oui/non", seuils = ""),
  bouteille = list(cat = "IYCF", label = "Alimentation au biberon",
                   type = "binary", unit = "oui/non", seuils = "D\u00e9conseill\u00e9"),
  nb_groupes_alim_enfant = list(cat = "IYCF", label = "Nombre de groupes alimentaires (enfant)",
                                type = "numeric", unit = "0-8", seuils = "\u2265 5 = diversit\u00e9 ad\u00e9quate"),

  # ═══ C. ANÉMIE (SENS) ═══
  hemoglobine = list(cat = "An\u00e9mie", label = "Taux d'h\u00e9moglobine (Hb)",
                     type = "numeric", unit = "g/dL", seuils = "< 11 = an\u00e9mie (enfant), < 12 = an\u00e9mie (femme)"),
  hb_ajustee = list(cat = "An\u00e9mie", label = "H\u00e9moglobine ajust\u00e9e (altitude/tabac)",
                    type = "numeric", unit = "g/dL", seuils = "Appliquer corrections OMS"),
  statut_anemie = list(cat = "An\u00e9mie", label = "Statut d'an\u00e9mie",
                       type = "categorical", unit = "", seuils = "L\u00e9g\u00e8re / Mod\u00e9r\u00e9e / S\u00e9v\u00e8re"),
  altitude = list(cat = "An\u00e9mie", label = "Altitude du site",
                  type = "numeric", unit = "m\u00e8tres", seuils = "Ajustement si > 1000 m"),
  ajust_altitude = list(cat = "An\u00e9mie", label = "Facteur d'ajustement altitude",
                        type = "numeric", unit = "g/dL", seuils = "Table OMS"),
  fumeur = list(cat = "An\u00e9mie", label = "Statut tabagique",
                type = "binary", unit = "oui/non", seuils = "Ajustement Hb si fumeur"),

  # ═══ D. PALUDISME / MOUSTIQUAIRES (SENS) ═══
  llin_possession = list(cat = "Paludisme", label = "Possession de moustiquaire (LLIN/ITN)",
                         type = "binary", unit = "oui/non", seuils = ""),
  llin_utilisation = list(cat = "Paludisme", label = "Utilisation moustiquaire (nuit pr\u00e9c\u00e9dente)",
                          type = "binary", unit = "oui/non", seuils = ""),
  llin_acces = list(cat = "Paludisme", label = "Acc\u00e8s \u00e0 une moustiquaire",
                    type = "binary", unit = "oui/non", seuils = "1 LLIN / 2 personnes"),
  nb_moustiquaires = list(cat = "Paludisme", label = "Nombre de moustiquaires dans le m\u00e9nage",
                          type = "numeric", unit = "nombre", seuils = ""),
  tdr_malaria = list(cat = "Paludisme", label = "R\u00e9sultat TDR paludisme",
                     type = "binary", unit = "positif/n\u00e9gatif", seuils = ""),
  fievre = list(cat = "Paludisme", label = "Fi\u00e8vre (2 derni\u00e8res semaines)",
                type = "binary", unit = "oui/non", seuils = ""),

  # ═══ E. WASH ═══
  source_eau = list(cat = "WASH", label = "Source d'eau principale",
                    type = "categorical", unit = "", seuils = "Am\u00e9lior\u00e9e / Non am\u00e9lior\u00e9e (JMP)"),
  temps_collecte_eau = list(cat = "WASH", label = "Temps de collecte d'eau (aller-retour)",
                            type = "numeric", unit = "minutes", seuils = "\u2264 30 min = acc\u00e8s ad\u00e9quat"),
  traitement_eau = list(cat = "WASH", label = "Traitement de l'eau au m\u00e9nage",
                        type = "categorical", unit = "", seuils = "Chloration / \u00c9bullition / Filtration / Rien"),
  quantite_eau = list(cat = "WASH", label = "Quantit\u00e9 d'eau par personne/jour",
                      type = "numeric", unit = "litres/personne/jour", seuils = "\u2265 15 L = minimum Sph\u00e8re"),
  type_latrine = list(cat = "WASH", label = "Type de latrine / installation sanitaire",
                      type = "categorical", unit = "", seuils = "Am\u00e9lior\u00e9e / Non am\u00e9lior\u00e9e (JMP)"),
  latrine_partagee = list(cat = "WASH", label = "Latrine partag\u00e9e",
                          type = "binary", unit = "oui/non", seuils = ""),
  defecation_air_libre = list(cat = "WASH", label = "D\u00e9f\u00e9cation \u00e0 l'air libre",
                              type = "binary", unit = "oui/non", seuils = ""),
  lavage_mains = list(cat = "WASH", label = "Dispositif de lavage des mains",
                      type = "binary", unit = "oui/non", seuils = ""),
  savon_disponible = list(cat = "WASH", label = "Savon disponible au point de lavage",
                          type = "binary", unit = "oui/non", seuils = ""),
  eau_savon_lavage = list(cat = "WASH", label = "Lavage des mains avec eau et savon",
                          type = "binary", unit = "oui/non", seuils = ""),
  gestion_dechets = list(cat = "WASH", label = "Gestion des d\u00e9chets solides",
                         type = "categorical", unit = "", seuils = ""),

  # ═══ F. MÉNAGE ═══
  taille_menage = list(cat = "M\u00e9nage", label = "Taille du m\u00e9nage",
                       type = "numeric", unit = "personnes", seuils = ""),
  taille_menage_0_5 = list(cat = "M\u00e9nage", label = "Enfants < 5 ans dans le m\u00e9nage",
                           type = "numeric", unit = "nombre", seuils = ""),
  taille_menage_5_17 = list(cat = "M\u00e9nage", label = "Enfants 5-17 ans dans le m\u00e9nage",
                            type = "numeric", unit = "nombre", seuils = ""),
  taille_menage_adultes = list(cat = "M\u00e9nage", label = "Adultes (18+) dans le m\u00e9nage",
                               type = "numeric", unit = "nombre", seuils = ""),
  chef_menage_sexe = list(cat = "M\u00e9nage", label = "Sexe du chef de m\u00e9nage",
                          type = "categorical", unit = "M/F", seuils = ""),
  chef_menage_age = list(cat = "M\u00e9nage", label = "\u00c2ge du chef de m\u00e9nage",
                         type = "numeric", unit = "ann\u00e9es", seuils = ""),
  revenus = list(cat = "M\u00e9nage", label = "Revenus du m\u00e9nage",
                 type = "numeric", unit = "monnaie locale", seuils = ""),
  source_revenus = list(cat = "M\u00e9nage", label = "Source principale de revenus",
                        type = "categorical", unit = "", seuils = ""),
  depenses = list(cat = "M\u00e9nage", label = "D\u00e9penses totales du m\u00e9nage",
                  type = "numeric", unit = "monnaie locale", seuils = ""),
  depenses_alim = list(cat = "M\u00e9nage", label = "D\u00e9penses alimentaires",
                       type = "numeric", unit = "monnaie locale", seuils = ""),
  moyens_subsistance = list(cat = "M\u00e9nage", label = "Moyens de subsistance",
                            type = "categorical", unit = "", seuils = ""),

  # ═══ G. ÉCHANTILLONNAGE & DESIGN ═══
  poids_echantillon = list(cat = "\u00c9chantillonnage", label = "Poids d'\u00e9chantillonnage",
                           type = "numeric", unit = "", seuils = ""),
  deff = list(cat = "\u00c9chantillonnage", label = "Effet de plan (DEFF / Design Effect)",
              type = "numeric", unit = "", seuils = "1.5 hypoth\u00e8se SMART, > 2 = forte corr\u00e9lation intra-grappe"),
  correction_cluster = list(cat = "\u00c9chantillonnage", label = "Correction pour effet grappe",
                            type = "numeric", unit = "", seuils = ""),
  id_menage = list(cat = "\u00c9chantillonnage", label = "Identifiant du m\u00e9nage",
                   type = "categorical", unit = "", seuils = ""),
  consentement = list(cat = "\u00c9chantillonnage", label = "Consentement \u00e9clair\u00e9",
                      type = "binary", unit = "oui/non", seuils = ""),

  # ═══ A2. MORTALITÉ — COMPLÉMENTS ═══
  sexe_decede = list(cat = "Mortalit\u00e9", label = "Sexe de la personne d\u00e9c\u00e9d\u00e9e",
                     type = "categorical", unit = "M/F", seuils = ""),
  age_decede = list(cat = "Mortalit\u00e9", label = "\u00c2ge de la personne d\u00e9c\u00e9d\u00e9e",
                    type = "numeric", unit = "ann\u00e9es", seuils = "< 5 ans pour U5MR"),
  recall_period = list(cat = "Mortalit\u00e9", label = "P\u00e9riode de rappel",
                       type = "numeric", unit = "jours", seuils = "G\u00e9n\u00e9ralement 90 jours (SMART)"),
  pop_debut = list(cat = "Mortalit\u00e9", label = "Population au d\u00e9but de la p\u00e9riode de rappel",
                   type = "numeric", unit = "personnes", seuils = ""),
  pop_fin = list(cat = "Mortalit\u00e9", label = "Population \u00e0 la fin de la p\u00e9riode de rappel",
                 type = "numeric", unit = "personnes", seuils = ""),

  # ═══ B2. IYCF — COMPLÉMENTS ═══
  prelacteal_feeds = list(cat = "IYCF", label = "Aliments pr\u00e9lactaux donn\u00e9s",
                          type = "binary", unit = "oui/non", seuils = "D\u00e9conseill\u00e9 par OMS"),
  duree_allaitement = list(cat = "IYCF", label = "Dur\u00e9e d'allaitement maternel",
                           type = "numeric", unit = "mois", seuils = "Recommand\u00e9 \u2265 24 mois"),
  lait_animal = list(cat = "IYCF", label = "Consommation de lait animal / artificiel",
                     type = "binary", unit = "oui/non", seuils = ""),
  boissons_sucrees = list(cat = "IYCF", label = "Consommation de boissons sucr\u00e9es",
                          type = "binary", unit = "oui/non", seuils = "D\u00e9conseill\u00e9 < 2 ans"),
  aliments_sucres = list(cat = "IYCF", label = "Consommation d'aliments sucr\u00e9s",
                         type = "binary", unit = "oui/non", seuils = "D\u00e9conseill\u00e9 < 2 ans"),
  aliments_ultra_transformes = list(cat = "IYCF", label = "Consommation aliments ultra-transform\u00e9s",
                                    type = "binary", unit = "oui/non", seuils = "D\u00e9conseill\u00e9"),
  zero_legumes_fruits = list(cat = "IYCF", label = "Z\u00e9ro l\u00e9gumes et fruits consomm\u00e9s",
                             type = "binary", unit = "oui/non", seuils = "Indicateur n\u00e9gatif"),
  repas_solides_semi = list(cat = "IYCF", label = "Aliments solides/semi-solides donn\u00e9s",
                            type = "binary", unit = "oui/non", seuils = "D\u00e8s 6 mois"),
  age_allaitement = list(cat = "IYCF", label = "\u00c2ge de l'enfant allait\u00e9",
                         type = "numeric", unit = "mois", seuils = ""),

  # ═══ C2. ANÉMIE — COMPLÉMENTS ═══
  enceinte = list(cat = "An\u00e9mie", label = "Femme enceinte",
                  type = "binary", unit = "oui/non", seuils = "Seuil an\u00e9mie < 11 g/dL"),
  trimestre_grossesse = list(cat = "An\u00e9mie", label = "Trimestre de grossesse",
                             type = "categorical", unit = "1er/2e/3e", seuils = ""),
  suppl_fer_folate = list(cat = "An\u00e9mie", label = "Suppl\u00e9mentation fer/acide folique (IFA)",
                          type = "binary", unit = "oui/non", seuils = "Recommand\u00e9 pendant grossesse"),
  allaitante = list(cat = "An\u00e9mie", label = "Femme allaitante",
                    type = "binary", unit = "oui/non", seuils = "Seuil an\u00e9mie < 12 g/dL"),
  deworming = list(cat = "An\u00e9mie", label = "D\u00e9parasitage (albendazole/m\u00e9bendazole)",
                   type = "binary", unit = "oui/non", seuils = "Recommand\u00e9 bi-annuel"),

  # ═══ D2. PALUDISME — COMPLÉMENTS ═══
  source_llin = list(cat = "Paludisme", label = "Source/provenance de la moustiquaire",
                     type = "categorical", unit = "", seuils = ""),
  age_llin = list(cat = "Paludisme", label = "\u00c2ge de la moustiquaire",
                  type = "numeric", unit = "mois", seuils = "Remplacement recommand\u00e9 \u00e0 36 mois"),
  traitement_palu = list(cat = "Paludisme", label = "Traitement antipalud\u00e9en re\u00e7u",
                         type = "binary", unit = "oui/non", seuils = "ACT recommand\u00e9"),
  prophylaxie_palu = list(cat = "Paludisme", label = "Prophylaxie antipaludique (TPIg/CPS)",
                          type = "binary", unit = "oui/non", seuils = ""),
  etat_llin = list(cat = "Paludisme", label = "\u00c9tat/int\u00e9grit\u00e9 de la moustiquaire",
                   type = "categorical", unit = "", seuils = "Bon / Trous / Inutilisable"),

  # ═══ E2. WASH — COMPLÉMENTS ═══
  stockage_eau = list(cat = "WASH", label = "Type de stockage de l'eau",
                      type = "categorical", unit = "", seuils = "R\u00e9cipient couvert recommand\u00e9"),
  distance_source_eau = list(cat = "WASH", label = "Distance \u00e0 la source d'eau",
                             type = "numeric", unit = "m\u00e8tres", seuils = "\u2264 500 m = acc\u00e8s raisonnable (Sph\u00e8re)"),
  eaux_usees = list(cat = "WASH", label = "Gestion des eaux us\u00e9es",
                    type = "categorical", unit = "", seuils = ""),
  hygiene_menstruelle = list(cat = "WASH", label = "Gestion de l'hygi\u00e8ne menstruelle (MHM)",
                             type = "categorical", unit = "", seuils = ""),
  recurrence_eau = list(cat = "WASH", label = "Disponibilit\u00e9/r\u00e9gularit\u00e9 de l'eau",
                        type = "categorical", unit = "", seuils = ""),

  # ═══ H. SANTÉ / VACCINATION DÉTAILLÉE ═══
  vac_bcg = list(cat = "Sant\u00e9 / Vaccination", label = "Vaccination BCG",
                 type = "binary", unit = "oui/non", seuils = "\u00c0 la naissance"),
  vac_penta = list(cat = "Sant\u00e9 / Vaccination", label = "Vaccination Penta/DTC (3 doses)",
                   type = "binary", unit = "oui/non", seuils = "6, 10, 14 semaines"),
  vac_polio = list(cat = "Sant\u00e9 / Vaccination", label = "Vaccination Polio (OPV3)",
                   type = "binary", unit = "oui/non", seuils = "6, 10, 14 semaines"),
  vac_complete = list(cat = "Sant\u00e9 / Vaccination", label = "Vaccination compl\u00e8te (PEV)",
                      type = "binary", unit = "oui/non", seuils = "Tous antig\u00e8nes re\u00e7us"),
  carnet_vaccination = list(cat = "Sant\u00e9 / Vaccination", label = "Carnet de vaccination vu",
                            type = "binary", unit = "oui/non", seuils = ""),
  consultation_soins = list(cat = "Sant\u00e9 / Vaccination", label = "Consultation / recours aux soins",
                            type = "binary", unit = "oui/non", seuils = ""),
  acces_sante = list(cat = "Sant\u00e9 / Vaccination", label = "Acc\u00e8s au centre de sant\u00e9",
                     type = "categorical", unit = "distance/temps", seuils = "\u2264 5 km ou \u2264 1h (Sph\u00e8re)"),
  fievre_traitement = list(cat = "Sant\u00e9 / Vaccination", label = "Traitement de la fi\u00e8vre",
                           type = "categorical", unit = "", seuils = ""),
  oreillons = list(cat = "Sant\u00e9 / Vaccination", label = "Vaccination oreillons",
                   type = "binary", unit = "oui/non", seuils = ""),

  # ═══ I. SÉCURITÉ ALIMENTAIRE — COMPLÉMENTS ═══
  lcsi = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Indice de Strat\u00e9gies de Subsistance (LCSI)",
              type = "categorical", unit = "", seuils = "Stress / Crise / Urgence / Aucun"),
  lcsi_stress = list(cat = "Composantes LCSI", label = "LCSI \u2014 Strat\u00e9gies de stress",
                     type = "binary", unit = "oui/non", seuils = ""),
  lcsi_crisis = list(cat = "Composantes LCSI", label = "LCSI \u2014 Strat\u00e9gies de crise",
                     type = "binary", unit = "oui/non", seuils = ""),
  lcsi_emergency = list(cat = "Composantes LCSI", label = "LCSI \u2014 Strat\u00e9gies d'urgence",
                        type = "binary", unit = "oui/non", seuils = ""),
  fies = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "\u00c9chelle d'Exp\u00e9rience d'Ins\u00e9curit\u00e9 Alimentaire (FIES)",
              type = "numeric", unit = "score 0-8", seuils = "\u2265 4 = ins\u00e9curit\u00e9 mod\u00e9r\u00e9e, \u2265 7 = s\u00e9v\u00e8re"),
  mdd_w = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Diversit\u00e9 Alimentaire Minimum Femmes (MDD-W)",
               type = "binary", unit = "oui/non", seuils = "\u2265 5 groupes sur 10"),
  source_nourriture = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Source principale de nourriture",
                           type = "categorical", unit = "", seuils = ""),
  aide_alimentaire = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Aide alimentaire re\u00e7ue",
                          type = "binary", unit = "oui/non", seuils = ""),
  nb_repas_jour = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Nombre de repas par jour",
                       type = "numeric", unit = "nombre", seuils = ""),

  # ═══ J. MÉNAGE — COMPLÉMENTS ═══
  type_abri = list(cat = "M\u00e9nage", label = "Type d'abri / logement",
                   type = "categorical", unit = "", seuils = "Durable / Semi-durable / Pr\u00e9caire"),
  statut_propriete = list(cat = "M\u00e9nage", label = "Statut de propri\u00e9t\u00e9 / occupation",
                          type = "categorical", unit = "", seuils = ""),
  education_cm = list(cat = "M\u00e9nage", label = "Niveau d'\u00e9ducation du chef de m\u00e9nage",
                      type = "categorical", unit = "", seuils = "Aucun / Primaire / Secondaire / Sup\u00e9rieur"),
  handicap_wg = list(cat = "Handicap (Washington Group)", label = "Statut de handicap (score WG)",
                     type = "categorical", unit = "", seuils = "Au moins 1 domaine = beaucoup de difficult\u00e9"),
  handicap_voir = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 \u00e0 voir",
                       type = "categorical", unit = "1-4", seuils = "1=aucune, 4=impossible"),
  handicap_entendre = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 \u00e0 entendre",
                           type = "categorical", unit = "1-4", seuils = "1=aucune, 4=impossible"),
  handicap_marcher = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 \u00e0 marcher",
                          type = "categorical", unit = "1-4", seuils = "1=aucune, 4=impossible"),
  handicap_memoire = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 de m\u00e9moire/concentration",
                          type = "categorical", unit = "1-4", seuils = "1=aucune, 4=impossible"),
  handicap_soins_personnels = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 soins personnels",
                                   type = "categorical", unit = "1-4", seuils = "1=aucune, 4=impossible"),
  handicap_communication = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 de communication",
                                type = "categorical", unit = "1-4", seuils = "1=aucune, 4=impossible"),

  # ═══ K. PROTECTION ═══
  enregistrement = list(cat = "Protection", label = "Enregistrement UNHCR / HCR",
                        type = "binary", unit = "oui/non", seuils = ""),
  documentation = list(cat = "Protection", label = "Possession de documents d'identit\u00e9",
                       type = "binary", unit = "oui/non", seuils = ""),
  incident_protection = list(cat = "Protection", label = "Incident de protection / VBG",
                             type = "binary", unit = "oui/non", seuils = ""),
  travail_enfant = list(cat = "Protection", label = "Travail des enfants",
                        type = "binary", unit = "oui/non", seuils = ""),
  mariage_precoce = list(cat = "Protection", label = "Mariage pr\u00e9coce / forc\u00e9",
                         type = "binary", unit = "oui/non", seuils = ""),

  # ═══ VARIABLES COMPLÉMENTAIRES SMART-SENS ═══

  # Démographie
  enfants_6_17 = list(cat = "M\u00e9nage", label = "Enfants 6-17 ans dans le m\u00e9nage",
                      type = "numeric", unit = "nombre", seuils = ""),


  # IYCF détaillé
  introduction_liq = list(cat = "IYCF", label = "Introduction de liquides (pr\u00e9-lacteaux)",
                          type = "binary", unit = "oui/non", seuils = ""),
  diversite_min = list(cat = "IYCF", label = "Diversit\u00e9 alimentaire minimale (MDD)",
                       type = "numeric", unit = "score/binaire", seuils = "\u2265 5 groupes"),
  frequence_min = list(cat = "IYCF", label = "Fr\u00e9quence minimale des repas (MMF)",
                       type = "numeric", unit = "score/binaire", seuils = "Variable selon \u00e2ge"),
  introduction_aliments = list(cat = "IYCF", label = "Introduction aliments compl\u00e9mentaires",
                               type = "binary", unit = "oui/non", seuils = "6-8 mois"),
  aliments_prelactees = list(cat = "IYCF", label = "Aliments pr\u00e9-lact\u00e9s",
                             type = "binary", unit = "oui/non", seuils = ""),
  lait_autre = list(cat = "IYCF", label = "Lait autre (animal/formule)",
                    type = "binary", unit = "oui/non", seuils = ""),
  repas_fer_enrichi = list(cat = "IYCF", label = "Repas enrichi en fer",
                           type = "binary", unit = "oui/non", seuils = ""),
  age_introduction = list(cat = "IYCF", label = "\u00c2ge d'introduction des aliments",
                          type = "numeric", unit = "mois", seuils = "6 mois recommand\u00e9"),
  allaitement_sein = list(cat = "IYCF", label = "A \u00e9t\u00e9 allait\u00e9 au sein",
                          type = "binary", unit = "oui/non", seuils = ""),
  allaitement_actuel = list(cat = "IYCF", label = "Allaitement en cours",
                            type = "binary", unit = "oui/non", seuils = ""),
  boissons_sucrees = list(cat = "IYCF", label = "Consommation boissons sucr\u00e9es",
                          type = "binary", unit = "oui/non", seuils = ""),
  aliments_sucres = list(cat = "IYCF", label = "Consommation aliments sucr\u00e9s",
                         type = "binary", unit = "oui/non", seuils = ""),
  zero_veg_fruits = list(cat = "IYCF", label = "Z\u00e9ro l\u00e9gume/fruit consomm\u00e9",
                         type = "binary", unit = "oui/non", seuils = ""),

  # Anémie compléments
  supplement_fer = list(cat = "An\u00e9mie", label = "Suppl\u00e9mentation en fer",
                        type = "binary", unit = "oui/non", seuils = ""),
  trimestre_grossesse = list(cat = "An\u00e9mie", label = "Trimestre de grossesse",
                             type = "numeric", unit = "1-3", seuils = ""),
  deworming = list(cat = "Sant\u00e9", label = "D\u00e9parasitage (anthelminthique)",
                   type = "binary", unit = "oui/non", seuils = ""),

  # Paludisme compléments
  tdr_resultat = list(cat = "Paludisme", label = "R\u00e9sultat TDR paludisme",
                      type = "categorical", unit = "positif/n\u00e9gatif", seuils = ""),
  prev_paludisme = list(cat = "Paludisme", label = "Pr\u00e9valence du paludisme",
                        type = "numeric", unit = "%", seuils = ""),
  source_antipaludeen = list(cat = "Paludisme", label = "Source du traitement antipalud\u00e9en",
                             type = "categorical", unit = "", seuils = ""),
  region_llin = list(cat = "Paludisme", label = "R\u00e9gion de distribution LLIN",
                     type = "categorical", unit = "", seuils = ""),
  traitement_paludisme = list(cat = "Paludisme", label = "Traitement antipalud\u00e9en re\u00e7u",
                              type = "binary", unit = "oui/non", seuils = ""),
  age_llin = list(cat = "Paludisme", label = "\u00c2ge de la moustiquaire",
                  type = "numeric", unit = "mois", seuils = "< 36 mois recommand\u00e9"),
  etat_llin = list(cat = "Paludisme", label = "\u00c9tat de la moustiquaire",
                   type = "categorical", unit = "", seuils = "bon/moyen/mauvais"),

  # Santé / Vaccination compléments
  vomissements_2sem = list(cat = "Sant\u00e9", label = "Vomissements (2 derni\u00e8res semaines)",
                           type = "binary", unit = "oui/non", seuils = ""),
  fievre_2sem_sante = list(cat = "Sant\u00e9", label = "Recherche de soins pour fi\u00e8vre",
                           type = "binary", unit = "oui/non", seuils = ""),
  consultation_curative = list(cat = "Sant\u00e9", label = "Consultation curative",
                               type = "binary", unit = "oui/non", seuils = ""),
  acces_formation_sante = list(cat = "Sant\u00e9", label = "Acc\u00e8s formation sanitaire",
                               type = "categorical", unit = "", seuils = ""),
  origine_vaccination = list(cat = "Sant\u00e9", label = "Source/lieu de vaccination",
                             type = "categorical", unit = "", seuils = ""),
  visites_cpn = list(cat = "Sant\u00e9", label = "Visites consultations pr\u00e9natales (CPN)",
                     type = "numeric", unit = "nombre", seuils = "\u2265 4 recommand\u00e9 OMS"),
  vac_bcg = list(cat = "Sant\u00e9", label = "Vaccination BCG",
                 type = "binary", unit = "oui/non", seuils = ""),
  vac_penta = list(cat = "Sant\u00e9", label = "Vaccination Pentavalent",
                   type = "binary", unit = "oui/non", seuils = "3 doses"),
  vac_polio = list(cat = "Sant\u00e9", label = "Vaccination Polio (OPV)",
                   type = "binary", unit = "oui/non", seuils = "3 doses"),
  vac_complete = list(cat = "Sant\u00e9", label = "Vaccination compl\u00e8te",
                      type = "binary", unit = "oui/non", seuils = ""),
  carnet_vaccination = list(cat = "Sant\u00e9", label = "Carnet de vaccination",
                            type = "binary", unit = "oui/non", seuils = ""),

  # WASH compléments
  gestion_excreta = list(cat = "WASH", label = "Gestion des excr\u00e9tas",
                         type = "categorical", unit = "", seuils = ""),
  gestion_eau_menage = list(cat = "WASH", label = "Gestion de l'eau au m\u00e9nage",
                            type = "categorical", unit = "", seuils = ""),
  qualite_eau = list(cat = "WASH", label = "Qualit\u00e9 de l'eau",
                     type = "categorical", unit = "", seuils = ""),
  fcs_eau = list(cat = "WASH", label = "FCS eau (sous-score)",
                 type = "numeric", unit = "score", seuils = ""),
  hygiene_menstruelle = list(cat = "WASH", label = "Gestion hygi\u00e8ne menstruelle",
                             type = "binary", unit = "oui/non", seuils = ""),

  # Sécurité alimentaire compléments
  fcs_categorie = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "Cat\u00e9gorie FCS",
                       type = "categorical", unit = "", seuils = "Pauvre/Limite/Acceptable"),
  lcsi_stress = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "LCSI \u2014 Strat\u00e9gies de stress",
                     type = "numeric", unit = "0/1", seuils = ""),
  lcsi_crise = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "LCSI \u2014 Strat\u00e9gies de crise",
                    type = "numeric", unit = "0/1", seuils = ""),
  lcsi_urgence = list(cat = "S\u00e9curit\u00e9 alimentaire", label = "LCSI \u2014 Strat\u00e9gies d'urgence",
                      type = "numeric", unit = "0/1", seuils = ""),

  # HDDS compléments
  hdds_vit_a = list(cat = "Composantes HDDS", label = "HDDS \u2014 Fruits/L\u00e9gumes riches vit. A",
                    type = "binary", unit = "0/1", seuils = ""),
  hdds_autres_legumes = list(cat = "Composantes HDDS", label = "HDDS \u2014 Autres l\u00e9gumes",
                             type = "binary", unit = "0/1", seuils = ""),

  # Washington Group compléments
  wg_audition = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 audition",
                     type = "categorical", unit = "1-4", seuils = "\u2265 3 = difficult\u00e9 significative"),
  wg_mobilite = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 mobilit\u00e9",
                     type = "categorical", unit = "1-4", seuils = "\u2265 3 = difficult\u00e9 significative"),
  wg_memoire = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 m\u00e9moire/concentration",
                    type = "categorical", unit = "1-4", seuils = "\u2265 3 = difficult\u00e9 significative"),
  wg_autonomie = list(cat = "Handicap (Washington Group)", label = "WG \u2014 Difficult\u00e9 autonomie/soins personnels",
                      type = "categorical", unit = "1-4", seuils = "\u2265 3 = difficult\u00e9 significative"),

  # Protection compléments
  document_identite = list(cat = "Protection", label = "Document d'identit\u00e9",
                           type = "binary", unit = "oui/non", seuils = ""),

  # Enquête / Métadonnées
  poids_echantillon = list(cat = "Enqu\u00eate", label = "Poids d'\u00e9chantillonnage",
                           type = "numeric", unit = "", seuils = ""),
  correction_plan = list(cat = "Enqu\u00eate", label = "Correction du plan de sondage (FPC)",
                         type = "numeric", unit = "", seuils = ""),

  # Variables supplémentaires
  handicap_menage = list(cat = "Handicap (Washington Group)", label = "Handicap au niveau m\u00e9nage",
                         type = "binary", unit = "oui/non", seuils = ""),
  fievre_paludisme = list(cat = "Paludisme", label = "Fi\u00e8vre li\u00e9e au paludisme",
                          type = "binary", unit = "oui/non", seuils = ""),
  latrine_acces = list(cat = "WASH", label = "Acc\u00e8s \u00e0 une latrine",
                       type = "binary", unit = "oui/non", seuils = ""),
  eau_lavage_mains = list(cat = "WASH", label = "Eau au point de lavage des mains",
                          type = "binary", unit = "oui/non", seuils = ""),
  rcsi_emprunt = list(cat = "Composantes rCSI", label = "rCSI \u2014 Emprunt nourriture (variante)",
                      type = "numeric", unit = "jours 0-7", seuils = "Poids OMS = 2"),
  hdds_noix = list(cat = "Composantes HDDS", label = "HDDS \u2014 Noix / Graines",
                   type = "binary", unit = "0/1", seuils = ""),
  statut_deplacement_enq = list(cat = "Enqu\u00eate", label = "Statut d\u00e9placement (enqu\u00eate)",
                                type = "categorical", unit = "", seuils = "")
)

# Détection intelligente d'une colonne
detect_col <- function(df_names, candidates) {
  dl <- tolower(df_names)
  # Variante sans underscore pour chaque candidat (gère le camelCase ex: FCSStap, HHSNoFood)
  cands_lo <- tolower(candidates)
  cands_nounderscore <- gsub("_", "", cands_lo)

  # Étape 1 : correspondance exacte (avec ou sans underscore)
  for (i in seq_along(cands_lo)) {
    idx <- which(dl == cands_lo[i])
    if (length(idx)) return(df_names[idx[1]])
    # Correspondance exacte après suppression des underscores des deux côtés
    dl_nu <- gsub("_", "", dl)
    idx2 <- which(dl_nu == cands_nounderscore[i])
    if (length(idx2)) return(df_names[idx2[1]])
  }
  # Étape 2 : correspondance par frontière (avec ou sans underscore)
  for (i in seq_along(cands_lo)) {
    c <- cands_lo[i]
    idx <- which(grepl(paste0("^", c, "$|_", c, "$|^", c, "_"), dl))
    if (length(idx)) return(df_names[idx[1]])
    # Même logique sans underscore dans le nom de colonne
    cn <- cands_nounderscore[i]
    dl_nu <- gsub("_", "", dl)
    if (nchar(cn) >= 4) {
      idx2 <- which(grepl(paste0("^", cn, "$"), dl_nu))
      if (length(idx2)) return(df_names[idx2[1]])
    }
  }
  # Étape 3 : sous-chaîne (candidats de longueur >= 3)
  for (c in cands_lo) {
    if (nchar(c) >= 3) {
      idx <- which(grepl(c, dl, fixed=TRUE))
      if (length(idx)) return(df_names[idx[1]])
    }
  }
  NULL
}

# Détection automatique complète (avec dé-duplication : chaque colonne n'est
# assignée qu'à une seule variable OMS, en respectant l'ordre de priorité)
auto_detect <- function(df) {
  nms       <- names(df)
  remaining <- nms                       # colonnes encore disponibles
  result    <- setNames(vector("list", length(OMS_DICT)), names(OMS_DICT))

  for (key in names(OMS_DICT)) {
    cands <- OMS_DICT[[key]]
    if (!key %in% cands) cands <- c(key, cands)
    matched <- detect_col(remaining, cands)
    if (!is.null(matched)) {
      result[[key]] <- matched
      remaining     <- setdiff(remaining, matched)
    }
  }
  result
}

# ── Vérification de conformité OMS ────────────────────────────────────────────
# Analyse chaque colonne du dataframe et détermine si elle correspond à une
# variable OMS standard. Retourne un data.frame avec les résultats.
oms_conformity_check <- function(df) {
  nms <- names(df)
  dl  <- tolower(nms)

  results <- lapply(seq_along(nms), function(i) {
    col  <- nms[i]
    clow <- dl[i]

    # Déterminer le type R de la colonne
    x <- df[[col]]
    col_type <- if (is.numeric(x)) "numeric"
                else if (is.logical(x)) "binary"
                else if (inherits(x, "Date") || inherits(x, "POSIXct")) "date"
                else "categorical"

    # Chercher la meilleure correspondance OMS
    best_key <- NULL
    best_conf <- "none"   # none < low < medium < high

    for (key in names(OMS_DICT)) {
      pats <- tolower(OMS_DICT[[key]])

      # Étape 1 : correspondance exacte (confiance haute)
      if (clow %in% pats) {
        best_key  <- key
        best_conf <- "high"
        break
      }

      # Étape 2 : correspondance par frontière (confiance haute)
      for (p in pats) {
        if (grepl(paste0("^", p, "$|_", p, "$|^", p, "_"), clow)) {
          best_key  <- key
          best_conf <- "high"
          break
        }
      }
      if (best_conf == "high") break

      # Étape 3 : sous-chaîne (confiance moyenne)
      for (p in pats) {
        if (nchar(p) >= 3 && grepl(p, clow, fixed = TRUE)) {
          if (best_conf != "medium") {
            best_key  <- key
            best_conf <- "medium"
          }
        }
      }
    }

    # Récupérer les métadonnées OMS si trouvé
    meta <- if (!is.null(best_key) && best_key %in% names(OMS_META))
              OMS_META[[best_key]] else list(cat = "", label = "", unit = "", seuils = "")

    data.frame(
      colonne       = col,
      type_r        = col_type,
      variable_oms  = if (!is.null(best_key)) best_key else NA_character_,
      label_oms     = if (!is.null(best_key)) meta$label else "",
      categorie_oms = if (!is.null(best_key)) meta$cat   else "",
      confiance     = best_conf,
      type_attendu  = if (!is.null(best_key)) meta$type  else "",
      unite         = if (!is.null(best_key)) meta$unit  else "",
      seuils        = if (!is.null(best_key)) meta$seuils else "",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}

# Type de variable
var_type <- function(x) {
  if (is.numeric(x)) {
    u <- length(unique(na.omit(x)))
    if (u <= 2 && all(na.omit(x) %in% 0:1)) return("binaire")
    if (u <= 7) return("discrete")
    return("continue")
  }
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    u <- length(unique(na.omit(as.character(x))))
    if (u <= 2) return("binaire")
    if (u <= 7) return("cat_peu")
    return("cat_many")
  }
  "autre"
}

# ── CALCUL INDICATEURS OMS ──────────────────────────────────────────────────
calc_indicateurs <- function(df, m) {
  # Oedème
  if (!is.null(m$oedema) && m$oedema %in% names(df)) {
    df$.oedema <- tolower(as.character(df[[m$oedema]])) %in%
      c("yes","oui","1","true","y","positif","positive","pos")
  } else df$.oedema <- rep(FALSE, nrow(df))

  # Scores Z
  for (sc in c("whz","haz","waz")) {
    col <- m[[sc]]
    if (!is.null(col) && col %in% names(df))
      df[[paste0(".",sc)]] <- suppressWarnings(as.numeric(df[[col]]))
    else df[[paste0(".",sc)]] <- NA_real_
  }

  # MUAC
  if (!is.null(m$muac) && m$muac %in% names(df)) {
    df$.muac <- suppressWarnings(as.numeric(df[[m$muac]]))
    # Convertir cm en mm si nécessaire
    if (!all(is.na(df$.muac)) && median(df$.muac, na.rm=TRUE) < 30)
      df$.muac <- df$.muac * 10
  } else df$.muac <- NA_real_

  # Malnutrition aiguë
  df$GAM         <- (!is.na(df$.whz) & df$.whz < -2) | df$.oedema
  df$SAM         <- (!is.na(df$.whz) & df$.whz < -3) | df$.oedema
  df$MAM         <- !is.na(df$.whz) & df$.whz >= -3 & df$.whz < -2 & !df$.oedema
  df$Oedema_seul <- !is.na(df$.whz) & df$.whz >= -2 & df$.oedema

  # Retard de croissance
  df$Stunting     <- !is.na(df$.haz) & df$.haz < -2
  df$Sev_Stunting <- !is.na(df$.haz) & df$.haz < -3
  df$Mod_Stunting <- !is.na(df$.haz) & df$.haz >= -3 & df$.haz < -2

  # Insuffisance pondérale
  df$Underweight     <- !is.na(df$.waz) & df$.waz < -2
  df$Sev_Underweight <- !is.na(df$.waz) & df$.waz < -3

  # MUAC
  df$MUAC_GAM <- !is.na(df$.muac) & df$.muac < 125
  df$MUAC_MAM <- !is.na(df$.muac) & df$.muac >= 115 & df$.muac < 125
  df$MUAC_SAM <- !is.na(df$.muac) & df$.muac < 115

  # Catégorie nutritionnelle
  df$nutr_cat <- case_when(df$SAM ~ "SAM", df$MAM ~ "MAM", TRUE ~ "Normal") |>
    factor(levels=c("Normal","MAM","SAM"))
  df$muac_cat <- case_when(
    !is.na(df$.muac) & df$.muac < 115  ~ "SAM (<115mm)",
    !is.na(df$.muac) & df$.muac < 125  ~ "MAM (115-124mm)",
    !is.na(df$.muac) ~ "Normal (≥125mm)",
    TRUE ~ NA_character_
  ) |> factor(levels=c("Normal (≥125mm)","MAM (115-124mm)","SAM (<115mm)"))

  # Âge
  if (!is.null(m$age_m) && m$age_m %in% names(df)) {
    df$.age <- suppressWarnings(as.numeric(df[[m$age_m]]))
    df$age_group <- cut(df$.age,
      breaks=c(0,5,11,17,23,35,47,59,Inf),
      labels=c("0-5m","6-11m","12-17m","18-23m","24-35m","36-47m","48-59m","60+m"),
      right=TRUE)
    df$age_group5 <- cut(df$.age,
      breaks=c(0,11,23,35,47,59,Inf),
      labels=c("<12m","12-23m","24-35m","36-47m","48-59m","60+m"),
      right=TRUE)
  }

  # Anthropométrie adulte
  if (!is.null(m$poids) && !is.null(m$taille) &&
      m$poids %in% names(df) && m$taille %in% names(df)) {
    p <- suppressWarnings(as.numeric(df[[m$poids]]))
    t <- suppressWarnings(as.numeric(df[[m$taille]]))
    t_m <- ifelse(t > 10, t/100, t)
    df$IMC <- p / t_m^2
  }

  # FCS (PAM)
  if (!is.null(m$fcs) && m$fcs %in% names(df)) {
    df$.fcs <- suppressWarnings(as.numeric(df[[m$fcs]]))
    df$fcs_cat <- cut(df$.fcs, breaks=c(-Inf,21,35,Inf),
      labels=c("Pauvre (<21)","Limite (21-35)","Acceptable (>35)"),
      right=TRUE)
  }

  # HHS (PAM) — score total seulement si m$hhs n'est PAS une composante individuelle
  hhs_comp_cols <- c(m$hhs_nofood, m$hhs_sleep, m$hhs_wholeday)
  hhs_is_comp   <- !is.null(m$hhs) && m$hhs %in% hhs_comp_cols
  if (!is.null(m$hhs) && m$hhs %in% names(df) && !hhs_is_comp) {
    df$.hhs <- suppressWarnings(as.numeric(df[[m$hhs]]))
  }

  # HHS — auto-calcul depuis les composantes si score total absent ou mal détecté
  if (!".hhs" %in% names(df) || all(is.na(df$.hhs))) {
    hhs_cols_comp <- list(
      nofood   = if (!is.null(m$hhs_nofood)   && m$hhs_nofood   %in% names(df)) m$hhs_nofood   else NULL,
      sleep    = if (!is.null(m$hhs_sleep)    && m$hhs_sleep    %in% names(df)) m$hhs_sleep    else NULL,
      wholeday = if (!is.null(m$hhs_wholeday) && m$hhs_wholeday %in% names(df)) m$hhs_wholeday else NULL
    )
    n_hhs_comp <- sum(!sapply(hhs_cols_comp, is.null))
    if (n_hhs_comp >= 2) {
      hhs_score <- rep(0, nrow(df))
      for (col in hhs_cols_comp[!sapply(hhs_cols_comp, is.null)]) {
        vals <- suppressWarnings(as.numeric(df[[col]]))
        # Normaliser : si valeurs 0-2 (standard HHS) conserver, si 0-3 (variant) rescaler
        max_v <- max(vals, na.rm = TRUE)
        if (!is.na(max_v) && max_v > 2) vals <- pmin(round(vals * 2 / max_v), 2)
        hhs_score <- hhs_score + replace(vals, is.na(vals), 0)
      }
      df$.hhs <- hhs_score
      if (n_hhs_comp < 3) df$.hhs <- df$.hhs  # Score partiel accepté
    }
  }

  # HHS — classification
  if (".hhs" %in% names(df) && !all(is.na(df$.hhs))) {
    df$hhs_cat <- case_when(
      df$.hhs == 0 ~ "Aucune faim",
      df$.hhs <= 1 ~ "Faim l\u00e9g\u00e8re",
      df$.hhs <= 3 ~ "Faim mod\u00e9r\u00e9e",
      TRUE         ~ "Faim s\u00e9v\u00e8re"
    ) |> factor(levels=c("Aucune faim","Faim l\u00e9g\u00e8re","Faim mod\u00e9r\u00e9e","Faim s\u00e9v\u00e8re"))
  }

  # rCSI (PAM/WFP) — auto-calcul depuis composantes
  # Poids officiels PAM : LessQlty×1, Borrow×2, MealNb×1, MealSize×1, MealAdult×3
  if (!".rcsi" %in% names(df)) {
    rcsi_comp <- list(
      lessqlty   = list(col = if (!is.null(m$rcsi_lessqlty)   && m$rcsi_lessqlty   %in% names(df)) m$rcsi_lessqlty   else NULL, w = 1),
      borrow     = list(col = if (!is.null(m$rcsi_borrow)     && m$rcsi_borrow     %in% names(df)) m$rcsi_borrow     else NULL, w = 2),
      meal_nb    = list(col = if (!is.null(m$rcsi_meal_nb)    && m$rcsi_meal_nb    %in% names(df)) m$rcsi_meal_nb    else NULL, w = 1),
      meal_size  = list(col = if (!is.null(m$rcsi_meal_size)  && m$rcsi_meal_size  %in% names(df)) m$rcsi_meal_size  else NULL, w = 1),
      meal_adult = list(col = if (!is.null(m$rcsi_meal_adult) && m$rcsi_meal_adult %in% names(df)) m$rcsi_meal_adult else NULL, w = 3)
    )
    n_rcsi_comp <- sum(!sapply(rcsi_comp, function(x) is.null(x$col)))
    if (n_rcsi_comp >= 3) {
      rcsi_score <- rep(0, nrow(df))
      for (item in rcsi_comp) {
        if (!is.null(item$col)) {
          vals <- suppressWarnings(as.numeric(df[[item$col]]))
          rcsi_score <- rcsi_score + replace(vals, is.na(vals), 0) * item$w
        }
      }
      df$.rcsi <- rcsi_score
      df$rcsi_cat <- case_when(
        df$.rcsi <  4 ~ "Pas/Peu de coping",
        df$.rcsi < 19 ~ "Coping moyen",
        TRUE          ~ "Coping \u00e9lev\u00e9 (\u226519)"
      ) |> factor(levels = c("Pas/Peu de coping","Coping moyen","Coping \u00e9lev\u00e9 (\u226519)"))
    }
  }

  # rCSI — si score pré-calculé et non encore traité
  if (!".rcsi" %in% names(df) && !is.null(m$rcsi) && m$rcsi %in% names(df)) {
    df$.rcsi <- suppressWarnings(as.numeric(df[[m$rcsi]]))
    df$rcsi_cat <- case_when(
      df$.rcsi <  4 ~ "Pas/Peu de coping",
      df$.rcsi < 19 ~ "Coping moyen",
      TRUE          ~ "Coping \u00e9lev\u00e9 (\u226519)"
    ) |> factor(levels = c("Pas/Peu de coping","Coping moyen","Coping \u00e9lev\u00e9 (\u226519)"))
  }

  # HDDS (FAO/FANTA) — auto-calcul depuis colonnes binaires des groupes
  hdds_keys <- c("hdds_cereales","hdds_tubercules","hdds_legumes_hdds","hdds_fruits_hdds",
                 "hdds_viande","hdds_oeufs","hdds_poisson","hdds_legumineuses_hdds",
                 "hdds_lait","hdds_huile","hdds_sucre","hdds_condiments")
  hdds_cols_found <- Filter(function(k) !is.null(m[[k]]) && m[[k]] %in% names(df), hdds_keys)
  if (length(hdds_cols_found) >= 4 && !".hdds" %in% names(df)) {
    hdds_score <- Reduce("+", lapply(hdds_cols_found, function(k) {
      vals <- suppressWarnings(as.numeric(df[[m[[k]]]]))
      # Binariser si nécessaire (certains encodages >1)
      pmin(replace(vals, is.na(vals), 0), 1)
    }))
    df$.hdds <- hdds_score
    df$hdds_cat <- case_when(
      df$.hdds <= 3 ~ "Faible diversit\u00e9 (\u22643)",
      df$.hdds <= 5 ~ "Diversit\u00e9 moyenne (4-5)",
      TRUE          ~ "Bonne diversit\u00e9 (\u22656)"
    ) |> factor(levels = c("Faible diversit\u00e9 (\u22643)","Diversit\u00e9 moyenne (4-5)","Bonne diversit\u00e9 (\u22656)"))
  }

  # HDDS — si score pré-calculé
  if (!".hdds" %in% names(df) && !is.null(m$hdds) && m$hdds %in% names(df)) {
    df$.hdds <- suppressWarnings(as.numeric(df[[m$hdds]]))
    df$hdds_cat <- case_when(
      df$.hdds <= 3 ~ "Faible diversit\u00e9 (\u22643)",
      df$.hdds <= 5 ~ "Diversit\u00e9 moyenne (4-5)",
      TRUE          ~ "Bonne diversit\u00e9 (\u22656)"
    ) |> factor(levels = c("Faible diversit\u00e9 (\u22643)","Diversit\u00e9 moyenne (4-5)","Bonne diversit\u00e9 (\u22656)"))
  }

  # ICV (indice composite vulnérabilité) — intègre FCS, HHS, rCSI, HDDS, MUAC, âge
  safe_range <- function(x) { r <- range(x, na.rm=TRUE); if (diff(r) == 0) c(r[1]-1, r[1]+1) else r }
  icv_parts <- list()
  if (".fcs" %in% names(df) && !all(is.na(df$.fcs)))
    icv_parts$alim  <- scales::rescale(df$.fcs,  to=c(0,20),  from=safe_range(df$.fcs))
  if (".hhs" %in% names(df) && !all(is.na(df$.hhs)))
    icv_parts$faim  <- scales::rescale(df$.hhs,  to=c(20,0),  from=safe_range(df$.hhs))
  if (".rcsi" %in% names(df) && !all(is.na(df$.rcsi)))
    icv_parts$rcsi  <- scales::rescale(df$.rcsi, to=c(20,0),  from=safe_range(df$.rcsi))
  if (".hdds" %in% names(df) && !all(is.na(df$.hdds)))
    icv_parts$hdds  <- scales::rescale(df$.hdds, to=c(0,20),  from=safe_range(df$.hdds))
  if (".muac" %in% names(df) && !all(is.na(df$.muac)))
    icv_parts$muac  <- scales::rescale(df$.muac, to=c(0,20),  from=safe_range(df$.muac))
  if (".age" %in% names(df) && !all(is.na(df$.age)))
    icv_parts$age   <- scales::rescale(df$.age,  to=c(20,0),  from=safe_range(df$.age))

  if (length(icv_parts) >= 2) {
    icv_mat <- do.call(cbind, icv_parts)
    # Chaque part est déjà dans [0,20], la moyenne est dans [0,20], × 5 → [0,100]
    df$ICV <- rowMeans(icv_mat, na.rm=TRUE) * 5
    df$ICV <- pmin(pmax(df$ICV, 0), 100)
    df$cat_vulner <- cut(df$ICV, breaks=c(0,40,60,80,100),
      labels=c("Tr\u00e8s vuln\u00e9rable","Vuln\u00e9rable","Mod\u00e9r\u00e9","R\u00e9silient"),
      include.lowest=TRUE)
  }

  df
}

# ── ENSURE SA COLUMNS ─────────────────────────────────────────────────────
# Crée .fcs, .rcsi, .hdds, .hhs et leurs catégories si absentes mais détectables.
# Appelée dans df_f() pour que les dashboards d'accueil fonctionnent même si
# calc_indicateurs a échoué ou si les données viennent d'une source externe.
ensure_sa_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  message("[ensure_sa] Appelé. nrow=", nrow(df),
          " | .fcs_avant=", ".fcs" %in% names(df),
          " | .rcsi_avant=", ".rcsi" %in% names(df),
          " | .hdds_avant=", ".hdds" %in% names(df))

  # Helper : trouver une colonne numérique par patterns regex
  find_col <- function(d, patterns) {
    for (p in patterns) {
      hits <- grep(p, names(d), ignore.case = TRUE, value = TRUE)
      hits <- hits[!grepl("^\\.|_cat$|_class$", hits)]
      for (h in hits) if (is.numeric(d[[h]]) && !all(is.na(d[[h]]))) return(h)
    }
    NULL
  }

  # ── FCS ──
  if (!".fcs" %in% names(df) || all(is.na(df$.fcs))) {
    col <- find_col(df, c("^fcs$", "^food_consumption_score$", "^fcs_score$",
                           "food.?consumption.?score", "^sca$", "^score_conso"))
    if (!is.null(col)) {
      df$.fcs <- suppressWarnings(as.numeric(df[[col]]))
      df$fcs_cat <- cut(df$.fcs, breaks = c(-Inf, 21, 35, Inf),
        labels = c("Pauvre (<21)", "Limite (21-35)", "Acceptable (>35)"),
        right = TRUE)
    }
  }
  # fcs_cat manquant mais .fcs présent
  if (".fcs" %in% names(df) && !"fcs_cat" %in% names(df) && !all(is.na(df$.fcs))) {
    df$fcs_cat <- cut(df$.fcs, breaks = c(-Inf, 21, 35, Inf),
      labels = c("Pauvre (<21)", "Limite (21-35)", "Acceptable (>35)"),
      right = TRUE)
  }

  # ── rCSI (score pré-calculé) ──
  if (!".rcsi" %in% names(df) || all(is.na(df$.rcsi))) {
    col <- find_col(df, c("^rcsi$", "^rcsi_score$", "^coping_strategy_index$",
                           "^reduced_csi$", "^r_csi$", "^rcsiscore$"))
    if (!is.null(col)) {
      df$.rcsi <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  # rCSI — auto-calcul depuis composantes si score toujours absent
  if ((!".rcsi" %in% names(df) || all(is.na(df$.rcsi)))) {
    rcsi_pats <- list(
      lessqlty   = list(p = c("rcsi_less", "rcsilessqlty", "rCSILessQlty", "rcsi1"), w = 1),
      borrow     = list(p = c("rcsi_borrow", "rcsiborrow", "rCSIBorrow", "rcsi2"), w = 2),
      meal_nb    = list(p = c("rcsi_meal_nb", "rcsimealnb", "rCSIMealNb", "rcsi3"), w = 1),
      meal_size  = list(p = c("rcsi_meal_size", "rcsimealsize", "rCSIMealSize", "rcsi4"), w = 1),
      meal_adult = list(p = c("rcsi_meal_adult", "rcsimealadult", "rCSIMealAdult", "rcsi5"), w = 3)
    )
    found_cols <- list()
    for (nm in names(rcsi_pats)) {
      col <- find_col(df, paste0("^", rcsi_pats[[nm]]$p, "$"))
      if (!is.null(col)) found_cols[[nm]] <- list(col = col, w = rcsi_pats[[nm]]$w)
    }
    if (length(found_cols) >= 3) {
      rcsi_score <- rep(0, nrow(df))
      for (item in found_cols) {
        vals <- suppressWarnings(as.numeric(df[[item$col]]))
        rcsi_score <- rcsi_score + replace(vals, is.na(vals), 0) * item$w
      }
      df$.rcsi <- rcsi_score
    }
  }
  # rcsi_cat
  if (".rcsi" %in% names(df) && !"rcsi_cat" %in% names(df) && !all(is.na(df$.rcsi))) {
    df$rcsi_cat <- dplyr::case_when(
      df$.rcsi <  4 ~ "Pas/Peu de coping",
      df$.rcsi < 19 ~ "Coping moyen",
      TRUE          ~ "Coping \u00e9lev\u00e9 (\u226519)"
    ) |> factor(levels = c("Pas/Peu de coping", "Coping moyen", "Coping \u00e9lev\u00e9 (\u226519)"))
  }

  # ── HDDS (score pré-calculé) ──
  if (!".hdds" %in% names(df) || all(is.na(df$.hdds))) {
    col <- find_col(df, c("^hdds$", "^hdds_score$", "^dietary_diversity_score$",
                           "^dds$", "^sdam$"))
    if (!is.null(col)) {
      df$.hdds <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  # hdds_cat
  if (".hdds" %in% names(df) && !"hdds_cat" %in% names(df) && !all(is.na(df$.hdds))) {
    df$hdds_cat <- dplyr::case_when(
      df$.hdds <= 3 ~ "Faible diversit\u00e9 (\u22643)",
      df$.hdds <= 5 ~ "Diversit\u00e9 moyenne (4-5)",
      TRUE          ~ "Bonne diversit\u00e9 (\u22656)"
    ) |> factor(levels = c("Faible diversit\u00e9 (\u22643)", "Diversit\u00e9 moyenne (4-5)", "Bonne diversit\u00e9 (\u22656)"))
  }

  # ── HHS ──
  if (!".hhs" %in% names(df) || all(is.na(df$.hhs))) {
    col <- find_col(df, c("^hhs$", "^hhs_score$", "^household_hunger_scale$",
                           "^hhs_total$", "^hunger_scale$"))
    if (!is.null(col)) {
      df$.hhs <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  if (".hhs" %in% names(df) && !"hhs_cat" %in% names(df) && !all(is.na(df$.hhs))) {
    df$hhs_cat <- dplyr::case_when(
      df$.hhs == 0 ~ "Aucune faim",
      df$.hhs <= 1 ~ "Faim l\u00e9g\u00e8re",
      df$.hhs <= 3 ~ "Faim mod\u00e9r\u00e9e",
      TRUE         ~ "Faim s\u00e9v\u00e8re"
    ) |> factor(levels = c("Aucune faim", "Faim l\u00e9g\u00e8re", "Faim mod\u00e9r\u00e9e", "Faim s\u00e9v\u00e8re"))
  }

  message("[ensure_sa] Termin\u00e9. .fcs=", ".fcs" %in% names(df),
          " | .rcsi=", ".rcsi" %in% names(df),
          " | .hdds=", ".hdds" %in% names(df),
          " | fcs_cat=", "fcs_cat" %in% names(df),
          " | rcsi_cat=", "rcsi_cat" %in% names(df))
  df
}

# ── STATISTIQUES ───────────────────────────────────────────────────────────
mode_stat  <- function(x) { ux <- unique(na.omit(x)); ux[which.max(tabulate(match(x,ux)))] }
cv_stat    <- function(x) round(sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100, 2)

stats_complete <- function(x) {
  v <- na.omit(as.numeric(x))
  if (length(v) < 2) return(NULL)
  sw <- tryCatch(shapiro.test(v[1:min(5000,length(v))]), error=function(e) NULL)
  tibble::tibble(
    Statistique = c("N valides","N manquants","Moyenne","Médiane","Mode",
                    "Écart-type","Variance","CV (%)","Min","Max",
                    "Q1 (25%)","Q3 (75%)","Étendue","IQR",
                    "Asymétrie (skewness)","Aplatissement (kurtosis)",
                    "Shapiro-Wilk W","Shapiro-Wilk p","Distribution"),
    Valeur = c(
      length(v), sum(is.na(as.numeric(x))),
      round(mean(v),4), round(median(v),4),
      round(as.numeric(mode_stat(v)),4),
      round(sd(v),4), round(var(v),4), cv_stat(v),
      round(min(v),4), round(max(v),4),
      round(quantile(v,.25),4), round(quantile(v,.75),4),
      round(max(v)-min(v),4), round(IQR(v),4),
      round(e1071::skewness(v),4), round(e1071::kurtosis(v),4),
      if(!is.null(sw)) round(sw$statistic,4) else "N/A",
      if(!is.null(sw)) round(sw$p.value,4) else "N/A",
      if(!is.null(sw)) if(sw$p.value<.05) "❌ Non normale (p<0.05)" else "✅ Normale (p≥0.05)" else "N/A"
    )
  )
}

calc_prev_wilson <- function(data, var, label=NULL) {
  x  <- sum(data[[var]], na.rm=TRUE)
  n  <- sum(!is.na(data[[var]]))
  if (n == 0) return(NULL)
  ic <- binom::binom.confint(x, n, methods="wilson")
  tibble::tibble(
    Indicateur      = if(is.null(label)) var else label,
    N               = n,
    Cas             = x,
    `Prév. (%)`     = round(100*ic$mean, 1),
    `IC95 bas (%)`  = round(100*ic$lower, 1),
    `IC95 haut (%)` = round(100*ic$upper, 1),
    Sévérité        = alerte_label(round(100*ic$mean,1))
  )
}

# Choix automatique du test statistique
auto_test <- function(d, var_x, var_y, type_x, type_y) {
  tryCatch({
    if (type_x %in% c("binaire","cat_peu","cat_many") &&
        type_y %in% c("binaire","cat_peu","cat_many")) {
      tbl <- table(d[[var_x]], d[[var_y]])
      rc  <- chisq.test(tbl, correct=FALSE)
      me  <- min(rc$expected)
      if (me < 5) {
        r <- fisher.test(tbl, simulate.p.value=TRUE)
        list(nom="Test exact de Fisher", stat=NA, p=r$p.value, df=NA,
             interpretation=paste("p =",round(r$p.value,4),
               if(r$p.value<.05) "→ Association significative" else "→ Pas d'association"))
      } else {
        list(nom="Test du Chi²", stat=round(rc$statistic,3),
             p=rc$p.value, df=rc$parameter,
             interpretation=paste("χ²(",rc$parameter,") =",round(rc$statistic,3),
               "p =",round(rc$p.value,4),
               if(rc$p.value<.05) "→ Association significative ✓" else "→ Pas d'association NS"))
      }
    } else if (type_x %in% c("binaire","cat_peu") &&
               type_y %in% c("continue","discrete")) {
      niv <- length(unique(na.omit(d[[var_x]])))
      sw  <- tryCatch(shapiro.test(na.omit(d[[var_y]])[1:min(5000,length(na.omit(d[[var_y]])))]),
                      error=function(e) list(p.value=0))
      if (niv == 2) {
        test_nom <- if(sw$p.value < .05) "Mann-Whitney (non paramétrique)" else "t-test (paramétrique)"
        if (sw$p.value < .05) {
          r <- wilcox.test(d[[var_y]] ~ d[[var_x]], exact=FALSE)
          list(nom=test_nom, stat=round(r$statistic,2), p=r$p.value, df=NA,
               interpretation=paste("W =",round(r$statistic,2),"p =",round(r$p.value,4),
                 if(r$p.value<.05)"→ Différence significative ✓" else "→ NS"))
        } else {
          r <- t.test(d[[var_y]] ~ d[[var_x]])
          list(nom=test_nom, stat=round(r$statistic,3), p=r$p.value, df=round(r$parameter,1),
               interpretation=paste("t(",round(r$parameter,1),") =",round(r$statistic,3),
                 "p =",round(r$p.value,4),
                 if(r$p.value<.05)"→ Différence significative ✓" else "→ NS"))
        }
      } else {
        r <- kruskal.test(d[[var_y]] ~ d[[var_x]])
        list(nom="Kruskal-Wallis (non paramétrique)", stat=round(r$statistic,3),
             p=r$p.value, df=r$parameter,
             interpretation=paste("H(",r$parameter,") =",round(r$statistic,3),
               "p =",round(r$p.value,4),
               if(r$p.value<.05)"→ Différence significative ✓ (post-hoc BH recommandé)" else "→ NS"))
      }
    } else if (type_x %in% c("continue","discrete") &&
               type_y %in% c("continue","discrete")) {
      sw_x <- tryCatch(shapiro.test(na.omit(d[[var_x]])[1:min(5000,sum(!is.na(d[[var_x]])))]),
                       error=function(e) list(p.value=0))
      sw_y <- tryCatch(shapiro.test(na.omit(d[[var_y]])[1:min(5000,sum(!is.na(d[[var_y]])))]),
                       error=function(e) list(p.value=0))
      if (sw_x$p.value > .05 && sw_y$p.value > .05) {
        r <- cor.test(d[[var_x]], d[[var_y]], method="pearson")
        list(nom="Corrélation de Pearson", stat=round(r$estimate,4), p=r$p.value, df=r$parameter,
             interpretation=paste("r =",round(r$estimate,4),"p =",round(r$p.value,4),
               if(abs(r$estimate)>=.7)"Forte corrélation"
               else if(abs(r$estimate)>=.4)"Corrélation modérée" else "Corrélation faible"))
      } else {
        r <- cor.test(d[[var_x]], d[[var_y]], method="spearman", exact=FALSE)
        list(nom="Corrélation de Spearman (non paramétrique)",
             stat=round(r$estimate,4), p=r$p.value, df=NA,
             interpretation=paste("ρ =",round(r$estimate,4),"p =",round(r$p.value,4),
               if(abs(r$estimate)>=.7)"Forte corrélation"
               else if(abs(r$estimate)>=.4)"Corrélation modérée" else "Corrélation faible"))
      }
    } else NULL
  }, error=function(e) list(nom="Test non applicable", stat=NA, p=NA, df=NA,
                             interpretation=paste("Erreur:", e$message)))
}

# ── PALETTES GLOBALES ──────────────────────────────────────────────────────
# Chaque thème définit 3 sous-palettes : séquentielle, catégorielle, divergente
PALETTES_GLOBAL <- list(
  "OMS officiel" = list(
    seq = c("#EFF8F1","#7BC8A4","#D4A843","#D98B14","#BF3636","#5C1A1A"),
    cat = c("#0077B6","#2E7D52","#D4A843","#D98B14","#BF3636","#00838F","#6B3FA0","#0B3954"),
    div = c("#5C1A1A","#BF3636","#D98B14","#FFF9E6","#7BC8A4","#2E7D52","#1B5E3B")
  ),
  "UNICEF" = list(
    seq = c("#e0f4ff","#80d4f6","#009fda","#00729a","#003d6b"),
    cat = c("#009fda","#80d4f6","#374ea2","#e2231a","#f26a21","#00833d","#ffc20e","#961a49"),
    div = c("#003d6b","#009fda","#80d4f6","#f5f5f5","#f26a21","#e2231a","#7a0019")
  ),
  "FAO" = list(
    seq = c("#f0f4e8","#c5d99a","#8aba56","#3d7c0f","#1a4301"),
    cat = c("#006cb5","#8aba56","#f9a825","#d84315","#5c6bc0","#00897b","#ef6c00","#6d4c41"),
    div = c("#1a4301","#3d7c0f","#8aba56","#fffde7","#f9a825","#d84315","#4a0000")
  ),
  "PAM / WFP" = list(
    seq = c("#e3f2fd","#90caf9","#1976d2","#0d47a1","#001838"),
    cat = c("#0072bc","#00a3e0","#f39c12","#e74c3c","#27ae60","#8e44ad","#f1c40f","#e67e22"),
    div = c("#001838","#0d47a1","#1976d2","#f5f5f5","#f39c12","#e74c3c","#5C1A1A")
  ),
  "Viridis" = list(
    seq = NULL, cat = NULL, div = NULL  # utilise viridis::viridis() directement
  ),
  "Inferno" = list(
    seq = NULL, cat = NULL, div = NULL  # utilise viridis::inferno() directement
  ),
  "Plasma" = list(
    seq = NULL, cat = NULL, div = NULL  # utilise viridis::plasma() directement
  ),
  "Spectral" = list(
    seq = rev(RColorBrewer::brewer.pal(11, "Spectral")),
    cat = RColorBrewer::brewer.pal(8, "Set1"),
    div = RColorBrewer::brewer.pal(11, "Spectral")
  ),
  "Set2 (Doux)" = list(
    seq = c("#e0f7fa","#80cbc4","#26a69a","#00796b","#004d40"),
    cat = RColorBrewer::brewer.pal(8, "Set2"),
    div = c("#004d40","#26a69a","#80cbc4","#f5f5f5","#ce93d8","#8e24aa","#4a0072")
  ),
  "Terre (Nature)" = list(
    seq = c("#fefae0","#dda15e","#bc6c25","#606c38","#283618"),
    cat = c("#606c38","#dda15e","#bc6c25","#283618","#fefae0","#4a7c59","#8b5e3c","#a68a64"),
    div = c("#283618","#606c38","#a7c957","#fefae0","#dda15e","#bc6c25","#5c2d06")
  )
)

# Noms des palettes disponibles
PALETTE_CHOICES <- names(PALETTES_GLOBAL)

# ══════════════════════════════════════════════════════════════════════════════
# THÈMES D'INTERFACE — Apparence globale (sidebar, header, fond, accents)
# ══════════════════════════════════════════════════════════════════════════════
UI_THEMES <- list(

  # ── 1. Défaut OMS (Deep Navy) ──
  "OMS Classique" = list(
    sidebar_bg    = "linear-gradient(180deg, #0B3954 0%, #082740 60%, #061C2F 100%)",
    sidebar_text  = "rgba(180,210,235,.8)",
    sidebar_hover = "rgba(0,119,182,.2)",
    sidebar_active_border = "#D4A843",
    header_bg     = "linear-gradient(90deg, #0B3954 0%, #0C4A6E 100%)",
    header_border = "#D4A843",
    logo_bg       = "#0B3954",
    body_bg       = "#F4F6FB",
    card_border   = "#e2e8f0",
    primary       = "#0077B6",
    accent        = "#00838F",
    gold          = "#D4A843",
    section_title = "#0077B6",
    btn_bg        = "#0077B6"
  ),

  # ── 2. UNICEF Bleu Ciel ──
  "UNICEF Bleu Ciel" = list(
    sidebar_bg    = "linear-gradient(180deg, #003B5C 0%, #00526E 60%, #006B8A 100%)",
    sidebar_text  = "rgba(200,230,255,.85)",
    sidebar_hover = "rgba(0,159,218,.25)",
    sidebar_active_border = "#FFD700",
    header_bg     = "linear-gradient(90deg, #009fda 0%, #00729a 100%)",
    header_border = "#FFD700",
    logo_bg       = "#00729a",
    body_bg       = "#F0F8FF",
    card_border   = "#b3e0f2",
    primary       = "#009fda",
    accent        = "#374ea2",
    gold          = "#FFD700",
    section_title = "#009fda",
    btn_bg        = "#009fda"
  ),

  # ── 3. Forêt / Nature ──
  "For\u00eat Nature" = list(
    sidebar_bg    = "linear-gradient(180deg, #1B3A2D 0%, #243B2F 60%, #2D4A3E 100%)",
    sidebar_text  = "rgba(180,220,190,.82)",
    sidebar_hover = "rgba(74,124,89,.3)",
    sidebar_active_border = "#A7C957",
    header_bg     = "linear-gradient(90deg, #2D4A3E 0%, #3D6B4F 100%)",
    header_border = "#A7C957",
    logo_bg       = "#1B3A2D",
    body_bg       = "#F5F7F2",
    card_border   = "#c8dcc0",
    primary       = "#4A7C59",
    accent        = "#606c38",
    gold          = "#A7C957",
    section_title = "#3D6B4F",
    btn_bg        = "#4A7C59"
  ),

  # ── 4. Sable / Terre ──
  "Terre & Sable" = list(
    sidebar_bg    = "linear-gradient(180deg, #3C2415 0%, #4A2E1B 60%, #5C3A24 100%)",
    sidebar_text  = "rgba(240,220,190,.82)",
    sidebar_hover = "rgba(188,108,37,.25)",
    sidebar_active_border = "#DDA15E",
    header_bg     = "linear-gradient(90deg, #5C3A24 0%, #7A4E2D 100%)",
    header_border = "#DDA15E",
    logo_bg       = "#3C2415",
    body_bg       = "#FAF6F0",
    card_border   = "#e0d0bb",
    primary       = "#BC6C25",
    accent        = "#606c38",
    gold          = "#DDA15E",
    section_title = "#7A4E2D",
    btn_bg        = "#BC6C25"
  ),

  # ── 5. Nuit Profonde (Dark) ──
  "Nuit Profonde" = list(
    sidebar_bg    = "linear-gradient(180deg, #0F0F1A 0%, #151525 60%, #1A1A30 100%)",
    sidebar_text  = "rgba(160,170,210,.8)",
    sidebar_hover = "rgba(99,102,241,.2)",
    sidebar_active_border = "#818CF8",
    header_bg     = "linear-gradient(90deg, #1E1E36 0%, #2A2A4A 100%)",
    header_border = "#818CF8",
    logo_bg       = "#0F0F1A",
    body_bg       = "#1A1A2E",
    card_border   = "#2D2D50",
    primary       = "#818CF8",
    accent        = "#6366F1",
    gold          = "#F59E0B",
    section_title = "#A5B4FC",
    btn_bg        = "#6366F1"
  ),

  # ── 6. Aurore Boréale ──
  "Aurore Bor\u00e9ale" = list(
    sidebar_bg    = "linear-gradient(180deg, #0C1445 0%, #121B5E 60%, #1A2570 100%)",
    sidebar_text  = "rgba(180,200,255,.82)",
    sidebar_hover = "rgba(56,189,248,.2)",
    sidebar_active_border = "#38BDF8",
    header_bg     = "linear-gradient(90deg, #1A2570 0%, #0F766E 100%)",
    header_border = "#38BDF8",
    logo_bg       = "#0C1445",
    body_bg       = "#F0FDFA",
    card_border   = "#b2e8e0",
    primary       = "#0EA5E9",
    accent        = "#0F766E",
    gold          = "#38BDF8",
    section_title = "#0EA5E9",
    btn_bg        = "#0EA5E9"
  ),

  # ── 7. Rose Santé ──
  "Rose Sant\u00e9" = list(
    sidebar_bg    = "linear-gradient(180deg, #4A1942 0%, #5C224F 60%, #6E2B5C 100%)",
    sidebar_text  = "rgba(240,200,230,.82)",
    sidebar_hover = "rgba(236,72,153,.2)",
    sidebar_active_border = "#F472B6",
    header_bg     = "linear-gradient(90deg, #6E2B5C 0%, #9D174D 100%)",
    header_border = "#F472B6",
    logo_bg       = "#4A1942",
    body_bg       = "#FDF2F8",
    card_border   = "#f5c6e0",
    primary       = "#DB2777",
    accent        = "#9D174D",
    gold          = "#F472B6",
    section_title = "#DB2777",
    btn_bg        = "#DB2777"
  ),

  # ── 8. Océan Profond ──
  "Oc\u00e9an Profond" = list(
    sidebar_bg    = "linear-gradient(180deg, #0A2647 0%, #0E3460 60%, #144272 100%)",
    sidebar_text  = "rgba(170,210,240,.82)",
    sidebar_hover = "rgba(20,130,200,.25)",
    sidebar_active_border = "#60A5FA",
    header_bg     = "linear-gradient(90deg, #144272 0%, #205295 100%)",
    header_border = "#60A5FA",
    logo_bg       = "#0A2647",
    body_bg       = "#F0F7FF",
    card_border   = "#BFDBFE",
    primary       = "#2563EB",
    accent        = "#1D4ED8",
    gold          = "#60A5FA",
    section_title = "#2563EB",
    btn_bg        = "#2563EB"
  ),

  # ── 9. Coucher de Soleil ──
  "Coucher de Soleil" = list(
    sidebar_bg    = "linear-gradient(180deg, #431407 0%, #5C1A0B 60%, #7C2D12 100%)",
    sidebar_text  = "rgba(255,220,180,.82)",
    sidebar_hover = "rgba(251,146,60,.2)",
    sidebar_active_border = "#FB923C",
    header_bg     = "linear-gradient(90deg, #7C2D12 0%, #B45309 100%)",
    header_border = "#FB923C",
    logo_bg       = "#431407",
    body_bg       = "#FFFBF5",
    card_border   = "#FED7AA",
    primary       = "#EA580C",
    accent        = "#B45309",
    gold          = "#FB923C",
    section_title = "#EA580C",
    btn_bg        = "#EA580C"
  ),

  # ── 10. Gris Élégant ──
  "Gris \u00c9l\u00e9gant" = list(
    sidebar_bg    = "linear-gradient(180deg, #1F2937 0%, #2D3748 60%, #374151 100%)",
    sidebar_text  = "rgba(200,210,220,.82)",
    sidebar_hover = "rgba(107,114,128,.25)",
    sidebar_active_border = "#9CA3AF",
    header_bg     = "linear-gradient(90deg, #374151 0%, #4B5563 100%)",
    header_border = "#9CA3AF",
    logo_bg       = "#1F2937",
    body_bg       = "#F9FAFB",
    card_border   = "#D1D5DB",
    primary       = "#4B5563",
    accent        = "#6B7280",
    gold          = "#9CA3AF",
    section_title = "#374151",
    btn_bg        = "#4B5563"
  )
)

UI_THEME_CHOICES <- names(UI_THEMES)

# Génère le CSS complet pour un thème d'interface donné
build_theme_css <- function(theme_name) {
  t <- UI_THEMES[[theme_name]]
  if (is.null(t)) t <- UI_THEMES[["OMS Classique"]]

  is_dark <- theme_name == "Nuit Profonde"
  text_col  <- if (is_dark) "#E2E8F0" else "#1e293b"
  muted_col <- if (is_dark) "#94A3B8" else "#64748b"
  card_bg   <- if (is_dark) "#1E1E36" else "#FFFFFF"
  box_bg    <- if (is_dark) "#252545" else "#FFFFFF"
  input_bg  <- if (is_dark) "#2D2D50" else "#FFFFFF"
  input_brd <- if (is_dark) "#3D3D60" else "#D1D5DB"
  table_stripe <- if (is_dark) "rgba(255,255,255,.03)" else "rgba(0,0,0,.02)"

  sprintf("
    /* ══ THÈME DYNAMIQUE : %s ══ */

    /* ── Fond global ── */
    .content-wrapper, .right-side { background: %s !important; }

    /* ── Sidebar ── */
    .main-sidebar, .left-side {
      background: %s !important;
    }
    .sidebar-menu > li > a {
      color: %s !important;
    }
    .sidebar-menu > li.active > a,
    .sidebar-menu > li > a:hover {
      background: %s !important;
      color: #FFFFFF !important;
      border-left-color: %s !important;
    }

    /* ── Header ── */
    .main-header .navbar {
      background: %s !important;
      border-bottom-color: %s !important;
    }
    .main-header .logo {
      background: %s !important;
    }
    .main-header .logo:hover { filter: brightness(1.1); }

    /* ── Boutons OMS ── */
    .btn-oms, .btn-oms:focus {
      background: %s !important;
      border-color: %s !important;
      color: white !important;
    }
    .btn-oms:hover { filter: brightness(1.12); }

    /* ── Titres de section ── */
    .nutri-section-title {
      border-left-color: %s !important;
      color: %s !important;
    }
    .section-header {
      color: %s !important;
      border-bottom-color: %s !important;
    }
    .chart-title { color: %s !important; }

    /* ── Cards / Boxes ── */
    .nutri-chart-card, .nutri-kpi-card {
      background: %s !important;
      border-color: %s !important;
    }
    .box { background: %s !important; border-top-color: %s !important; }
    .box-header { color: %s !important; }

    /* ── Texte global ── */
    .content-wrapper, .content-wrapper p, .content-wrapper span,
    .content-wrapper label, .content-wrapper div {
      color: %s;
    }
    .content-wrapper .text-muted, .sc-lbl { color: %s !important; }

    /* ── Inputs (mode sombre) ── */
    .content-wrapper .form-control,
    .content-wrapper .selectize-input,
    .content-wrapper .selectize-dropdown {
      background: %s !important;
      border-color: %s !important;
      color: %s !important;
    }

    /* ── Tables ── */
    .dataTable tbody tr:nth-child(even) { background: %s !important; }
    .dataTable { color: %s !important; }

    /* ── Badges alerte ── */
    .alerte-box { border-color: %s; }

    /* ── Wizard steps ── */
    .wizard-step { border-color: %s !important; }
    .stat-card { background: %s !important; border-color: %s !important; }
  ",
    theme_name,
    t$body_bg,
    t$sidebar_bg, t$sidebar_text, t$sidebar_hover, t$sidebar_active_border,
    t$header_bg, t$header_border, t$logo_bg,
    t$btn_bg, t$btn_bg,
    t$primary, text_col, t$section_title, t$primary, t$section_title,
    card_bg, t$card_border, box_bg, t$primary, text_col,
    text_col, muted_col,
    input_bg, input_brd, text_col,
    table_stripe, text_col,
    t$card_border, t$card_border,
    card_bg, t$card_border
  )
}

# ── Fonction centrale : obtenir n couleurs selon thème global ──
get_global_pal <- function(n, type = "cat", palette_name = "OMS officiel") {
  if (is.null(palette_name) || palette_name == "") palette_name <- "OMS officiel"

  # Palettes viridis spéciales
  if (palette_name == "Viridis")  return(viridis::viridis(n))
  if (palette_name == "Inferno")  return(viridis::inferno(n))
  if (palette_name == "Plasma")   return(viridis::plasma(n))

  pal <- PALETTES_GLOBAL[[palette_name]]
  if (is.null(pal)) pal <- PALETTES_GLOBAL[["OMS officiel"]]

  base <- switch(type,
    "cat" = pal$cat,
    "div" = pal$div,
    "seq" = pal$seq,
    "oms" = pal_gam,
    pal$seq
  )

  if (is.null(base)) return(viridis::viridis(n))
  if (n <= length(base)) return(base[seq_len(n)])
  colorRampPalette(base)(n)
}

# ── Couleur primaire du thème (pour graphiques à 1 variable) ──
get_primary_color <- function(palette_name = "OMS officiel") {
  switch(palette_name,
    "OMS officiel"   = "#0077B6",
    "UNICEF"         = "#009fda",
    "FAO"            = "#006cb5",
    "PAM / WFP"      = "#0072bc",
    "Viridis"        = "#21918c",
    "Inferno"        = "#bb3754",
    "Plasma"         = "#cc4778",
    "Spectral"       = "#2b83ba",
    "Set2 (Doux)"    = "#66c2a5",
    "Terre (Nature)" = "#606c38",
    "#0077B6"
  )
}

get_secondary_color <- function(palette_name = "OMS officiel") {
  switch(palette_name,
    "OMS officiel"   = "#BF3636",
    "UNICEF"         = "#e2231a",
    "FAO"            = "#d84315",
    "PAM / WFP"      = "#e74c3c",
    "Viridis"        = "#fde725",
    "Inferno"        = "#fcffa4",
    "Plasma"         = "#f0f921",
    "Spectral"       = "#d7191c",
    "Set2 (Doux)"    = "#fc8d62",
    "Terre (Nature)" = "#bc6c25",
    "#BF3636"
  )
}

# ── Rétro-compatibilité : get_pal() utilise le thème par défaut ──
get_pal <- function(n, type = "seq", palette_name = "OMS officiel") {
  get_global_pal(n, type, palette_name)
}

# ── Rétro-compatibilité : pal_fn_uni() (univarié) ──
pal_fn_uni <- function(n, nom) {
  # Si le nom correspond à un thème global, utiliser get_global_pal
  if (nom %in% PALETTE_CHOICES) return(get_global_pal(n, "cat", nom))
  # Aliases historiques
  switch(nom,
    "Gradient bleu/rouge" = colorRampPalette(c(C$bleu, "white", C$rouge))(n),
    "RdYlGn"              = colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))(n),
    get_global_pal(n, "cat", "OMS officiel")
  )
}

# ── PLOTLY CONFIG STANDARD — modebar + download + stats ───────────────────
# Appliqué à TOUS les plotly de l'app via |> nutri_config()
nutri_config <- function(p) {
  if (!inherits(p, "plotly")) return(p)

  # Extraire les données sous-jacentes pour calculer les stats
  # et les afficher comme annotation toggle
  tryCatch({
    p |> plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToAdd = list(
        list(
          name   = "Moyenne \u00b1 \u00c9T",
          icon   = list(
            width  = 1000, height = 1000, path = "M500 100 L500 900 M200 500 L800 500",
            transform = "matrix(1 0 0 1 0 0)"
          ),
          click  = htmlwidgets::JS("
            function(gd) {
              // Calcule et affiche mean +/- SD pour chaque trace numérique
              var dominated = gd.data;
              var msgs = [];
              for (var i = 0; i < dominated.length; i++) {
                var tr = dominated[i];
                var vals = tr.y || tr.x || [];
                if (typeof vals[0] !== 'number') continue;
                var clean = vals.filter(function(v) { return v !== null && !isNaN(v); });
                if (clean.length === 0) continue;
                var n = clean.length;
                var mean = clean.reduce(function(a,b){return a+b;},0) / n;
                var sd = Math.sqrt(clean.reduce(function(a,b){return a + (b-mean)*(b-mean);},0) / (n-1));
                var name = tr.name || ('S\\u00e9rie ' + (i+1));
                msgs.push(name + ' : \\u03bc=' + mean.toFixed(2) + ' \\u00b1 ' + sd.toFixed(2) + ' (n=' + n + ')');
              }
              if (msgs.length > 0) {
                Plotly.relayout(gd, {
                  'annotations[0]': {
                    text: msgs.join('<br>'),
                    showarrow: false,
                    x: 0.5, xref: 'paper', xanchor: 'center',
                    y: 1.12, yref: 'paper', yanchor: 'bottom',
                    font: {size: 11, color: '#334155'},
                    bgcolor: 'rgba(255,255,255,0.92)',
                    bordercolor: '#cbd5e1', borderwidth: 1, borderpad: 6
                  }
                });
              } else {
                alert('Pas de données numériques pour calculer les statistiques.');
              }
            }
          ")
        )
      ),
      toImageButtonOptions = list(
        format  = "png",
        filename = paste0("NutriAnalyse_", format(Sys.Date(), "%Y%m%d")),
        width   = 1200, height = 700, scale = 2
      ),
      locale = "fr",
      modeBarButtonsToRemove = c("select2d", "lasso2d"),
      displaylogo = FALSE
    )
  }, error = function(e) p)
}

# ── GRAPHIQUES PROFESSIONNELS — utilitaires ─────────────────────────────────

# Thème cohérent pour tous les graphiques de l'app
theme_nutri <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      text             = element_text(color = "#334155"),
      plot.title       = element_text(size = base_size + 2, face = "bold",
                                      color = "#1e293b", margin = margin(b = 8)),
      plot.subtitle    = element_text(size = base_size - 1, color = "#64748b",
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = base_size - 2, color = "#94a3b8",
                                      hjust = 0),
      axis.title       = element_text(size = base_size - 0.5, color = "#475569",
                                      face = "plain"),
      axis.text        = element_text(size = base_size - 1, color = "#64748b"),
      legend.text      = element_text(size = base_size - 1),
      legend.title     = element_text(size = base_size, face = "bold"),
      panel.grid.major = element_line(color = "#f1f5f9", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = base_size, face = "bold",
                                      color = "#334155")
    )
}

# Tronque les labels trop longs
truncate_label <- function(x, max_chars = 22) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 1), "\u2026"), x)
}

# Retourne les éléments theme() adaptés au nombre de modalités
smart_axis_x <- function(n_levels, max_chars = 22) {
  if (n_levels <= 8) {
    theme(axis.text.x = element_text(
      size = 10, color = "#64748b"))
  } else if (n_levels <= 15) {
    theme(axis.text.x = element_text(
      angle = 45, hjust = 1, size = 9, color = "#64748b"))
  } else {
    theme(axis.text.x = element_text(
      angle = 70, hjust = 1, size = 8, color = "#64748b"))
  }
}

# Regroupe les catégories au-delà du top N en "Autres"
smart_top_n <- function(cnt, val_col, n_col = "n", max_n = 25) {
  if (nrow(cnt) <= max_n) return(cnt)
  cnt <- cnt[order(-cnt[[n_col]]), ]
  top   <- cnt[seq_len(max_n), ]
  reste <- cnt[seq(max_n + 1, nrow(cnt)), ]
  autres <- top[1, , drop = FALSE]
  autres[[val_col]] <- "Autres"
  autres[[n_col]]   <- sum(reste[[n_col]])
  if ("pct" %in% names(autres))
    autres[["pct"]] <- round(autres[[n_col]] / sum(cnt[[n_col]]) * 100, 1)
  rbind(top, autres)
}

# ── IMPORT MULTI-FORMAT ──────────────────────────────────────────────────────
import_data <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  tryCatch({
    df <- switch(ext,
      "csv" = , "tsv" = {
        raw <- readLines(path, n=3)
        sep <- if (sum(grepl(";", raw)) > sum(grepl(",", raw))) ";" else ","
        dec <- if (sum(grepl(",", gsub("[^,;]","",raw))) > 0 && sep==";") "," else "."
        read.csv(path, sep=sep, dec=dec, stringsAsFactors=FALSE, encoding="UTF-8")
      },
      "xlsx" = , "xls" = as.data.frame(readxl::read_excel(path)),
      "sav"  = as.data.frame(haven::read_sav(path)),
      "dta"  = as.data.frame(haven::read_dta(path)),
      "parquet" = {
        if (!requireNamespace("arrow", quietly = TRUE))
          stop("Le package 'arrow' est requis pour lire les fichiers .parquet. Installez-le avec install.packages('arrow').")
        as.data.frame(arrow::read_parquet(path))
      },
      stop("Format non supporté : ", ext)
    )
    # Convertir labeled → factor (SPSS/Stata)
    df <- as.data.frame(lapply(df, function(x) {
      if (haven::is.labelled(x)) haven::as_factor(x) else x
    }))
    names(df) <- tolower(gsub("\\s+","_", names(df)))
    df
  }, error=function(e) { list(error=e$message) })
}

# ── RAPPORT AUTOMATIQUE ────────────────────────────────────────────────────
generate_report_rmd <- function(df, m, prev_data, output_format="html_document") {
  rmd_content <- paste0(
'---
title: "Rapport Nutritionnel Automatique"
subtitle: "Généré par NutriAnalyse Pro v2"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  ', output_format, ':
    toc: true
    number_sections: true
lang: fr
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(tidyverse); library(knitr); library(kableExtra)
```

# Résumé exécutif

Ce rapport présente les indicateurs nutritionnels calculés selon les normes **OMS 2006**, **PAM** et **UNICEF**.

**Échantillon analysé :** ', nrow(df), ' observations · ', ncol(df), ' variables

## Indicateurs clés

```{r}
if("GAM" %in% names(df)){
  gam_p <- round(mean(df$GAM, na.rm=TRUE)*100, 1)
  sam_p <- round(mean(df$SAM, na.rm=TRUE)*100, 1)
  tibble(
    Indicateur=c("GAM","SAM","MAM","Stunting","Insuff. pondérale"),
    `Prévalence (%)`=c(
      round(mean(df$GAM,na.rm=T)*100,1),
      round(mean(df$SAM,na.rm=T)*100,1),
      round(mean(df$MAM,na.rm=T)*100,1),
      if("Stunting"%in%names(df)) round(mean(df$Stunting,na.rm=T)*100,1) else NA,
      if("Underweight"%in%names(df)) round(mean(df$Underweight,na.rm=T)*100,1) else NA
    ),
    `Seuil OMS`=c("< 5% acceptable","< 1% critique","< 3% acceptable","< 20% acceptable","< 10% acceptable"),
    Sévérité=c(
      if(gam_p<5)"🟢 Acceptable" else if(gam_p<10)"🟡 Alerte" else if(gam_p<15)"🟠 Sérieux" else "🔴 Critique",
      if(sam_p<1)"🟢 Acceptable" else if(sam_p<2)"🟡 Alerte" else "🔴 Critique",
      "Voir normes PAM","-","-"
    )
  ) |> kable(caption="Indicateurs de malnutrition — Normes OMS 2006") |> kable_styling()
}
```

# Méthodologie

Les indicateurs de malnutrition ont été calculés selon les **Standards de Croissance OMS 2006** :

- **GAM** (Global Acute Malnutrition) : WHZ < -2 DS ou œdèmes bilatéraux
- **SAM** (Severe Acute Malnutrition) : WHZ < -3 DS ou œdèmes bilatéraux  
- **MAM** (Moderate Acute Malnutrition) : -3 ≤ WHZ < -2 DS sans œdèmes
- **Stunting** (retard de croissance) : HAZ < -2 DS
- **Underweight** (insuffisance pondérale) : WAZ < -2 DS

Les intervalles de confiance sont calculés par la **méthode Wilson** (méthode recommandée OMS/SMART).

# Statistiques descriptives

```{r}
vars_num <- names(df)[sapply(df, is.numeric)]
vars_num <- vars_num[!startsWith(vars_num, ".")]
if(length(vars_num) > 0) {
  df[vars_num] |> 
    tidyr::pivot_longer(everything()) |>
    group_by(name) |>
    summarise(N=sum(!is.na(value)), Moy=round(mean(value,na.rm=T),2),
              SD=round(sd(value,na.rm=T),2), Min=round(min(value,na.rm=T),2),
              Max=round(max(value,na.rm=T),2)) |>
    kable(caption="Statistiques descriptives — Variables continues") |>
    kable_styling(font_size=11)
}
```

---
*Rapport généré automatiquement par NutriAnalyse Pro v2 · Style OMS/UNICEF*
')
  rmd_content
}

cat("utils.R chargé\n")
