IYCF_INDICATORS <- list(
  ebf = list(
    key = "ebf", short = "EBF",
    label = "Allaitement maternel exclusif (0–5 mois)",
    target_age = c(0, 5), target = 50,
    target_source = "WHA 2025 (résolution 78.7)",
    target_label = "Cible mondiale",
    color = "#2ecc71", type = "protective", composite = TRUE,
    domain = "Allaitement", priority = 1,
    definition = "Nourrissons de 0–5 mois nourris exclusivement au lait maternel, sans eau, liquide ni aliment solide."
  ),
  cbf = list(
    key = "cbf", short = "CBF",
    label = "Allaitement continu (12–23 mois)",
    target_age = c(12, 23), target = 80,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#3498db", type = "protective", composite = TRUE,
    domain = "Allaitement", priority = 1,
    definition = "Enfants de 12–23 mois encore allaités au sein."
  ),
  eibf = list(
    key = "eibf", short = "EIBF",
    label = "Mise au sein précoce (< 1 heure)",
    target_age = c(0, 23), target = 70,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#9b59b6", type = "protective", composite = TRUE,
    domain = "Allaitement", priority = 1,
    definition = "Enfants de 0–23 mois mis au sein dans l'heure suivant la naissance."
  ),
  isf = list(
    key = "isf", short = "ISF",
    label = "Introduction des aliments solides (6–8 mois)",
    target_age = c(6, 8), target = 80,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#f39c12", type = "protective", composite = TRUE,
    domain = "Alimentation", priority = 1,
    definition = "Nourrissons de 6–8 mois ayant reçu des aliments solides, semi-solides ou mous."
  ),
  mdd = list(
    key = "mdd", short = "MDD",
    label = "Diversité alimentaire minimale (6–23 mois)",
    target_age = c(6, 23), target = 50,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#e67e22", type = "protective", composite = TRUE,
    domain = "Alimentation", priority = 1,
    definition = "Enfants de 6–23 mois ayant consommé au moins 5 des 8 groupes alimentaires au cours des 24 dernières heures."
  ),
  mmf = list(
    key = "mmf", short = "MMF",
    label = "Fréquence minimale des repas (6–23 mois)",
    target_age = c(6, 23), target = 70,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#1abc9c", type = "protective", composite = TRUE,
    domain = "Alimentation", priority = 1,
    definition = "Enfants de 6–23 mois ayant reçu le nombre minimal de repas : 2 repas à 6–8 mois, 3 repas à 9–23 mois."
  ),
  mad = list(
    key = "mad", short = "MAD",
    label = "Régime minimum acceptable (6–23 mois)",
    target_age = c(6, 23), target = 30,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#e74c3c", type = "protective", composite = TRUE,
    domain = "Alimentation", priority = 1,
    definition = "Enfants de 6–23 mois ayant à la fois la diversité alimentaire minimale et la fréquence minimale des repas."
  ),
  iron = list(
    key = "iron", short = "Fer",
    label = "Aliments riches en fer (6–23 mois)",
    target_age = c(6, 23), target = 50,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#c0392b", type = "protective", composite = TRUE,
    domain = "Alimentation", priority = 2,
    definition = "Enfants de 6–23 mois ayant consommé au moins un aliment riche en fer au cours des 24 dernières heures."
  ),
  vita = list(
    key = "vita", short = "Vit A",
    label = "Supplémentation en vitamine A (6–59 mois)",
    target_age = c(6, 59), target = 80,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#D4A843", type = "protective", composite = TRUE,
    domain = "Contexte", priority = 2,
    definition = "Enfants de 6–59 mois ayant reçu une dose de vitamine A dans les 6 derniers mois."
  ),
  anc4 = list(
    key = "anc4", short = "ANC4+",
    label = "ANC 4+ visites prénatales (contexte maternel)",
    target_age = c(0, 59), target = 80,
    target_source = "Benchmark programmatique",
    target_label = "Cible de référence",
    color = "#8E44AD", type = "protective", composite = FALSE,
    domain = "Contexte", priority = 3,
    definition = "Mères ayant effectué au moins 4 consultations prénatales ; indicateur de contexte maternel."
  ),
  bottle = list(
    key = "bottle", short = "Biberon",
    label = "Alimentation au biberon (0–23 mois)",
    target_age = c(0, 23), target = NA_real_,
    target_source = "Aucune cible normative",
    target_label = "Pratique à surveiller",
    color = "#95a5a6", type = "risk", composite = FALSE,
    domain = "Risques", priority = 2,
    definition = "Enfants de 0–23 mois nourris au biberon ; pratique à surveiller selon le contexte."
  ),
  sweet = list(
    key = "sweet", short = "Sucrées",
    label = "Boissons sucrées (6–23 mois)",
    target_age = c(6, 23), target = NA_real_,
    target_source = "Aucune cible normative",
    target_label = "Pratique à surveiller",
    color = "#e74c3c", type = "risk", composite = FALSE,
    domain = "Risques", priority = 2,
    definition = "Enfants de 6–23 mois ayant consommé une boisson sucrée au cours des 24 dernières heures."
  ),
  upt = list(
    key = "upt", short = "Ultra-transformés",
    label = "Aliments ultra-transformés (6–23 mois)",
    target_age = c(6, 23), target = NA_real_,
    target_source = "Aucune cible normative",
    target_label = "Pratique à surveiller",
    color = "#c0392b", type = "risk", composite = FALSE,
    domain = "Risques", priority = 2,
    definition = "Enfants de 6–23 mois ayant consommé un aliment ultra-transformé au cours des 24 dernières heures."
  ),
  zvf = list(
    key = "zvf", short = "Zéro LF",
    label = "Zéro légumes/fruits (6–23 mois)",
    target_age = c(6, 23), target = NA_real_,
    target_source = "Aucune cible normative",
    target_label = "Pratique à surveiller",
    color = "#8e44ad", type = "risk", composite = FALSE,
    domain = "Risques", priority = 2,
    definition = "Enfants de 6–23 mois n'ayant consommé aucun légume ni fruit au cours des 24 dernières heures."
  )
)

IYCF_INDICATOR_ORDER <- names(IYCF_INDICATORS)

IYCF_CLASSES <- data.frame(
  seuil = c(85, 70, 55, -Inf),
  label = c("Sur la bonne voie", "Proche des cibles", "Sous les cibles", "Écart critique"),
  color = c("#27AE60", "#0E7C86", "#E67E22", "#C80000"),
  bg = c("#eafaf1", "#e6f4f5", "#fff3e0", "#fdf0f0"),
  stringsAsFactors = FALSE
)

IYCF_HAS_SURVEY <- requireNamespace("survey", quietly = TRUE)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

iycf_norm <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  x <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

iycf_missing <- function(x) {
  if (is.null(x)) return(rep(TRUE, 0))
  if (is.numeric(x) || is.logical(x)) return(is.na(x))
  xs <- trimws(as.character(x))
  is.na(x) | !nzchar(xs) | toupper(xs) %in% c("NA", "N/A", "NULL", ".", "NAN")
}

iycf_binary <- function(x) {
  n <- length(x)
  if (is.logical(x)) {
    return(list(pos = !is.na(x) & x, known = !is.na(x), type = "logical",
                n_known = sum(!is.na(x)), n_pos = sum(!is.na(x) & x),
                values = c(FALSE, TRUE)))
  }
  if (is.numeric(x) || is.integer(x)) {
    u <- unique(x[!is.na(x)])
    if (length(u) == 0) {
      return(list(pos = rep(FALSE, n), known = rep(FALSE, n), type = "numeric",
                  n_known = 0L, n_pos = 0L, values = u))
    }
    if (all(u %in% c(0, 1))) {
      return(list(pos = !is.na(x) & x == 1, known = !is.na(x), type = "numeric",
                  n_known = sum(!is.na(x)), n_pos = sum(!is.na(x) & x == 1), values = u))
    }
    if (all(u %in% c(1, 2))) {
      return(list(pos = !is.na(x) & x == 1, known = !is.na(x), type = "numeric",
                  n_known = sum(!is.na(x)), n_pos = sum(!is.na(x) & x == 1), values = u))
    }
    return(list(pos = rep(FALSE, n), known = rep(FALSE, n), type = "numeric",
                n_known = 0L, n_pos = 0L, values = u))
  }
  xs <- iycf_norm(x)
  miss <- iycf_missing(x)
  pos_set <- c("oui", "yes", "1", "true", "vrai", "y", "positif", "positive",
               "atteint", "present", "exclusif", "exclusive", "ebf", "cbf",
               "continu", "continued", "current", "actuel", "en_cours", "still_breastfeeding")
  neg_set <- c("non", "no", "0", "false", "faux", "n", "negatif", "negative",
               "absent", "aucun", "none", "weaned", "sevrage", "arrete",
               "artificiel", "formule", "formula")
  known <- !miss & (xs %in% pos_set | xs %in% neg_set)
  list(pos = known & xs %in% pos_set, known = known, type = "character",
       n_known = sum(known), n_pos = sum(known & xs %in% pos_set),
       values = sort(unique(xs[known])))
}

iycf_count <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  v <- suppressWarnings(as.numeric(as.character(x)))
  known <- is.finite(v) & v >= 0
  list(value = v, known = known, n_known = sum(known), values = sort(unique(v[known])))
}

iycf_match_col <- function(d, candidates, exclude = character()) {
  nms <- names(d)
  if (length(nms) == 0 || length(candidates) == 0) return(NULL)
  nn <- iycf_norm(nms)
  cc <- iycf_norm(candidates)
  avail <- !(nms %in% exclude)
  for (c in cc) {
    idx <- which(nn == c & avail)
    if (length(idx) > 0) return(nms[idx[1]])
  }
  for (c in cc) {
    key <- gsub("_", "", c)
    idx <- which(gsub("_", "", nn) == key & avail)
    if (length(idx) > 0) return(nms[idx[1]])
  }
  for (c in cc) {
    if (nchar(c) >= 4) {
      idx <- which(grepl(c, nn, fixed = TRUE) & avail)
      if (length(idx) > 0) return(nms[idx[1]])
    }
  }
  NULL
}

IYCF_DIRECT_CANDIDATES <- list(
  ebf = c("allaitement_excl", "allaitement_exclusif", "allaitement_maternel_exclusif",
          "exclusive_breastfeeding", "excl_breastfeeding", "breastfeeding_exclusive",
          "ebf", "exclusive_bf", "sein_exclusif"),
  cbf = c("allaitement_cont", "allaitement_continu", "continued_breastfeeding",
          "cbf", "still_breastfeeding", "allaitement_actuel", "allaitement_sein",
          "breastfed_currently", "breastfeeding_current"),
  eibf = c("initiation_allaitement", "initiation_precoce", "early_initiation_bf",
           "first_hour_bf", "mise_au_sein_precoce", "eibf", "early_bf",
           "allaitement_premiere_heure"),
  isf = c("intro_aliments_solides", "introduction_aliments", "introduction_solid_foods",
          "intro_complementary", "isf", "solid_food_intro", "debut_alimentation_comp"),
  mdd = c("diversite_alim_min", "diversite_min", "minimum_dietary_diversity",
          "mdd", "min_dietary_diversity", "child_dds", "iycf_mdd",
          "diversite_alimentaire_min"),
  mmf = c("freq_repas_min", "frequence_min", "minimum_meal_frequency",
          "mmf", "meal_frequency", "min_meal_freq", "iycf_mmf",
          "frequence_repas_minimum"),
  mad = c("regime_acceptable_min", "regime_min_acceptable", "minimum_acceptable_diet",
          "mad", "min_acceptable_diet", "iycf_mad", "acceptable_diet"),
  iron = c("cons_fer", "aliments_riches_fer", "iron_rich_foods", "iron_consumption",
           "iron_food", "repas_fer_enrichi", "apport_fer"),
  bottle = c("bouteille", "alimentation_biberon", "bottle_feeding", "bottle_use",
             "biberon", "bottle_fed", "bottle"),
  sweet = c("boissons_sucrees", "sugar_sweet_beverages", "sweetened_beverages",
            "sugary_drinks", "sweet_drinks"),
  upt = c("aliments_ultra_transformes", "ultra_processed_foods",
          "ultraprocessed_food", "ultra_processed"),
  zvf = c("zero_legumes_fruits", "zero_veg_fruits", "no_fruit_vegetables",
          "zero_vegetables_fruits", "no_veg_fruit"),
  vita = c("vita", "vitamine_a", "vitamin_a", "vitamin_a_supplementation",
           "vit_a", "vitamin_a_supp"),
  anc4 = c("anc4", "anc_4plus", "visites_cpn", "anc_visits", "cpn_visits",
           "nb_cpn", "anc4plus")
)

IYCF_AUX_CANDIDATES <- list(
  age = c("age_m", "age_months", "child_age_months", "age_mois", "age_en_mois",
          "age_child_months", "age_in_months", "agem", "age"),
  bf_status = c("breastfeeding_status", "statut_allaitement", "allaitement_statut",
                "bf_status", "statut_alimentation"),
  bf_current = c("allaitement_actuel", "allaitement_sein", "breastfed_currently",
                 "currently_breastfed", "still_breastfeeding", "allaitement_cont",
                 "allaitement_continu", "allaitement_en_cours"),
  first_hour = c("heure_mise_au_sein", "delai_mise_au_sein", "hours_to_bf",
                 "time_to_bf", "first_hour", "first_hour_bf", "initiation_allaitement"),
  age_intro = c("age_introduction", "age_intro_aliments", "age_intro_solides",
                "age_debut_aliments", "introduction_age", "age_intro"),
  n_groups = c("nb_groupes_alim_enfant", "groupes_alim_enfant", "child_food_groups",
               "nb_groupes", "n_groups", "child_diet_groups", "nb_groupes_alimentaires"),
  meals = c("nb_repas", "repas_solides_semi", "meal_count", "meals_per_day",
            "nb_repas_jour", "repas_par_jour", "nombre_repas"),
  sex = c("sexe", "sex", "child_sex", "sexe_enfant"),
  round = c("survey_round", "round", "vague", "vague_enquete", "wave", "passage")
)

IYCF_DESIGN_CANDIDATES <- list(
  cluster = c("cluster", "grappe", "psu", "cluster_id", "primary_sampling_unit",
              "unite_echantillonnage", "grappe_id"),
  strata = c("strate", "strata", "stratum", "sampling_strata",
             "strate_echantillonnage"),
  weights = c("poids_echantillon", "sampling_weight", "survey_weight",
              "design_weight", "weight_sample", "ponderation", "poids_pondere",
              "weights")
)

iycf_meta <- function(key) IYCF_INDICATORS[[key]]

iycf_eligible <- function(age, key) {
  rng <- IYCF_INDICATORS[[key]]$target_age
  !is.na(age) & age >= rng[1] & age <= rng[2]
}

iycf_detect_design <- function(d, overrides = list(), use_design = TRUE) {
  cluster <- overrides$cluster
  strata <- overrides$strata
  weights <- overrides$weights
  if (is.null(cluster) || !cluster %in% names(d))
    cluster <- iycf_match_col(d, IYCF_DESIGN_CANDIDATES$cluster)
  if (is.null(strata) || !strata %in% names(d))
    strata <- iycf_match_col(d, IYCF_DESIGN_CANDIDATES$strata)
  if (is.null(weights) || !weights %in% names(d))
    weights <- iycf_match_col(d, IYCF_DESIGN_CANDIDATES$weights)
  enabled <- isTRUE(use_design) && IYCF_HAS_SURVEY &&
    (!is.null(cluster) || !is.null(strata) || !is.null(weights))
  list(enabled = enabled, cluster = cluster, strata = strata, weights = weights,
       survey_available = IYCF_HAS_SURVEY)
}

iycf_detect_columns <- function(d, overrides = list(), use_design = TRUE) {
  d <- as.data.frame(d)
  age_col <- overrides$age
  if (is.null(age_col) || !age_col %in% names(d))
    age_col <- iycf_match_col(d, IYCF_AUX_CANDIDATES$age)
  aux <- list(age = age_col)
  for (nm in names(IYCF_AUX_CANDIDATES)) {
    if (nm == "age") next
    value <- overrides[[nm]]
    if (is.null(value) || !value %in% names(d))
      value <- iycf_match_col(d, IYCF_AUX_CANDIDATES[[nm]])
    aux[[nm]] <- value
  }
  design <- iycf_detect_design(d, overrides, use_design)
  columns <- list()
  used <- character()
  for (key in IYCF_INDICATOR_ORDER) {
    col <- overrides[[key]]
    if (is.null(col) || !col %in% names(d))
      col <- iycf_match_col(d, IYCF_DIRECT_CANDIDATES[[key]], exclude = used)
    if (!is.null(col) && col %in% names(d)) {
      if (key == "anc4") {
        info <- iycf_count(d[[col]])
        valid <- info$n_known > 0
      } else {
        info <- iycf_binary(d[[col]])
        valid <- info$n_known > 0
      }
      if (valid) {
        columns[[key]] <- col
        used <- c(used, col)
      }
    }
  }
  list(columns = columns, aux = aux, design = design,
       data_n = nrow(d), data_names = names(d))
}

iycf_resolve_indicators <- function(d, det) {
  age <- suppressWarnings(as.numeric(d[[det$aux$age]]))
  resolved <- list()
  for (key in IYCF_INDICATOR_ORDER) {
    col <- det$columns[[key]]
    if (is.null(col) || !col %in% names(d)) next
    if (key == "anc4") {
      bin <- iycf_binary(d[[col]])
      if (bin$n_known > 0 && all(bin$values %in% c(0, 1, FALSE, TRUE))) {
        pos <- bin$pos
        valid <- bin$known
      } else {
        cnt <- iycf_count(d[[col]])
        pos <- cnt$known & cnt$value >= 4
        valid <- cnt$known
      }
    } else {
      bin <- iycf_binary(d[[col]])
      pos <- bin$pos
      valid <- bin$known
    }
    if (sum(valid, na.rm = TRUE) == 0) next
    resolved[[key]] <- list(pos = pos, valid = valid, source = col,
                            method = "Colonne dédiée", status = "Direct",
                            confidence = "Élevée", note = "")
  }

  derive <- function(key, pos, valid, method, source, note = "") {
    if (is.null(resolved[[key]]) && sum(valid, na.rm = TRUE) > 0) {
      resolved[[key]] <<- list(pos = pos, valid = valid, source = source,
                               method = method, status = "Dérivé",
                               confidence = "Moyenne", note = note)
    }
  }

  if (is.null(resolved$ebf) && !is.null(det$aux$bf_status) && det$aux$bf_status %in% names(d)) {
    xs <- iycf_norm(d[[det$aux$bf_status]])
    known <- !iycf_missing(d[[det$aux$bf_status]])
    pos <- known & (grepl("exclus", xs) | xs %in% c("ebf", "exclusive_breastfeeding"))
    derive("ebf", pos, known, "Dérivation depuis le statut d'allaitement",
           det$aux$bf_status, "La catégorie exclusive est convertie en indicateur EBF.")
  }

  if (is.null(resolved$cbf)) {
    if (!is.null(det$aux$bf_current) && det$aux$bf_current %in% names(d)) {
      bin <- iycf_binary(d[[det$aux$bf_current]])
      if (bin$n_known > 0) {
        derive("cbf", bin$pos, bin$known, "Dérivation depuis l'allaitement actuel",
               det$aux$bf_current, "L'allaitement actuel est utilisé comme proxy du CBF.")
      }
    }
    if (is.null(resolved$cbf) && !is.null(det$aux$bf_status) && det$aux$bf_status %in% names(d)) {
      xs <- iycf_norm(d[[det$aux$bf_status]])
      known <- !iycf_missing(d[[det$aux$bf_status]])
      neg <- grepl("aucun|none|non|weaned|sevrage|arrete|artificiel|formula", xs)
      pos <- known & !neg & grepl("allait|sein|breast|continu|exclus|mixte|maternel", xs)
      derive("cbf", pos, known, "Dérivation depuis le statut d'allaitement",
             det$aux$bf_status, "Les statuts d'allaitement actif sont considérés comme CBF.")
    }
  }

  if (is.null(resolved$eibf) && !is.null(det$aux$first_hour) && det$aux$first_hour %in% names(d)) {
    x <- d[[det$aux$first_hour]]
    bin <- iycf_binary(x)
    if (bin$n_known > 0) {
      derive("eibf", bin$pos, bin$known, "Dérivation depuis la mise au sein précoce",
             det$aux$first_hour)
    } else {
      cnt <- iycf_count(x)
      derive("eibf", cnt$known & cnt$value <= 1, cnt$known,
             "Dérivation depuis le délai de mise au sein",
             det$aux$first_hour, "Délai ≤ 1 heure converti en mise au sein précoce.")
    }
  }

  if (is.null(resolved$isf) && !is.null(det$aux$age_intro) && det$aux$age_intro %in% names(d)) {
    cnt <- iycf_count(d[[det$aux$age_intro]])
    valid <- cnt$known & !is.na(age) & age >= 6 & age <= 8
    pos <- valid & cnt$value <= age
    derive("isf", pos, valid, "Dérivation depuis l'âge d'introduction",
           det$aux$age_intro, "Introduction d'aliments solides déduite de l'âge déclaré.")
  }

  if (is.null(resolved$mdd) && !is.null(det$aux$n_groups) && det$aux$n_groups %in% names(d)) {
    cnt <- iycf_count(d[[det$aux$n_groups]])
    valid <- cnt$known & !is.na(age) & age >= 6 & age <= 23
    derive("mdd", valid & cnt$value >= 5, valid,
           "Dérivation depuis le nombre de groupes alimentaires",
           det$aux$n_groups, "MDD = au moins 5 groupes alimentaires sur 8.")
  }

  if (is.null(resolved$mmf) && !is.null(det$aux$meals) && det$aux$meals %in% names(d)) {
    cnt <- iycf_count(d[[det$aux$meals]])
    valid <- cnt$known & !is.na(age) & age >= 6 & age <= 23
    seuil <- ifelse(age <= 8, 2, 3)
    derive("mmf", valid & cnt$value >= seuil, valid,
           "Dérivation depuis le nombre de repas",
           det$aux$meals, "Seuil de 2 repas à 6–8 mois et 3 repas à 9–23 mois.")
  }

  if (is.null(resolved$mad) && !is.null(resolved$mdd) && !is.null(resolved$mmf)) {
    valid <- resolved$mdd$valid & resolved$mmf$valid
    pos <- valid & resolved$mdd$pos & resolved$mmf$pos
    derive("mad", pos, valid, "Dérivation depuis MDD et MMF",
           paste(c(resolved$mdd$source, resolved$mmf$source), collapse = " + "),
           "MAD = MDD et MMF simultanément atteints.")
  }

  n_elig <- vapply(IYCF_INDICATOR_ORDER, function(k) {
    sum(iycf_eligible(age, k), na.rm = TRUE)
  }, integer(1))

  mapping <- do.call(rbind, lapply(IYCF_INDICATOR_ORDER, function(key) {
    meta <- iycf_meta(key)
    r <- resolved[[key]]
    if (is.null(r)) {
      data.frame(
        key = key, Sigle = meta$short, Indicateur = meta$label,
        Domaine = meta$domain, Type = meta$type,
        Statut = "Non détecté", Colonne = "", Methode = "",
        Source = "", Fiabilite = "—",
        N_connu = 0L, N_eligible = n_elig[[key]], Evaluable = FALSE,
        Note = "Aucune colonne ou combinaison de colonnes exploitable.",
        stringsAsFactors = FALSE
      )
    } else {
      note <- r$note
      evaluable <- sum(r$valid, na.rm = TRUE) > 0 && n_elig[[key]] > 0
      if (n_elig[[key]] == 0) {
        note <- paste(c(note, "Aucun enfant dans la tranche d'âge cible."), collapse = " ")
      }
      if (sum(r$valid, na.rm = TRUE) == 0) {
        note <- paste(c(note, "Aucune valeur connue dans la colonne source."), collapse = " ")
      }
      data.frame(
        key = key, Sigle = meta$short, Indicateur = meta$label,
        Domaine = meta$domain, Type = meta$type,
        Statut = r$status, Colonne = r$source, Methode = r$method,
        Source = r$source, Fiabilite = r$confidence,
        N_connu = sum(r$valid, na.rm = TRUE),
        N_eligible = n_elig[[key]], Evaluable = evaluable,
        Note = trimws(note), stringsAsFactors = FALSE
      )
    }
  }))
  rownames(mapping) <- NULL
  list(resolved = resolved, mapping = mapping, age = age, n_elig = n_elig)
}

iycf_prepare_design <- function(d, info) {
  empty <- list(enabled = FALSE, design = NULL, row_id = seq_len(nrow(d)),
                n = nrow(d), n_design = nrow(d), weighted_n = NA_real_,
                mean_w = 1, cluster = info$cluster, strata = info$strata,
                weights = info$weights, method = "Proportion simple (IC Wilson)",
                note = "Plan de sondage non utilisé.")
  if (!isTRUE(info$enabled) || !IYCF_HAS_SURVEY) {
    if (!IYCF_HAS_SURVEY) empty$note <- "Package survey absent : IC de Wilson."
    return(empty)
  }
  n <- nrow(d)
  row_ok <- rep(TRUE, n)
  if (!is.null(info$cluster) && info$cluster %in% names(d))
    row_ok <- row_ok & !iycf_missing(d[[info$cluster]])
  if (!is.null(info$strata) && info$strata %in% names(d))
    row_ok <- row_ok & !iycf_missing(d[[info$strata]])
  if (!is.null(info$weights) && info$weights %in% names(d)) {
    w_raw <- suppressWarnings(as.numeric(as.character(d[[info$weights]])))
    row_ok <- row_ok & is.finite(w_raw) & w_raw > 0
  } else {
    w_raw <- rep(1, n)
  }
  rid <- which(row_ok)
  if (length(rid) < 2) {
    empty$note <- "Effectif insuffisant après nettoyage des variables de sondage."
    return(empty)
  }
  dd <- d[rid, , drop = FALSE]
  if (!is.null(info$cluster) && info$cluster %in% names(dd)) {
    cl <- as.character(dd[[info$cluster]])
    dd$.iycf_cluster <- factor(cl)
  } else {
    dd$.iycf_cluster <- factor(rep("1", nrow(dd)))
  }
  if (!is.null(info$strata) && info$strata %in% names(dd)) {
    dd$.iycf_strata <- factor(as.character(dd[[info$strata]]))
    strata_f <- ~.iycf_strata
  } else {
    strata_f <- NULL
  }
  mean_w <- mean(w_raw[rid])
  dd$.iycf_w <- w_raw[rid] / mean_w
  if (sum(dd$.iycf_w <= 0, na.rm = TRUE) > 0) {
    empty$note <- "Poids de sondage invalides."
    return(empty)
  }
  options(survey.lonely.psu = "adjust")
  des <- tryCatch({
    if (!is.null(strata_f)) {
      survey::svydesign(ids = ~.iycf_cluster, strata = strata_f,
                        weights = ~.iycf_w, data = dd, nest = TRUE)
    } else {
      survey::svydesign(ids = ~.iycf_cluster, weights = ~.iycf_w,
                        data = dd, nest = TRUE)
    }
  }, error = function(e) NULL)
  if (is.null(des)) {
    empty$note <- "Échec de création du design de sondage ; IC de Wilson utilisés."
    return(empty)
  }
  list(enabled = TRUE, design = des, row_id = rid, n = n, n_design = length(rid),
       weighted_n = sum(w_raw[rid]), mean_w = mean_w,
       cluster = info$cluster, strata = info$strata, weights = info$weights,
       method = "Design-based (cluster/strates/poids)",
       note = "Prévalences pondérées et IC 95 % basés sur le plan de sondage.")
}

iycf_estimate <- function(pos, valid, design_info) {
  valid <- !is.na(valid) & valid
  pos <- !is.na(pos) & pos
  n_all <- sum(valid, na.rm = TRUE)
  k_all <- sum(valid & pos, na.rm = TRUE)
  if (n_all == 0) {
    return(list(pct = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
                se = NA_real_, deff = NA_real_, n = 0L, n_pos = 0L,
                n_weighted = NA_real_, method = "Non évaluable",
                ci_method = "—", note = "Aucune observation valide."))
  }
  if (isTRUE(design_info$enabled) && !is.null(design_info$design)) {
    rid <- design_info$row_id
    des <- design_info$design
    des$variables$.iycf_pos <- as.integer(pos[rid])
    des$variables$.iycf_valid <- as.integer(valid[rid])
    sub <- tryCatch(subset(des, .iycf_valid == 1), error = function(e) NULL)
    if (!is.null(sub) && nrow(sub$variables) >= 2) {
      m <- tryCatch(survey::svymean(~.iycf_pos, sub, na.rm = TRUE),
                    error = function(e) NULL)
      if (!is.null(m)) {
        p <- as.numeric(stats::coef(m))[1]
        se <- as.numeric(survey::SE(m))[1]
        ci <- tryCatch({
          z <- survey::svyciprop(~.iycf_pos, sub, method = "logit")
          as.numeric(stats::confint(z))
        }, error = function(e) NULL)
        if (is.null(ci) || length(ci) < 2 || any(!is.finite(ci))) {
          ci <- tryCatch({
            z <- survey::svyciprop(~.iycf_pos, sub, method = "beta")
            as.numeric(stats::confint(z))
          }, error = function(e) NULL)
        }
        if (is.null(ci) || length(ci) < 2 || any(!is.finite(ci))) {
          ci <- c(max(0, p - 1.96 * se), min(1, p + 1.96 * se))
        }
        n <- nrow(sub$variables)
        k <- sum(sub$variables$.iycf_pos, na.rm = TRUE)
        deff <- if (is.finite(se) && se > 0 && p > 0 && p < 1 && n > 0)
          (se^2) / (p * (1 - p) / n) else NA_real_
        return(list(
          pct = round(p * 100, 2), ci_lower = round(max(0, ci[1]) * 100, 1),
          ci_upper = round(min(1, ci[2]) * 100, 1), se = round(se * 100, 2),
          deff = round(deff, 2), n = n, n_pos = as.integer(round(k)),
          n_weighted = round(sum(stats::weights(sub)) * design_info$mean_w, 0),
          method = "Design-based", ci_method = "svyciprop logit",
          note = ""
        ))
      }
    }
  }
  p <- k_all / n_all
  ci <- if (exists("wilson_ci", mode = "function")) {
    wilson_ci(k_all, n_all)
  } else {
    z <- stats::qnorm(0.975)
    denom <- 1 + z^2 / n_all
    centre <- (p + z^2 / (2 * n_all)) / denom
    marge <- z * sqrt((p * (1 - p) + z^2 / (4 * n_all)) / n_all) / denom
    c(max(0, centre - marge) * 100, min(1, centre + marge) * 100)
  }
  list(pct = round(p * 100, 2), ci_lower = round(ci[1], 1),
       ci_upper = round(ci[2], 1), se = round(sqrt(p * (1 - p) / n_all) * 100, 2),
       deff = NA_real_, n = as.integer(n_all), n_pos = as.integer(k_all),
       n_weighted = as.numeric(n_all), method = "Proportion simple",
       ci_method = "Wilson", note = "")
}

calc_iycf_indicators <- function(d, col_age, col_ebf = NULL, col_cbf = NULL,
                                  col_eibf = NULL, col_isf = NULL,
                                  col_mdd = NULL, col_mmf = NULL, col_mad = NULL,
                                  col_iron = NULL, col_bottle = NULL,
                                  col_sweet = NULL, col_upt = NULL, col_zvf = NULL,
                                  col_bfstatus = NULL, col_vita = NULL,
                                  col_anc = NULL, col_cluster = NULL,
                                  col_strata = NULL, col_weights = NULL,
                                  use_design = TRUE) {
  tryCatch({
    d <- as.data.frame(d)
    if (is.null(col_age) || !col_age %in% names(d)) {
      col_age <- iycf_match_col(d, IYCF_AUX_CANDIDATES$age)
      if (is.null(col_age)) stop("Aucune colonne d'âge n'a pu être identifiée.")
    }
    overrides <- list(
      age = col_age, ebf = col_ebf, cbf = col_cbf, eibf = col_eibf,
      isf = col_isf, mdd = col_mdd, mmf = col_mmf, mad = col_mad,
      iron = col_iron, bottle = col_bottle, sweet = col_sweet,
      upt = col_upt, zvf = col_zvf, vita = col_vita, anc4 = col_anc,
      bf_status = col_bfstatus, cluster = col_cluster, strata = col_strata,
      weights = col_weights
    )
    overrides <- overrides[!vapply(overrides, is.null, logical(1))]
    det <- iycf_detect_columns(d, overrides, use_design = use_design)
    if (is.null(det$aux$age)) stop("Colonne d'âge introuvable.")
    rs <- iycf_resolve_indicators(d, det)
    age <- rs$age
    design_info <- iycf_prepare_design(d, det$design)
    results <- list()
    rows <- list()
    for (key in names(rs$resolved)) {
      r <- rs$resolved[[key]]
      meta <- iycf_meta(key)
      elig <- iycf_eligible(age, key)
      valid <- r$valid & elig
      est <- iycf_estimate(r$pos, valid, design_info)
      map_row <- rs$mapping[rs$mapping$key == key, , drop = FALSE]
      results[[key]] <- list(
        key = key, n = est$n, n_pos = est$n_pos, pct = est$pct,
        ci_lower = est$ci_lower, ci_upper = est$ci_upper,
        se = est$se, deff = est$deff, n_weighted = est$n_weighted,
        method = est$method, ci_method = est$ci_method,
        mask = valid, pos = valid & r$pos, eligible = elig,
        source = r$source, status = r$status, mapping_method = r$method,
        note = r$note, design = design_info
      )
      rows[[key]] <- data.frame(
        Indicateur = meta$label, key = key, Sigle = meta$short,
        Domaine = meta$domain, Type = meta$type,
        N = est$n, Positifs = est$n_pos, Prevalence = est$pct,
        IC_inf = est$ci_lower, IC_sup = est$ci_upper, SE = est$se,
        DEFF = est$deff, N_pondere = est$n_weighted,
        N_eligible = sum(elig, na.rm = TRUE),
        Manquants = sum(elig, na.rm = TRUE) - est$n,
        Taux_completude = if (sum(elig, na.rm = TRUE) > 0)
          round(est$n / sum(elig, na.rm = TRUE) * 100, 1) else NA_real_,
        Cible_OMS = meta$target, Cible_reference = meta$target,
        Cible_source = meta$target_source, Cible_label = meta$target_label,
        Methode = est$method, IC_methode = est$ci_method,
        Statut_mapping = if (nrow(map_row) > 0) map_row$Statut[1] else "Non détecté",
        Colonne = if (nrow(map_row) > 0) map_row$Colonne[1] else "",
        Fiabilite = if (nrow(map_row) > 0) map_row$Fiabilite[1] else "—",
        stringsAsFactors = FALSE
      )
    }
    summary_df <- if (length(rows) > 0) {
      out <- do.call(rbind, rows)
      rownames(out) <- NULL
      out
    } else data.frame()
    res <- list(
      results = results, summary = summary_df, n_indicators = length(results),
      n_children = sum(!is.na(age) & age >= 0 & age <= 59),
      n_rows = nrow(d), age_col = det$aux$age, age = age,
      columns = det$columns, aux = det$aux, mapping = rs$mapping,
      design_info = design_info, design = design_info$design,
      data_n = nrow(d), indicator_order = IYCF_INDICATOR_ORDER,
      error = NULL
    )
    res$consistency <- iycf_check_consistency(d, det, rs)
    res$quality <- calc_iycf_quality(d, res)
    res$age_profile <- calc_iycf_age_profile(res, d)
    res
  }, error = function(e) {
    list(results = list(), summary = data.frame(), n_indicators = 0,
         n_children = 0, mapping = data.frame(), error = conditionMessage(e))
  })
}

calc_iycf_gap <- function(iycf_res) {
  if (is.null(iycf_res) || is.null(iycf_res$summary) || nrow(iycf_res$summary) == 0)
    return(data.frame())
  md <- iycf_res$summary
  if (!"Cible_reference" %in% names(md)) md$Cible_reference <- md$Cible_OMS
  if (!"Cible_OMS" %in% names(md)) md$Cible_OMS <- md$Cible_reference
  if (!"Type" %in% names(md))
    md$Type <- vapply(md$key, function(k) iycf_meta(k)$type %||% "protective", character(1))
  if (!"Cible_source" %in% names(md))
    md$Cible_source <- vapply(md$key, function(k) iycf_meta(k)$target_source %||% "", character(1))
  if (!"Domaine" %in% names(md))
    md$Domaine <- vapply(md$key, function(k) iycf_meta(k)$domain %||% "", character(1))
  cible <- ifelse(is.na(md$Cible_reference), md$Cible_OMS, md$Cible_reference)
  md$Cible_OMS <- cible
  md$Cible_reference <- cible
  md$Gap <- ifelse(is.na(cible) | is.na(md$Prevalence), NA_real_,
                   round(md$Prevalence - cible, 1))
  md$Statut <- ifelse(md$Type == "risk", "À surveiller",
                ifelse(is.na(md$Gap), "Non évalué",
                 ifelse(md$Gap >= 0, "Atteint", "Non atteint")))
  md$`Taux d'atteinte` <- ifelse(md$Type == "protective" & !is.na(cible) &
                                   !is.na(md$Prevalence) & cible > 0,
                                 round(pmin(100, md$Prevalence / cible * 100), 1),
                                 NA_real_)
  md$Priorite <- ifelse(md$Type == "risk", "Surveillance",
                 ifelse(is.na(md$Gap), "Non évaluée",
                  ifelse(md$Gap >= 0, "Atteint",
                   ifelse(md$Gap <= -20, "Haute",
                    ifelse(md$Gap <= -10, "Moyenne", "Modérée")))))
  md
}

calc_iycf_verdict <- function(iycf_res) {
  empty <- list(score = NA_real_, class = "Non évaluable", badge = "NON ÉVALUABLE",
                color = "#6c757d", bg = "#f1f3f5",
                headline = "Aucun indicateur IYCF calculé : vérifier le mapping des colonnes.",
                bullets = character(0), recommendation = "", n_prot = 0, below = 0,
                n_children = iycf_res$n_children %||% NA, domains = data.frame(),
                ci_low = NA_real_, ci_high = NA_real_, risk_burden = NA_real_,
                design_method = iycf_res$design_info$method %||% "—")
  if (is.null(iycf_res) || is.null(iycf_res$summary) || nrow(iycf_res$summary) == 0)
    return(empty)
  md <- calc_iycf_gap(iycf_res)
  comp <- md[md$Type == "protective" & !is.na(md$Cible_OMS) & !is.na(md$Prevalence), , drop = FALSE]
  comp <- comp[comp$key != "anc4", , drop = FALSE]
  if (nrow(comp) == 0) {
    empty$headline <- "Aucun indicateur protecteur avec cible n'a pu être calculé."
    empty$recommendation <- "Mapper les colonnes IYCF (EBF, CBF, MDD, MMF, MAD...) pour évaluer l'atteinte des cibles."
    return(empty)
  }
  ratios <- pmin(1, comp$Prevalence / comp$Cible_OMS)
  score <- round(mean(ratios) * 100, 1)
  idx <- which(score >= IYCF_CLASSES$seuil)[1]
  cls <- IYCF_CLASSES[idx, ]
  below <- sum(comp$Prevalence < comp$Cible_OMS)
  domains <- do.call(rbind, lapply(split(comp, comp$Domaine), function(z) {
    data.frame(Domaine = z$Domaine[1], N = nrow(z),
               Score = round(mean(pmin(1, z$Prevalence / z$Cible_OMS)) * 100, 1),
               Sous_cible = sum(z$Prevalence < z$Cible_OMS),
               stringsAsFactors = FALSE)
  }))
  rownames(domains) <- NULL
  worst_domain <- domains[which.min(domains$Score), , drop = FALSE]
  set.seed(20261120)
  sims <- replicate(2000, {
    p <- comp$Prevalence / 100
    se <- ifelse(is.na(comp$SE), 0, comp$SE / 100)
    draw <- pmin(1, pmax(0, stats::rnorm(length(p), p, se)))
    mean(pmin(1, draw / (comp$Cible_OMS / 100))) * 100
  })
  ci <- stats::quantile(sims, c(0.05, 0.95), na.rm = TRUE)
  bullets <- vapply(seq_len(nrow(comp)), function(i) {
    r <- comp[i, ]
    sprintf("%s : %.1f %% (IC 95 %% [%s–%s] ; n = %d) — %s %s (%s, écart %+.1f pts).",
            iycf_meta(r$key)$label, r$Prevalence,
            ifelse(is.na(r$IC_inf), "—", r$IC_inf),
            ifelse(is.na(r$IC_sup), "—", r$IC_sup),
            r$N, r$Cible_label %||% "Cible de référence", r$Cible_OMS,
            r$Cible_source, r$Prevalence - r$Cible_OMS)
  }, character(1))
  risks <- md[md$Type == "risk" & !is.na(md$Prevalence), , drop = FALSE]
  risk_burden <- if (nrow(risks) > 0) round(mean(risks$Prevalence), 1) else NA_real_
  if (nrow(risks) > 0) {
    bullets <- c(bullets, vapply(seq_len(nrow(risks)), function(i) {
      r <- risks[i, ]
      sprintf("%s : %.1f %% (IC 95 %% [%s–%s]) — pratique à surveiller, pas de cible normative.",
              iycf_meta(r$key)$label, r$Prevalence,
              ifelse(is.na(r$IC_inf), "—", r$IC_inf),
              ifelse(is.na(r$IC_sup), "—", r$IC_sup))
    }, character(1)))
  }
  ctx <- md[md$key %in% c("anc4"), , drop = FALSE]
  if (nrow(ctx) > 0 && !is.na(ctx$Prevalence[1])) {
    bullets <- c(bullets, sprintf(
      "Contexte maternel — ANC 4+ : %.1f %% des mères (cible de référence %s %%; n = %d).",
      ctx$Prevalence[1], ctx$Cible_OMS[1], ctx$N[1]))
  }
  headline <- sprintf(
    "Indice d'atteinte IYCF : %.1f/100 — %s (IC 90 %% simulé %.1f–%.1f). %d indicateur(s) sur %d sous la cible ; %d enfants évalués.",
    score, cls$label, ci[1], ci[2], below, nrow(comp), iycf_res$n_children %||% NA)
  recommendation <- if (idx == 1) {
    sprintf("Les pratiques sont globalement conformes aux cibles de référence. Consolider les acquis et surveiller le domaine le plus fragile (%s, %.1f/100).", worst_domain$Domaine, worst_domain$Score)
  } else if (idx == 2) {
    sprintf("La situation est proche des cibles. Prioriser le domaine %s (%.1f/100) et les indicateurs les plus éloignés, notamment la diversité et la fréquence des repas.", worst_domain$Domaine, worst_domain$Score)
  } else if (idx == 3) {
    sprintf("Les cibles de référence ne sont pas atteintes. Prioriser le domaine %s (%.1f/100), l'éducation nutritionnelle des mères, l'introduction appropriée des aliments complémentaires et le suivi de la croissance.", worst_domain$Domaine, worst_domain$Score)
  } else {
    sprintf("Écart critique aux références OMS/UNICEF. Intervention urgente sur le domaine %s (%.1f/100) : conseil en alimentation du nourrisson, appui à l'allaitement, accès aux aliments riches en fer et enquête qualitative sur les pratiques.", worst_domain$Domaine, worst_domain$Score)
  }
  list(score = score, class = cls$label, badge = toupper(cls$label),
       color = cls$color, bg = cls$bg, headline = headline,
       bullets = bullets, recommendation = recommendation,
       n_prot = nrow(comp), below = below,
       n_children = iycf_res$n_children %||% NA, comp = comp,
       domains = domains, ci_low = round(ci[1], 1), ci_high = round(ci[2], 1),
       risk_burden = risk_burden,
       design_method = iycf_res$design_info$method %||% "Proportion simple")
}

iycf_check_consistency <- function(d, det, rs) {
  out <- list()
  add <- function(key, alt_pos, alt_valid, source, note) {
    r <- rs$resolved[[key]]
    if (is.null(r)) return(invisible(NULL))
    ok <- r$valid & alt_valid & !is.na(r$pos) & !is.na(alt_pos)
    if (sum(ok) < 5) return(invisible(NULL))
    agreement <- mean(r$pos[ok] == alt_pos[ok]) * 100
    out[[length(out) + 1]] <<- data.frame(
      key = key, Indicateur = iycf_meta(key)$label,
      N_compare = sum(ok), Concordance = round(agreement, 1),
      Source_alternative = source,
      Note = if (agreement < 80) note else "",
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
  age <- rs$age
  if (!is.null(det$aux$n_groups) && det$aux$n_groups %in% names(d)) {
    cnt <- iycf_count(d[[det$aux$n_groups]])
    valid <- cnt$known & !is.na(age) & age >= 6 & age <= 23
    add("mdd", valid & cnt$value >= 5, valid, det$aux$n_groups,
        "Écart entre la colonne MDD et le nombre de groupes alimentaires.")
  }
  if (!is.null(rs$resolved$mdd) && !is.null(rs$resolved$mmf)) {
    valid <- rs$resolved$mdd$valid & rs$resolved$mmf$valid
    add("mad", valid & rs$resolved$mdd$pos & rs$resolved$mmf$pos, valid,
        "MDD + MMF", "Écart entre la colonne MAD et la combinaison MDD/MMF.")
  }
  if (!is.null(det$aux$bf_status) && det$aux$bf_status %in% names(d)) {
    xs <- iycf_norm(d[[det$aux$bf_status]])
    known <- !iycf_missing(d[[det$aux$bf_status]])
    add("ebf", known & grepl("exclus", xs), known, det$aux$bf_status,
        "Écart entre la colonne EBF et le statut d'allaitement exclusif.")
  }
  if (!is.null(det$aux$bf_current) && det$aux$bf_current %in% names(d)) {
    bin <- iycf_binary(d[[det$aux$bf_current]])
    if (bin$n_known > 0)
      add("cbf", bin$pos, bin$known, det$aux$bf_current,
          "Écart entre la colonne CBF et l'allaitement actuel.")
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

calc_iycf_quality <- function(d, res) {
  mapping <- res$mapping %||% data.frame()
  design_info <- res$design_info %||% list(enabled = FALSE, n = nrow(d), n_design = nrow(d))
  age <- res$age
  if (is.null(age)) age <- suppressWarnings(as.numeric(d[[res$age_col]]))
  child <- list()
  add_check <- function(check, value, statut, commentaire) {
    child[[length(child) + 1]] <<- data.frame(
      Contrôle = check, Valeur = value, Statut = statut,
      Commentaire = commentaire, stringsAsFactors = FALSE)
  }
  n_age_na <- sum(is.na(age))
  n_age_out <- sum(!is.na(age) & (age < 0 | age > 59))
  add_check("Âge renseigné", sum(!is.na(age)), if (n_age_na > 0) "Vigilance" else "OK",
            if (n_age_na > 0) paste0(n_age_na, " ligne(s) sans âge.") else "Aucun âge manquant.")
  if (n_age_out > 0)
    add_check("Âge dans 0–59 mois", sum(!is.na(age) & age >= 0 & age <= 59), "Vigilance",
              paste0(n_age_out, " âge(s) hors de la plage ANJE."))
  id_col <- intersect(c("child_id", "id_enfant", "id_child", "id"), names(d))
  if (length(id_col) > 0) {
    dup <- sum(duplicated(as.character(d[[id_col[1]]])) & !iycf_missing(d[[id_col[1]]]))
    if (dup > 0)
      add_check("Identifiants enfants uniques", length(unique(d[[id_col[1]]])), "Vigilance",
                paste0(dup, " identifiant(s) dupliqué(s) ; vérifier les doublons."))
  }
  if (isTRUE(design_info$enabled)) {
    excl <- design_info$n - design_info$n_design
    add_check("Couverture du design de sondage", design_info$n_design,
              if (excl > 0) "Vigilance" else "OK",
              if (excl > 0) paste0(excl, " ligne(s) exclue(s) du design pour données manquantes ou poids invalide.")
              else "Toutes les lignes sont couvertes par le design.")
  } else {
    add_check("Design de sondage", "Non utilisé", "Vigilance",
              design_info$note %||% "Prévalences non pondérées.")
  }
  if (!is.null(design_info$weights) && design_info$weights %in% names(d)) {
    w <- suppressWarnings(as.numeric(as.character(d[[design_info$weights]])))
    bad_w <- sum(!is.finite(w) | w <= 0)
    add_check("Poids de sondage valides", sum(is.finite(w) & w > 0),
              if (bad_w > 0) "Vigilance" else "OK",
              if (bad_w > 0) paste0(bad_w, " poids manquant(s) ou non positif(s).")
              else "Poids exploitables.")
  }
  if (nrow(mapping) > 0) {
    n_nd <- sum(mapping$Statut == "Non détecté")
    add_check("Indicateurs détectés", sum(mapping$Statut != "Non détecté"),
              if (n_nd > 0) "Vigilance" else "OK",
              if (n_nd > 0) paste0(n_nd, " indicateur(s) sans colonne ou dérivation.")
              else "Tous les indicateurs ont une source.")
  }
  cons <- res$consistency %||% data.frame()
  if (nrow(cons) > 0) {
    bad <- cons[cons$Concordance < 80, , drop = FALSE]
    add_check("Cohérence direct/dérivé", nrow(cons) - nrow(bad),
              if (nrow(bad) > 0) "Vigilance" else "OK",
              if (nrow(bad) > 0) paste0(nrow(bad), " indicateur(s) avec concordance < 80 %.")
              else "Concordance satisfaisante.")
  }
  child_df <- if (length(child) > 0) do.call(rbind, child) else data.frame()

  ind <- data.frame()
  if (!is.null(res$summary) && nrow(res$summary) > 0) {
    s <- res$summary
    ind <- data.frame(
      Indicateur = s$Indicateur, Sigle = s$Sigle,
      N_eligible = s$N_eligible, N = s$N, Manquants = s$Manquants,
      Completude = s$Taux_completude,
      IC_large = ifelse(is.na(s$IC_inf) | is.na(s$IC_sup), NA,
                        (s$IC_sup - s$IC_inf) > 30),
      Petit_effectif = !is.na(s$N) & s$N > 0 & s$N < 30,
      DEFF = s$DEFF, Methode = s$Methode,
      Statut = ifelse(is.na(s$Prevalence), "Non évaluable",
               ifelse(!is.na(s$N) & s$N < 30, "Petit effectif",
                ifelse(!is.na(s$Taux_completude) & s$Taux_completude < 70, "Complétude faible",
                 "Acceptable"))),
      Commentaire = ifelse(is.na(s$Prevalence), "Aucune donnée exploitable.",
                    ifelse(!is.na(s$N) & s$N < 30, "IC large ; interpréter avec prudence.",
                     ifelse(!is.na(s$IC_inf) & !is.na(s$IC_sup) & (s$IC_sup - s$IC_inf) > 30,
                            "IC très large.", ""))),
      stringsAsFactors = FALSE
    )
  }
  issues <- character(0)
  if (n_age_na > 0) issues <- c(issues, paste0(n_age_na, " âge(s) manquant(s)"))
  if (n_age_out > 0) issues <- c(issues, paste0(n_age_out, " âge(s) hors plage"))
  if (nrow(mapping) > 0 && any(mapping$Statut == "Non détecté"))
    issues <- c(issues, paste0(sum(mapping$Statut == "Non détecté"), " indicateur(s) non détecté(s)"))
  if (nrow(cons) > 0 && any(cons$Concordance < 80))
    issues <- c(issues, "incohérence direct/dérivé")
  if (nrow(ind) > 0 && any(ind$Petit_effectif, na.rm = TRUE))
    issues <- c(issues, "petits effectifs")
  if (isTRUE(design_info$enabled) && design_info$n_design < design_info$n)
    issues <- c(issues, "lignes exclues du design")
  score <- 100
  score <- score - min(15, 3 * n_age_na)
  score <- score - min(10, 2 * n_age_out)
  if (nrow(mapping) > 0) score <- score - min(15, 2 * sum(mapping$Statut == "Non détecté"))
  if (nrow(cons) > 0) score <- score - min(15, 3 * sum(cons$Concordance < 80))
  if (nrow(ind) > 0) score <- score - min(15, 2 * sum(ind$Petit_effectif, na.rm = TRUE))
  if (isTRUE(design_info$enabled) && design_info$n_design < design_info$n) score <- score - 5
  score <- max(0, round(score, 1))
  label <- if (score >= 85) "Qualité élevée" else if (score >= 70) "Qualité acceptable" else "Qualité à renforcer"
  list(child = child_df, indicators = ind, score = score, label = label,
       issues = issues, consistency = cons)
}

calc_iycf_age_profile <- function(res, d) {
  if (is.null(res) || is.null(res$results) || length(res$results) == 0) return(data.frame())
  age <- res$age
  grp <- cut(age, breaks = c(-Inf, 5, 8, 11, 17, 23, 59),
             labels = c("0–5", "6–8", "9–11", "12–17", "18–23", "24–59"),
             right = TRUE, include.lowest = TRUE)
  out <- list()
  for (key in names(res$results)) {
    r <- res$results[[key]]
    for (g in levels(grp)) {
      valid <- r$mask & !is.na(grp) & grp == g
      if (sum(valid, na.rm = TRUE) == 0) next
      est <- iycf_estimate(r$pos, valid, res$design_info)
      out[[length(out) + 1]] <- data.frame(
        key = key, Indicateur = iycf_meta(key)$label, Groupe_age = g,
        N_eligible = sum(r$eligible & !is.na(grp) & grp == g, na.rm = TRUE),
        N = est$n, Positifs = est$n_pos, Prevalence = est$pct,
        IC_inf = est$ci_lower, IC_sup = est$ci_upper,
        N_pondere = est$n_weighted, Methode = est$method,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) return(data.frame())
  ans <- do.call(rbind, out)
  ans$Groupe_age <- factor(ans$Groupe_age,
                           levels = c("0–5", "6–8", "9–11", "12–17", "18–23", "24–59"))
  ans[order(ans$key, ans$Groupe_age), , drop = FALSE]
}

iycf_estimate_groups <- function(res, key, group) {
  r <- res$results[[key]]
  if (is.null(r)) return(data.frame())
  group <- as.character(group)
  vals <- sort(unique(group[!is.na(group)]))
  out <- list()
  for (g in vals) {
    valid <- r$mask & !is.na(group) & group == g
    if (sum(valid, na.rm = TRUE) == 0) next
    est <- iycf_estimate(r$pos, valid, res$design_info)
    out[[length(out) + 1]] <- data.frame(
      group = g, n = est$n, k = est$n_pos, pct = est$pct,
      ci_lo = est$ci_lower, ci_hi = est$ci_upper,
      n_weighted = est$n_weighted, method = est$method,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

iycf_association_test <- function(res, key, group) {
  r <- res$results[[key]]
  if (is.null(r)) return(list(p = NA_real_, V = NA_real_, n = 0,
                              note = "Indicateur indisponible.", method = "—"))
  group <- as.character(group)
  cls <- ifelse(r$mask & !is.na(group), ifelse(r$pos, "Atteint", "Non atteint"), NA_character_)
  ok <- !is.na(cls) & !is.na(group)
  if (sum(ok) < 5)
    return(list(p = NA_real_, V = NA_real_, n = sum(ok),
                note = "Effectif insuffisant pour tester l'association.", method = "—"))
  if (isTRUE(res$design_info$enabled) && !is.null(res$design_info$design)) {
    rid <- res$design_info$row_id
    des <- res$design_info$design
    des$variables$.iycf_pos <- as.integer(r$pos[rid])
    des$variables$.iycf_grp <- factor(group[rid])
    des$variables$.iycf_valid <- as.integer(r$mask[rid] & !is.na(group[rid]))
    sub <- tryCatch(subset(des, .iycf_valid == 1), error = function(e) NULL)
    if (!is.null(sub) && length(unique(sub$variables$.iycf_grp)) >= 2) {
      test <- tryCatch(survey::svychisq(~.iycf_pos + .iycf_grp, sub,
                                        statistic = "adjWald"),
                       error = function(e) NULL)
      if (!is.null(test)) {
        p <- as.numeric(test$p.value)[1]
        V <- tryCatch({
          tab <- table(sub$variables$.iycf_grp, sub$variables$.iycf_pos)
          chi <- suppressWarnings(as.numeric(stats::chisq.test(tab, correct = FALSE)$statistic))
          sqrt(chi / (sum(tab) * (min(dim(tab)) - 1)))
        }, error = function(e) NA_real_)
        return(list(p = p, V = V, n = sum(sub$variables$.iycf_valid),
                    note = "χ² ajusté au design (adjWald)", method = "Design-based"))
      }
    }
  }
  if (exists("calc_strat_test", mode = "function")) {
    t <- calc_strat_test(cls[ok], group[ok])
    return(list(p = t$p, V = t$V, n = t$n, note = t$note, method = "Non pondéré"))
  }
  tab <- table(group[ok], cls[ok])
  p <- tryCatch(suppressWarnings(stats::chisq.test(tab)$p.value), error = function(e) NA_real_)
  chi <- tryCatch(suppressWarnings(as.numeric(stats::chisq.test(tab, correct = FALSE)$statistic)),
                  error = function(e) NA_real_)
  V <- if (is.finite(chi) && sum(tab) > 0 && min(dim(tab)) > 1)
    sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))) else NA_real_
  list(p = p, V = V, n = sum(ok), note = "χ² d'indépendance", method = "Non pondéré")
}

calc_iycf_strat <- function(iycf_res, key, strat_var, d) {
  if (is.null(iycf_res) || is.null(iycf_res$results[[key]])) return(NULL)
  if (is.null(strat_var) || !strat_var %in% names(d)) return(NULL)
  r <- iycf_res$results[[key]]
  group <- as.character(d[[strat_var]])
  freq <- iycf_estimate_groups(iycf_res, key, group)
  if (nrow(freq) == 0) return(NULL)
  test <- iycf_association_test(iycf_res, key, group)
  list(test = test, freq = freq, key = key,
       label = iycf_meta(key)$label, var = strat_var,
       n = test$n, method = test$method)
}

calc_iycf_equity <- function(iycf_res, d, group_var, keys = NULL) {
  if (is.null(iycf_res) || is.null(iycf_res$results)) return(data.frame())
  if (is.null(group_var) || !group_var %in% names(d)) return(data.frame())
  if (is.null(keys)) keys <- intersect(c("eibf", "ebf", "cbf", "isf", "mdd", "mmf",
                                        "mad", "iron", "vita", "bottle", "sweet",
                                        "upt", "zvf"), names(iycf_res$results))
  group <- as.character(d[[group_var]])
  out <- list()
  for (key in keys) {
    if (is.null(iycf_res$results[[key]])) next
    freq <- iycf_estimate_groups(iycf_res, key, group)
    if (nrow(freq) == 0) next
    test <- iycf_association_test(iycf_res, key, group)
    overall <- iycf_res$summary[iycf_res$summary$key == key, , drop = FALSE]
    overall_pct <- if (nrow(overall) > 0) overall$Prevalence[1] else NA_real_
    freq$key <- key
    freq$Indicateur <- iycf_meta(key)$label
    freq$Groupe_var <- group_var
    freq$Prevalence_globale <- overall_pct
    freq$Ecart_pts <- ifelse(is.na(freq$pct) | is.na(overall_pct), NA_real_,
                             round(freq$pct - overall_pct, 1))
    freq$p_value <- test$p
    freq$Test <- test$note
    freq$Methode_test <- test$method
    out[[length(out) + 1]] <- freq
  }
  if (length(out) == 0) return(data.frame())
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

calc_iycf_rounds <- function(iycf_res, key, round_var, d) {
  if (is.null(iycf_res) || is.null(iycf_res$results[[key]])) return(NULL)
  if (is.null(round_var) || !round_var %in% names(d)) return(NULL)
  r <- iycf_res$results[[key]]
  rd <- as.character(d[[round_var]])
  summ <- iycf_estimate_groups(iycf_res, key, rd)
  if (nrow(summ) == 0) return(NULL)
  ord <- order(suppressWarnings(as.numeric(summ$group)), summ$group, na.last = TRUE)
  summ <- summ[ord, , drop = FALSE]
  names(summ)[names(summ) == "group"] <- "round"
  p_first <- NA_real_
  diff_pp <- NA_real_
  if (nrow(summ) >= 2) {
    a <- summ[1, ]
    b <- summ[nrow(summ), ]
    diff_pp <- round(b$pct - a$pct, 1)
    p_first <- tryCatch(stats::prop.test(c(a$k, b$k), c(a$n, b$n),
                                         correct = FALSE)$p.value,
                        error = function(e) NA_real_)
  }
  p_trend <- NA_real_
  trend_method <- "Non calculable"
  if (nrow(summ) >= 3) {
    p_trend <- tryCatch(stats::prop.trend.test(summ$k, summ$n,
                                               score = seq_len(nrow(summ)))$p.value,
                        error = function(e) NA_real_)
    trend_method <- "Cochran–Armitage (non pondéré)"
    if (isTRUE(iycf_res$design_info$enabled) && !is.null(iycf_res$design_info$design)) {
      rid <- iycf_res$design_info$row_id
      des <- iycf_res$design_info$design
      des$variables$.iycf_pos <- as.integer(r$pos[rid])
      des$variables$.iycf_round_num <- suppressWarnings(as.numeric(rd[rid]))
      des$variables$.iycf_valid <- as.integer(r$mask[rid] & is.finite(des$variables$.iycf_round_num))
      sub <- tryCatch(subset(des, .iycf_valid == 1), error = function(e) NULL)
      if (!is.null(sub) && length(unique(sub$variables$.iycf_round_num)) >= 3) {
        fit <- tryCatch(survey::svyglm(.iycf_pos ~ .iycf_round_num,
                                       design = sub,
                                       family = stats::quasibinomial()),
                        error = function(e) NULL)
        if (!is.null(fit)) {
          co <- summary(fit)$coefficients
          if (nrow(co) >= 2 && is.finite(co[2, 4])) {
            p_trend <- co[2, 4]
            trend_method <- "svyglm quasi-binomial (design-based)"
          }
        }
      }
    }
  }
  list(summary = summ, p = p_trend, p_first_last = p_first,
       p_trend = p_trend, trend_method = trend_method,
       diff = diff_pp, key = key, label = iycf_meta(key)$label,
       n = sum(summ$n), first = summ$round[1], last = summ$round[nrow(summ)],
       method = iycf_res$design_info$method %||% "Proportion simple")
}

iycf_fmt_p <- function(p) {
  if (length(p) == 0 || is.null(p) || is.na(p)) return("p non calculable")
  if (exists("fmt_p", mode = "function")) return(fmt_p(p))
  if (p < 0.001) "p < 0,001" else sprintf("p = %.3f", p)
}

iycf_fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x)) return("—")
  format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE)
}

iycf_design_label <- function(res) {
  di <- res$design_info %||% list(enabled = FALSE)
  if (!isTRUE(di$enabled)) return("Prévalences non pondérées (plan de sondage non utilisé)")
  paste0(di$method %||% "Design-based", " — ", di$n_design %||% res$data_n,
         " lignes, ", iycf_fmt_num(di$weighted_n, 0), " pondérées")
}

iycf_target_label <- function(key) {
  meta <- iycf_meta(key)
  if (is.null(meta)) return("Cible non définie")
  if (is.na(meta$target)) return("Pas de cible normative")
  sprintf("%s (%s)", meta$target_label %||% "Cible de référence", meta$target_source %||% "")
}

iycf_definitions_table <- function() {
  do.call(rbind, lapply(IYCF_INDICATOR_ORDER, function(key) {
    meta <- iycf_meta(key)
    data.frame(
      key = key, Sigle = meta$short, Indicateur = meta$label,
      Domaine = meta$domain,
      Type = if (meta$type == "risk") "Risque" else "Protecteur",
      Age_cible = paste0(meta$target_age[1], "–", meta$target_age[2], " mois"),
      Cible = meta$target,
      Source_cible = meta$target_source,
      Libelle_cible = meta$target_label,
      Definition = meta$definition,
      stringsAsFactors = FALSE
    )
  }))
}

iycf_mapping_table <- function(iycf_res) {
  if (is.null(iycf_res) || is.null(iycf_res$mapping) || nrow(iycf_res$mapping) == 0)
    return(data.frame())
  cols <- intersect(c("key", "Sigle", "Indicateur", "Domaine", "Type", "Statut",
                      "Colonne", "Source", "Methode", "Fiabilite", "N_connu",
                      "N_eligible", "Evaluable", "Note"), names(iycf_res$mapping))
  out <- iycf_res$mapping[, cols, drop = FALSE]
  out
}

build_iycf_child_table <- function(d, iycf_res) {
  if (is.null(iycf_res) || is.null(iycf_res$results) || length(iycf_res$results) == 0)
    return(NULL)
  n <- nrow(d)
  base_candidates <- c("household_id", "menage_id", "region", "province", "department",
                       "commune", "cluster", "cluster_id", "grappe", "strate", "strata",
                       "survey_round", "vague_enquete", "round", "child_id", "id_enfant",
                       "child_age_months", "age_m", "sexe", "child_sex", "mother_age",
                       "allaitement_actuel", "vitamine_a", "visites_cpn")
  base <- unique(intersect(base_candidates, names(d)))
  if (!is.null(iycf_res$age_col) && iycf_res$age_col %in% names(d))
    base <- unique(c(base, iycf_res$age_col))
  if (!is.null(iycf_res$design_info$cluster) &&
      iycf_res$design_info$cluster %in% names(d))
    base <- unique(c(base, iycf_res$design_info$cluster))
  if (!is.null(iycf_res$design_info$strata) &&
      iycf_res$design_info$strata %in% names(d))
    base <- unique(c(base, iycf_res$design_info$strata))
  if (!is.null(iycf_res$design_info$weights) &&
      iycf_res$design_info$weights %in% names(d))
    base <- unique(c(base, iycf_res$design_info$weights))
  out <- if (length(base) > 0) d[, base, drop = FALSE] else data.frame(row = seq_len(n))
  age <- iycf_res$age
  if (is.null(age)) age <- suppressWarnings(as.numeric(d[[iycf_res$age_col]]))
  out$Age_mois <- age
  out$Groupe_age <- cut(age, breaks = c(-Inf, 5, 8, 11, 17, 23, 59),
                        labels = c("0–5", "6–8", "9–11", "12–17", "18–23", "24–59"),
                        right = TRUE, include.lowest = TRUE)
  for (key in names(iycf_res$results)) {
    r <- iycf_res$results[[key]]
    meta <- iycf_meta(key)
    v <- rep(NA_character_, n)
    mask <- !is.na(r$mask) & r$mask
    v[mask] <- ifelse(r$pos[mask], "Oui", "Non")
    out[[meta$short]] <- v
  }
  out
}

iycf_export_frames <- function(iycf_res, verdict = NULL, strat = NULL,
                               rounds = NULL, child = NULL, equity = NULL) {
  if (is.null(iycf_res) || is.null(iycf_res$summary) || nrow(iycf_res$summary) == 0)
    return(list())
  md <- calc_iycf_gap(iycf_res)
  fmt <- function(x) {
    if (is.null(x) || nrow(x) == 0) return(data.frame())
    colv <- function(nm, default = NA) {
      if (nm %in% names(x)) x[[nm]] else rep(default, nrow(x))
    }
    cible <- if ("Cible_reference" %in% names(x)) x$Cible_reference else colv("Cible_OMS")
    gap <- if ("Gap" %in% names(x)) x$Gap else ifelse(is.na(cible) | is.na(x$Prevalence),
                                                       NA_real_, round(x$Prevalence - cible, 1))
    type <- if ("Type" %in% names(x)) x$Type else rep("protective", nrow(x))
    data.frame(
      Indicateur = x$Indicateur,
      Sigle = if ("Sigle" %in% names(x)) x$Sigle else vapply(x$key, function(k) iycf_meta(k)$short, character(1)),
      Domaine = if ("Domaine" %in% names(x)) x$Domaine else vapply(x$key, function(k) iycf_meta(k)$domain, character(1)),
      Type = ifelse(type == "risk", "Risque", "Protecteur"),
      N = colv("N"), Positifs = colv("Positifs"),
      `Prévalence (%)` = x$Prevalence,
      `IC 95% inf` = colv("IC_inf"), `IC 95% sup` = colv("IC_sup"),
      `N pondéré` = colv("N_pondere"), DEFF = colv("DEFF"),
      `Cible de référence (%)` = cible,
      `Source de la cible` = colv("Cible_source", ""),
      `Libellé de la cible` = colv("Cible_label", ""),
      `Écart (pts)` = gap,
      Statut = if ("Statut" %in% names(x)) x$Statut else ifelse(is.na(gap), "Non évalué", ifelse(gap >= 0, "Atteint", "Non atteint")),
      `Taux d'atteinte (%)` = colv("Taux d'atteinte", NA_real_),
      `Méthode IC` = colv("IC_methode", ""),
      `Statut mapping` = colv("Statut_mapping", ""),
      Colonne = colv("Colonne", ""),
      check.names = FALSE
    )
  }
  sheets <- list()
  sheets[["Synthese"]] <- fmt(md)
  bf_keys <- c("eibf", "ebf", "cbf")
  diet_keys <- c("isf", "mdd", "mmf", "mad", "iron", "vita")
  risk_keys <- c("bottle", "sweet", "upt", "zvf")
  ctx_keys <- c("anc4", "vita")
  if (any(md$key %in% bf_keys))
    sheets[["Allaitement"]] <- fmt(md[md$key %in% bf_keys, , drop = FALSE])
  if (any(md$key %in% diet_keys))
    sheets[["Alimentation"]] <- fmt(md[md$key %in% diet_keys, , drop = FALSE])
  if (any(md$key %in% risk_keys))
    sheets[["Risques"]] <- fmt(md[md$key %in% risk_keys, , drop = FALSE])
  if (any(md$key %in% ctx_keys))
    sheets[["Contexte maternel"]] <- fmt(md[md$key %in% ctx_keys, , drop = FALSE])
  if (any(!is.na(md$Cible_OMS)))
    sheets[["Ecarts cibles"]] <- fmt(md[!is.na(md$Cible_OMS), , drop = FALSE])
  if (!is.null(strat) && !is.null(strat$freq)) {
    st <- as.data.frame(strat$freq)
    st$Indicateur <- strat$label
    t <- strat$test %||% list(p = NA_real_, V = NA_real_, n = nrow(st), note = "—", method = "—")
    st$Test <- sprintf("%s ; %s ; V de Cramér = %s ; n = %s",
                       t$note %||% "—", iycf_fmt_p(t$p),
                       ifelse(is.na(t$V), "—", round(t$V, 3)), t$n %||% nrow(st))
    st$Methode_test <- t$method %||% "—"
    sheets[["Par strate"]] <- st
  }
  if (!is.null(rounds) && !is.null(rounds$summary)) {
    rt <- as.data.frame(rounds$summary)
    rt$Indicateur <- rounds$label
    rt$`Test 1er vs dernier` <- sprintf("%s ; écart = %s pts",
                                       iycf_fmt_p(rounds$p_first_last),
                                       ifelse(is.na(rounds$diff), "—", rounds$diff))
    rt$`Test de tendance` <- sprintf("%s ; %s", iycf_fmt_p(rounds$p_trend),
                                     rounds$trend_method %||% "—")
    sheets[["Evolution"]] <- rt
  }
  if (!is.null(child) && nrow(child) > 0) sheets[["Enfants"]] <- child
  if (!is.null(verdict)) {
    sheets[["Verdict"]] <- data.frame(
      Rubrique = c("Indice", "Niveau", "IC 90% simulé", "Constats", "Recommandation",
                   "Enfants évalués", "Indicateurs comparés", "Sous la cible",
                   "Charge des risques", "Méthode"),
      Contenu = c(sprintf("%.1f/100", verdict$score), verdict$badge,
                  ifelse(is.na(verdict$ci_low), "—", sprintf("%.1f–%.1f",
                                                           verdict$ci_low, verdict$ci_high)),
                  paste(verdict$bullets, collapse = " | "),
                  verdict$recommendation,
                  as.character(verdict$n_children),
                  as.character(verdict$n_prot), as.character(verdict$below),
                  ifelse(is.na(verdict$risk_burden), "—",
                         paste0(iycf_fmt_num(verdict$risk_burden), " %")),
                  verdict$design_method %||% "—"),
      check.names = FALSE)
  }
  mapping <- iycf_mapping_table(iycf_res)
  if (nrow(mapping) > 0) sheets[["Mapping"]] <- mapping
  if (!is.null(iycf_res$quality)) {
    if (!is.null(iycf_res$quality$child) && nrow(iycf_res$quality$child) > 0)
      sheets[["Qualite_controles"]] <- iycf_res$quality$child
    if (!is.null(iycf_res$quality$indicators) && nrow(iycf_res$quality$indicators) > 0)
      sheets[["Qualite_indicateurs"]] <- iycf_res$quality$indicators
  }
  if (!is.null(iycf_res$age_profile) && nrow(iycf_res$age_profile) > 0)
    sheets[["Profil_age"]] <- as.data.frame(iycf_res$age_profile)
  if (!is.null(equity) && nrow(equity) > 0) sheets[["Equite"]] <- as.data.frame(equity)
  sheets[["Definitions"]] <- iycf_definitions_table()
  sheets[["Methodologie"]] <- data.frame(
    Rubrique = c("Source de données", "Indicateurs", "Enfants", "Plan de sondage",
                 "Pondération", "IC des prévalences", "IC de l'indice", "Cibles"),
    Contenu = c(
      sprintf("%d lignes ; %d enfants 0–59 mois", iycf_res$data_n %||% NA,
              iycf_res$n_children %||% NA),
      sprintf("%d indicateur(s) calculé(s)", iycf_res$n_indicators %||% NA),
      as.character(iycf_res$n_children %||% NA),
      iycf_design_label(iycf_res),
      ifelse(isTRUE(iycf_res$design_info$enabled),
             "Poids rescale à une moyenne de 1 ; grappes et strates déclarées.",
             "Aucune pondération appliquée."),
      "svyciprop logit/beta ou Wilson si design absent.",
      "Monte Carlo 2000 tirages, IC 90 %.",
      "Cibles mondiales ou benchmarks programmatiques documentés par indicateur."
    ),
    check.names = FALSE)
  sheets
}

iycf_reading_synthesis <- function(verdict) {
  if (is.null(verdict)) return(NULL)
  sprintf(paste0("%s %s L'indice composite est accompagné d'un IC 90 %% simulé ; ",
                 "les prévalences sont accompagnées d'un IC 95 %%. %s"),
          verdict$headline, verdict$recommendation,
          ifelse(is.null(verdict$design_method), "",
                 paste0("Méthode d'estimation : ", verdict$design_method, ".")))
}

iycf_reading_dashboard <- function(res) {
  if (is.null(res) || is.null(res$summary) || nrow(res$summary) == 0) return(NULL)
  md <- calc_iycf_gap(res)
  prot <- md[md$Type == "protective" & !is.na(md$Prevalence) & !is.na(md$Cible_OMS), , drop = FALSE]
  prot <- prot[prot$key != "anc4", , drop = FALSE]
  if (nrow(prot) == 0)
    return("Aucun indicateur protecteur avec cible disponible pour la comparaison.")
  prot$ratio <- prot$Prevalence / prot$Cible_OMS
  best <- prot[which.max(prot$ratio), , drop = FALSE]
  worst <- prot[which.min(prot$ratio), , drop = FALSE]
  target_label <- if ("Cible_label" %in% names(worst)) worst$Cible_label[1] else "Cible de référence"
  sprintf(paste0(
    "Lecture experte — L'indicateur le plus proche de sa cible est %s (%.1f %% vs %d %%, %+.1f pts). ",
    "L'écart le plus marqué concerne %s (%.1f %% ; IC 95 %% [%s–%s] vs %d %%, %+.1f pts ; n = %d). ",
    "%d indicateur(s) sur %d sont sous la cible. %s. Le graphique superpose les prévalences observées ",
    "et les cibles de référence : tout écart négatif signale un déficit programmatique."),
    iycf_meta(best$key)$label, best$Prevalence, best$Cible_OMS, best$Prevalence - best$Cible_OMS,
    iycf_meta(worst$key)$label, worst$Prevalence,
    ifelse(is.na(worst$IC_inf), "—", worst$IC_inf), ifelse(is.na(worst$IC_sup), "—", worst$IC_sup),
    worst$Cible_OMS, worst$Prevalence - worst$Cible_OMS, worst$N,
    sum(prot$Prevalence < prot$Cible_OMS), nrow(prot), target_label)
}

iycf_reading_bf <- function(res) {
  if (is.null(res)) return(NULL)
  keys <- c("eibf", "ebf", "cbf")
  md <- calc_iycf_gap(res)
  md <- md[md$key %in% keys & !is.na(md$Prevalence), , drop = FALSE]
  if (nrow(md) == 0)
    return("Aucun indicateur d'allaitement calculé (colonnes EIBF/EBF/CBF ou statut d'allaitement absents).")
  lines <- vapply(seq_len(nrow(md)), function(i) {
    r <- md[i, ]
    sprintf("%s %.1f %% (IC 95 %% [%s–%s] ; n = %d)",
            IYCF_INDICATORS[[r$key]]$label, r$Prevalence,
            ifelse(is.na(r$IC_inf), "—", r$IC_inf), ifelse(is.na(r$IC_sup), "—", r$IC_sup), r$N)
  }, character(1))
  md$ratio <- md$Prevalence / md$Cible_OMS
  frag <- md[which.min(md$ratio), , drop = FALSE]
  sprintf(paste0(
    "Lecture experte — Cascade de l'allaitement : %s. ",
    "Le maillon le plus fragile est %s. Renforcer le conseil dès la maternité, l'appui à l'allaitement ",
    "exclusif et la poursuite de l'allaitement jusqu'à 23 mois. Une prévalence reposant sur moins de 30 enfants ",
    "doit être interprétée avec prudence (IC large)."),
    paste(lines, collapse = " ; "), IYCF_INDICATORS[[frag$key]]$label)
}

iycf_reading_diet <- function(res) {
  if (is.null(res)) return(NULL)
  keys <- c("isf", "mdd", "mmf", "mad", "iron", "vita")
  md <- calc_iycf_gap(res)
  md <- md[md$key %in% keys & !is.na(md$Prevalence) & !is.na(md$Cible_OMS), , drop = FALSE]
  if (nrow(md) == 0)
    return("Aucun indicateur d'alimentation complémentaire calculé (MDD, MMF, MAD, fer, vitamine A).")
  md$ratio <- md$Prevalence / md$Cible_OMS
  worst <- md[which.min(md$ratio), , drop = FALSE]
  mad <- md[md$key == "mad", , drop = FALSE]
  txt <- sprintf(paste0(
    "Lecture experte — Alimentation complémentaire : %s. ",
    "L'indicateur le plus déficitaire est %s (%.1f %% contre une cible de référence de %d %%). "),
    paste(vapply(seq_len(nrow(md)), function(i)
      sprintf("%s %.1f %%", IYCF_INDICATORS[[md$key[i]]]$short, md$Prevalence[i]),
      character(1)), collapse = ", "),
    IYCF_INDICATORS[[worst$key]]$label, worst$Prevalence, worst$Cible_OMS)
  if (nrow(mad) > 0 && !is.na(mad$Prevalence[1])) {
    txt <- paste0(txt, sprintf(
      "Le régime minimum acceptable (MAD), qui combine diversité et fréquence, n'est atteint que par %.1f %% des enfants : ",
      mad$Prevalence[1]))
  }
  paste0(txt, "prioriser la diversification alimentaire, la fréquence des repas et les aliments riches en fer.")
}

iycf_reading_risks <- function(res) {
  if (is.null(res)) return(NULL)
  keys <- c("bottle", "sweet", "upt", "zvf")
  md <- calc_iycf_gap(res)
  md <- md[md$key %in% keys & !is.na(md$Prevalence), , drop = FALSE]
  if (nrow(md) == 0)
    return("Aucun indicateur de pratique à risque calculé (biberon, boissons sucrées, ultra-transformés, zéro légumes/fruits).")
  burden <- mean(md$Prevalence, na.rm = TRUE)
  sprintf(paste0(
    "Lecture experte — Pratiques à risque : %s. ",
    "La charge moyenne de ces pratiques est de %.1f %%. Ces prévalences sont des cibles de réduction : ",
    "leur niveau doit être mis en regard du contexte d'approvisionnement et des messages de conseil. ",
    "Une prévalence élevée d'aliments ultra-transformés ou de boissons sucrées justifie des actions de ",
    "communication et de disponibilité d'aliments locaux adaptés."),
    paste(vapply(seq_len(nrow(md)), function(i)
      sprintf("%s %.1f %% (IC 95 %% [%s–%s])",
              IYCF_INDICATORS[[md$key[i]]]$label, md$Prevalence[i],
              ifelse(is.na(md$IC_inf[i]), "—", md$IC_inf[i]),
              ifelse(is.na(md$IC_sup[i]), "—", md$IC_sup[i])),
      character(1)), collapse = " ; "), burden)
}

iycf_reading_gaps <- function(gap_tbl) {
  if (is.null(gap_tbl) || nrow(gap_tbl) == 0) return(NULL)
  g <- gap_tbl[!is.na(gap_tbl$Gap), , drop = FALSE]
  if (nrow(g) == 0) return("Aucune cible de référence disponible pour les indicateurs calculés.")
  worst <- g[which.min(g$Gap), , drop = FALSE]
  source_txt <- if ("Cible_source" %in% names(worst)) worst$Cible_source[1] else ""
  label_txt <- if ("Cible_label" %in% names(worst)) worst$Cible_label[1] else "Cible de référence"
  sprintf(paste0(
    "Lecture experte — %d indicateur(s) atteignent leur cible, %d sont en deçà. ",
    "Le déficit le plus important est %s (%+.1f pts ; %.1f %% vs %d %%). ",
    "%s : %s. Les écarts exprimés en points de pourcentage facilitent la priorisation programmatique."),
    sum(g$Gap >= 0), sum(g$Gap < 0),
    IYCF_INDICATORS[[worst$key]]$label, worst$Gap, worst$Prevalence, worst$Cible_OMS,
    label_txt, source_txt)
}

iycf_reading_strat <- function(strat_res) {
  if (is.null(strat_res)) return(NULL)
  t <- strat_res$test
  if (is.null(t)) return("Test d'association non calculable.")
  force <- if (is.na(t$V)) "indéterminée" else if (t$V < 0.1) "faible" else if (t$V < 0.3) "modérée" else "forte"
  sprintf(paste0(
    "Lecture experte — Association entre %s et %s : %s, %s, V de Cramér = %s (association %s ; n = %d). ",
    "Les strates de petite taille (n < 30) produisent des intervalles larges et ne doivent pas être surinterprétées. ",
    "Un écart fort entre strates doit orienter le ciblage géographique des interventions."),
    strat_res$label, strat_res$var, t$note, iycf_fmt_p(t$p),
    ifelse(is.na(t$V), "—", round(t$V, 3)), force, t$n %||% strat_res$n)
}

iycf_reading_rounds <- function(rounds_res) {
  if (is.null(rounds_res)) return(NULL)
  s <- rounds_res$summary
  if (is.null(s) || nrow(s) < 2)
    return("Un seul round disponible : impossible de tester une tendance inter-rounds.")
  trend_txt <- if (is.na(rounds_res$p_trend)) {
    if (is.na(rounds_res$p_first_last)) "test non calculable" else
      paste0("comparaison 1er–dernier round ", iycf_fmt_p(rounds_res$p_first_last))
  } else if (rounds_res$p_trend < 0.05) {
    "évolution statistiquement significative"
  } else {
    "évolution non significative"
  }
  sprintf(paste0(
    "Lecture experte — %s : %.1f %% au round %s, %.1f %% au round %s (%+.1f pts ; %s, %s ; %s). ",
    "Les IC à 95 %% par round permettent d'apprécier la précision. ",
    "Une tendance significative à la baisse sur un indicateur protecteur appelle une revue des actions mises en œuvre."),
    rounds_res$label, s$pct[1], s$round[1], s$pct[nrow(s)], s$round[nrow(s)],
    rounds_res$diff, iycf_fmt_p(rounds_res$p_trend), trend_txt,
    rounds_res$trend_method %||% "méthode non précisée")
}

iycf_reading_quality <- function(res) {
  if (is.null(res) || is.null(res$quality)) return(NULL)
  q <- res$quality
  issues <- if (length(q$issues) > 0) paste(q$issues, collapse = " ; ") else "aucun problème majeur détecté"
  sprintf(paste0(
    "Contrôle qualité — Score %s/100 (%s). Contrôles disponibles : %d. ",
    "Points de vigilance : %s. Les prévalences dont l'IC est très large ou dont l'effectif est inférieur à 30 ",
    "doivent être lues avec prudence."),
    iycf_fmt_num(q$score, 0), q$label, nrow(q$child %||% data.frame()), issues)
}

iycf_reading_age <- function(res) {
  if (is.null(res) || is.null(res$age_profile) || nrow(res$age_profile) == 0)
    return("Profil par âge non disponible.")
  ap <- res$age_profile
  ap <- ap[!is.na(ap$Prevalence), , drop = FALSE]
  if (nrow(ap) == 0) return("Aucune prévalence par groupe d'âge exploitable.")
  sprintf("Profil par âge — %d groupes d'âge documentés ; les variations entre 6–8, 9–11, 12–17 et 18–23 mois aident à identifier les fenêtres d'action prioritaires.",
          length(unique(ap$Groupe_age)))
}

iycf_reading_equity <- function(equity) {
  if (is.null(equity) || nrow(equity) == 0) return(NULL)
  sprintf("Lecture équité — %d comparaison(s) par groupe ; %d groupe(s) documenté(s). Les écarts en points de pourcentage sont calculés par rapport à la prévalence globale de chaque indicateur.",
          nrow(equity), length(unique(equity$group)))
}

plot_iycf_dashboard <- function(iycf_res) {
  tryCatch({
    df <- iycf_res$summary
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (all(is.na(df$Prevalence))) return(NULL)
    df$Indicateur <- factor(df$Indicateur, levels = rev(unique(df$Indicateur)))
    colors <- vapply(df$key, function(k) IYCF_INDICATORS[[k]]$color, character(1))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Indicateur, y = Prevalence)) +
      ggplot2::geom_bar(stat = "identity", fill = colors, alpha = 0.85, width = 0.7) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = IC_inf, ymax = IC_sup),
                             width = 0.2, color = "#2c3e50", na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(Prevalence), "—",
                                                     paste0(format(round(Prevalence, 1), nsmall = 1), "%"))),
                         hjust = -0.2, size = 3.1, fontface = "bold", na.rm = TRUE) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
      ggplot2::labs(title = "Indicateurs IYCF — OMS/UNICEF 2021",
                    subtitle = iycf_design_label(iycf_res),
                    x = "", y = "Prévalence (%)",
                    caption = "Losange = cible de référence") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 8, color = "#666"),
                     panel.grid.minor = ggplot2::element_blank())
    if (any(!is.na(df$Cible_OMS))) {
      p <- p + ggplot2::geom_point(data = df[!is.na(df$Cible_OMS), , drop = FALSE],
                                   ggplot2::aes(x = Indicateur, y = Cible_OMS),
                                   shape = 18, size = 3.8, color = "#2c3e50",
                                   inherit.aes = FALSE)
    }
    p
  }, error = function(e) NULL)
}

plot_breastfeeding_cascade <- function(iycf_res) {
  tryCatch({
    keys <- c("eibf", "ebf", "cbf")
    df <- iycf_res$summary[iycf_res$summary$key %in% keys, , drop = FALSE]
    df <- df[!is.na(df$Prevalence), , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    ord <- keys[keys %in% df$key]
    df$Indicateur <- factor(df$Indicateur, levels = df$Indicateur[match(ord, df$key)])
    df <- df[order(match(df$key, keys)), , drop = FALSE]
    fills <- vapply(df$key, function(k) IYCF_INDICATORS[[k]]$color, character(1))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Indicateur, y = Prevalence)) +
      ggplot2::geom_col(fill = fills, alpha = 0.88, width = 0.62) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = IC_inf, ymax = IC_sup),
                             width = 0.18, color = "#2c3e50", na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(format(round(Prevalence, 1), nsmall = 1), "%")),
                         vjust = -0.7, fontface = "bold", size = 3.4) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
      ggplot2::labs(title = "Cascade de l'allaitement maternel",
                    subtitle = "EIBF → EBF → CBF ; IC 95 %",
                    x = "", y = "Prévalence (%)") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     panel.grid.minor = ggplot2::element_blank())
    if (any(!is.na(df$Cible_OMS))) {
      p <- p + ggplot2::geom_point(data = df[!is.na(df$Cible_OMS), , drop = FALSE],
                                   ggplot2::aes(x = Indicateur, y = Cible_OMS),
                                   shape = 18, size = 3.8, color = "#2c3e50",
                                   inherit.aes = FALSE)
    }
    p
  }, error = function(e) NULL)
}

plot_iycf_gauge <- function(verdict) {
  if (is.null(verdict) || is.na(verdict$score))
    return(plotly::plotly_empty() |>
             plotly::layout(title = list(text = "Indice IYCF non évaluable", x = 0.5)))
  plotly::plot_ly(
    type = "indicator", mode = "gauge+number",
    value = verdict$score,
    number = list(suffix = "/100", font = list(size = 34, color = verdict$color)),
    title = list(text = paste0("<b>Indice d'atteinte IYCF</b><br><sup>", verdict$class, "</sup>"),
                 font = list(size = 13, color = "#0B3954")),
    gauge = list(
      axis = list(range = c(0, 100), tickwidth = 1, tickcolor = "#666"),
      bar = list(color = verdict$color, thickness = 0.35),
      bg = "white", borderwidth = 1, bordercolor = "#ddd",
      steps = list(
        list(range = c(0, 55), color = "#fdecea"),
        list(range = c(55, 70), color = "#fff3e0"),
        list(range = c(70, 85), color = "#e6f4f5"),
        list(range = c(85, 100), color = "#eafaf1")
      ),
      threshold = list(line = list(color = "#2c3e50", width = 3), thickness = 0.75, value = 85)
    )
  ) |>
    plotly::layout(paper_bgcolor = "white", margin = list(l = 30, r = 30, t = 60, b = 10))
}

plot_iycf_gap <- function(gap_tbl) {
  if (is.null(gap_tbl) || nrow(gap_tbl) == 0)
    return(plotly::plotly_empty() |>
             plotly::layout(title = list(text = "Aucune cible disponible", x = 0.5)))
  df <- gap_tbl[!is.na(gap_tbl$Prevalence) & !is.na(gap_tbl$Cible_OMS) &
                  gap_tbl$Type == "protective", , drop = FALSE]
  if (nrow(df) == 0)
    return(plotly::plotly_empty() |>
             plotly::layout(title = list(text = "Aucun indicateur protecteur avec cible", x = 0.5)))
  df$label <- vapply(df$key, function(k) IYCF_INDICATORS[[k]]$short, character(1))
  df$label <- factor(df$label, levels = df$label[order(df$Prevalence)])
  df$Statut <- factor(df$Statut, levels = c("Atteint", "Non atteint"))
  cols <- c("Atteint" = "#27AE60", "Non atteint" = "#E67E22")
  plotly::plot_ly(
    df, x = ~Prevalence, y = ~label, type = "bar", orientation = "h",
    color = ~Statut, colors = cols,
    text = ~paste0(format(round(Prevalence, 1), nsmall = 1), "%"), textposition = "outside",
    hovertext = ~paste0(Indicateur, "<br>Observé : ", Prevalence, "%<br>Cible : ",
                        Cible_OMS, "%<br>Écart : ", ifelse(is.na(Gap), "—", sprintf("%+.1f", Gap)),
                        " pts<br>n = ", N),
    hoverinfo = "text"
  ) |>
    plotly::add_markers(
      x = df$Cible_OMS, y = df$label, inherit = FALSE,
      marker = list(symbol = "diamond", size = 11, color = "#0B3954",
                    line = list(color = "white", width = 1)),
      name = "Cible de référence", hoverinfo = "skip"
    ) |>
    plotly::layout(
      barmode = "group",
      title = list(text = "<b>Écart aux cibles de référence</b><br><sup>Barres : prévalence observée — losanges : cible</sup>",
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "Prévalence (%)", range = c(0, 105), ticksuffix = "%"),
      yaxis = list(title = "", automargin = TRUE),
      legend = list(orientation = "h", y = -0.2),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 10, r = 40, b = 70)
    )
}

plot_iycf_risk <- function(gap_tbl) {
  if (is.null(gap_tbl) || nrow(gap_tbl) == 0)
    return(plotly::plotly_empty() |>
             plotly::layout(title = list(text = "Aucun indicateur de risque", x = 0.5)))
  df <- gap_tbl[gap_tbl$Type == "risk" & !is.na(gap_tbl$Prevalence), , drop = FALSE]
  if (nrow(df) == 0)
    return(plotly::plotly_empty() |>
             plotly::layout(title = list(text = "Aucun indicateur de risque", x = 0.5)))
  df$label <- vapply(df$key, function(k) IYCF_INDICATORS[[k]]$short, character(1))
  df$label <- factor(df$label, levels = df$label[order(df$Prevalence)])
  plotly::plot_ly(
    df, x = ~Prevalence, y = ~label, type = "bar", orientation = "h",
    marker = list(color = "#C0392B"),
    text = ~paste0(format(round(Prevalence, 1), nsmall = 1), "%"), textposition = "outside",
    hovertext = ~paste0(Indicateur, "<br>", Prevalence, "% (IC 95 % [",
                        IC_inf, "–", IC_sup, "])<br>n = ", N),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = list(text = "<b>Pratiques à risque</b><br><sup>Prévalences — cibles de réduction</sup>",
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "% enfants", range = c(0, max(df$Prevalence) * 1.25 + 2),
                   ticksuffix = "%"),
      yaxis = list(title = "", automargin = TRUE),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 10, r = 30, b = 50)
    )
}

plot_iycf_strat <- function(strat_res) {
  if (is.null(strat_res) || is.null(strat_res$freq)) return(NULL)
  df <- as.data.frame(strat_res$freq)
  df <- df[!is.na(df$pct), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df$Non <- round(100 - df$pct, 1)
  df$group <- factor(df$group, levels = df$group[order(df$pct)])
  t <- strat_res$test %||% list(p = NA_real_, V = NA_real_, note = "—", method = "—")
  plotly::plot_ly(df, x = ~pct, y = ~group, type = "bar", orientation = "h",
                  name = "Atteint", marker = list(color = "#27AE60"),
                  hovertext = ~paste0(group, " : ", k, "/", n, " (", pct, "%)"),
                  hoverinfo = "text") |>
    plotly::add_trace(x = ~Non, name = "Non atteint", marker = list(color = "#E67E22"),
                      hovertext = ~paste0(group, " : ", n - k, "/", n),
                      hoverinfo = "text") |>
    plotly::layout(
      barmode = "stack",
      title = list(text = paste0("<b>", strat_res$label, " selon ", strat_res$var, "</b><br><sup>",
                                 t$note, " · ", iycf_fmt_p(t$p),
                                 if (!is.na(t$V)) paste0(" · V = ", round(t$V, 3)) else "",
                                 " · ", t$method %||% "—", "</sup>"),
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "% enfants", range = c(0, 100), ticksuffix = "%"),
      yaxis = list(title = "", automargin = TRUE),
      legend = list(orientation = "h", y = -0.2),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 10, r = 20, b = 70)
    )
}

plot_iycf_rounds <- function(rounds_res) {
  if (is.null(rounds_res) || is.null(rounds_res$summary)) return(NULL)
  s <- as.data.frame(rounds_res$summary)
  if (nrow(s) == 0) return(NULL)
  s$err <- pmax(s$pct - s$ci_lo, s$ci_hi - s$pct)
  subtitle <- paste0(iycf_fmt_p(rounds_res$p_trend), " · ",
                     rounds_res$trend_method %||% "—", " · écart 1er–dernier : ",
                     ifelse(is.na(rounds_res$diff), "—", sprintf("%+.1f pts", rounds_res$diff)))
  plotly::plot_ly(s, x = ~round, y = ~pct, type = "scatter", mode = "lines+markers",
                  line = list(color = "#0B3954", width = 3),
                  marker = list(size = 10, color = "#2E86AB"),
                  error_y = list(type = "data", array = s$err, color = "#666", thickness = 1.2),
                  text = ~paste0("Round ", round, " : ", pct, "% (", k, "/", n, ")"),
                  hoverinfo = "text") |>
    plotly::layout(
      title = list(text = paste0("<b>", rounds_res$label, " par round</b><br><sup>", subtitle, "</sup>"),
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "Round", type = "category"),
      yaxis = list(title = "Prévalence (%)", range = c(0, 100), ticksuffix = "%"),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 50, r = 20, b = 60)
    )
}

plot_iycf_age <- function(iycf_res, key = NULL) {
  tryCatch({
    ap <- iycf_res$age_profile
    if (is.null(ap) || nrow(ap) == 0) return(NULL)
    ap <- ap[!is.na(ap$Prevalence), , drop = FALSE]
    if (nrow(ap) == 0) return(NULL)
    if (is.null(key) || !key %in% ap$key) key <- ap$key[1]
    df <- ap[ap$key == key, , drop = FALSE]
    df$Groupe_age <- factor(df$Groupe_age, levels = c("0–5", "6–8", "9–11", "12–17", "18–23", "24–59"))
    color <- IYCF_INDICATORS[[key]]$color %||% "#2E86AB"
    ggplot2::ggplot(df, ggplot2::aes(x = Groupe_age, y = Prevalence)) +
      ggplot2::geom_col(fill = color, alpha = 0.85, width = 0.65) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = IC_inf, ymax = IC_sup),
                             width = 0.18, color = "#2c3e50", na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(format(round(Prevalence, 1), nsmall = 1), "%")),
                         vjust = -0.6, size = 3.2, fontface = "bold") +
      ggplot2::scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
      ggplot2::labs(title = paste0(IYCF_INDICATORS[[key]]$label, " selon l'âge"),
                    subtitle = "Prévalence et IC 95 %", x = "Groupe d'âge (mois)",
                    y = "Prévalence (%)") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     panel.grid.minor = ggplot2::element_blank())
  }, error = function(e) NULL)
}

plot_iycf_equity <- function(equity, key = NULL) {
  if (is.null(equity) || nrow(equity) == 0) return(NULL)
  df <- equity
  if (is.null(key) || !key %in% df$key) key <- df$key[1]
  df <- df[df$key == key, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df$group <- factor(df$group, levels = unique(df$group[order(df$pct)]))
  label <- if ("Indicateur" %in% names(df)) df$Indicateur[1] else df$key[1]
  plotly::plot_ly(df, x = ~pct, y = ~group, type = "bar", orientation = "h",
                  marker = list(color = "#2E86AB"),
                  text = ~paste0(format(round(pct, 1), nsmall = 1), "%"),
                  textposition = "outside",
                  hovertext = ~paste0(group, " : ", k, "/", n, " (", pct,
                                      " %) ; écart global : ",
                                      ifelse(is.na(Ecart_pts), "—", sprintf("%+.1f pts", Ecart_pts))),
                  hoverinfo = "text") |>
    plotly::layout(
      title = list(text = paste0("<b>", label, " — équité</b><br><sup>Prévalence par groupe</sup>"),
                   font = list(size = 13, color = "#0B3954"), x = 0),
      xaxis = list(title = "Prévalence (%)", range = c(0, 105), ticksuffix = "%"),
      yaxis = list(title = "", automargin = TRUE),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 10, r = 30, b = 50)
    )
}

render_iycf_kpi <- function(iycf_res) {
  if (is.null(iycf_res) || is.null(iycf_res$summary) || nrow(iycf_res$summary) == 0) return("")
  banner <- sprintf('<div style="font-size:11px;color:#555;background:#f8f9fa;border-radius:8px;padding:7px 11px;margin:4px 6px 10px 6px;">%s</div>',
                    iycf_design_label(iycf_res))
  cards <- lapply(seq_len(nrow(iycf_res$summary)), function(i) {
    r <- iycf_res$summary[i, ]
    meta <- IYCF_INDICATORS[[r$key]]
    color <- meta$color
    target_txt <- if (!is.na(r$Cible_OMS)) {
      sprintf("Cible %s : %s %%", r$Cible_label %||% "de référence", r$Cible_OMS)
    } else "Pas de cible normative"
    status_txt <- if (is.na(r$Prevalence)) {
      "Non évaluable"
    } else if (meta$type == "risk") {
      "Pratique à surveiller"
    } else if (!is.na(r$Cible_OMS)) {
      ifelse(r$Prevalence >= r$Cible_OMS, "Atteint", "Sous la cible")
    } else "—"
    prec <- if (is.na(r$Prevalence)) "—" else format(round(r$Prevalence, 1), nsmall = 1)
    deff_txt <- if (!is.na(r$DEFF)) paste0(" · DEFF ", round(r$DEFF, 2)) else ""
    n_weighted <- if (!is.na(r$N_pondere)) paste0(" · N pondéré ", round(r$N_pondere)) else ""
    sprintf(
      '<div class="zs-kpi-card" style="display:inline-block;width:238px;margin:6px;padding:12px;
       background:%s;color:white;border-radius:10px;text-align:center;font-size:12px;
       vertical-align:top;box-shadow:0 1px 4px rgba(0,0,0,.12);">
       <div style="font-size:11px;opacity:0.95;min-height:30px;">%s</div>
       <div style="font-size:27px;font-weight:bold;">%s%%</div>
       <div>IC 95 %% : [%s — %s] · n = %d%s%s</div>
       <div style="margin-top:3px;">%s · %s</div></div>',
      color, meta$label, prec,
      ifelse(is.na(r$IC_inf), "—", r$IC_inf),
      ifelse(is.na(r$IC_sup), "—", r$IC_sup),
      r$N, deff_txt, n_weighted, target_txt, status_txt
    )
  })
  paste(c(banner, cards), collapse = "\n")
}

cat("module_iycf.R chargé — moteur expert IYCF (14 indicateurs, design, qualité, verdict, exports)\n")



