# ============================================================================
# RAG Nutrition Knowledge Base — NutriAnalyse Pro v2
# ============================================================================
# Base de connaissances domaine-expert pour le chatbot IA
# Sources : OMS, UNICEF, WFP/PAM, Sphere Handbook, SMART, Lancet, STROBE
# Couverture : Malnutrition · Nutrition · Sécurité alimentaire · Stats/Modèles
# ============================================================================

RAG_KNOWLEDGE_BASE <- list(

  # ══════════════════════════════════════════════════════════════════════
  # MALNUTRITION AIGÜE — Définitions & Seuils
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "mam_gam_def",
    domain = "malnutrition",
    keywords = c("gam","mam","sam","malnutrition","aiguë","wasting","émaciation","prévalence"),
    content = "La Malnutrition Aigüe Globale (MAG/GAM) est définie par un Z-score Poids-pour-Taille (WHZ) < -2 ET/OU un Périmètre Brachial (PB/MUAC) < 125 mm ET/OU la présence d'œdèmes bilatéraux. La Malnutrition Aigüe Modérée (MAM) : WHZ entre -3 et -2 OU PB entre 115 et 125 mm (sans œdème). La Malnutrition Aigüe Sévère (MAS/SAM) : WHZ < -3 OU PB < 115 mm OU présence d'œdèmes bilatéraux. Ces seuils s'appliquent aux enfants de 6 à 59 mois.",
    source = "OMS/UNICEF. Management of Severe Acute Malnutrition in Children. 2013. | SMART Methodology."
  ),
  list(
    id = "gam_thresholds",
    domain = "malnutrition",
    keywords = c("seuil","critique","urgence","prévalence","gam","acceptable","alerte","sérieux"),
    content = "Seuils OMS de prévalence de la MAG pour classification de l'urgence nutritionnelle (enquêtes SMART) : < 5% = Acceptable ; 5–9,9% = Alerte ; 10–14,9% = Sérieux ; 15–29,9% = Critique ; ≥ 30% = Urgence extrême. Ces seuils sont applicables aux enquêtes en population dans les pays à revenu faible/moyen. Une prévalence ≥ 15% nécessite une réponse humanitaire d'urgence immédiate selon les standards Sphere.",
    source = "OMS. The Management of Nutrition in Major Emergencies. 2000. | Sphere Handbook 2018, chapitre Nutrition."
  ),
  list(
    id = "sam_treatment",
    domain = "malnutrition",
    keywords = c("traitement","sam","mas","atpe","rutf","crenas","urenas","protocole","prise en charge"),
    content = "Le traitement de la MAS non-compliquée se fait en ambulatoire (CRENAS/OTP) avec les Aliments Thérapeutiques Prêts à l'Emploi (ATPE/RUTF), comme le Plumpy'Nut®. La dose est de 200 kcal/kg/j. Le traitement de la MAS avec complications médicales (anorexie, œdèmes +++, infections sévères) se fait en hospitalisé (CRENI/SC). La MAS non-compliquée peut être traitée avec un taux de guérison > 75%, selon les critères SPHERE.",
    source = "OMS/UNICEF/PAM/UNSSCN. Community-based Management of Severe Acute Malnutrition. 2007."
  ),
  list(
    id = "muac_thresholds",
    domain = "malnutrition",
    keywords = c("muac","pb","périmètre","brachial","vert","jaune","rouge","seuil","enfant","femme","admissions"),
    content = "Seuils MUAC/PB pour les enfants 6–59 mois : ≥ 125 mm = Vert (normal) ; 115–124 mm = Jaune (MAM) ; < 115 mm = Rouge (MAS). Critères d'admission en CRENAS : PB < 125 mm + pas d'œdèmes. Critères d'admission en CRENI : PB < 115 mm OU œdèmes. Pour les femmes enceintes et allaitantes (FEFA) : MUAC < 210 mm = malnutrition ; < 190 mm = MAS. Le MUAC est recommandé pour le dépistage communautaire car plus rapide et moins sujet aux erreurs.",
    source = "UNICEF/OMS. MUAC-for-Age Z-Score. 2012. | ACF International. Protocoles nutritionnels."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # Z-SCORES OMS 2006
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "zscore_interpretation",
    domain = "malnutrition",
    keywords = c("zscore","z-score","whz","haz","waz","sd","écart-type","interprétation","flagging","plausibilité"),
    content = "Les Z-scores OMS 2006 expriment la position d'un enfant par rapport à la population de référence OMS (en écarts-types). WHZ (Poids-pour-Taille) = émaciation (wasting) ; HAZ (Taille-pour-Âge) = retard de croissance (stunting) ; WAZ (Poids-pour-Âge) = insuffisance pondérale (underweight). Seuils : < -2 SD = malnutrition modérée ; < -3 SD = malnutrition sévère. SMART Flags pour plausibilité : WHZ doit être entre -5 et +5 ; HAZ entre -6 et +6 ; WAZ entre -6 et +5. Les valeurs hors flags sont exclues de l'analyse. Un écart-type (SD) des Z-scores WHZ dans une enquête saine doit être proche de 1,0 (acceptable : 0,8–1,2).",
    source = "OMS. WHO Child Growth Standards. Genève, 2006. | SMART Methodology. Plausibility Checks."
  ),
  list(
    id = "stunting_def",
    domain = "malnutrition",
    keywords = c("stunting","retard","croissance","haz","malnutrition","chronique","taille","âge"),
    content = "Le retard de croissance (stunting) est défini par un Z-score Taille-pour-Âge (HAZ) < -2 SD. C'est un indicateur de malnutrition chronique reflétant des carences nutritionnelles prolongées et/ou des infections répétées. La prévalence mondiale est d'environ 22% (2022, UNICEF). Seuils OMS : < 20% = Faible ; 20–29,9% = Moyen ; 30–39,9% = Élevé ; ≥ 40% = Très élevé. Le stunting est irréversible après 2 ans et a des conséquences à long terme sur le développement cognitif.",
    source = "OMS/UNICEF/BM. Niveaux et tendances de la malnutrition infantile. 2023. | Black et al., Lancet 2013."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # ENQUÊTES SMART
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "smart_methodology",
    domain = "epidemiology",
    keywords = c("smart","enquête","survey","taille","échantillon","cluster","deff","plausibilité","qualité"),
    content = "La méthodologie SMART (Standardized Monitoring and Assessment of Relief and Transitions) est le standard pour les enquêtes nutritionnelles d'urgence. Elle utilise un sondage en grappes à 2 degrés (30 clusters × N ménages). La taille d'échantillon recommandée pour estimer la prévalence MAG ±3% (IC95%) avec DEFF=1,5 est d'environ 600 enfants. Le plausibility check SMART évalue : taux de flagging (<2,5%), écart-type des Z-scores (WHZ, HAZ, WAZ proches de 1), préférence aux chiffres ronds (p>0,05), age ratio (garçons/filles ≈1), sex ratio.",
    source = "SMART Initiative. Measuring and Interpreting Malnutrition and Mortality. 2nd edition, 2012."
  ),
  list(
    id = "deff_design_effect",
    domain = "epidemiology",
    keywords = c("deff","design effect","effet de grappe","cluster","variance","ic95","intervalle confiance"),
    content = "L'Effet de Design (DEFF) quantifie l'augmentation de variance due à l'échantillonnage en grappes par rapport à un sondage aléatoire simple. DEFF = 1 = pas d'effet de grappe. Pour les enquêtes nutritionnelles, DEFF typique = 1,5–2,0. Un DEFF > 3 indique une hétérogénéité importante entre clusters. La taille d'échantillon doit être multipliée par le DEFF. Les intervalles de confiance calculés sans tenir compte du DEFF sont sous-estimés, ce qui gonfle artificiellement la précision.",
    source = "Cochran, W.G. Sampling Techniques (3ème éd.). Wiley, 1977. | SMART Methodology."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # SÉCURITÉ ALIMENTAIRE — FCS, rCSI, HDDS, HHS
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "fcs_definition",
    domain = "food_security",
    keywords = c("fcs","food consumption score","score","alimentaire","consommation","seuil","pauvre","limite","acceptable"),
    content = "Le Food Consumption Score (FCS) est un indicateur composite de la sécurité alimentaire des ménages, développé par le PAM. Il est calculé en multipliant la fréquence de consommation sur 7 jours par des poids nutritionnels : Céréales (2), Légumineuses (3), Légumes (1), Fruits (1), Viande/Poisson/Œufs (4), Lait/Produits laitiers (4), Sucre (0,5), Huile (0,5). Seuils standards PAM : FCS ≤ 21 = Pauvre ; 21,5–35 = Limite (borderline) ; > 35 = Acceptable. En populations avec forte consommation de sucre/huile, des seuils alternatifs peuvent être utilisés (FCS ≤ 28 = Pauvre ; > 42 = Acceptable).",
    source = "PAM/WFP. Food Consumption Score Nutritional Quality Analysis (FCS-N). VAM Technical Guidance, 2011."
  ),
  list(
    id = "rcsi_definition",
    domain = "food_security",
    keywords = c("rcsi","coping","stratégies","adaptation","alimentation","insécurité","score","index"),
    content = "Le reduced Coping Strategy Index (rCSI) mesure les stratégies d'adaptation alimentaire des ménages face à l'insécurité alimentaire. Il comprend 5 comportements pondérés : Manger des aliments moins préférés (1), Emprunter de la nourriture ou dépendre de l'aide (2), Réduire les portions (1), Réduire le nombre de repas par jour (1), Restreindre la consommation des adultes pour les enfants (3). Score calculé sur 7 jours. Un rCSI ≥ 19 indique une insécurité alimentaire élevée. Plus le score est élevé, plus la situation est critique.",
    source = "Maxwell et al. Measuring Food Insecurity: The Frequency and Severity of Coping Strategies. Food Policy, 1999. | PAM Guide rCSI."
  ),
  list(
    id = "hdds_definition",
    domain = "food_security",
    keywords = c("hdds","diversité","alimentaire","ménage","groupes","aliments","score"),
    content = "Le Household Dietary Diversity Score (HDDS) mesure la diversité alimentaire des ménages. Il compte le nombre de groupes d'aliments consommés sur 24h parmi 12 groupes : Céréales, Racines/tubercules, Légumes, Fruits, Viande/volaille, Œufs, Poisson/fruits de mer, Légumineuses/noix, Lait/produits laitiers, Huiles/graisses, Sucre/miel, Épices/condiments/boissons. Score 0–12. Un HDDS ≤ 3 = faible diversité ; 4–5 = diversité moyenne ; ≥ 6 = bonne diversité. C'est un proxy de la qualité nutritionnelle du régime.",
    source = "FAO. Measuring Household Food Insecurity. Guidelines for Measuring Household and Individual Dietary Diversity. FAO, 2011."
  ),
  list(
    id = "hhs_definition",
    domain = "food_security",
    keywords = c("hhs","household hunger scale","faim","ménage","sévérité","score","insécurité"),
    content = "Le Household Hunger Scale (HHS) mesure la sévérité de la faim au niveau du ménage. Basé sur 3 questions avec fréquence (Rarement=1, Parfois=2, Souvent/Toujours=3) : (1) Aucune nourriture à manger ; (2) Se coucher le soir sans manger ; (3) Toute la journée et nuit sans manger. Score 0–6 : 0–1 = Pas de faim ; 2–3 = Faim modérée ; 4–6 = Faim sévère. Le HHS est robuste culturellement et validé dans 30 pays.",
    source = "Deitchler et al. Validation of a Measure of Household Hunger for Cross-Cultural Use. FHI 360/FANTA, 2010."
  ),
  list(
    id = "ipc_phases",
    domain = "food_security",
    keywords = c("ipc","cadre harmonisé","phase","insécurité","crise","urgence","famine","classification","ch"),
    content = "Le Cadre Intégré de Classification de la Sécurité Alimentaire (IPC/CH) classe la situation alimentaire en 5 phases : Phase 1 (Minimale/OK) ; Phase 2 (Sous pression) ; Phase 3 (Crise) ; Phase 4 (Urgence) ; Phase 5 (Catastrophe/Famine). Une Famine (Phase 5) est officiellement déclarée si : ≥ 20% des ménages souffrent de faim extrême, MAG > 30%, ≥ 2 décès/10 000 personnes/jour. L'IPC intègre sécurité alimentaire, malnutrition, mortalité et accès aux marchés.",
    source = "IPC Global Partners. Integrated Food Security Phase Classification Technical Manual v3.1. Rome, 2021."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # MICRONUTRIMENTS & ANÉMIE
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "anemia_def",
    domain = "malnutrition",
    keywords = c("anémie","hémoglobine","fer","carence","seuil","enfant","femme","enceinte","adulte","thalassémie"),
    content = "L'anémie est définie par un taux d'hémoglobine (Hb) inférieur au seuil OMS selon l'âge et le sexe. Seuils (g/dL) : Enfants 6–59 mois = < 11 ; Enfants 5–11 ans = < 11,5 ; Enfants 12–14 ans = < 12 ; Femmes non enceintes ≥ 15 ans = < 12 ; Femmes enceintes = < 11 ; Hommes ≥ 15 ans = < 13. Sévérité : Légère (10–11,9 g/dL), Modérée (7–9,9 g/dL), Sévère (< 7 g/dL). La carence en fer est la cause la plus fréquente (50% des anémies), suivie du paludisme et des helminthes en zones tropicales.",
    source = "OMS. Haemoglobin Concentrations for the Diagnosis of Anaemia and Assessment of Severity. 2011."
  ),
  list(
    id = "vitamin_a_def",
    domain = "malnutrition",
    keywords = c("vitamine","vitaminea","rétinol","xerophtalmie","carence","supplémentation","micronutriment"),
    content = "La carence en vitamine A (CVA) est définie par un rétinol sérique < 0,70 µmol/L (carence subclinique) ou < 0,35 µmol/L (carence sévère). La prévalence CVA ≥ 15% = problème de santé publique modéré ; ≥ 20% = sévère. La CVA augmente la mortalité par rougeole, diarrhée, paludisme. La supplémentation en vitamine A deux fois par an (100 000 UI < 12 mois, 200 000 UI 12–59 mois) réduit la mortalité infantile de 24% (méta-analyse Cochrane). Indicateurs cliniques : xérophtalmie (sécheresse conjonctivale), tache de Bitot.",
    source = "OMS. Global Prevalence of Vitamin A Deficiency in Populations at Risk 1995–2005. | Imdad et al. Cochrane Review, 2017."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # ALLAITEMENT & ALIMENTATION DU JEUNE ENFANT (ANJE/IYCF)
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "breastfeeding",
    domain = "malnutrition",
    keywords = c("allaitement","sein","maternel","exclusif","iycf","anje","alimentation","nourrisson","complémentaire","sevrage"),
    content = "L'OMS recommande : (1) Allaitement exclusif pendant les 6 premiers mois (aucun autre aliment ni liquide sauf médicaments/vitamines) ; (2) Introduction d'aliments de complément à 6 mois avec maintien de l'allaitement jusqu'à 2 ans ou plus. Indicateurs IYCF clés : Taux d'allaitement exclusif < 6 mois (cible ≥ 50%) ; Diversité alimentaire minimale (≥ 4 groupes sur 24h) ; Fréquence minimale des repas ; Régime alimentaire minimum acceptable. L'allaitement exclusif réduit la mortalité infantile de 13% (Lancet Nutrition Series).",
    source = "OMS/UNICEF. Indicators for Assessing Infant and Young Child Feeding Practices. 2010. | Black et al., Lancet 2013."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # ÉPIDÉMIOLOGIE & BIOSTATISTIQUE
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "or_rr_interpretation",
    domain = "epidemiology",
    keywords = c("odds ratio","risque relatif","or","rr","association","interprétation","ic95","confiance","exposition"),
    content = "Odds Ratio (OR) : ratio des cotes entre exposés et non-exposés. Utilisé dans les études cas-témoins. OR > 1 = facteur de risque ; OR < 1 = facteur protecteur ; OR = 1 = pas d'association. Risque Relatif (RR) : ratio des probabilités entre exposés et non-exposés. Utilisé dans les cohortes et essais. Pour les événements rares (< 10%), OR ≈ RR. Intervalles de confiance à 95% : si l'IC ne croise pas 1, l'association est statistiquement significative (p < 0,05). Attention : significativité statistique ≠ pertinence clinique. Toujours rapporter l'IC95% avec OR/RR.",
    source = "Rothman K.J. Epidemiology: An Introduction, 2nd ed. Oxford University Press, 2012. | STROBE Statement."
  ),
  list(
    id = "prevalence_incidence",
    domain = "epidemiology",
    keywords = c("prévalence","incidence","taux","morbidité","mortalité","transversale","cohorte","période","ponctuelle"),
    content = "Prévalence (ponctuelle) = Nombre de cas existants / Population totale × 100 (ou × 10 000). Mesure la charge de maladie à un instant T. Incidence (taux) = Nouveaux cas / Personnes-temps × 10 000. Mesure le risque de développer la maladie. Dans les enquêtes transversales nutritionnelles, on mesure la prévalence de la malnutrition. La prévalence d'une maladie courte (comme la MAG) ≈ Incidence × Durée moyenne de l'épisode. Mortalité brute = Décès toutes causes / 10 000 personnes/jour (seuil urgence = 1/10 000/j ; > 2 = urgence extrême).",
    source = "Last, J.M. Dictionary of Epidemiology, 5th ed. Oxford, 2008. | Sphere Handbook 2018."
  ),
  list(
    id = "sample_size_power",
    domain = "epidemiology",
    keywords = c("taille","échantillon","puissance","power","statistique","erreur","type","alpha","beta","détection"),
    content = "La puissance statistique (1-β) est la probabilité de détecter un effet réel. Conventions : puissance ≥ 80%, erreur de type I (α) = 5%. Formule simplifiée pour 2 proportions : n = [z_α/2 + z_β]² × [p1(1-p1) + p2(1-p2)] / (p1-p2)². Avec DEFF, multiplier n par DEFF. Les erreurs de type I (faux positif, α) et type II (faux négatif, β) sont inversement liées. Augmenter la taille d'échantillon réduit les deux erreurs. Un test sous-dimensionné manque les effets réels ; un test sur-dimensionné détecte des différences cliniquement insignifiantes.",
    source = "Lwanga SK, Lemeshow S. Sample Size Determination in Health Studies. OMS, 1991. | Cohen J. Statistical Power Analysis for the Behavioral Sciences, 2nd ed."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # STATISTIQUES — TESTS & RÉGRESSIONS
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "t_test_anova",
    domain = "statistics",
    keywords = c("t-test","student","anova","comparaison","moyennes","normalité","hypothèse","variance","groupe"),
    content = "Le test t de Student compare les moyennes de 2 groupes (données normales). Conditions : normalité (Shapiro-Wilk, p > 0,05) ET homogénéité des variances (test de Levene). Si conditions non remplies : test de Mann-Whitney (non-paramétrique). L'ANOVA compare ≥ 3 groupes ; si significative, tests post-hoc (Tukey, Bonferroni) identifient les paires différentes. Rappel : p-value = probabilité d'obtenir les données si H₀ est vraie. Ne signifie pas que l'hypothèse alternative est vraie. Toujours rapporter : statistique du test, degrés de liberté, p-value, taille d'effet (Cohen's d).",
    source = "Field A. Discovering Statistics using IBM SPSS Statistics, 5th ed. SAGE, 2018."
  ),
  list(
    id = "chi_square_test",
    domain = "statistics",
    keywords = c("chi","khi","carré","test","association","contingence","catégorielle","proportion","fréquence","fisher"),
    content = "Le test du Chi-carré (χ²) teste l'indépendance entre 2 variables catégorielles. Conditions : effectifs théoriques ≥ 5 dans chaque cellule (sinon : test exact de Fisher). Formule : χ² = Σ (Observé - Attendu)² / Attendu. Degré de liberté = (lignes-1)(colonnes-1). Si p < 0,05 : association significative. Taille d'effet : V de Cramér = √(χ²/n × min(r-1, c-1)). Interprétation V : 0,1 = faible, 0,3 = moyen, ≥ 0,5 = fort. Pour tableau 2×2 avec N < 5 dans une cellule, utiliser test exact de Fisher.",
    source = "Field A. Discovering Statistics using IBM SPSS Statistics, 5th ed. SAGE, 2018. | Agresti A. Categorical Data Analysis. Wiley, 2002."
  ),
  list(
    id = "logistic_regression",
    domain = "statistics",
    keywords = c("régression","logistique","odds ratio","or","coefficient","prédiction","binaire","probabilité","logit"),
    content = "La régression logistique modélise la probabilité d'un événement binaire (0/1). Les coefficients β sont sur l'échelle du log-odds. L'OR = exp(β). Interprétation : pour une augmentation d'une unité de X, l'odds de Y est multiplié par exp(β). Pour les variables continues, OR s'interprète par unité. Qualité du modèle : AUC/C-statistic (0,5 = hasard, > 0,7 = acceptable, > 0,8 = bien, > 0,9 = excellent), test de Hosmer-Lemeshow (calibration, p > 0,05 = bonne calibration). Vérifier multicolinéarité (VIF < 10), taille d'échantillon suffisante (≥ 10 événements par variable).",
    source = "Hosmer DW, Lemeshow S. Applied Logistic Regression, 3rd ed. Wiley, 2013."
  ),
  list(
    id = "linear_regression",
    domain = "statistics",
    keywords = c("régression","linéaire","r2","coefficient","résidu","ols","prédiction","variable","continue","interprétation"),
    content = "La régression linéaire modélise Y = β₀ + β₁X₁ + ... + ε. Hypothèses : linéarité, homoscédasticité (variance constante des résidus), normalité des résidus, indépendance. Diagnostic : graphique résidus vs valeurs ajustées (homoscédasticité), Q-Q plot (normalité), Cook's distance (points influents > 4/n). R² = proportion de variance expliquée (R² ajusté pénalise l'ajout de variables). RMSE = racine carrée de l'erreur quadratique moyenne. Coefficient β : pour une augmentation d'une unité de X, Y augmente de β unités toutes choses égales. Attention à la multicolinéarité (VIF > 10 = problème).",
    source = "Montgomery DC et al. Introduction to Linear Regression Analysis, 5th ed. Wiley, 2012."
  ),
  list(
    id = "pca_interpretation",
    domain = "statistics",
    keywords = c("acp","pca","composante","principale","variance","eigenvalue","biplot","valeur","propre","inertie","facto"),
    content = "L'Analyse en Composantes Principales (ACP) réduit la dimensionnalité en trouvant les directions de variance maximale. Interprétation : (1) Eigenvalues (valeurs propres) > 1 = règle de Kaiser pour sélection des axes ; (2) % variance expliquée par axe (cumulé) ; (3) Contributions des variables aux axes (cos² = qualité de représentation) ; (4) Biplot : variables bien représentées si cos² > 0,6. Axes orthogonaux → variables projections non-corrélées. Attention : ACP centrée-réduite obligatoire si variables d'échelles différentes. Score factoriel = projection d'individus sur les axes.",
    source = "Jolliffe IT. Principal Component Analysis, 2nd ed. Springer, 2002. | Lê S et al. FactoMineR. J. Stat. Soft. 2008."
  ),
  list(
    id = "clustering_methods",
    domain = "statistics",
    keywords = c("cluster","clustering","kmeans","hiérarchique","dendrogramme","silhouette","k","groupes","classification"),
    content = "Le clustering K-means partitionne les données en K groupes en minimisant la variance intra-groupe. Choisir K optimal : méthode du coude (Elbow), indice de silhouette (valeur 0,5 à 0,7 = structure raisonnable, > 0,7 = forte structure). Le clustering hiérarchique (CAH) crée un dendrogramme ; méthodes de liaison : Ward (minimise variance, recommandé pour données continues), Complete, Average. La silhouette moyenne > 0,5 indique un clustering acceptable. Valider avec bootstrap ou indices externes (Rand, Jaccard). Les clusters doivent être interprétables contextuellement, pas seulement statistiquement.",
    source = "Kaufman L, Rousseeuw PJ. Finding Groups in Data. Wiley, 1990. | Charrad et al. NbClust package R."
  ),
  list(
    id = "multiple_testing",
    domain = "statistics",
    keywords = c("tests","multiples","correction","bonferroni","benjamini","fdr","inflation","erreur","alpha","familywise"),
    content = "Lorsqu'on réalise k tests simultanés, la probabilité d'au moins une erreur de type I augmente : P(au moins 1 faux positif) = 1 - (1-α)^k. Pour α=0,05 et 20 tests : P ≈ 64%. Corrections : (1) Bonferroni : α_ajusté = α/k (conservatrice) ; (2) Benjamini-Hochberg (FDR) : contrôle le taux de faux positifs (moins conservative, recommandée en découverte). En pratique : définir les comparaisons a priori, distinguer analyses exploratoires (pas de correction obligatoire) et confirmatoires (correction requise). Toujours rapporter les p-values brutes ET ajustées.",
    source = "Benjamini Y, Hochberg Y. Controlling the False Discovery Rate. J. R. Stat. Soc. B, 1995. | Westfall PH, Young SS. Resampling-Based Multiple Testing. Wiley, 1993."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # INTERPRÉTABILITÉ DES MODÈLES (ML/Statistique)
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "model_metrics",
    domain = "statistics",
    keywords = c("auc","roc","accuracy","précision","rappel","f1","rmse","mae","performance","modèle","classification","prédiction"),
    content = "Métriques de performance des modèles : CLASSIFICATION : AUC-ROC (0,5=hasard, >0,7 acceptable, >0,8 bien, >0,9 excellent) ; Accuracy = (VP+VN)/(total) — biaisée si classes déséquilibrées ; Précision = VP/(VP+FP) ; Rappel/Sensibilité = VP/(VP+FN) ; F1 = 2×Précision×Rappel/(Précision+Rappel) ; Kappa de Cohen (>0,6 bien). RÉGRESSION : RMSE (même unité que Y, penalise outliers) ; MAE (plus robuste) ; R² (0 à 1) ; MAPE (en %). Toujours évaluer sur données TEST (jamais sur données d'entraînement). Préférer validation croisée (k-fold) si échantillon limité.",
    source = "Hastie T, Tibshirani R, Friedman J. The Elements of Statistical Learning. Springer, 2009. | Kuhn M, Johnson K. Applied Predictive Modeling. Springer, 2013."
  ),
  list(
    id = "shap_feature_importance",
    domain = "statistics",
    keywords = c("shap","importance","variable","xgboost","forêt","random forest","interprétabilité","explication","feature","contribution"),
    content = "L'interprétabilité des modèles complexes (Random Forest, XGBoost, etc.) utilise plusieurs approches : (1) Feature Importance (Gini/Gain) : contribution moyenne de chaque variable aux décisions de l'arbre. (2) SHAP (SHapley Additive exPlanations) : valeurs de Shapley attribuant la contribution de chaque variable à chaque prédiction individuelle. SHAP positif = augmente la prédiction ; négatif = diminue. Avantage de SHAP : additivité, cohérence, invariance par rapport aux permutations. (3) PDP (Partial Dependence Plots) : effet marginal d'une variable sur la prédiction. (4) LIME : approximation locale linéaire du modèle.",
    source = "Lundberg SM, Lee SI. A Unified Approach to Interpreting Model Predictions. NIPS, 2017. | Ribeiro et al. LIME. KDD, 2016."
  ),
  list(
    id = "overfitting_validation",
    domain = "statistics",
    keywords = c("overfitting","surapprentissage","validation","cross","croisée","train","test","généralisation","régularisation","biais"),
    content = "L'overfitting (surapprentissage) = modèle qui mémorise les données d'entraînement mais ne généralise pas. Signes : performance train >> performance test. Solutions : (1) Division train/validation/test (60-20-20 ou 70-15-15) ; (2) Cross-validation k-fold (typiquement k=5 ou 10) ; (3) Régularisation : Ridge (L2), Lasso (L1), Elastic Net ; (4) Early stopping (réseaux de neurones) ; (5) Dropout (deep learning). Le biais-variance tradeoff : modèles simples = haut biais (underfitting) ; modèles complexes = haute variance (overfitting). Objectif : trouver l'équilibre via la complexité optimale.",
    source = "Hastie T, Tibshirani R, Friedman J. The Elements of Statistical Learning. Springer, 2009."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # MORTALITÉ & ÉPIDÉMIO DE TERRAIN
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "mortality_rates",
    domain = "epidemiology",
    keywords = c("mortalité","décès","taux","crude","u5mr","infant","sous-5","rétrospective","rappel","fdeath"),
    content = "Le Taux Brut de Mortalité (TBM/CMR) est exprimé en décès/10 000 personnes/jour. Méthode SMART : période de rappel = 90 jours (3 mois). Seuils : TBM < 0,5 = acceptable ; 0,5–0,99 = alerte ; 1,0–1,99 = urgence ; ≥ 2 = urgence extrême/catastrophe. Taux de mortalité ≤ 5 ans (U5MR) : seuils multipliés par 2 (urgence extrême si ≥ 4). La mortalité rétrospective est sujette aux biais de mémoire et de changement de ménage. La méthode des survivants et des arrivants permet de corriger la taille du ménage pendant la période de rappel.",
    source = "SMART Initiative. Measuring and Interpreting Malnutrition and Mortality. 2012. | Sphere Handbook 2018."
  ),
  list(
    id = "wash_nutrition_link",
    domain = "malnutrition",
    keywords = c("wash","eau","assainissement","hygiène","diarrhée","infection","malnutrition","environnement","eet"),
    content = "Le lien WASH-Nutrition est documenté : 50% de la malnutrition infantile est associée à l'eau, l'assainissement et l'hygiène insuffisants (UNICEF, 2013). L'Environmental Enteric Dysfunction (EED) est un mécanisme clé : contamination fécale-orale → inflammation intestinale chronique → malabsorption → malnutrition réfractaire. Le stunting résiduel dans les pays où le régime alimentaire est adéquat est souvent attribuable à l'EED. Indicateurs WASH en lien avec nutrition : source d'eau améliorée, latrines améliorées, pratiques de lavage des mains, contamination fécale des aliments.",
    source = "Cumming O, Cairncross S. Can water, sanitation and hygiene help eliminate stunting? Current evidence and policy implications. Maternal & Child Nutrition, 2016."
  ),
  list(
    id = "lancet_nutrition_determinants",
    domain = "malnutrition",
    keywords = c("déterminants","malnutrition","causes","unicef","conceptuel","modèle","immédiate","sous-jacente","profonde","lancet"),
    content = "Le cadre conceptuel UNICEF (1990, actualisé Lancet 2013) identifie 3 niveaux de causes de la malnutrition : (1) CAUSES IMMÉDIATES : alimentation inadéquate + maladies infectieuses répétées ; (2) CAUSES SOUS-JACENTES : insécurité alimentaire des ménages, pratiques inadéquates de soins, environnement inadéquat (eau/assainissement/santé) ; (3) CAUSES PROFONDES : pauvreté, inégalités, conflits, gouvernance. Les interventions doivent cibler tous les niveaux. Les 'interventions spécifiques à la nutrition' (alimentation, micronutriments) ne peuvent pas à elles seules éradiquer la malnutrition sans les 'interventions sensibles à la nutrition' (agriculture, protection sociale, WASH).",
    source = "UNICEF. Strategy for Improved Nutrition of Children and Women. 1990. | Black et al. Lancet Nutrition Series, 2013."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # PROTOCOLES PAYS — CAMEROUN & AFRIQUE CENTRALE/OUEST
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "cmam_protocol",
    domain = "malnutrition",
    keywords = c("cmam","imam","ambulatoire","communautaire","intégré","prise en charge","protocole","programme","mam","sam"),
    content = "La CMAM (Community-based Management of Acute Malnutrition) est l'approche standard OMS/UNICEF/PAM depuis 2007. Elle comprend 4 composantes : (1) Dépistage communautaire actif (DCA) avec rubans MUAC ; (2) Soins ambulatoires MAS non-compliquées (OTP/CRENAS) avec ATPE ; (3) Soins hospitaliers MAS compliquées (SC/CRENI) avec F-75/F-100 ; (4) Traitement MAM (SFP/CRENAM) avec suppléments. L'approche CMAM a multiplié par 10 la couverture des traitements vs centres hospitaliers uniquement. Standards de performance CMAM : taux de guérison ≥ 75% ; taux d'abandon ≤ 15% ; taux de décès ≤ 10%.",
    source = "OMS/UNICEF/PAM/UNSSCN. Community-based Management of SAM. Joint Statement, 2007. | VALID International. CMAM Forum."
  ),
  list(
    id = "crenam_cameroun",
    domain = "malnutrition",
    keywords = c("crenam","cameroun","mam","ambulatoire","modéré","supplémentaire","pbma","réhabilitation","farine","bouillie","plumpy","sup"),
    content = "Au Cameroun, le CRENAM (Centre de Réhabilitation Nutritionnelle Ambulatoire pour les Modérés) prend en charge la MAM. Critères d'admission : PB 115–124 mm sans complication OU FEFA avec MUAC < 210 mm. Ration PRONANUT Cameroun : Plumpy'Sup® (92g/j = 500 kcal) ou Farine Maïs-Soja Enrichie (FMSE). Durée : 8–12 semaines, visites hebdomadaires. Critères de guérison : PB ≥ 125 mm + gain pondéral ≥ 15% + absence d'œdèmes pendant 2 semaines. Critères de transfert vers CRENAS : PB < 115 mm OU apparition d'œdèmes OU appétit négatif.",
    source = "MINSANTE Cameroun / PRONANUT. Protocole National de Prise en Charge de la Malnutrition Aigüe, édition 2022. | UNICEF Cameroun."
  ),
  list(
    id = "crenas_cameroun",
    domain = "malnutrition",
    keywords = c("crenas","cameroun","sam","ambulatoire","sévère","atpe","rutf","plumpy","nut","admission","sortie","guérison","abandon"),
    content = "Le CRENAS (Centre de Réhabilitation Nutritionnelle Ambulatoire pour les Sévères) au Cameroun traite la MAS non-compliquée. Critères d'admission : PB < 115 mm OU œdèmes 1+/2+ SANS complications ET test d'appétit positif (≥ 1/4 sachet ATPE en 30 min). Ration ATPE : 200 kcal/kg/j (2–3 sachets Plumpy'Nut® 92g/j selon poids). Visites hebdomadaires. Critères de guérison : PB ≥ 125 mm + gain ≥ 15% + absence d'œdèmes 2 semaines + appétit positif. Transfert CRENI si : anorexie, œdèmes 3+, complications médicales.",
    source = "MINSANTE Cameroun / PRONANUT. Protocole National de Prise en Charge de la Malnutrition Aigüe, édition 2022."
  ),
  list(
    id = "creni_cameroun",
    domain = "malnutrition",
    keywords = c("creni","cameroun","hospitalier","f75","f100","stabilisation","transition","réhabilitation","complications","œdème","anorexie","inpatient"),
    content = "Le CRENI (Centre de Réhabilitation Nutritionnelle Intensive) traite la MAS compliquée (anorexie, œdèmes 3+, infections sévères). PHASE 1 — Stabilisation (J1–J7) : F-75 (75 kcal/100ml), 8 repas/j, ReSoMal (pas SRO standard), antibiotiques (Amoxicilline ± Métronidazole), sucre entre repas (hypoglycémie). PHASE DE TRANSITION (J7–J14) : F-75 → F-100 ou ATPE si appétit retrouvé. PHASE 2 — Réhabilitation : F-100 (100 kcal/100ml) ou ATPE, 150–200 kcal/kg/j. Supplémentation systématique : Vitamine A (J1), acide folique, zinc, fer (Phase 2 uniquement). Transfert CRENAS si appétit positif + complications résolues.",
    source = "MINSANTE Cameroun / PRONANUT. Protocole National 2022. | OMS. Prise en Charge de la Malnutrition Sévère. 1999."
  ),
  list(
    id = "pronanut_cameroun",
    domain = "malnutrition",
    keywords = c("pronanut","cameroun","programme","national","nutrition","minsante","politique","pamsn","plan","action","coordination"),
    content = "Le PRONANUT (Programme National de Nutrition, MINSANTE Cameroun) coordonne toutes les interventions nutritionnelles nationales : validation des protocoles CRENAM/CRENAS/CRENI, supervision des enquêtes SMART, gestion de la chaîne d'approvisionnement en intrants (ATPE, FMSE, F-75, F-100). Le Plan d'Action Multisectoriel de Nutrition (PAMSN) 2020–2024 vise : MAG nationale < 5%, retard de croissance < 25%, allaitement exclusif > 50%. Les régions prioritaires sont l'Extrême-Nord, le Nord, l'Adamaoua et l'Est.",
    source = "MINSANTE Cameroun. Plan d'Action Multisectoriel de Nutrition (PAMSN) 2020–2024. Yaoundé, 2020."
  ),
  list(
    id = "tsfp_bsfp_programs",
    domain = "malnutrition",
    keywords = c("tsfp","bsfp","sfp","supplémentaire","blanket","ciblé","prévention","saisonnier","plumpy","sup","programme"),
    content = "Programmes d'alimentation supplémentaire (SFP) : (1) TSFP (Targeted SFP) — ciblé enfants MAM + FEFA malnutries, Plumpy'Sup® 500 kcal/j, 8–12 semaines ; (2) BSFP (Blanket SFP) — distribué à tous les 6–59 mois + FEFA indépendamment du statut, utilisé lors de pics saisonniers ou crise alimentaire sévère. Au Cameroun (Extrême-Nord, Adamaoua, Nord), les BSFP sont déployés pendant la soudure (juin–septembre). Plumpy'Sup® : 92g = 500 kcal, 15g protéines + micronutriments.",
    source = "PAM/WFP. Supplementary Feeding Programmes: Operational Guidance. Rome, 2011. | UNICEF. TSFP Guidelines."
  ),
  list(
    id = "appetite_test",
    domain = "malnutrition",
    keywords = c("appétit","test","atpe","rutf","plumpy","admission","triage","sortie","anorexie","sachet","triage"),
    content = "Le test d'appétit est obligatoire pour l'admission/suivi CRENAS. Protocole : proposer 1 sachet Plumpy'Nut® (92g) dans un endroit calme. Si l'enfant consomme ≥ 1/4 sachet en 30 min → appétit positif (admission CRENAS). Si < 1/4 → appétit négatif → transfert CRENI. L'anorexie est le marqueur clinique le plus important pour décider de l'hospitalisation. Le test doit être répété à chaque visite de suivi.",
    source = "OMS/UNICEF. Admission and Discharge Criteria for CMAM. 2012. | MINSANTE Cameroun. Protocole National 2022."
  ),
  list(
    id = "seasonal_malnutrition",
    domain = "malnutrition",
    keywords = c("saisonnier","soudure","saison","pic","pluie","cyclique","nord","cameroun","sahel","lean","afrique","juin","septembre"),
    content = "La malnutrition est fortement saisonnière en Afrique subsaharienne. Au Cameroun (Extrême-Nord, Nord, Adamaoua), le pic de MAG survient pendant la soudure (mai–septembre) : épuisement des stocks alimentaires + maladies liées aux pluies (paludisme, diarrhées) + travail agricole réduisant les soins aux enfants. La MAG peut doubler pendant la soudure. La planification des enquêtes SMART tient compte de cette saisonnalité (enquêtes recommandées en période de soudure pour capturer le pic).",
    source = "Vaitla B et al. Seasonal Hunger: A Neglected Problem. PLOS Medicine, 2009. | SMART Cameroun. Rapport national 2023."
  ),
  list(
    id = "malaria_malnutrition",
    domain = "malnutrition",
    keywords = c("paludisme","malaria","malnutrition","interaction","anémie","plasmodium","cameroun","afrique","coinfection","traitement"),
    content = "Au Cameroun, paludisme et malnutrition interagissent de manière bidirectionnelle : la malnutrition affaiblit l'immunité (↑ risque paludisme sévère) et le paludisme provoque anorexie/anémie (↑ malnutrition). Prévalence paludisme < 5 ans au Cameroun : 38% (EDS 2018). En MAS, inclure systématiquement : TDR paludisme, traitement antipaludique présomptif (artéméther-luméfantrine), supplémentation en fer dès Phase 2. Le paludisme sévère peut mimer les signes de MAS (prostration, anorexie, hypoglycémie).",
    source = "MINSANTE Cameroun / PNLP. Plan Stratégique de Lutte contre le Paludisme 2019–2023. | OMS. Malnutrition and Malaria: Impact on Treatment. 2017."
  ),
  list(
    id = "oedeme_kwashiorkor",
    domain = "malnutrition",
    keywords = c("œdème","œdèmes","kwashiorkor","bilatéral","nutritionnel","godet","signe","classification","grade","1+","2+","3+"),
    content = "Les œdèmes nutritionnels bilatéraux (prenant le godet) = MAS sévère. Classification : Grade 1+ = pieds uniquement ; Grade 2+ = pieds + tibias ; Grade 3+ = généralisés (visage). Grade 3+ → hospitalisation CRENI immédiate. Grades 1+ et 2+ sans complication → CRENAS (protocole PRONANUT). Mécanisme : carence protéique → hypoalbuminémie → fuite hydrique. Ne pas confondre avec œdèmes cardiologiques ou rénaux (bilan clinique complet).",
    source = "OMS. Prise en charge de la malnutrition sévère. 1999. | MINSANTE Cameroun. Protocole National 2022."
  ),
  list(
    id = "stunting_cameroun",
    domain = "malnutrition",
    keywords = c("stunting","retard","croissance","cameroun","haz","prévalence","régionale","adamaoua","extrême-nord","eds","mics","disparité"),
    content = "Au Cameroun, le retard de croissance (HAZ < -2) présente de fortes disparités régionales (EDS 2018) : Extrême-Nord ≈ 42% (très élevé), Nord ≈ 38%, Adamaoua ≈ 35% ; contre Littoral ≈ 18%, Sud ≈ 16%. Prévalence nationale 2022 ≈ 29% (niveau élevé OMS). Facteurs associés : pratiques IYCF inadéquates, WASH insuffisant, insécurité alimentaire chronique, faible niveau d'éducation maternelle. Irréversible après 2 ans → prévention prioritaire via window of opportunity (0–24 mois).",
    source = "INS Cameroun / ICF. EDS Cameroun 2018. | UNICEF Cameroun. Situation des Enfants 2022."
  ),
  list(
    id = "iycf_cameroun",
    domain = "malnutrition",
    keywords = c("iycf","anje","allaitement","exclusif","complémentaire","nourrisson","pratiques","cameroun","eds","diversité"),
    content = "Pratiques IYCF au Cameroun (EDS 2018) : Allaitement exclusif < 6 mois = 28% (cible OMS : 50%) ; Diversité alimentaire minimale (6–23 mois) = 23% ; Régime alimentaire minimum acceptable = 14%. Ces indicateurs faibles contribuent au retard de croissance élevé. Barrières : reprise précoce du travail maternel, influence familiale, croyances culturelles (eau/tisanes dès naissance), marketing des substituts du lait maternel.",
    source = "INS Cameroun / ICF. EDS Cameroun 2018. | UNICEF Cameroun. Analyse de la Situation des Enfants 2022."
  ),
  list(
    id = "coverage_survey",
    domain = "epidemiology",
    keywords = c("couverture","coverage","squeac","sleac","programme","thérapeutique","accès","barrière","estimé","méthode"),
    content = "Évaluation de couverture des programmes CMAM : (1) SQUEAC : méthode qualitative + quantitative (~60–80 jours), estime la couverture d'un programme ; (2) SLEAC : méthode rapide LQAS, classifie faible/modérée/haute couverture ; (3) CSAS : évaluation exhaustive par recensement. Standards Sphere : couverture ≥ 50% en crise, ≥ 70% en contexte stable. Au Cameroun, les SQUEAC révèlent souvent des barrières : distance aux centres, méconnaissance des signes, coûts de transport.",
    source = "Myatt M et al. SQUEAC: Low Resource Method for Evaluating Access and Coverage. Food and Nutrition Bulletin, 2012."
  ),
  list(
    id = "sphere_nutrition_standards",
    domain = "malnutrition",
    keywords = c("sphere","standard","humanitaire","minimum","urgence","ration","énergie","protéine","distribution","générale"),
    content = "Standards Sphere 2018 en nutrition humanitaire : (1) Ration Alimentaire Générale (RAG) : 2 100 kcal/pers/j (10% protéines, 17% lipides) ; (2) Traitement MAM (TSFP) : ration +500–1 000 kcal/j ; (3) Traitement MAS ambulatoire : ATPE 200 kcal/kg/j ≤ 12 semaines. Déclenchement réponse nutritionnelle urgente : MAG ≥ 15% OU ≥ 10% avec facteurs aggravants (insécurité alimentaire, épidémies, déplacements). Indicateur Sphere nutrition d'urgence : couverture programme ≥ 50% (contexte crise).",
    source = "The Sphere Handbook: Humanitarian Charter and Minimum Standards. 4th edition, Geneva, 2018."
  ),
  list(
    id = "nutrition_surveillance",
    domain = "epidemiology",
    keywords = c("surveillance","sentinelle","monitoring","système","alerte","précoce","snsap","données","routine","district","registre"),
    content = "Systèmes de surveillance nutritionnelle : (1) Routine : données mensuelles CRENAS/CRENAM (admissions, guérisons, abandons, décès) agrégées par district ; (2) Sites sentinelles : enquêtes anthropométriques légères régulières ; (3) SAP/FEWSNET : système d'alerte précoce intégrant agriculture, prix, MUAC, mortalité. Au Cameroun, le SNSAP (Système National de Surveillance Alimentaire et Nutritionnelle) collecte des données trimestrielles dans les régions à risque sous la coordination MINSANTE/MINADER.",
    source = "MINSANTE / MINADER Cameroun. Guide du Système National de Surveillance. 2021. | FEWSNET. Cameroun Livelihoods Zone Report."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # TCHAD — Contexte & Protocoles Nutritionnels
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "contexte_nutritionnel_tchad",
    domain = "malnutrition",
    keywords = c("tchad","chad","pnin","msnp","lac","sahel","boko","haram","déplacement","contexte","urgence","crise","prévalence"),
    content = "Le Tchad présente l'une des prévalences de malnutrition les plus élevées au monde. MAG nationale : 13–17% selon les années (urgence chronique), atteignant 20–25% dans les zones du Lac Tchad et du Sahel pendant la soudure. Le retard de croissance national ≈ 38–40% (niveau très élevé). Contexte : triple crise (conflit/Boko Haram, réfugiés soudanais/centrafricains, chocs climatiques). Le PNIN (Programme Nutritionnel d'Interventions Nutritionnelles) est la structure nationale de coordination, sous la tutelle du Ministère de la Santé et des Affaires Sociales (MSNP). Les régions prioritaires : Lac, Kanem, Batha, Barh-El-Gazel, Ouaddaï.",
    source = "UNICEF Tchad. Enquête SMART 2023. | OCHA Tchad. Humanitarian Needs Overview 2024. | MSNP Tchad. Plan National de Réponse Nutritionnelle."
  ),
  list(
    id = "protocoles_cmam_tchad",
    domain = "malnutrition",
    keywords = c("tchad","cmam","crenas","crenam","creni","protocole","admission","sortie","atpe","rutf","mam","sam","prise en charge"),
    content = "Au Tchad, les protocoles CMAM sont alignés sur les standards OMS/UNICEF avec des adaptations contextuelles. Critères d'admission CRENAS : PB < 115 mm OU œdèmes ± complications, test d'appétit positif (≥ 1/4 sachet ATPE en 30 min). Ration ATPE : Plumpy'Nut® 200 kcal/kg/j. CRENAM/MAM : PB 115–124 mm, ration Plumpy'Sup® 92g/j ou CSB++. Spécificités contexte Tchad : populations pastorales (Toubou, Arabes, Peul) nécessitant des approches mobiles ; intégration systématique bilan paludisme (RDT/TDR) et déparasitage. Standard SPHERE appliqué : guérison ≥ 75%, abandon ≤ 15%, décès ≤ 10%.",
    source = "MSNP Tchad. Protocole National de Prise en Charge de la Malnutrition Aigüe, édition 2021. | UNICEF Tchad."
  ),
  list(
    id = "populations_mobiles_tchad",
    domain = "malnutrition",
    keywords = c("pastoral","mobile","nomade","transhumance","tchad","sahel","accès","couverture","dépistage","communautaire","toubou","peul"),
    content = "Au Tchad, environ 40% de la population vit de l'élevage/pastoralisme (Toubou, Arabes, Peul/Fula). Ces populations posent des défis majeurs pour les programmes nutritionnels : mobilité saisonnière (transhumance nord-sud), éloignement des centres fixes (CRENAS), faible utilisation des services de santé. Approches adaptées : (1) Équipes mobiles mensuelles avec ATPE et MUAC tapes ; (2) Dépistage communautaire via relais communautaires nomades ; (3) Distribution de stocks d'ATPE pour plusieurs semaines (take-home rations) ; (4) Intégration avec programmes vétérinaires (One Health). Couverture cible SQUEAC dans zones pastorales ≥ 40% (plus réaliste que 50%).",
    source = "ACF / Action Contre la Faim. Nutrition Mobile Team Guidelines for Pastoralist Populations. 2019. | OCHA Tchad. Rapport humanitaire 2023."
  ),
  list(
    id = "securite_alimentaire_tchad",
    domain = "food_security",
    keywords = c("tchad","ipc","cadre harmonisé","soudure","lac","sahel","crise","phase","famine","grenier","pastoral"),
    content = "La sécurité alimentaire au Tchad est chroniquement précaire. Selon le Cadre Harmonisé 2024 (période de soudure) : environ 2,5–3,5 millions de personnes en Phase 3 (Crise) ou 4 (Urgence), avec concentration dans les régions du Lac, Kanem, Batha et Wadi Fira. FCS moyen < 35 dans les ménages des zones affectées. Les chocs répétés (sécheresse, inondations, criquets, conflits) maintiennent des niveaux de vulnérabilité élevés. Principaux acteurs : PAM (distributions générales + TSFP), FAO (moyens de subsistance), FEWSNET (alerte précoce).",
    source = "CH Tchad. Cadre Harmonisé Analyse IPC Tchad, juin 2024. | PAM Tchad. Rapport VAM 2024. | FEWSNET. Tchad Food Security Outlook."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # NIGER — Contexte & Protocoles Nutritionnels
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "contexte_nutritionnel_niger",
    domain = "malnutrition",
    keywords = c("niger","pnan","i3n","niamey","maradi","zinder","tahoua","agadez","tillabéri","dosso","sahel","prévalence","stunting"),
    content = "Le Niger a historiquement les indicateurs nutritionnels les plus alarmants au monde. MAG nationale ≈ 13–16% (avec pics > 20% à Maradi/Zinder/Tahoua pendant la soudure). Retard de croissance ≈ 47% (niveau catastrophique, le plus élevé en Afrique subsaharienne). Le PNAN (Plan National d'Action Nutrition) est le cadre directeur, renforcé par l'I3N (Initiative Nigeriens Nourrissent les Nigeriens) de sécurité alimentaire. Le BEFEN (Bureau d'Enquête, de Formation et d'Evaluation en Nutrition) assure la coordination des enquêtes nutritionnelles. Régions les plus touchées : Maradi, Zinder, Tahoua, Tillabéri, Dosso.",
    source = "MSP Niger. Plan National d'Action Nutrition (PNAN) 2022–2026. | INS Niger. ENSAN 2023. | UNICEF Niger."
  ),
  list(
    id = "protocoles_cmam_niger",
    domain = "malnutrition",
    keywords = c("niger","crenas","crenam","creni","protocole","admission","atpe","rutf","plumpy","csb","farine","mise","echelle","couverture"),
    content = "Le Niger dispose d'un système CMAM à large échelle, parmi les plus développés d'Afrique de l'Ouest. Critères d'admission CRENAS : PB < 115 mm OU œdèmes OU WHZ < -3, + test d'appétit positif. Ration ATPE : 200 kcal/kg/j (Plumpy'Nut® ou ATPE local Fabriqué au Niger = ATPE-N). Le Niger a développé une production locale d'ATPE à base d'arachide/niébé (programme UNICEF/USAID). CRENAM : CSB++ ou Plumpy'Sup®. Spécificité Niger : protocole intégré paludisme-malnutrition (traitement présomptif ACT lors d'admission CRENAS en saison de transmission). Taux de guérison national > 80% (parmi les meilleurs en Afrique).",
    source = "MSP Niger / DS Nutrition. Protocole National de Prise en Charge de la Malnutrition Aigüe. Niamey, 2023. | UNICEF Niger."
  ),
  list(
    id = "stunting_niger",
    domain = "malnutrition",
    keywords = c("stunting","retard","croissance","niger","maradi","zinder","haz","prévalence","chronique","anje","eau","wash"),
    content = "Le retard de croissance au Niger est le plus élevé au monde (HAZ < -2 SD ≈ 47% nationalement, jusqu'à 55% à Maradi et Zinder). Ce niveau catastrophique est alimenté par : (1) Pratiques IYCF très insuffisantes (allaitement exclusif < 25%) ; (2) Faible diversité alimentaire (accès limité aux protéines animales) ; (3) WASH précaire (défécation à l'air libre > 60% en milieu rural) ; (4) Insécurité alimentaire chronique ; (5) Mariages précoces (47% des filles mariées avant 18 ans → grossesses précoces). Le programme PNAN Niger cible la Fenêtre d'Opportunité 1 000 jours (conception → 2 ans) comme priorité absolue.",
    source = "INS Niger. ENSAN 2023. | UNICEF Niger. Multiple Indicator Cluster Survey (MICS) 2021. | MSP Niger. PNAN 2022–2026."
  ),
  list(
    id = "sap_niger",
    domain = "food_security",
    keywords = c("sap","alerte","précoce","niger","système","fewsnet","ipc","cadre harmonisé","soudure","banque","céréalière","sahel"),
    content = "Le Niger dispose d'un Système d'Alerte Précoce (SAP/DNPGCCA) parmi les plus anciens et robustes d'Afrique de l'Ouest (créé 1989). Le SAP intègre : production agricole/pastorale, prix des marchés, indicateurs nutritionnels (MUAC), mortalité. Classification IPC/Cadre Harmonisé Niger 2024 : 3,3 millions de personnes en Phase 3–4 (Crise-Urgence) pendant la soudure. Le dispositif de Réserve Alimentaire Nationale (RAN) stocke des céréales pour intervenir lors des crises. Depuis le coup d'État (2023), l'aide internationale est contrainte, ce qui aggrave les gaps de réponse nutritionnelle.",
    source = "DNPGCCA Niger. Rapport SAP 2024. | CH Niger. Analyse IPC juin 2024. | FEWSNET. Niger Food Security Outlook."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # RÉPUBLIQUE CENTRAFRICAINE (RCA) — Contexte & Protocoles
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "contexte_nutritionnel_rca",
    domain = "malnutrition",
    keywords = c("rca","centrafrique","centrafricaine","minsass","bangui","bossangoa","kaga","bandoro","bamingui","conflit","déplacement","réfugié","idp"),
    content = "La République Centrafricaine (RCA) est classée parmi les pays les plus fragiles au monde. La MAG nationale est chroniquement élevée (12–18%), avec des pics en zones de conflit (Ouham, Ouham-Pendé, Nana-Gribizi). Le retard de croissance ≈ 40%. Le contexte de conflit prolongé (groupes armés, UPC, FPRC, APRD) crée des millions de déplacés internes (PDI) et réfugiés, fragilisant les systèmes de santé. Le Ministère de la Santé et de la Population (MINSANP/MINSASS) coordonne avec UNICEF, PAM, MSF, IRC, Médecins du Monde. La chaîne d'approvisionnement en intrants nutritionnels est fréquemment interrompue par l'insécurité.",
    source = "UNICEF RCA. Rapport Situation Humanitaire 2024. | OCHA RCA. Humanitarian Needs Overview 2024. | MINSANP RCA."
  ),
  list(
    id = "protocoles_cmam_rca",
    domain = "malnutrition",
    keywords = c("rca","cmam","protocole","msf","unicef","mam","sam","crenas","crenam","creni","atpe","rutf","contexte","fragile","conflit"),
    content = "En RCA, la CMAM est mise en œuvre par UNICEF, MSF, IRC et les ONG partenaires en soutien au système de santé gouvernemental affaibli. Protocoles alignés sur standards OMS mais avec adaptations sécuritaires : (1) Décentralisation maximale vers les cases de santé (CS) pour éviter les longues distances ; (2) Équipes mobiles dans zones inaccessibles ; (3) Distribution multimenstruelle d'ATPE (2–4 semaines de stock) pour réduire les visites en zones à risque ; (4) Intégration avec programmes malaria (TDR + traitement ACT systématique à l'admission). MSF gère plusieurs CRENI (Bangui, Bossangoa) avec protocoles F-75/F-100 standards. Taux d'utilisation des services de santé parmi les plus bas en Afrique sub-saharienne (< 30%).",
    source = "MSF. Clinical Guidelines for SAM Management in Fragile Contexts. 2022. | UNICEF RCA. Nutrition Cluster Reports 2023–2024."
  ),
  list(
    id = "securite_alimentaire_rca",
    domain = "food_security",
    keywords = c("rca","ipc","cadre harmonisé","famine","phase","crise","urgence","déplacement","marché","accès","corridor","humanitaire"),
    content = "La RCA est l'un des pays avec la plus haute proportion de population en insécurité alimentaire sévère. IPC 2024 : plus de 2 millions de personnes en Phase 3–4 (Crise/Urgence) sur une population de ~5 millions. Des zones entières sont en Phase 4 (Urgence), notamment Ouham-Pendé, Bamingui-Bangoran. Les corridors humanitaires sont régulièrement bloqués (insécurité routière), rendant les distributions PAM/UNICEF impossibles pendant des semaines. Le FCS moyen des ménages PDI est souvent < 21 (insécurité alimentaire sévère). L'accès aux marchés est fortement perturbé (marchés fermés lors des affrontements).",
    source = "IPC RCA. Analyse IPC juin 2024. | PAM RCA. Emergency Food Security Assessment 2024. | FEWSNET RCA."
  ),

  # ══════════════════════════════════════════════════════════════════════
  # RÉFÉRENCES UNIVERSELLES — Applicables dans tout pays
  # ══════════════════════════════════════════════════════════════════════
  list(
    id = "global_cmam_performance",
    domain = "malnutrition",
    keywords = c("performance","indicateur","taux","guérison","abandon","décès","sphère","programme","évaluation","qualité","global","universel"),
    content = "Indicateurs de performance universels des programmes CMAM (Sphere 2018 / OMS) : Taux de guérison ≥ 75% (MAS ambulatoire) ; Taux d'abandon ≤ 15% ; Taux de décès ≤ 10% ; Durée médiane de traitement ≤ 8 semaines (MAS). Pour la MAM : guérison ≥ 75%, abandon ≤ 15%, durée ≤ 12 semaines. Un taux d'abandon élevé signale des barrières d'accès (distance, coûts). Un taux de décès élevé signale une admission trop tardive ou une mauvaise gestion des complications. Ces standards s'appliquent universellement, indépendamment du pays ou du contexte.",
    source = "Sphere Handbook 2018. | OMS/UNICEF. Admission and Discharge Criteria for CMAM. 2012. | VALID International. CMAM Monitoring and Evaluation."
  ),
  list(
    id = "global_nutrition_response",
    domain = "malnutrition",
    keywords = c("réponse","humanitaire","cluster","nutrition","coordination","ocha","unicef","pam","lead","plan","réponse","global","pays"),
    content = "Dans tout contexte humanitaire, la réponse nutritionnelle est coordonnée via le Cluster Nutrition (lead UNICEF) et le Cluster Sécurité Alimentaire (lead PAM/FAO). La Politique de Nutrition en Situation d'Urgence de l'IASC fixe les standards : déclaration d'urgence si MAG ≥ 15% OU ≥ 10% avec facteurs aggravants, Plans de Réponse Humanitaire (HRP) annuels. L'approche 3W (Qui fait Quoi Où) est utilisée pour éviter les doubles couvertures et les gaps. Les SMART surveys fournissent les données de base pour le HRP. Le suivi 5W (Qui, Quoi, Où, Quand, Pour Combien) monitore les activités des partenaires.",
    source = "IASC. Nutrition in Emergencies Policy. 2021. | UNICEF. Nutrition Cluster Coordination Guidelines. 2022."
  ),
  list(
    id = "global_double_burden",
    domain = "malnutrition",
    keywords = c("double","fardeau","surpoids","obésité","malnutrition","micronutriment","transition","nutritionnelle","urbain","rural","mondial"),
    content = "Le double fardeau de la malnutrition (DBM) décrit la coexistence dans les mêmes pays/ménages de la sous-nutrition (MAG, stunting, carences en micronutriments) ET de la surnutrition (surpoids, obésité, maladies chroniques). Il affecte plus de 130 pays. En Afrique subsaharienne, le DBM est croissant avec l'urbanisation : des ménages peuvent avoir simultanément un enfant avec retard de croissance et une mère en surpoids. Les politiques nutritionnelles doivent répondre aux deux formes. L'IMC adulte (OMS) : < 18,5 = insuffisance pondérale ; 25–29,9 = surpoids ; ≥ 30 = obésité.",
    source = "OMS. Double Burden of Malnutrition: Policy Brief. 2017. | IFPRI. Global Nutrition Report 2024."
  ),
  list(
    id = "global_water_sanitation",
    domain = "malnutrition",
    keywords = c("eau","assainissement","hygiène","wash","improved","source","latrines","potable","contamination","global","unicef","jmp"),
    content = "L'accès à l'eau potable et à l'assainissement amélioré est un déterminant clé de la nutrition dans tous les pays. Selon le Programme Conjoint OMS/UNICEF de Suivi (JMP) 2023 : 2 milliards de personnes manquent encore d'eau potable sécurisée ; 3,6 milliards manquent d'assainissement géré en toute sécurité. Indicateurs WASH en lien nutrition : source d'eau améliorée (forage protégé, robinet, eau en bouteille) ; latrines améliorées (fosse avec dalle, chasse d'eau) ; lavage des mains avec savon à des moments clés. Une amélioration du WASH peut réduire le stunting de 15–25% (Cochrane, 2019) et réduire l'incidence des diarrhées de 30–40% (OMS).",
    source = "OMS/UNICEF JMP. Progress on Household Drinking Water, Sanitation and Hygiene 2023. | Cumming O et al. Cochrane Review WASH and Nutrition, 2019."
  ),
  list(
    id = "global_refugee_nutrition",
    domain = "malnutrition",
    keywords = c("réfugié","déplacé","pdis","idp","camp","urgence","déplacement","protection","hcr","unhcr","population","vulnérable"),
    content = "Les populations réfugiées et déplacées présentent des risques nutritionnels élevés : rupture des systèmes de production alimentaire, dépendance à l'aide humanitaire, surpopulation dans les camps (risques épidémies). Standards HCR/Sphere pour populations déplacées : Ration Alimentaire Générale ≥ 2 100 kcal/pers/j ; MAG < 10% comme cible d'urgence (< 5% en contexte stable) ; couverture CMAM ≥ 50%. Dans les camps, les enquêtes nutritionnelles SMART sont obligatoires tous les 6 mois. Le MUAC screening hebdomadaire dans les centres de nutrition communautaire est recommandé. Les dépistages actifs par agents communautaires réduisent le retard de prise en charge.",
    source = "HCR/UNHCR. Operational Guidance on Nutrition in Refugee Situations. 2021. | Sphere Handbook 2018. | OMS. Malnutrition in Emergencies."
  )
)

# ============================================================================
# FONCTION DE RETRIEVAL RAG
# ============================================================================
# Retourne les chunks les plus pertinents pour une question donnée.
# Utilise TF-IDF simplifié (matching de mots-clés pondéré).
# ============================================================================

rag_retrieve <- function(query, top_k = 4, domain_hint = NULL) {

  if (is.null(query) || !nzchar(trimws(query))) return("")

  # Tokeniser la question (mots de longueur > 2)
  query_lower <- tolower(query)
  query_tokens <- unique(
    unlist(strsplit(gsub("[^a-zàâäéèêëîïôùûüç0-9]", " ", query_lower), "\\s+"))
  )
  query_tokens <- query_tokens[nchar(query_tokens) > 2]

  # Score chaque chunk
  scores <- vapply(RAG_KNOWLEDGE_BASE, function(chunk) {
    kw_lower <- tolower(chunk$keywords)
    content_words <- unique(unlist(strsplit(
      gsub("[^a-zàâäéèêëîïôùûüç0-9]", " ", tolower(chunk$content)),
      "\\s+"
    )))
    content_words <- content_words[nchar(content_words) > 2]

    # Score = correspondances dans keywords (poids 3) + correspondances dans content (poids 1)
    kw_matches <- sum(query_tokens %in% kw_lower) * 3L
    ct_matches <- sum(query_tokens %in% content_words)
    score <- kw_matches + ct_matches

    # Bonus si domain_hint correspond
    if (!is.null(domain_hint) && chunk$domain == domain_hint) {
      score <- score + 2L
    }

    as.numeric(score)
  }, numeric(1))

  # Sélectionner les top_k chunks avec score > 0
  ranked <- order(scores, decreasing = TRUE)
  selected <- ranked[scores[ranked] > 0]
  selected <- head(selected, top_k)

  if (length(selected) == 0) return("")

  # Construire le bloc de contexte RAG
  chunks_text <- vapply(selected, function(i) {
    ch <- RAG_KNOWLEDGE_BASE[[i]]
    sprintf(
      "▶ %s\n%s\n[Source : %s]",
      toupper(gsub("_", " ", ch$id)),
      ch$content,
      ch$source
    )
  }, character(1))

  paste0(
    "══ RÉFÉRENCES SCIENTIFIQUES PERTINENTES (RAG) ══\n",
    paste(chunks_text, collapse = "\n\n"),
    "\n══ FIN DES RÉFÉRENCES ══\n\n",
    "Utilise ces références pour ancrer ta réponse. Cite les sources entre parenthèses."
  )
}

# ============================================================================
# RETRIEVAL AVEC MÉTADONNÉES — retourne contexte + liste des sources utilisées
# ============================================================================

rag_retrieve_with_meta <- function(query, top_k = 4, domain_hint = NULL) {

  if (is.null(query) || !nzchar(trimws(query))) {
    return(list(context = "", sources = list()))
  }

  query_lower <- tolower(query)
  query_tokens <- unique(
    unlist(strsplit(gsub("[^a-zàâäéèêëîïôùûüç0-9]", " ", query_lower), "\\s+"))
  )
  query_tokens <- query_tokens[nchar(query_tokens) > 2]

  scores <- vapply(RAG_KNOWLEDGE_BASE, function(chunk) {
    kw_lower  <- tolower(chunk$keywords)
    ct_words  <- unique(unlist(strsplit(
      gsub("[^a-zàâäéèêëîïôùûüç0-9]", " ", tolower(chunk$content)), "\\s+"
    )))
    ct_words <- ct_words[nchar(ct_words) > 2]
    score <- sum(query_tokens %in% kw_lower) * 3L + sum(query_tokens %in% ct_words)
    if (!is.null(domain_hint) && chunk$domain == domain_hint) score <- score + 2L
    as.numeric(score)
  }, numeric(1))

  ranked   <- order(scores, decreasing = TRUE)
  selected <- ranked[scores[ranked] > 0]
  selected <- head(selected, top_k)

  if (length(selected) == 0) return(list(context = "", sources = list()))

  # Construire le texte de contexte
  chunks_text <- vapply(selected, function(i) {
    ch <- RAG_KNOWLEDGE_BASE[[i]]
    sprintf("▶ %s\n%s\n[Source : %s]",
            toupper(gsub("_", " ", ch$id)), ch$content, ch$source)
  }, character(1))

  context <- paste0(
    "══ RÉFÉRENCES SCIENTIFIQUES PERTINENTES (RAG) ══\n",
    paste(chunks_text, collapse = "\n\n"),
    "\n══ FIN DES RÉFÉRENCES ══\n\n",
    "Utilise ces références pour ancrer ta réponse. Cite les sources entre parenthèses."
  )

  # Métadonnées des sources utilisées (pour l'affichage UI)
  sources <- lapply(selected, function(i) {
    ch <- RAG_KNOWLEDGE_BASE[[i]]
    list(
      id     = ch$id,
      domain = ch$domain,
      label  = gsub("_", " ", ch$id),
      source = ch$source
    )
  })

  list(context = context, sources = sources)
}

# ============================================================================
# FILTRE DE DOMAINE — Rejet des questions hors-domaine
# ============================================================================

RAG_ALLOWED_DOMAINS <- c(
  # Malnutrition & anthropométrie
  "nutrition", "malnutrition", "muac", "zscore", "z-score", "gam", "sam", "mam",
  "stunting", "wasting", "underweight", "retard", "croissance", "émaciation",
  "iycf", "anje", "allaitement", "micronutriment", "anémie", "vitamine", "œdème",
  "crenam", "crenas", "creni", "cmam", "imam", "atpe", "rutf", "plumpy",
  "f-75", "f-100", "pronanut", "minsante", "cameroun", "kwashiorkor", "marasme",
  "soudure", "saisonnier", "paludisme", "malaria",
  # Sécurité alimentaire
  "fcs", "rcsi", "hdds", "hhs", "ipc", "cadre harmonisé", "fewsnet",
  "sécurité alimentaire", "tsfp", "bsfp", "sfp", "marché", "ration",
  # Épidémiologie & enquêtes
  "smart", "enquête", "prévalence", "incidence", "mortalité", "taux",
  "wash", "épidémiologie", "squeac", "sleac", "coverage", "couverture",
  "surveillance", "snsap", "odds ratio", "risque relatif", "deff",
  "puissance", "échantillon", "cluster", "lancet", "oms", "unicef", "wfp", "pam", "sphere",
  # Statistiques & modèles
  "statistique", "régression", "acp", "test", "modèle", "interprétation",
  "distribution", "corrélation", "anova", "khi-carré", "chi", "hypothèse",
  "shap", "auc", "roc", "overfitting", "validation", "données", "analyse"
)

rag_is_in_domain <- function(query) {
  q_lower <- tolower(query)
  any(sapply(RAG_ALLOWED_DOMAINS, function(kw) grepl(kw, q_lower, fixed = TRUE)))
}

cat("✅ RAG Nutrition Knowledge Base chargée —",
    length(RAG_KNOWLEDGE_BASE), "références ·",
    length(RAG_ALLOWED_DOMAINS), "domaines autorisés\n")
