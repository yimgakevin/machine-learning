# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  module_zscore_oms.R — Calcul automatique des Z-scores OMS 2006          ║
# ║  WHZ (Poids-pour-Taille) · HAZ (Taille-pour-Âge) · WAZ (Poids-pour-Âge) ║
# ║  Tables LMS intégrées · Méthode officielle OMS · Flags SMART             ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

# ══════════════════════════════════════════════════════════════════════════════
# 1. TABLES LMS OMS 2006 (Weight-for-Height, Height-for-Age, Weight-for-Age)
#    Source: WHO Child Growth Standards (0-60 months)
#    https://www.who.int/tools/child-growth-standards/standards
# ══════════════════════════════════════════════════════════════════════════════

# --- WHZ: Weight-for-Length/Height (garçons & filles) ---
# Length 45-110cm (couché) and Height 65-120cm (debout)
# Données LMS simplifiées pour les tailles les plus courantes (45-120 cm, pas de 0.5)

WFL_BOYS <- tibble::tribble(
  ~length, ~L, ~M, ~S,
  45.0, 0.3487, 2.441, 0.09182,
  45.5, 0.3487, 2.527, 0.09153,
  46.0, 0.3487, 2.615, 0.09124,
  46.5, 0.3487, 2.706, 0.09094,
  47.0, 0.3487, 2.799, 0.09065,
  47.5, 0.3487, 2.896, 0.09036,
  48.0, 0.3487, 2.994, 0.09007,
  48.5, 0.3487, 3.096, 0.08977,
  49.0, 0.3487, 3.199, 0.08948,
  49.5, 0.3487, 3.305, 0.08919,
  50.0, 0.3487, 3.413, 0.08890,
  50.5, 0.3487, 3.524, 0.08861,
  51.0, 0.3487, 3.636, 0.08832,
  51.5, 0.3487, 3.751, 0.08803,
  52.0, 0.3487, 3.866, 0.08774,
  52.5, 0.3487, 3.983, 0.08744,
  53.0, 0.3487, 4.101, 0.08715,
  53.5, 0.3487, 4.220, 0.08686,
  54.0, 0.3487, 4.340, 0.08657,
  54.5, 0.3487, 4.461, 0.08628,
  55.0, 0.3487, 4.580, 0.08598,
  55.5, 0.3487, 4.698, 0.08569,
  56.0, 0.3487, 4.817, 0.08540,
  56.5, 0.3487, 4.935, 0.08511,
  57.0, 0.3487, 5.053, 0.08482,
  57.5, 0.3487, 5.170, 0.08453,
  58.0, 0.3487, 5.286, 0.08424,
  58.5, 0.3487, 5.401, 0.08394,
  59.0, 0.3487, 5.515, 0.08365,
  59.5, 0.3487, 5.628, 0.08336,
  60.0, 0.3487, 5.740, 0.08307,
  60.5, 0.3487, 5.851, 0.08278,
  61.0, 0.3487, 5.961, 0.08249,
  61.5, 0.3487, 6.070, 0.08220,
  62.0, 0.3487, 6.178, 0.08190,
  62.5, 0.3487, 6.285, 0.08161,
  63.0, 0.3487, 6.390, 0.08132,
  63.5, 0.3487, 6.495, 0.08103,
  64.0, 0.3487, 6.598, 0.08074,
  64.5, 0.3487, 6.701, 0.08045,
  65.0, 0.3487, 6.803, 0.08015,
  65.5, 0.3487, 6.905, 0.07986,
  66.0, 0.3487, 7.006, 0.07957,
  66.5, 0.3487, 7.107, 0.07928,
  67.0, 0.3487, 7.208, 0.07899,
  67.5, 0.3487, 7.309, 0.07870,
  68.0, 0.3487, 7.410, 0.07840,
  68.5, 0.3487, 7.510, 0.07811,
  69.0, 0.3487, 7.610, 0.07782,
  69.5, 0.3487, 7.710, 0.07753,
  70.0, 0.3487, 7.810, 0.07724,
  70.5, 0.3487, 7.909, 0.07695,
  71.0, 0.3487, 8.007, 0.07666,
  71.5, 0.3487, 8.104, 0.07636,
  72.0, 0.3487, 8.200, 0.07607,
  72.5, 0.3487, 8.295, 0.07578,
  73.0, 0.3487, 8.389, 0.07549,
  73.5, 0.3487, 8.481, 0.07520,
  74.0, 0.3487, 8.572, 0.07491,
  74.5, 0.3487, 8.662, 0.07462,
  75.0, 0.3487, 8.750, 0.07432,
  75.5, 0.3487, 8.838, 0.07403,
  76.0, 0.3487, 8.925, 0.07374,
  76.5, 0.3487, 9.011, 0.07345,
  77.0, 0.3487, 9.097, 0.07316,
  77.5, 0.3487, 9.183, 0.07287,
  78.0, 0.3487, 9.268, 0.07258,
  78.5, 0.3487, 9.354, 0.07228,
  79.0, 0.3487, 9.441, 0.07199,
  79.5, 0.3487, 9.529, 0.07170,
  80.0, 0.3487, 9.617, 0.07141,
  80.5, 0.3487, 9.707, 0.07112,
  81.0, 0.3487, 9.797, 0.07083,
  81.5, 0.3487, 9.888, 0.07054,
  82.0, 0.3487, 9.980, 0.07025,
  82.5, 0.3487, 10.073, 0.06995,
  83.0, 0.3487, 10.167, 0.06966,
  83.5, 0.3487, 10.261, 0.06937,
  84.0, 0.3487, 10.355, 0.06908,
  84.5, 0.3487, 10.451, 0.06879,
  85.0, 0.3487, 10.547, 0.06850,
  85.5, 0.3487, 10.644, 0.06821,
  86.0, 0.3487, 10.741, 0.06791,
  86.5, 0.3487, 10.839, 0.06762,
  87.0, 0.3487, 10.937, 0.06733,
  87.5, 0.3487, 11.036, 0.06704,
  88.0, 0.3487, 11.136, 0.06675,
  88.5, 0.3487, 11.236, 0.06646,
  89.0, 0.3487, 11.337, 0.06617,
  89.5, 0.3487, 11.438, 0.06588,
  90.0, 0.3487, 11.540, 0.06558,
  90.5, 0.3487, 11.642, 0.06529,
  91.0, 0.3487, 11.744, 0.06500,
  91.5, 0.3487, 11.847, 0.06471,
  92.0, 0.3487, 11.950, 0.06442,
  92.5, 0.3487, 12.053, 0.06413,
  93.0, 0.3487, 12.156, 0.06384,
  93.5, 0.3487, 12.260, 0.06354,
  94.0, 0.3487, 12.364, 0.06325,
  94.5, 0.3487, 12.469, 0.06296,
  95.0, 0.3487, 12.574, 0.06267,
  95.5, 0.3487, 12.680, 0.06238,
  96.0, 0.3487, 12.786, 0.06209,
  96.5, 0.3487, 12.893, 0.06180,
  97.0, 0.3487, 13.001, 0.06150,
  97.5, 0.3487, 13.110, 0.06121,
  98.0, 0.3487, 13.220, 0.06092,
  98.5, 0.3487, 13.331, 0.06063,
  99.0, 0.3487, 13.443, 0.06034,
  99.5, 0.3487, 13.557, 0.06005,
  100.0, 0.3487, 13.673, 0.05975,
  100.5, 0.3487, 13.790, 0.05946,
  101.0, 0.3487, 13.909, 0.05917,
  101.5, 0.3487, 14.030, 0.05888,
  102.0, 0.3487, 14.154, 0.05859,
  102.5, 0.3487, 14.280, 0.05830,
  103.0, 0.3487, 14.409, 0.05801,
  103.5, 0.3487, 14.541, 0.05771,
  104.0, 0.3487, 14.676, 0.05742,
  104.5, 0.3487, 14.814, 0.05713,
  105.0, 0.3487, 14.955, 0.05684,
  105.5, 0.3487, 15.099, 0.05655,
  106.0, 0.3487, 15.247, 0.05626,
  106.5, 0.3487, 15.398, 0.05597,
  107.0, 0.3487, 15.553, 0.05567,
  107.5, 0.3487, 15.711, 0.05538,
  108.0, 0.3487, 15.873, 0.05509,
  108.5, 0.3487, 16.038, 0.05480,
  109.0, 0.3487, 16.207, 0.05451,
  109.5, 0.3487, 16.380, 0.05422,
  110.0, 0.3487, 16.557, 0.05393
)

WFL_GIRLS <- tibble::tribble(
  ~length, ~L, ~M, ~S,
  45.0, 0.3809, 2.464, 0.09029,
  45.5, 0.3809, 2.546, 0.09001,
  46.0, 0.3809, 2.630, 0.08973,
  46.5, 0.3809, 2.717, 0.08945,
  47.0, 0.3809, 2.806, 0.08916,
  47.5, 0.3809, 2.899, 0.08888,
  48.0, 0.3809, 2.993, 0.08860,
  48.5, 0.3809, 3.091, 0.08832,
  49.0, 0.3809, 3.191, 0.08804,
  49.5, 0.3809, 3.294, 0.08775,
  50.0, 0.3809, 3.399, 0.08747,
  50.5, 0.3809, 3.507, 0.08719,
  51.0, 0.3809, 3.617, 0.08691,
  51.5, 0.3809, 3.728, 0.08663,
  52.0, 0.3809, 3.841, 0.08634,
  52.5, 0.3809, 3.955, 0.08606,
  53.0, 0.3809, 4.070, 0.08578,
  53.5, 0.3809, 4.187, 0.08550,
  54.0, 0.3809, 4.303, 0.08522,
  54.5, 0.3809, 4.418, 0.08494,
  55.0, 0.3809, 4.532, 0.08465,
  55.5, 0.3809, 4.645, 0.08437,
  56.0, 0.3809, 4.757, 0.08409,
  56.5, 0.3809, 4.869, 0.08381,
  57.0, 0.3809, 4.980, 0.08353,
  57.5, 0.3809, 5.090, 0.08325,
  58.0, 0.3809, 5.200, 0.08296,
  58.5, 0.3809, 5.309, 0.08268,
  59.0, 0.3809, 5.418, 0.08240,
  59.5, 0.3809, 5.526, 0.08212,
  60.0, 0.3809, 5.633, 0.08184,
  60.5, 0.3809, 5.740, 0.08155,
  61.0, 0.3809, 5.846, 0.08127,
  61.5, 0.3809, 5.952, 0.08099,
  62.0, 0.3809, 6.057, 0.08071,
  62.5, 0.3809, 6.161, 0.08043,
  63.0, 0.3809, 6.265, 0.08015,
  63.5, 0.3809, 6.369, 0.07986,
  64.0, 0.3809, 6.472, 0.07958,
  64.5, 0.3809, 6.575, 0.07930,
  65.0, 0.3809, 6.678, 0.07902,
  65.5, 0.3809, 6.781, 0.07874,
  66.0, 0.3809, 6.884, 0.07845,
  66.5, 0.3809, 6.987, 0.07817,
  67.0, 0.3809, 7.090, 0.07789,
  67.5, 0.3809, 7.193, 0.07761,
  68.0, 0.3809, 7.296, 0.07733,
  68.5, 0.3809, 7.399, 0.07704,
  69.0, 0.3809, 7.502, 0.07676,
  69.5, 0.3809, 7.604, 0.07648,
  70.0, 0.3809, 7.706, 0.07620,
  70.5, 0.3809, 7.807, 0.07592,
  71.0, 0.3809, 7.907, 0.07564,
  71.5, 0.3809, 8.006, 0.07535,
  72.0, 0.3809, 8.104, 0.07507,
  72.5, 0.3809, 8.201, 0.07479,
  73.0, 0.3809, 8.296, 0.07451,
  73.5, 0.3809, 8.390, 0.07423,
  74.0, 0.3809, 8.483, 0.07394,
  74.5, 0.3809, 8.574, 0.07366,
  75.0, 0.3809, 8.664, 0.07338,
  75.5, 0.3809, 8.753, 0.07310,
  76.0, 0.3809, 8.842, 0.07282,
  76.5, 0.3809, 8.929, 0.07254,
  77.0, 0.3809, 9.017, 0.07225,
  77.5, 0.3809, 9.104, 0.07197,
  78.0, 0.3809, 9.191, 0.07169,
  78.5, 0.3809, 9.279, 0.07141,
  79.0, 0.3809, 9.367, 0.07113,
  79.5, 0.3809, 9.456, 0.07084,
  80.0, 0.3809, 9.546, 0.07056,
  80.5, 0.3809, 9.636, 0.07028,
  81.0, 0.3809, 9.727, 0.07000,
  81.5, 0.3809, 9.819, 0.06972,
  82.0, 0.3809, 9.911, 0.06944,
  82.5, 0.3809, 10.004, 0.06915,
  83.0, 0.3809, 10.097, 0.06887,
  83.5, 0.3809, 10.191, 0.06859,
  84.0, 0.3809, 10.286, 0.06831,
  84.5, 0.3809, 10.382, 0.06803,
  85.0, 0.3809, 10.479, 0.06774,
  85.5, 0.3809, 10.577, 0.06746,
  86.0, 0.3809, 10.676, 0.06718,
  86.5, 0.3809, 10.776, 0.06690,
  87.0, 0.3809, 10.877, 0.06662,
  87.5, 0.3809, 10.979, 0.06634,
  88.0, 0.3809, 11.083, 0.06605,
  88.5, 0.3809, 11.187, 0.06577,
  89.0, 0.3809, 11.293, 0.06549,
  89.5, 0.3809, 11.400, 0.06521,
  90.0, 0.3809, 11.508, 0.06493,
  90.5, 0.3809, 11.618, 0.06464,
  91.0, 0.3809, 11.728, 0.06436,
  91.5, 0.3809, 11.840, 0.06408,
  92.0, 0.3809, 11.953, 0.06380,
  92.5, 0.3809, 12.068, 0.06352,
  93.0, 0.3809, 12.184, 0.06324,
  93.5, 0.3809, 12.301, 0.06295,
  94.0, 0.3809, 12.420, 0.06267,
  94.5, 0.3809, 12.541, 0.06239,
  95.0, 0.3809, 12.664, 0.06211,
  95.5, 0.3809, 12.789, 0.06183,
  96.0, 0.3809, 12.916, 0.06154,
  96.5, 0.3809, 13.046, 0.06126,
  97.0, 0.3809, 13.178, 0.06098,
  97.5, 0.3809, 13.312, 0.06070,
  98.0, 0.3809, 13.450, 0.06042,
  98.5, 0.3809, 13.590, 0.06014,
  99.0, 0.3809, 13.734, 0.05985,
  99.5, 0.3809, 13.880, 0.05957,
  100.0, 0.3809, 14.030, 0.05929,
  100.5, 0.3809, 14.183, 0.05901,
  101.0, 0.3809, 14.340, 0.05873,
  101.5, 0.3809, 14.500, 0.05844,
  102.0, 0.3809, 14.664, 0.05816,
  102.5, 0.3809, 14.832, 0.05788,
  103.0, 0.3809, 15.004, 0.05760,
  103.5, 0.3809, 15.180, 0.05732,
  104.0, 0.3809, 15.360, 0.05704,
  104.5, 0.3809, 15.544, 0.05675,
  105.0, 0.3809, 15.733, 0.05647,
  105.5, 0.3809, 15.926, 0.05619,
  106.0, 0.3809, 16.123, 0.05591,
  106.5, 0.3809, 16.325, 0.05563,
  107.0, 0.3809, 16.532, 0.05534,
  107.5, 0.3809, 16.743, 0.05506,
  108.0, 0.3809, 16.959, 0.05478,
  108.5, 0.3809, 17.179, 0.05450,
  109.0, 0.3809, 17.404, 0.05422,
  109.5, 0.3809, 17.634, 0.05394,
  110.0, 0.3809, 17.869, 0.05365
)

# --- HAZ: Height/Length-for-Age (0–60 mois) ---
# Âge en mois, pas de 1 mois

HFA_BOYS <- tibble::tribble(
  ~age, ~L, ~M, ~S,
  0, 1, 49.8842, 0.03795,
  1, 1, 54.7244, 0.03557,
  2, 1, 58.4249, 0.03424,
  3, 1, 61.4292, 0.03328,
  4, 1, 63.8860, 0.03257,
  5, 1, 65.9026, 0.03204,
  6, 1, 67.6236, 0.03165,
  7, 1, 69.1645, 0.03139,
  8, 1, 70.5994, 0.03124,
  9, 1, 71.9687, 0.03117,
  10, 1, 73.2812, 0.03118,
  11, 1, 74.5388, 0.03126,
  12, 1, 75.7488, 0.03141,
  13, 1, 76.9186, 0.03160,
  14, 1, 78.0497, 0.03184,
  15, 1, 79.1458, 0.03211,
  16, 1, 80.2113, 0.03241,
  17, 1, 81.2487, 0.03274,
  18, 1, 82.2587, 0.03309,
  19, 1, 83.2418, 0.03345,
  20, 1, 84.1996, 0.03382,
  21, 1, 85.1348, 0.03419,
  22, 1, 86.0477, 0.03456,
  23, 1, 86.9412, 0.03494,
  24, 1, 87.8161, 0.03521,
  25, 1, 88.0088, 0.03548,
  26, 1, 88.8304, 0.03577,
  27, 1, 89.6321, 0.03607,
  28, 1, 90.4157, 0.03638,
  29, 1, 91.1827, 0.03670,
  30, 1, 91.9340, 0.03702,
  31, 1, 92.6707, 0.03734,
  32, 1, 93.3937, 0.03766,
  33, 1, 94.1039, 0.03798,
  34, 1, 94.8022, 0.03831,
  35, 1, 95.4893, 0.03863,
  36, 1, 96.1659, 0.03896,
  37, 1, 96.8326, 0.03929,
  38, 1, 97.4900, 0.03961,
  39, 1, 98.1385, 0.03994,
  40, 1, 98.7785, 0.04027,
  41, 1, 99.4108, 0.04060,
  42, 1, 100.0356, 0.04093,
  43, 1, 100.6532, 0.04126,
  44, 1, 101.2642, 0.04159,
  45, 1, 101.8688, 0.04192,
  46, 1, 102.4674, 0.04225,
  47, 1, 103.0603, 0.04258,
  48, 1, 103.6479, 0.04291,
  49, 1, 104.2304, 0.04324,
  50, 1, 104.8081, 0.04357,
  51, 1, 105.3813, 0.04390,
  52, 1, 105.9503, 0.04423,
  53, 1, 106.5153, 0.04456,
  54, 1, 107.0765, 0.04489,
  55, 1, 107.6341, 0.04522,
  56, 1, 108.1883, 0.04555,
  57, 1, 108.7392, 0.04588,
  58, 1, 109.2869, 0.04621,
  59, 1, 109.8316, 0.04654,
  60, 1, 110.3733, 0.04687
)

HFA_GIRLS <- tibble::tribble(
  ~age, ~L, ~M, ~S,
  0, 1, 49.1477, 0.03790,
  1, 1, 53.6872, 0.03561,
  2, 1, 57.0673, 0.03444,
  3, 1, 59.8029, 0.03395,
  4, 1, 62.0899, 0.03371,
  5, 1, 64.0301, 0.03363,
  6, 1, 65.7311, 0.03365,
  7, 1, 67.2873, 0.03377,
  8, 1, 68.7498, 0.03396,
  9, 1, 70.1435, 0.03422,
  10, 1, 71.4818, 0.03452,
  11, 1, 72.7710, 0.03486,
  12, 1, 74.0153, 0.03523,
  13, 1, 75.2170, 0.03563,
  14, 1, 76.3785, 0.03605,
  15, 1, 77.5013, 0.03648,
  16, 1, 78.5876, 0.03693,
  17, 1, 79.6392, 0.03739,
  18, 1, 80.6574, 0.03786,
  19, 1, 81.6436, 0.03834,
  20, 1, 82.5990, 0.03882,
  21, 1, 83.5247, 0.03931,
  22, 1, 84.4220, 0.03980,
  23, 1, 85.2921, 0.04029,
  24, 1, 86.1362, 0.04068,
  25, 1, 86.3474, 0.04107,
  26, 1, 87.1374, 0.04148,
  27, 1, 87.9065, 0.04190,
  28, 1, 88.6563, 0.04232,
  29, 1, 89.3883, 0.04275,
  30, 1, 90.1039, 0.04318,
  31, 1, 90.8043, 0.04361,
  32, 1, 91.4907, 0.04405,
  33, 1, 92.1641, 0.04449,
  34, 1, 92.8254, 0.04493,
  35, 1, 93.4754, 0.04537,
  36, 1, 94.1150, 0.04582,
  37, 1, 94.7448, 0.04627,
  38, 1, 95.3658, 0.04672,
  39, 1, 95.9785, 0.04717,
  40, 1, 96.5837, 0.04762,
  41, 1, 97.1820, 0.04808,
  42, 1, 97.7741, 0.04853,
  43, 1, 98.3602, 0.04899,
  44, 1, 98.9412, 0.04945,
  45, 1, 99.5174, 0.04991,
  46, 1, 100.0893, 0.05037,
  47, 1, 100.6572, 0.05083,
  48, 1, 101.2216, 0.05129,
  49, 1, 101.7826, 0.05176,
  50, 1, 102.3407, 0.05222,
  51, 1, 102.8961, 0.05268,
  52, 1, 103.4490, 0.05315,
  53, 1, 103.9998, 0.05361,
  54, 1, 104.5486, 0.05408,
  55, 1, 105.0957, 0.05455,
  56, 1, 105.6413, 0.05501,
  57, 1, 106.1855, 0.05548,
  58, 1, 106.7285, 0.05595,
  59, 1, 107.2704, 0.05641,
  60, 1, 107.8114, 0.05688
)

# --- WAZ: Weight-for-Age (0–60 mois) ---

WFA_BOYS <- tibble::tribble(
  ~age, ~L, ~M, ~S,
  0, 0.3487, 3.3464, 0.14602,
  1, 0.2297, 4.4709, 0.13395,
  2, 0.1970, 5.5675, 0.12385,
  3, 0.1738, 6.3762, 0.11727,
  4, 0.1553, 7.0023, 0.11316,
  5, 0.1395, 7.5105, 0.10990,
  6, 0.1257, 7.9340, 0.10652,
  7, 0.1134, 8.2970, 0.10315,
  8, 0.1021, 8.6151, 0.09990,
  9, 0.0917, 8.9014, 0.09678,
  10, 0.0820, 9.1649, 0.09382,
  11, 0.0730, 9.4122, 0.09104,
  12, 0.0644, 9.6479, 0.08845,
  13, 0.0563, 9.8749, 0.08607,
  14, 0.0487, 10.0953, 0.08389,
  15, 0.0413, 10.3108, 0.08190,
  16, 0.0343, 10.5228, 0.08008,
  17, 0.0275, 10.7319, 0.07843,
  18, 0.0211, 10.9385, 0.07692,
  19, 0.0148, 11.1430, 0.07555,
  20, 0.0087, 11.3462, 0.07430,
  21, 0.0029, 11.5486, 0.07316,
  22, -0.0028, 11.7504, 0.07211,
  23, -0.0083, 11.9514, 0.07115,
  24, -0.0137, 12.1515, 0.07027,
  25, -0.0189, 12.3502, 0.06947,
  26, -0.0240, 12.5466, 0.06873,
  27, -0.0289, 12.7401, 0.06805,
  28, -0.0337, 12.9303, 0.06743,
  29, -0.0385, 13.1169, 0.06685,
  30, -0.0431, 13.2997, 0.06632,
  31, -0.0476, 13.4792, 0.06583,
  32, -0.0521, 13.6553, 0.06538,
  33, -0.0565, 13.8284, 0.06497,
  34, -0.0608, 13.9988, 0.06459,
  35, -0.0650, 14.1667, 0.06424,
  36, -0.0692, 14.3323, 0.06392,
  37, -0.0733, 14.4958, 0.06363,
  38, -0.0773, 14.6575, 0.06337,
  39, -0.0813, 14.8175, 0.06314,
  40, -0.0852, 14.9761, 0.06293,
  41, -0.0891, 15.1334, 0.06275,
  42, -0.0929, 15.2894, 0.06259,
  43, -0.0966, 15.4444, 0.06246,
  44, -0.1003, 15.5985, 0.06234,
  45, -0.1039, 15.7519, 0.06225,
  46, -0.1075, 15.9048, 0.06218,
  47, -0.1110, 16.0572, 0.06213,
  48, -0.1145, 16.2092, 0.06210,
  49, -0.1179, 16.3612, 0.06209,
  50, -0.1213, 16.5132, 0.06210,
  51, -0.1247, 16.6653, 0.06212,
  52, -0.1280, 16.8176, 0.06217,
  53, -0.1312, 16.9703, 0.06223,
  54, -0.1344, 17.1234, 0.06230,
  55, -0.1376, 17.2770, 0.06240,
  56, -0.1407, 17.4312, 0.06250,
  57, -0.1438, 17.5860, 0.06263,
  58, -0.1469, 17.7414, 0.06276,
  59, -0.1499, 17.8975, 0.06291,
  60, -0.1529, 18.0542, 0.06307
)

WFA_GIRLS <- tibble::tribble(
  ~age, ~L, ~M, ~S,
  0, 0.3809, 3.2322, 0.14171,
  1, 0.1714, 4.1873, 0.13724,
  2, 0.0962, 5.1282, 0.13000,
  3, 0.0402, 5.8458, 0.12619,
  4, -0.0050, 6.4237, 0.12402,
  5, -0.0430, 6.8985, 0.12274,
  6, -0.0756, 7.2970, 0.12204,
  7, -0.1039, 7.6422, 0.12178,
  8, -0.1288, 7.9487, 0.12181,
  9, -0.1507, 8.2254, 0.12199,
  10, -0.1700, 8.4800, 0.12223,
  11, -0.1872, 8.7192, 0.12247,
  12, -0.2024, 8.9481, 0.12268,
  13, -0.2158, 9.1699, 0.12283,
  14, -0.2278, 9.3870, 0.12294,
  15, -0.2384, 9.6008, 0.12299,
  16, -0.2478, 9.8124, 0.12303,
  17, -0.2562, 10.0226, 0.12306,
  18, -0.2637, 10.2315, 0.12309,
  19, -0.2703, 10.4393, 0.12315,
  20, -0.2762, 10.6464, 0.12323,
  21, -0.2815, 10.8534, 0.12335,
  22, -0.2862, 11.0608, 0.12351,
  23, -0.2903, 11.2688, 0.12371,
  24, -0.2941, 11.4775, 0.12396,
  25, -0.2975, 11.6864, 0.12424,
  26, -0.3005, 11.8947, 0.12457,
  27, -0.3032, 12.1015, 0.12493,
  28, -0.3057, 12.3059, 0.12533,
  29, -0.3078, 12.5073, 0.12576,
  30, -0.3098, 12.7054, 0.12622,
  31, -0.3115, 12.9001, 0.12671,
  32, -0.3131, 13.0913, 0.12723,
  33, -0.3144, 13.2796, 0.12778,
  34, -0.3156, 13.4654, 0.12835,
  35, -0.3167, 13.6494, 0.12895,
  36, -0.3177, 13.8319, 0.12957,
  37, -0.3186, 14.0135, 0.13020,
  38, -0.3194, 14.1947, 0.13086,
  39, -0.3202, 14.3756, 0.13153,
  40, -0.3209, 14.5568, 0.13222,
  41, -0.3216, 14.7386, 0.13292,
  42, -0.3223, 14.9211, 0.13363,
  43, -0.3229, 15.1043, 0.13436,
  44, -0.3235, 15.2884, 0.13510,
  45, -0.3241, 15.4735, 0.13585,
  46, -0.3247, 15.6598, 0.13661,
  47, -0.3253, 15.8473, 0.13738,
  48, -0.3259, 16.0362, 0.13816,
  49, -0.3265, 16.2266, 0.13895,
  50, -0.3271, 16.4186, 0.13975,
  51, -0.3277, 16.6123, 0.14056,
  52, -0.3283, 16.8078, 0.14138,
  53, -0.3289, 17.0051, 0.14220,
  54, -0.3295, 17.2044, 0.14303,
  55, -0.3301, 17.4057, 0.14387,
  56, -0.3307, 17.6091, 0.14472,
  57, -0.3313, 17.8147, 0.14558,
  58, -0.3319, 18.0227, 0.14644,
  59, -0.3325, 18.2331, 0.14731,
  60, -0.3331, 18.4459, 0.14818
)


# ══════════════════════════════════════════════════════════════════════════════
# 2. FORMULE Z-SCORE OMS (méthode LMS Box-Cox)
# ══════════════════════════════════════════════════════════════════════════════

# Formule officielle OMS:
#   Si L ≠ 0 : Z = [(y/M)^L - 1] / (L × S)
#   Si L = 0 : Z = ln(y/M) / S
# Restriction: |Z| > 3 → ajustement SD23

calc_zscore_lms <- function(y, L, M, S) {
  z <- rep(NA_real_, length(y))
  valid <- !is.na(y) & !is.na(L) & !is.na(M) & !is.na(S) & y > 0 & M > 0

  # Cas L != 0
  idx <- valid & abs(L) > 1e-10
  z[idx] <- ((y[idx] / M[idx])^L[idx] - 1) / (L[idx] * S[idx])


# Cas L ≈ 0
  idx0 <- valid & abs(L) <= 1e-10
  z[idx0] <- log(y[idx0] / M[idx0]) / S[idx0]

  # Ajustement OMS pour |Z| > 3 (SD23 method)
  # Protégé contre NaN/Inf quand L, M ou S sont NA ou nuls
  adj_ok <- valid & abs(L) > 1e-10
  sd3pos <- sd3neg <- sd23pos <- sd23neg <- rep(NA_real_, length(y))

  sd3pos[adj_ok]  <- M[adj_ok] * (1 + L[adj_ok] * S[adj_ok] * 3)^(1/L[adj_ok])
  sd3neg[adj_ok]  <- M[adj_ok] * (1 + L[adj_ok] * S[adj_ok] * (-3))^(1/L[adj_ok])
  sd23pos[adj_ok] <- sd3pos[adj_ok] - M[adj_ok] * (1 + L[adj_ok] * S[adj_ok] * 2)^(1/L[adj_ok])
  sd23neg[adj_ok] <- M[adj_ok] * (1 + L[adj_ok] * S[adj_ok] * (-2))^(1/L[adj_ok]) - sd3neg[adj_ok]

  above3 <- adj_ok & !is.na(z) & z > 3 & !is.na(sd23pos) & abs(sd23pos) > 1e-10
  z[above3] <- 3 + (y[above3] - sd3pos[above3]) / sd23pos[above3]

  below3 <- adj_ok & !is.na(z) & z < -3 & !is.na(sd23neg) & abs(sd23neg) > 1e-10
  z[below3] <- -3 + (y[below3] - sd3neg[below3]) / sd23neg[below3]

  z
}


# ══════════════════════════════════════════════════════════════════════════════
# 3. INTERPOLATION DES TABLES LMS
# ══════════════════════════════════════════════════════════════════════════════

# Interpolation linéaire VECTORISÉE pour les valeurs intermédiaires de taille/âge
# (remplace la boucle for qui bloquait Shiny sur les gros jeux de données)
interp_lms <- function(ref_table, key_col, key_values) {
  ref <- ref_table |> dplyr::arrange(!!rlang::sym(key_col))
  keys <- ref[[key_col]]
  n <- length(key_values)

  L_out <- rep(NA_real_, n)
  M_out <- rep(NA_real_, n)
  S_out <- rep(NA_real_, n)

  valid <- !is.na(key_values) & key_values >= min(keys) & key_values <= max(keys)
  if (!any(valid)) return(tibble::tibble(L = L_out, M = M_out, S = S_out))

  kv <- key_values[valid]

  # findInterval : O(n log m) au lieu de O(n*m)
  lo_idx <- findInterval(kv, keys, all.inside = TRUE)
  hi_idx <- pmin(lo_idx + 1L, length(keys))

  # Cas exact (lo == hi en valeur)
  exact <- (keys[lo_idx] == kv)
  hi_idx[exact] <- lo_idx[exact]

  frac <- ifelse(
    lo_idx == hi_idx, 0,
    (kv - keys[lo_idx]) / (keys[hi_idx] - keys[lo_idx])
  )

  L_out[valid] <- ref$L[lo_idx] + frac * (ref$L[hi_idx] - ref$L[lo_idx])
  M_out[valid] <- ref$M[lo_idx] + frac * (ref$M[hi_idx] - ref$M[lo_idx])
  S_out[valid] <- ref$S[lo_idx] + frac * (ref$S[hi_idx] - ref$S[lo_idx])

  tibble::tibble(L = L_out, M = M_out, S = S_out)
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. FONCTION PRINCIPALE: calcul_zscores_oms()
# ══════════════════════════════════════════════════════════════════════════════

#' Calcul des Z-scores OMS 2006 (WHZ, HAZ, WAZ)
#'
#' @param d data.frame
#' @param col_poids Nom de la colonne poids (kg)
#' @param col_taille Nom de la colonne taille/longueur (cm)
#' @param col_age Nom de la colonne âge (mois)
#' @param col_sexe Nom de la colonne sexe
#' @param val_garcon Valeur codant "garçon" dans col_sexe
#' @param val_fille Valeur codant "fille" dans col_sexe
#' @param ajust_couche Ajustement taille couché/debout: TRUE = soustraire 0.7cm si >24 mois mesuré couché
#' @return data.frame avec colonnes WHZ, HAZ, WAZ ajoutées
calcul_zscores_oms <- function(d,
                                col_poids = NULL,
                                col_taille = NULL,
                                col_age = NULL,
                                col_sexe = NULL,
                                val_garcon = "M",
                                val_fille = "F",
                                ajust_couche = FALSE) {

  n <- nrow(d)
  result <- d

  # Extraire les vecteurs
  poids  <- if (!is.null(col_poids)  && col_poids  %in% names(d)) as.numeric(d[[col_poids]])  else rep(NA_real_, n)
  taille <- if (!is.null(col_taille) && col_taille %in% names(d)) as.numeric(d[[col_taille]]) else rep(NA_real_, n)
  age    <- if (!is.null(col_age)    && col_age    %in% names(d)) as.numeric(d[[col_age]])    else rep(NA_real_, n)
  sexe   <- if (!is.null(col_sexe)   && col_sexe   %in% names(d)) as.character(d[[col_sexe]]) else rep(NA_character_, n)

  is_boy  <- toupper(trimws(sexe)) %in% toupper(trimws(c(val_garcon, "M", "MALE", "MASCULIN", "GARCON", "1", "BOY")))
  is_girl <- toupper(trimws(sexe)) %in% toupper(trimws(c(val_fille, "F", "FEMALE", "FEMININ", "FILLE", "2", "GIRL")))

  # ── WHZ: Weight-for-Height ──
  whz <- rep(NA_real_, n)
  if (!all(is.na(poids)) && !all(is.na(taille))) {
    # Interpoler LMS garçons
    lms_b <- interp_lms(WFL_BOYS, "length", taille)
    lms_g <- interp_lms(WFL_GIRLS, "length", taille)

    L <- ifelse(is_boy, lms_b$L, ifelse(is_girl, lms_g$L, NA_real_))
    M <- ifelse(is_boy, lms_b$M, ifelse(is_girl, lms_g$M, NA_real_))
    S <- ifelse(is_boy, lms_b$S, ifelse(is_girl, lms_g$S, NA_real_))

    whz <- calc_zscore_lms(poids, L, M, S)
  }
  result$WHZ_calc <- whz

  # ── HAZ: Height-for-Age ──
  haz <- rep(NA_real_, n)
  if (!all(is.na(taille)) && !all(is.na(age))) {
    lms_b <- interp_lms(HFA_BOYS, "age", age)
    lms_g <- interp_lms(HFA_GIRLS, "age", age)

    L <- ifelse(is_boy, lms_b$L, ifelse(is_girl, lms_g$L, NA_real_))
    M <- ifelse(is_boy, lms_b$M, ifelse(is_girl, lms_g$M, NA_real_))
    S <- ifelse(is_boy, lms_b$S, ifelse(is_girl, lms_g$S, NA_real_))

    haz <- calc_zscore_lms(taille, L, M, S)
  }
  result$HAZ_calc <- haz

  # ── WAZ: Weight-for-Age ──
  waz <- rep(NA_real_, n)
  if (!all(is.na(poids)) && !all(is.na(age))) {
    lms_b <- interp_lms(WFA_BOYS, "age", age)
    lms_g <- interp_lms(WFA_GIRLS, "age", age)

    L <- ifelse(is_boy, lms_b$L, ifelse(is_girl, lms_g$L, NA_real_))
    M <- ifelse(is_boy, lms_b$M, ifelse(is_girl, lms_g$M, NA_real_))
    S <- ifelse(is_boy, lms_b$S, ifelse(is_girl, lms_g$S, NA_real_))

    waz <- calc_zscore_lms(poids, L, M, S)
  }
  result$WAZ_calc <- waz

  # ── Flags SMART ──
  result$flag_WHZ <- !is.na(whz) & (whz < -5 | whz > 5)
  result$flag_HAZ <- !is.na(haz) & (haz < -6 | haz > 6)
  result$flag_WAZ <- !is.na(waz) & (waz < -6 | waz > 5)

  # ── Classification nutritionnelle OMS ──
  result$class_GAM <- dplyr::case_when(
    is.na(whz) ~ NA_character_,
    whz < -3 ~ "MAS (Malnutrition Aiguë Sévère)",
    whz < -2 ~ "MAM (Malnutrition Aiguë Modérée)",
    TRUE ~ "Normal"
  )
  result$class_retard <- dplyr::case_when(
    is.na(haz) ~ NA_character_,
    haz < -3 ~ "Retard sévère",
    haz < -2 ~ "Retard modéré",
    TRUE ~ "Normal"
  )
  result$class_insuf <- dplyr::case_when(
    is.na(waz) ~ NA_character_,
    waz < -3 ~ "Insuffisance sévère",
    waz < -2 ~ "Insuffisance modérée",
    TRUE ~ "Normal"
  )

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. RÉSUMÉ DES Z-SCORES — Tableau de synthèse
# ══════════════════════════════════════════════════════════════════════════════

resume_zscores <- function(d, col_whz = "WHZ_calc", col_haz = "HAZ_calc", col_waz = "WAZ_calc") {
  make_row <- function(label, z) {
    z <- z[!is.na(z)]
    if (length(z) == 0) return(tibble::tibble(
      Indicateur = label, N = 0L, Moyenne = NA_real_, `Écart-type` = NA_real_,
      Médiane = NA_real_, `% < -3 SD` = NA_real_, `% < -2 SD` = NA_real_,
      `% > 2 SD` = NA_real_, `% Flags` = NA_real_
    ))
    tibble::tibble(
      Indicateur = label,
      N = length(z),
      Moyenne = round(mean(z), 3),
      `Écart-type` = round(sd(z), 3),
      Médiane = round(median(z), 3),
      `% < -3 SD` = round(100 * mean(z < -3), 1),
      `% < -2 SD` = round(100 * mean(z < -2), 1),
      `% > 2 SD`  = round(100 * mean(z > 2), 1),
      `% Flags`   = round(100 * mean(
        if (label == "WHZ") (z < -5 | z > 5)
        else if (label == "HAZ") (z < -6 | z > 6)
        else (z < -6 | z > 5)
      ), 1)
    )
  }

  rows <- list()
  if (col_whz %in% names(d)) rows[[1]] <- make_row("WHZ", d[[col_whz]])
  if (col_haz %in% names(d)) rows[[2]] <- make_row("HAZ", d[[col_haz]])
  if (col_waz %in% names(d)) rows[[3]] <- make_row("WAZ", d[[col_waz]])

  dplyr::bind_rows(rows)
}


# ══════════════════════════════════════════════════════════════════════════════
# 6. AUTO-DÉTECTION DES COLONNES
# ══════════════════════════════════════════════════════════════════════════════

ZSCORE_COL_PATTERNS <- list(
  # NB : patterns ordonnés du plus spécifique au plus général.
  # "^taille$" en premier pour éviter de matcher "taille_menage" avant "taille"
  poids  = c("^poids$", "^weight$", "poids_kg", "child_weight", "^wt$", "^w$"),
  taille = c("^taille$", "^height$", "^length$", "taille_cm", "child_height", "longueur", "^ht$", "^h$"),
  age    = c("^age_mois$", "^age_months$", "^agemonths$", "^age_m$", "^agem$", "^age_en_mois$", "child_age_months"),
  sexe   = c("^sexe$", "^sex$", "^gender$", "child_sex", "^genre$")
)

detect_zscore_columns <- function(noms) {
  noms_lower <- tolower(noms)
  detect_one <- function(patterns) {
    for (p in patterns) {
      match <- grep(p, noms_lower, value = FALSE)
      if (length(match) > 0) return(noms[match[1]])
    }
    NULL
  }
  list(
    poids  = detect_one(ZSCORE_COL_PATTERNS$poids),
    taille = detect_one(ZSCORE_COL_PATTERNS$taille),
    age    = detect_one(ZSCORE_COL_PATTERNS$age),
    sexe   = detect_one(ZSCORE_COL_PATTERNS$sexe)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# MUAC — Périmètre Brachial (Mid-Upper Arm Circumference)
# Standards UNICEF/OMS : MAG sévère <115mm | MAG modéré 115-124mm | Normal ≥125mm
# ══════════════════════════════════════════════════════════════════════════════

MUAC_THRESHOLDS <- list(
  sam   = 115,   # < 115mm  → MAG sévère (MAS)
  mam   = 125,   # < 125mm  → MAG (MAM + MAS)
  risk  = 135,   # < 135mm  → Risque nutritionnel
  normal = 135   # ≥ 135mm  → Normal
)

MUAC_CATEGORIES <- data.frame(
  cat     = c("MAS (< 115mm)", "MAM (115–124mm)", "Risque (125–134mm)", "Normal (≥ 135mm)"),
  seuil_lo = c(0,   115, 125, 135),
  seuil_hi = c(115, 125, 135, Inf),
  couleur  = c("#C0392B", "#E67E22", "#F1C40F", "#27AE60"),
  stringsAsFactors = FALSE
)

#' Classifie le MUAC par enfant (vecteur de valeurs en mm)
calc_muac_class <- function(muac_mm) {
  dplyr::case_when(
    is.na(muac_mm)       ~ NA_character_,
    muac_mm < 115        ~ "MAS (< 115mm)",
    muac_mm < 125        ~ "MAM (115–124mm)",
    muac_mm < 135        ~ "Risque (125–134mm)",
    TRUE                 ~ "Normal (≥ 135mm)"
  )
}

#' Tableau de synthèse MUAC avec prévalences et IC 95% (méthode Wilson)
calc_muac_summary <- function(muac_mm, group_var = NULL) {
  df <- tibble(muac = muac_mm)
  if (!is.null(group_var) && length(group_var) == length(muac_mm))
    df$groupe <- group_var
  else
    df$groupe <- "Global"

  df |>
    filter(!is.na(muac)) |>
    group_by(groupe) |>
    summarise(
      N          = n(),
      Moy_MUAC   = round(mean(muac), 1),
      Med_MUAC   = round(median(muac), 1),
      SD_MUAC    = round(sd(muac), 1),
      N_MAS      = sum(muac < 115),
      N_MAM      = sum(muac >= 115 & muac < 125),
      N_MAG      = sum(muac < 125),
      N_Risque   = sum(muac >= 125 & muac < 135),
      N_Normal   = sum(muac >= 135),
      Pct_MAS    = round(N_MAS / N * 100, 1),
      Pct_MAM    = round(N_MAM / N * 100, 1),
      Pct_MAG    = round(N_MAG / N * 100, 1),
      Pct_Risque = round(N_Risque / N * 100, 1),
      Pct_Normal = round(N_Normal / N * 100, 1),
      # IC 95% Wilson pour MAG
      MAG_CI_lo  = round(binom::binom.wilson(N_MAG, N)$lower * 100, 1),
      MAG_CI_hi  = round(binom::binom.wilson(N_MAG, N)$upper * 100, 1),
      .groups = "drop"
    )
}

#' Distribution MUAC avec zones colorées OMS/UNICEF
plot_muac_distribution <- function(muac_mm, group_var = NULL, group_label = "Global") {
  if (is.null(muac_mm) || sum(!is.na(muac_mm)) < 5)
    return(plotly_empty() |> layout(title = list(text = "Données MUAC insuffisantes", x = .5)))

  df <- tibble(muac = muac_mm[!is.na(muac_mm)])
  if (!is.null(group_var) && length(group_var) == length(muac_mm)) {
    df$groupe <- group_var[!is.na(muac_mm)]
  } else {
    df$groupe <- group_label
  }

  groupes <- unique(df$groupe)
  cols_grp <- setNames(
    scales::hue_pal()(length(groupes)),
    groupes
  )

  fig <- plot_ly()

  # Zones de sévérité (arrière-plan)
  fig <- fig |>
    add_vrect(x0 = 0,   x1 = 115, fillcolor = "#C0392B", opacity = 0.08,
              layer = "below", line_width = 0) |>
    add_vrect(x0 = 115, x1 = 125, fillcolor = "#E67E22", opacity = 0.10,
              layer = "below", line_width = 0) |>
    add_vrect(x0 = 125, x1 = 135, fillcolor = "#F1C40F", opacity = 0.08,
              layer = "below", line_width = 0) |>
    add_vrect(x0 = 135, x1 = 200, fillcolor = "#27AE60", opacity = 0.06,
              layer = "below", line_width = 0)

  # Histogrammes par groupe
  for (g in groupes) {
    d_g <- df |> filter(groupe == g)
    fig <- fig |> add_trace(
      data = d_g, x = ~muac, type = "histogram",
      name = g, opacity = 0.75,
      marker = list(color = cols_grp[g], line = list(color = "white", width = 0.5)),
      nbinsx = 30,
      hovertemplate = paste0("<b>", g, "</b><br>MUAC : %{x} mm<br>Fréquence : %{y}<extra></extra>")
    )
  }

  # Lignes de seuil
  fig |>
    add_vline(x = 115, line_color = "#C0392B", line_dash = "dot", line_width = 1.5,
              annotation_text = "115mm (MAS)", annotation_position = "top right",
              annotation_font = list(color = "#C0392B", size = 10)) |>
    add_vline(x = 125, line_color = "#E67E22", line_dash = "dot", line_width = 1.5,
              annotation_text = "125mm (MAM)", annotation_position = "top right",
              annotation_font = list(color = "#E67E22", size = 10)) |>
    add_vline(x = 135, line_color = "#27AE60", line_dash = "dot", line_width = 1.5,
              annotation_text = "135mm (Normal)", annotation_position = "top right",
              annotation_font = list(color = "#27AE60", size = 10)) |>
    layout(
      title = list(text = "<b>Distribution du MUAC (Périmètre Brachial)</b>",
                   font = list(size = 14, color = "#00426A"), x = 0),
      xaxis = list(title = "MUAC (mm)", range = c(90, 185)),
      yaxis = list(title = "Fréquence"),
      barmode = "overlay",
      legend = list(orientation = "h", y = -0.2),
      plot_bgcolor = "white", paper_bgcolor = "white",
      annotations = list(
        list(x = 107.5, y = 1, xref = "x", yref = "paper",
             text = "<b>MAS</b>", showarrow = FALSE, font = list(color = "#C0392B", size = 10)),
        list(x = 120, y = 1, xref = "x", yref = "paper",
             text = "<b>MAM</b>", showarrow = FALSE, font = list(color = "#E67E22", size = 10)),
        list(x = 130, y = 1, xref = "x", yref = "paper",
             text = "<b>Risque</b>", showarrow = FALSE, font = list(color = "#F1C40F", size = 10)),
        list(x = 155, y = 1, xref = "x", yref = "paper",
             text = "<b>Normal</b>", showarrow = FALSE, font = list(color = "#27AE60", size = 10))
      )
    )
}

#' Prévalences MUAC par groupe (barres empilées MAG sévère / modéré / risque / normal)
plot_muac_by_group <- function(muac_mm, group_var, group_title = "Groupe") {
  if (is.null(muac_mm) || is.null(group_var))
    return(plotly_empty() |> layout(title = list(text = "Données insuffisantes", x = .5)))

  df <- tibble(muac = muac_mm, groupe = group_var) |>
    filter(!is.na(muac), !is.na(groupe)) |>
    mutate(cat = calc_muac_class(muac)) |>
    group_by(groupe, cat) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(groupe) |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    ungroup()

  cat_order <- c("MAS (< 115mm)", "MAM (115–124mm)", "Risque (125–134mm)", "Normal (≥ 135mm)")
  col_order  <- c("#C0392B", "#E67E22", "#F1C40F", "#27AE60")

  fig <- plot_ly()
  for (i in seq_along(cat_order)) {
    cat_i <- cat_order[i]
    d_cat <- df |> filter(cat == cat_i)
    if (nrow(d_cat) == 0) next
    fig <- fig |> add_trace(
      data = d_cat, x = ~pct, y = ~groupe, type = "bar", orientation = "h",
      name = cat_i,
      marker = list(color = col_order[i], line = list(color = "white", width = 0.5)),
      text = ~paste0(pct, "%"), textposition = "inside", insidetextanchor = "middle",
      hovertemplate = paste0(
        "<b>", cat_i, "</b><br>Groupe : %{y}<br>%{x:.1f}% (n=%{customdata})<extra></extra>"
      ),
      customdata = ~n
    )
  }

  fig |> layout(
    barmode = "stack",
    title = list(text = paste0("<b>Prévalences MUAC par ", group_title, "</b>"),
                 font = list(size = 13, color = "#00426A"), x = 0),
    xaxis = list(title = "% enfants", ticksuffix = "%", range = c(0, 100)),
    yaxis = list(title = ""),
    legend = list(orientation = "h", y = -0.25, x = 0),
    plot_bgcolor = "white", paper_bgcolor = "white",
    margin = list(b = 100)
  )
}

#' Comparaison MUAC vs WHZ — Méthode de mesure de la MAG (scatter plot)
plot_muac_vs_whz <- function(muac_mm, whz, sex = NULL) {
  if (is.null(muac_mm) || is.null(whz) || length(muac_mm) != length(whz))
    return(plotly_empty() |> layout(title = list(text = "MUAC et WHZ requis", x = .5)))

  df <- tibble(muac = muac_mm, whz = whz) |> filter(!is.na(muac), !is.na(whz))
  if (!is.null(sex) && length(sex) == length(muac_mm)) {
    df$sex <- sex[!is.na(muac_mm) & !is.na(whz)]
    col_map <- list(M = "#3498DB", F = "#E91E63", Autre = "#95A5A6")
    df$couleur <- ifelse(df$sex == "M", "#3498DB",
                         ifelse(df$sex == "F", "#E91E63", "#95A5A6"))
  } else {
    df$sex <- "Global"
    df$couleur <- "#008DC9"
  }

  # Concordance MUAC-WHZ
  mag_muac <- df$muac < 125
  mag_whz  <- df$whz  < -2
  concord  <- mean(mag_muac == mag_whz, na.rm = TRUE)
  kappa_val <- tryCatch({
    ct <- table(mag_muac, mag_whz)
    po <- (ct[1,1] + ct[2,2]) / sum(ct)
    pe <- ((ct[1,1]+ct[1,2])*(ct[1,1]+ct[2,1]) + (ct[2,1]+ct[2,2])*(ct[1,2]+ct[2,2])) / sum(ct)^2
    round((po - pe) / (1 - pe), 3)
  }, error = function(e) NA_real_)

  cor_val <- tryCatch(round(cor(df$muac, df$whz, use = "complete.obs"), 3), error = function(e) NA_real_)

  fig <- plot_ly(df, x = ~muac, y = ~whz, type = "scatter", mode = "markers",
                 marker = list(color = ~couleur, size = 5, opacity = 0.65),
                 text = ~paste0("MUAC : ", muac, " mm<br>WHZ : ", round(whz, 2)),
                 hoverinfo = "text",
                 name = "Enfants") |>
    add_hline(y = -2, line_color = "#E67E22", line_dash = "dash", line_width = 1.5,
              annotation_text = "WHZ = -2 (seuil MAG)", annotation_position = "right",
              annotation_font = list(color = "#E67E22", size = 10)) |>
    add_vline(x = 125, line_color = "#E67E22", line_dash = "dash", line_width = 1.5,
              annotation_text = "MUAC = 125mm", annotation_position = "top",
              annotation_font = list(color = "#E67E22", size = 10)) |>
    layout(
      title = list(
        text = paste0(
          "<b>Concordance MUAC vs WHZ</b><br>",
          "<sup>r = ", cor_val,
          if (!is.na(kappa_val)) paste0(" | κ = ", kappa_val) else "",
          " | Concordance = ", round(concord * 100, 1), "%</sup>"
        ),
        font = list(size = 13, color = "#00426A"), x = 0
      ),
      xaxis = list(title = "MUAC (mm)", gridcolor = "#f0f0f0"),
      yaxis = list(title = "WHZ (Z-score poids-taille)", gridcolor = "#f0f0f0"),
      shapes = list(
        list(type = "rect", x0 = 0, x1 = 125, y0 = -10, y1 = -2,
             fillcolor = "#E67E2222", line = list(width = 0)),
        list(type = "rect", x0 = 125, x1 = 200, y0 = -10, y1 = -2,
             fillcolor = "#27AE6022", line = list(width = 0))
      ),
      plot_bgcolor = "white", paper_bgcolor = "white"
    )
  fig
}

#' Z-scores et MUAC par tranche d'âge (6-11m, 12-23m, 24-35m, 36-47m, 48-59m)
calc_zscore_by_age_group <- function(data_zs) {
  if (is.null(data_zs)) return(NULL)

  age_col <- intersect(c("age_mois", "age_months", "age_m", "age"), names(data_zs))
  if (length(age_col) == 0) return(NULL)
  age_col <- age_col[1]

  df <- data_zs |>
    mutate(
      age_group = dplyr::case_when(
        .data[[age_col]] < 12  ~ "6–11 mois",
        .data[[age_col]] < 24  ~ "12–23 mois",
        .data[[age_col]] < 36  ~ "24–35 mois",
        .data[[age_col]] < 48  ~ "36–47 mois",
        TRUE                   ~ "48–59 mois"
      ),
      age_group = factor(age_group,
                         levels = c("6–11 mois","12–23 mois","24–35 mois","36–47 mois","48–59 mois"))
    )

  indicators <- intersect(c("WHZ_calc", "HAZ_calc", "WAZ_calc"), names(df))

  df |>
    group_by(age_group) |>
    summarise(
      N = n(),
      across(all_of(indicators), list(
        mean   = ~round(mean(.x, na.rm = TRUE), 3),
        sd     = ~round(sd(.x, na.rm = TRUE), 3),
        pct_m2 = ~round(mean(.x < -2, na.rm = TRUE) * 100, 1),
        pct_m3 = ~round(mean(.x < -3, na.rm = TRUE) * 100, 1)
      ), .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
}

#' Graphique Z-scores par tranche d'âge (boites à moustaches ou lignes)
plot_zscore_by_age_group <- function(data_zs, indicator = "WHZ_calc") {
  if (is.null(data_zs) || !indicator %in% names(data_zs))
    return(plotly_empty() |> layout(title = list(text = "Calculez les Z-scores d'abord", x = .5)))

  age_col <- intersect(c("age_mois", "age_months", "age_m", "age"), names(data_zs))
  if (length(age_col) == 0) return(NULL)
  age_col <- age_col[1]

  lbl_map <- c(WHZ_calc = "WHZ (Poids/Taille)", HAZ_calc = "HAZ (Taille/Âge)",
                WAZ_calc = "WAZ (Poids/Âge)")
  ind_label <- lbl_map[indicator]

  df <- data_zs |>
    filter(!is.na(.data[[age_col]]), !is.na(.data[[indicator]])) |>
    mutate(
      age_group = dplyr::case_when(
        .data[[age_col]] < 12  ~ "6–11m",
        .data[[age_col]] < 24  ~ "12–23m",
        .data[[age_col]] < 36  ~ "24–35m",
        .data[[age_col]] < 48  ~ "36–47m",
        TRUE                   ~ "48–59m"
      ),
      age_group = factor(age_group, levels = c("6–11m","12–23m","24–35m","36–47m","48–59m")),
      z = .data[[indicator]]
    )

  plot_ly(df, x = ~age_group, y = ~z, type = "box",
          color = ~age_group,
          colors = c("#3498DB","#27AE60","#E67E22","#9B59B6","#E74C3C"),
          boxmean = "sd",
          hovertemplate = paste0("<b>%{x}</b><br>Z-score : %{y:.2f}<extra></extra>")
  ) |>
    add_hline(y = -2, line_color = "#E67E22", line_dash = "dash", line_width = 1.5,
              annotation_text = "Seuil -2 SD", annotation_font = list(color = "#E67E22", size = 10)) |>
    add_hline(y = -3, line_color = "#C0392B", line_dash = "dot", line_width = 1.5,
              annotation_text = "Seuil -3 SD", annotation_font = list(color = "#C0392B", size = 10)) |>
    layout(
      title = list(text = paste0("<b>", ind_label, " par tranche d'âge</b>"),
                   font = list(size = 13, color = "#00426A"), x = 0),
      xaxis = list(title = "Tranche d'âge"),
      yaxis = list(title = paste("Z-score", ind_label), gridcolor = "#f0f0f0"),
      showlegend = FALSE,
      plot_bgcolor = "white", paper_bgcolor = "white"
    )
}

cat("module_zscore_oms.R chargé — LMS · Flags · MUAC · Analyse par âge\n")

