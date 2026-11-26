# ============================================================================
# CONFIGURATION GADM MONDIALE : Tous les pays du monde (~180 pays)
# Généré automatiquement depuis rnaturalearth (adm0_a3)
# Support : Continent → Pays → Région → Département → Arrondissement
# ============================================================================

GADM_WORLDWIDE <- list(

  "BDI" = list(
    name = "Burundi",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "BDI",
    iso2 = "BI",
    zoom_bbox = c(ymin = -4.5, ymax = -2.3485, xmin = 29.0249, xmax = 30.7522),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BDI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Commune")
  ),

  "DJI" = list(
    name = "Djibouti",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "DJI",
    iso2 = "DJ",
    zoom_bbox = c(ymin = 10.9269, ymax = 12.6996, xmin = 41.6618, xmax = 43.3179),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_DJI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ERI" = list(
    name = "Eritrea",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "ERI",
    iso2 = "ER",
    zoom_bbox = c(ymin = 12.4554, ymax = 17.9983, xmin = 36.3232, xmax = 43.0812),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ERI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ETH" = list(
    name = "Ethiopia",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "ETH",
    iso2 = "ET",
    zoom_bbox = c(ymin = 3.4221, ymax = 14.9594, xmin = 32.9542, xmax = 47.7894),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ETH_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "État/Région", L2 = "Zone", L3 = "District")
  ),

  "KEN" = list(
    name = "Kenya",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "KEN",
    iso2 = "KE",
    zoom_bbox = c(ymin = -4.6768, ymax = 5.506, xmin = 33.8936, xmax = 41.8551),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KEN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Comté", L2 = "Circonscription", L3 = "Quartier/Ward")
  ),

  "MDG" = list(
    name = "Madagascar",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "MDG",
    iso2 = "MG",
    zoom_bbox = c(ymin = -25.6014, ymax = -12.0406, xmin = 43.2542, xmax = 50.4765),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MDG_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District", L3 = "District")
  ),

  "MWI" = list(
    name = "Malawi",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "MWI",
    iso2 = "MW",
    zoom_bbox = c(ymin = -16.8013, ymax = -9.2306, xmin = 32.6882, xmax = 35.7719),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MWI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "District", L2 = "Sous-district")
  ),

  "MOZ" = list(
    name = "Mozambique",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "MOZ",
    iso2 = "MZ",
    zoom_bbox = c(ymin = -26.7422, ymax = -10.3171, xmin = 30.1795, xmax = 40.7755),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MOZ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District", L3 = "Localité")
  ),

  "RWA" = list(
    name = "Rwanda",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "RWA",
    iso2 = "RW",
    zoom_bbox = c(ymin = -2.9179, ymax = -1.1347, xmin = 29.0249, xmax = 30.8161),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_RWA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District")
  ),

  "SDS" = list(
    name = "S. Sudan",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "SDS",
    iso2 = "SS",
    zoom_bbox = c(ymin = 3.5092, ymax = 12.248, xmin = 23.887, xmax = 35.298),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SDS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SOM" = list(
    name = "Somalia",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "SOM",
    iso2 = "SO",
    zoom_bbox = c(ymin = -1.6833, ymax = 12.0246, xmin = 40.9811, xmax = 51.1339),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SOM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SOL" = list(
    name = "Somaliland",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "SOL",
    iso2 = "",
    zoom_bbox = c(ymin = 7.9969, ymax = 11.462, xmin = 42.5588, xmax = 48.9482),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SOL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "TZA" = list(
    name = "Tanzania",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "TZA",
    iso2 = "TZ",
    zoom_bbox = c(ymin = -11.7209, ymax = -0.95, xmin = 29.34, xmax = 40.3166),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TZA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Quartier/Ward")
  ),

  "UGA" = list(
    name = "Uganda",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "UGA",
    iso2 = "UG",
    zoom_bbox = c(ymin = -1.4433, ymax = 4.2499, xmin = 29.5795, xmax = 35.036),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_UGA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "District", L2 = "Comté", L3 = "Sous-comté")
  ),

  "ZMB" = list(
    name = "Zambia",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "ZMB",
    iso2 = "ZM",
    zoom_bbox = c(ymin = -17.9612, ymax = -8.2383, xmin = 21.8878, xmax = 33.4857),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ZMB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District")
  ),

  "ZWE" = list(
    name = "Zimbabwe",
    continent = "Afrique",
    region = "Eastern Africa",
    iso3 = "ZWE",
    iso2 = "ZW",
    zoom_bbox = c(ymin = -22.2716, ymax = -15.5078, xmin = 25.2642, xmax = 32.8499),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ZWE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District", L3 = "Quartier/Ward")
  ),

  "AGO" = list(
    name = "Angola",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "AGO",
    iso2 = "AO",
    zoom_bbox = c(ymin = -17.9306, ymax = -4.438, xmin = 11.6401, xmax = 24.0799),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_AGO_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Municipalité", L3 = "Commune")
  ),

  "CMR" = list(
    name = "Cameroon",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "CMR",
    iso2 = "CM",
    zoom_bbox = c(ymin = 1.7277, ymax = 12.8594, xmin = 8.4888, xmax = 16.0129),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CMR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Département", L3 = "Arrondissement")
  ),

  "CAF" = list(
    name = "Central African Rep.",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "CAF",
    iso2 = "CF",
    zoom_bbox = c(ymin = 2.2676, ymax = 11.1424, xmin = 14.4594, xmax = 27.3742),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CAF_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Préfecture", L2 = "Sous-préfecture")
  ),

  "TCD" = list(
    name = "Chad",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "TCD",
    iso2 = "TD",
    zoom_bbox = c(ymin = 7.4219, ymax = 23.4097, xmin = 13.5404, xmax = 23.8869),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TCD_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Département", L3 = "Sous-préfecture")
  ),

  "COG" = list(
    name = "Congo",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "COG",
    iso2 = "CG",
    zoom_bbox = c(ymin = -5.038, ymax = 3.7282, xmin = 11.0938, xmax = 18.4531),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_COG_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "COD" = list(
    name = "Dem. Rep. Congo",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "COD",
    iso2 = "CD",
    zoom_bbox = c(ymin = -13.2572, ymax = 5.2561, xmin = 12.1823, xmax = 31.1741),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_COD_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Territoire")
  ),

  "GNQ" = list(
    name = "Eq. Guinea",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "GNQ",
    iso2 = "GQ",
    zoom_bbox = c(ymin = 1.0101, ymax = 2.2839, xmin = 9.3056, xmax = 11.2851),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GNQ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District")
  ),

  "GAB" = list(
    name = "Gabon",
    continent = "Afrique",
    region = "Middle Africa",
    iso3 = "GAB",
    iso2 = "GA",
    zoom_bbox = c(ymin = -3.9788, ymax = 2.3268, xmin = 8.798, xmax = 14.4255),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GAB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Département")
  ),

  "DZA" = list(
    name = "Algeria",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "DZA",
    iso2 = "DZ",
    zoom_bbox = c(ymin = 19.0574, ymax = 37.1184, xmin = -8.6844, xmax = 11.9995),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_DZA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Commune")
  ),

  "EGY" = list(
    name = "Egypt",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "EGY",
    iso2 = "EG",
    zoom_bbox = c(ymin = 22, ymax = 31.5857, xmin = 24.7001, xmax = 36.8662),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_EGY_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Gouvernorat", L2 = "Subdivision")
  ),

  "LBY" = list(
    name = "Libya",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "LBY",
    iso2 = "LY",
    zoom_bbox = c(ymin = 19.5805, ymax = 33.137, xmin = 9.3194, xmax = 25.1648),
    levels = 1,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LBY_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "District")
  ),

  "MAR" = list(
    name = "Morocco",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "MAR",
    iso2 = "MA",
    zoom_bbox = c(ymin = 21.4207, ymax = 35.76, xmin = -17.0204, xmax = -1.1246),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MAR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Province", L3 = "District")
  ),

  "SDN" = list(
    name = "Sudan",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "SDN",
    iso2 = "SD",
    zoom_bbox = c(ymin = 8.2292, ymax = 22, xmin = 21.9368, xmax = 38.4101),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SDN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "État", L2 = "District", L3 = "District")
  ),

  "TUN" = list(
    name = "Tunisia",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "TUN",
    iso2 = "TN",
    zoom_bbox = c(ymin = 30.3076, ymax = 37.35, xmin = 7.5245, xmax = 11.4888),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TUN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Gouvernorat", L2 = "Délégation")
  ),

  "SAH" = list(
    name = "W. Sahara",
    continent = "Afrique",
    region = "Northern Africa",
    iso3 = "SAH",
    iso2 = "EH",
    zoom_bbox = c(ymin = 20.9998, ymax = 27.6564, xmin = -17.0634, xmax = -8.6651),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SAH_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BWA" = list(
    name = "Botswana",
    continent = "Afrique",
    region = "Southern Africa",
    iso3 = "BWA",
    iso2 = "BW",
    zoom_bbox = c(ymin = -26.8285, ymax = -17.6618, xmin = 19.8955, xmax = 29.4322),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BWA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "LSO" = list(
    name = "Lesotho",
    continent = "Afrique",
    region = "Southern Africa",
    iso3 = "LSO",
    iso2 = "LS",
    zoom_bbox = c(ymin = -30.6451, ymax = -28.6475, xmin = 26.9993, xmax = 29.3252),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LSO_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "NAM" = list(
    name = "Namibia",
    continent = "Afrique",
    region = "Southern Africa",
    iso3 = "NAM",
    iso2 = "NA",
    zoom_bbox = c(ymin = -29.0455, ymax = -16.9413, xmin = 11.7342, xmax = 25.0844),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NAM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ZAF" = list(
    name = "South Africa",
    continent = "Afrique",
    region = "Southern Africa",
    iso3 = "ZAF",
    iso2 = "ZA",
    zoom_bbox = c(ymin = -34.8192, ymax = -22.0913, xmin = 16.345, xmax = 32.8301),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ZAF_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SWZ" = list(
    name = "eSwatini",
    continent = "Afrique",
    region = "Southern Africa",
    iso3 = "SWZ",
    iso2 = "SZ",
    zoom_bbox = c(ymin = -27.2859, ymax = -25.6602, xmin = 30.6766, xmax = 32.0717),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SWZ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BEN" = list(
    name = "Benin",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "BEN",
    iso2 = "BJ",
    zoom_bbox = c(ymin = 6.1422, ymax = 12.2356, xmin = 0.7723, xmax = 3.7971),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BEN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "Commune", L3 = "Arrondissement")
  ),

  "BFA" = list(
    name = "Burkina Faso",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "BFA",
    iso2 = "BF",
    zoom_bbox = c(ymin = 9.6108, ymax = 15.1162, xmin = -5.4706, xmax = 2.1771),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BFA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Province", L3 = "Département")
  ),

  "CIV" = list(
    name = "Côte d'Ivoire",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "CIV",
    iso2 = "CI",
    zoom_bbox = c(ymin = 4.3383, ymax = 10.5241, xmin = -8.6029, xmax = -2.5622),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CIV_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "District", L2 = "Région", L3 = "Département")
  ),

  "GMB" = list(
    name = "Gambia",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "GMB",
    iso2 = "GM",
    zoom_bbox = c(ymin = 13.1303, ymax = 13.8765, xmin = -16.8415, xmax = -13.845),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GMB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Division", L2 = "District")
  ),

  "GHA" = list(
    name = "Ghana",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "GHA",
    iso2 = "GH",
    zoom_bbox = c(ymin = 4.7105, ymax = 11.0983, xmin = -3.2444, xmax = 1.0601),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GHA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Municipalité")
  ),

  "GIN" = list(
    name = "Guinea",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "GIN",
    iso2 = "GN",
    zoom_bbox = c(ymin = 7.309, ymax = 12.5862, xmin = -15.1303, xmax = -7.8321),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GIN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Préfecture", L3 = "Sous-préfecture")
  ),

  "GNB" = list(
    name = "Guinea-Bissau",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "GNB",
    iso2 = "GW",
    zoom_bbox = c(ymin = 11.0404, ymax = 12.6282, xmin = -16.6775, xmax = -13.7005),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GNB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Secteur")
  ),

  "LBR" = list(
    name = "Liberia",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "LBR",
    iso2 = "LR",
    zoom_bbox = c(ymin = 4.3558, ymax = 8.5411, xmin = -11.4388, xmax = -7.5397),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LBR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Comté", L2 = "District", L3 = "Chefferie")
  ),

  "MLI" = list(
    name = "Mali",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "MLI",
    iso2 = "ML",
    zoom_bbox = c(ymin = 10.0964, ymax = 24.9746, xmin = -12.1708, xmax = 4.2702),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MLI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Cercle", L3 = "Arrondissement")
  ),

  "MRT" = list(
    name = "Mauritania",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "MRT",
    iso2 = "MR",
    zoom_bbox = c(ymin = 14.6168, ymax = 27.3957, xmin = -17.0634, xmax = -4.9233),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MRT_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Département")
  ),

  "NER" = list(
    name = "Niger",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "NER",
    iso2 = "NE",
    zoom_bbox = c(ymin = 11.6602, ymax = 23.4717, xmin = 0.2956, xmax = 15.9032),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NER_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Département", L3 = "Commune")
  ),

  "NGA" = list(
    name = "Nigeria",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "NGA",
    iso2 = "NG",
    zoom_bbox = c(ymin = 4.2406, ymax = 13.8659, xmin = 2.6917, xmax = 14.5772),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NGA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "État", L2 = "Collectivité locale")
  ),

  "SEN" = list(
    name = "Senegal",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "SEN",
    iso2 = "SN",
    zoom_bbox = c(ymin = 12.3321, ymax = 16.5983, xmin = -17.625, xmax = -11.4679),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SEN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Département", L3 = "Arrondissement")
  ),

  "SLE" = list(
    name = "Sierra Leone",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "SLE",
    iso2 = "SL",
    zoom_bbox = c(ymin = 6.7859, ymax = 10.047, xmin = -13.2466, xmax = -10.2301),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SLE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District", L3 = "Chefferie")
  ),

  "TGO" = list(
    name = "Togo",
    continent = "Afrique",
    region = "Western Africa",
    iso3 = "TGO",
    iso2 = "TG",
    zoom_bbox = c(ymin = 5.9288, ymax = 11.0187, xmin = -0.0498, xmax = 1.8652),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TGO_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Préfecture", L3 = "Commune")
  ),

  "KAZ" = list(
    name = "Kazakhstan",
    continent = "Asie",
    region = "Central Asia",
    iso3 = "KAZ",
    iso2 = "KZ",
    zoom_bbox = c(ymin = 40.6623, ymax = 55.3853, xmin = 46.4664, xmax = 87.36),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KAZ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "KGZ" = list(
    name = "Kyrgyzstan",
    continent = "Asie",
    region = "Central Asia",
    iso3 = "KGZ",
    iso2 = "KG",
    zoom_bbox = c(ymin = 39.2795, ymax = 43.2983, xmin = 69.4649, xmax = 80.26),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KGZ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "TJK" = list(
    name = "Tajikistan",
    continent = "Asie",
    region = "Central Asia",
    iso3 = "TJK",
    iso2 = "TJ",
    zoom_bbox = c(ymin = 36.7382, ymax = 40.9602, xmin = 67.4422, xmax = 74.98),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TJK_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "TKM" = list(
    name = "Turkmenistan",
    continent = "Asie",
    region = "Central Asia",
    iso3 = "TKM",
    iso2 = "TM",
    zoom_bbox = c(ymin = 35.2707, ymax = 42.7516, xmin = 52.5025, xmax = 66.5462),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TKM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "UZB" = list(
    name = "Uzbekistan",
    continent = "Asie",
    region = "Central Asia",
    iso3 = "UZB",
    iso2 = "UZ",
    zoom_bbox = c(ymin = 37.145, ymax = 45.5868, xmin = 55.9289, xmax = 73.0554),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_UZB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CHN" = list(
    name = "China",
    continent = "Asie",
    region = "Eastern Asia",
    iso3 = "CHN",
    iso2 = "CN",
    zoom_bbox = c(ymin = 18.1977, ymax = 53.4588, xmin = 73.6754, xmax = 135.0263),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CHN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "JPN" = list(
    name = "Japan",
    continent = "Asie",
    region = "Eastern Asia",
    iso3 = "JPN",
    iso2 = "JP",
    zoom_bbox = c(ymin = 31.0296, ymax = 45.5515, xmin = 129.4085, xmax = 145.5431),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_JPN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "MNG" = list(
    name = "Mongolia",
    continent = "Asie",
    region = "Eastern Asia",
    iso3 = "MNG",
    iso2 = "MN",
    zoom_bbox = c(ymin = 41.5974, ymax = 52.0474, xmin = 87.7513, xmax = 119.7728),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MNG_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "PRK" = list(
    name = "North Korea",
    continent = "Asie",
    region = "Eastern Asia",
    iso3 = "PRK",
    iso2 = "KP",
    zoom_bbox = c(ymin = 37.6691, ymax = 42.9854, xmin = 124.2656, xmax = 130.78),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PRK_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "KOR" = list(
    name = "South Korea",
    continent = "Asie",
    region = "Eastern Asia",
    iso3 = "KOR",
    iso2 = "KR",
    zoom_bbox = c(ymin = 34.39, ymax = 38.6122, xmin = 126.1174, xmax = 129.4683),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KOR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "TWN" = list(
    name = "Taiwan",
    continent = "Asie",
    region = "Eastern Asia",
    iso3 = "TWN",
    iso2 = "TW",
    zoom_bbox = c(ymin = 21.9706, ymax = 25.2955, xmin = 120.1062, xmax = 121.9512),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TWN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BRN" = list(
    name = "Brunei",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "BRN",
    iso2 = "BN",
    zoom_bbox = c(ymin = 4.0076, ymax = 5.4477, xmin = 114.204, xmax = 115.4507),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BRN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "KHM" = list(
    name = "Cambodia",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "KHM",
    iso2 = "KH",
    zoom_bbox = c(ymin = 10.4865, ymax = 14.5706, xmin = 102.3481, xmax = 107.6145),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KHM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "IDN" = list(
    name = "Indonesia",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "IDN",
    iso2 = "ID",
    zoom_bbox = c(ymin = -10.36, ymax = 5.4798, xmin = 95.293, xmax = 141.0339),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_IDN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "LAO" = list(
    name = "Laos",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "LAO",
    iso2 = "LA",
    zoom_bbox = c(ymin = 13.8811, ymax = 22.4648, xmin = 100.116, xmax = 107.5645),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LAO_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "MYS" = list(
    name = "Malaysia",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "MYS",
    iso2 = "MY",
    zoom_bbox = c(ymin = 0.7731, ymax = 6.9281, xmin = 100.0858, xmax = 119.1819),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MYS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "MMR" = list(
    name = "Myanmar",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "MMR",
    iso2 = "MM",
    zoom_bbox = c(ymin = 9.933, ymax = 28.3359, xmin = 92.3032, xmax = 101.18),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MMR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Division/État", L2 = "District", L3 = "Canton/Village")
  ),

  "PHL" = list(
    name = "Philippines",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "PHL",
    iso2 = "PH",
    zoom_bbox = c(ymin = 5.581, ymax = 18.5052, xmin = 117.1743, xmax = 126.5374),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PHL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "THA" = list(
    name = "Thailand",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "THA",
    iso2 = "TH",
    zoom_bbox = c(ymin = 5.6914, ymax = 20.4178, xmin = 97.3759, xmax = 105.589),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_THA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "TLS" = list(
    name = "Timor-Leste",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "TLS",
    iso2 = "TL",
    zoom_bbox = c(ymin = -9.3932, ymax = -8.2733, xmin = 124.9687, xmax = 127.3359),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TLS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "VNM" = list(
    name = "Vietnam",
    continent = "Asie",
    region = "South-Eastern Asia",
    iso3 = "VNM",
    iso2 = "VN",
    zoom_bbox = c(ymin = 8.5998, ymax = 23.3521, xmin = 102.1704, xmax = 109.3353),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_VNM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "AFG" = list(
    name = "Afghanistan",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "AFG",
    iso2 = "AF",
    zoom_bbox = c(ymin = 29.3186, ymax = 38.4863, xmin = 60.5284, xmax = 75.158),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_AFG_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District")
  ),

  "BGD" = list(
    name = "Bangladesh",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "BGD",
    iso2 = "BD",
    zoom_bbox = c(ymin = 20.6709, ymax = 26.4465, xmin = 88.0844, xmax = 92.6727),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BGD_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Division", L2 = "District", L3 = "Upazilla")
  ),

  "BTN" = list(
    name = "Bhutan",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "BTN",
    iso2 = "BT",
    zoom_bbox = c(ymin = 26.7194, ymax = 28.2964, xmin = 88.8142, xmax = 92.1037),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BTN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "IND" = list(
    name = "India",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "IND",
    iso2 = "IN",
    zoom_bbox = c(ymin = 7.9655, ymax = 35.494, xmin = 68.1766, xmax = 97.4026),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_IND_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "État/UT", L2 = "District", L3 = "Taluk")
  ),

  "IRN" = list(
    name = "Iran",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "IRN",
    iso2 = "IR",
    zoom_bbox = c(ymin = 25.0782, ymax = 39.713, xmin = 44.1092, xmax = 63.3166),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_IRN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "NPL" = list(
    name = "Nepal",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "NPL",
    iso2 = "NP",
    zoom_bbox = c(ymin = 26.3979, ymax = 30.4227, xmin = 80.0884, xmax = 88.1748),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NPL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Zone", L3 = "District")
  ),

  "PAK" = list(
    name = "Pakistan",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "PAK",
    iso2 = "PK",
    zoom_bbox = c(ymin = 23.692, ymax = 37.133, xmin = 60.8742, xmax = 77.8375),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PAK_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Division", L3 = "District")
  ),

  "LKA" = list(
    name = "Sri Lanka",
    continent = "Asie",
    region = "Southern Asia",
    iso3 = "LKA",
    iso2 = "LK",
    zoom_bbox = c(ymin = 5.9684, ymax = 9.8241, xmin = 79.6952, xmax = 81.788),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LKA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ARM" = list(
    name = "Armenia",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "ARM",
    iso2 = "AM",
    zoom_bbox = c(ymin = 38.7412, ymax = 41.2481, xmin = 43.5827, xmax = 46.5057),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ARM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "AZE" = list(
    name = "Azerbaijan",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "AZE",
    iso2 = "AZ",
    zoom_bbox = c(ymin = 38.2704, ymax = 41.8607, xmin = 44.794, xmax = 50.3928),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_AZE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CYP" = list(
    name = "Cyprus",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "CYP",
    iso2 = "CY",
    zoom_bbox = c(ymin = 34.5719, ymax = 35.1731, xmin = 32.2567, xmax = 34.0049),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CYP_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "GEO" = list(
    name = "Georgia",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "GEO",
    iso2 = "GE",
    zoom_bbox = c(ymin = 41.0644, ymax = 43.5531, xmin = 39.955, xmax = 46.6379),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GEO_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "IRQ" = list(
    name = "Iraq",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "IRQ",
    iso2 = "IQ",
    zoom_bbox = c(ymin = 29.099, ymax = 37.3853, xmin = 38.7923, xmax = 48.568),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_IRQ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "District")
  ),

  "ISR" = list(
    name = "Israel",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "ISR",
    iso2 = "IL",
    zoom_bbox = c(ymin = 29.5013, ymax = 33.2774, xmin = 34.2654, xmax = 35.8364),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ISR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "JOR" = list(
    name = "Jordan",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "JOR",
    iso2 = "JO",
    zoom_bbox = c(ymin = 29.1975, ymax = 33.3787, xmin = 34.9226, xmax = 39.1955),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_JOR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Province", L2 = "Sous-Province")
  ),

  "KWT" = list(
    name = "Kuwait",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "KWT",
    iso2 = "KW",
    zoom_bbox = c(ymin = 28.5261, ymax = 30.0591, xmin = 46.5687, xmax = 48.4161),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KWT_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "LBN" = list(
    name = "Lebanon",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "LBN",
    iso2 = "LB",
    zoom_bbox = c(ymin = 33.089, ymax = 34.6449, xmin = 35.1261, xmax = 36.6118),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LBN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Gouvernorat", L2 = "District", L3 = "Municipalité")
  ),

  "CYN" = list(
    name = "N. Cyprus",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "CYN",
    iso2 = "",
    zoom_bbox = c(ymin = 35.0003, ymax = 35.6716, xmin = 32.7318, xmax = 34.5765),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CYN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "OMN" = list(
    name = "Oman",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "OMN",
    iso2 = "OM",
    zoom_bbox = c(ymin = 16.6511, ymax = 26.3959, xmin = 52, xmax = 59.8081),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_OMN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "PSX" = list(
    name = "Palestine",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "PSX",
    iso2 = "PS",
    zoom_bbox = c(ymin = 31.3534, ymax = 32.5325, xmin = 34.9274, xmax = 35.5457),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PSX_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "QAT" = list(
    name = "Qatar",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "QAT",
    iso2 = "QA",
    zoom_bbox = c(ymin = 24.5563, ymax = 26.1146, xmin = 50.7439, xmax = 51.6067),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_QAT_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SAU" = list(
    name = "Saudi Arabia",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "SAU",
    iso2 = "SA",
    zoom_bbox = c(ymin = 16.3479, ymax = 32.161, xmin = 34.6323, xmax = 55.6667),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SAU_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "SYR" = list(
    name = "Syria",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "SYR",
    iso2 = "SY",
    zoom_bbox = c(ymin = 32.3129, ymax = 37.2299, xmin = 35.7008, xmax = 42.3496),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SYR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Gouvernorat", L2 = "District")
  ),

  "TUR" = list(
    name = "Turkey",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "TUR",
    iso2 = "TR",
    zoom_bbox = c(ymin = 35.8215, ymax = 42.1415, xmin = 26.0434, xmax = 44.794),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TUR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "ARE" = list(
    name = "United Arab Emirates",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "ARE",
    iso2 = "AE",
    zoom_bbox = c(ymin = 22.4969, ymax = 26.0555, xmin = 51.5795, xmax = 56.3968),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ARE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "YEM" = list(
    name = "Yemen",
    continent = "Asie",
    region = "Western Asia",
    iso3 = "YEM",
    iso2 = "YE",
    zoom_bbox = c(ymin = 12.586, ymax = 19, xmin = 42.6049, xmax = 53.1086),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_YEM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Gouvernorat", L2 = "District")
  ),

  "BLR" = list(
    name = "Belarus",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "BLR",
    iso2 = "BY",
    zoom_bbox = c(ymin = 51.3195, ymax = 56.1691, xmin = 23.1995, xmax = 32.6936),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BLR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BGR" = list(
    name = "Bulgaria",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "BGR",
    iso2 = "BG",
    zoom_bbox = c(ymin = 41.2345, ymax = 44.2349, xmin = 22.3805, xmax = 28.5581),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BGR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CZE" = list(
    name = "Czechia",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "CZE",
    iso2 = "CZ",
    zoom_bbox = c(ymin = 48.5553, ymax = 51.1173, xmin = 12.2401, xmax = 18.8531),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CZE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "HUN" = list(
    name = "Hungary",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "HUN",
    iso2 = "HU",
    zoom_bbox = c(ymin = 45.7595, ymax = 48.6239, xmin = 16.2023, xmax = 22.7105),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_HUN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "MDA" = list(
    name = "Moldova",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "MDA",
    iso2 = "MD",
    zoom_bbox = c(ymin = 45.4883, ymax = 48.4671, xmin = 26.6193, xmax = 30.0247),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MDA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "POL" = list(
    name = "Poland",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "POL",
    iso2 = "PL",
    zoom_bbox = c(ymin = 49.0274, ymax = 54.8515, xmin = 14.0745, xmax = 24.03),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_POL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "ROU" = list(
    name = "Romania",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "ROU",
    iso2 = "RO",
    zoom_bbox = c(ymin = 43.6884, ymax = 48.2209, xmin = 20.2202, xmax = 29.6265),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ROU_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "RUS" = list(
    name = "Russia",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "RUS",
    iso2 = "RU",
    zoom_bbox = c(ymin = 41.1514, ymax = 81.2504, xmin = -180, xmax = 180),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_RUS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "SVK" = list(
    name = "Slovakia",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "SVK",
    iso2 = "SK",
    zoom_bbox = c(ymin = 47.7584, ymax = 49.5716, xmin = 16.88, xmax = 22.5581),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SVK_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "UKR" = list(
    name = "Ukraine",
    continent = "Europe",
    region = "Eastern Europe",
    iso3 = "UKR",
    iso2 = "UA",
    zoom_bbox = c(ymin = 45.2933, ymax = 52.3351, xmin = 22.0856, xmax = 40.0808),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_UKR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "DNK" = list(
    name = "Denmark",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "DNK",
    iso2 = "DK",
    zoom_bbox = c(ymin = 54.8, ymax = 57.73, xmin = 8.09, xmax = 12.69),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_DNK_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "EST" = list(
    name = "Estonia",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "EST",
    iso2 = "EE",
    zoom_bbox = c(ymin = 57.4745, ymax = 59.6111, xmin = 23.3398, xmax = 28.1317),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_EST_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "FIN" = list(
    name = "Finland",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "FIN",
    iso2 = "FI",
    zoom_bbox = c(ymin = 59.8464, ymax = 70.1642, xmin = 20.6456, xmax = 31.5161),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_FIN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ISL" = list(
    name = "Iceland",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "ISL",
    iso2 = "IS",
    zoom_bbox = c(ymin = 63.4964, ymax = 66.5268, xmin = -24.3262, xmax = -13.6097),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ISL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "IRL" = list(
    name = "Ireland",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "IRL",
    iso2 = "IE",
    zoom_bbox = c(ymin = 51.6693, ymax = 55.1316, xmin = -9.9771, xmax = -6.033),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_IRL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "LVA" = list(
    name = "Latvia",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "LVA",
    iso2 = "LV",
    zoom_bbox = c(ymin = 55.6151, ymax = 57.9702, xmin = 21.0558, xmax = 28.1767),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LVA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "LTU" = list(
    name = "Lithuania",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "LTU",
    iso2 = "LT",
    zoom_bbox = c(ymin = 53.9057, ymax = 56.3725, xmin = 21.0558, xmax = 26.5883),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LTU_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "NOR" = list(
    name = "Norway",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "NOR",
    iso2 = "NO",
    zoom_bbox = c(ymin = 58.0789, ymax = 80.6571, xmin = 4.9921, xmax = 31.2934),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NOR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SWE" = list(
    name = "Sweden",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "SWE",
    iso2 = "SE",
    zoom_bbox = c(ymin = 55.3617, ymax = 69.1062, xmin = 11.0274, xmax = 23.9034),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SWE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "GBR" = list(
    name = "United Kingdom",
    continent = "Europe",
    region = "Northern Europe",
    iso3 = "GBR",
    iso2 = "GB",
    zoom_bbox = c(ymin = 49.96, ymax = 58.635, xmin = -7.5722, xmax = 1.6815),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GBR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "ALB" = list(
    name = "Albania",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "ALB",
    iso2 = "AL",
    zoom_bbox = c(ymin = 39.625, ymax = 42.6882, xmin = 19.3045, xmax = 21.02),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ALB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BIH" = list(
    name = "Bosnia and Herz.",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "BIH",
    iso2 = "BA",
    zoom_bbox = c(ymin = 42.65, ymax = 45.2338, xmin = 15.75, xmax = 19.5998),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BIH_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "HRV" = list(
    name = "Croatia",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "HRV",
    iso2 = "HR",
    zoom_bbox = c(ymin = 42.48, ymax = 46.5038, xmin = 13.657, xmax = 19.3905),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_HRV_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "GRC" = list(
    name = "Greece",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "GRC",
    iso2 = "GR",
    zoom_bbox = c(ymin = 34.92, ymax = 41.8269, xmin = 20.15, xmax = 26.6042),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GRC_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ITA" = list(
    name = "Italy",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "ITA",
    iso2 = "IT",
    zoom_bbox = c(ymin = 36.62, ymax = 47.1154, xmin = 6.75, xmax = 18.4802),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ITA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "KOS" = list(
    name = "Kosovo",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "KOS",
    iso2 = "XK",
    zoom_bbox = c(ymin = 41.8471, ymax = 43.2721, xmin = 20.0707, xmax = 21.7751),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_KOS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "MNE" = list(
    name = "Montenegro",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "MNE",
    iso2 = "ME",
    zoom_bbox = c(ymin = 41.8776, ymax = 43.5238, xmin = 18.45, xmax = 20.3398),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MNE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "MKD" = list(
    name = "North Macedonia",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "MKD",
    iso2 = "MK",
    zoom_bbox = c(ymin = 40.8427, ymax = 42.3203, xmin = 20.4632, xmax = 22.9524),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MKD_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "PRT" = list(
    name = "Portugal",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "PRT",
    iso2 = "PT",
    zoom_bbox = c(ymin = 36.8383, ymax = 42.2805, xmin = -9.5266, xmax = -6.3891),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PRT_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SRB" = list(
    name = "Serbia",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "SRB",
    iso2 = "RS",
    zoom_bbox = c(ymin = 42.2452, ymax = 46.1717, xmin = 18.8298, xmax = 22.986),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SRB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SVN" = list(
    name = "Slovenia",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "SVN",
    iso2 = "SI",
    zoom_bbox = c(ymin = 45.4523, ymax = 46.8524, xmin = 13.6981, xmax = 16.5648),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SVN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ESP" = list(
    name = "Spain",
    continent = "Europe",
    region = "Southern Europe",
    iso3 = "ESP",
    iso2 = "ES",
    zoom_bbox = c(ymin = 35.9469, ymax = 43.7483, xmin = -9.3929, xmax = 3.0395),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ESP_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "AUT" = list(
    name = "Austria",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "AUT",
    iso2 = "AT",
    zoom_bbox = c(ymin = 46.4318, ymax = 49.0391, xmin = 9.48, xmax = 16.9797),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_AUT_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BEL" = list(
    name = "Belgium",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "BEL",
    iso2 = "BE",
    zoom_bbox = c(ymin = 49.5295, ymax = 51.475, xmin = 2.5136, xmax = 6.1567),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BEL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "FRA" = list(
    name = "France",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "FRA",
    iso2 = "FR",
    zoom_bbox = c(ymin = 2.0534, ymax = 51.1485, xmin = -54.5248, xmax = 9.56),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_FRA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "DEU" = list(
    name = "Germany",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "DEU",
    iso2 = "DE",
    zoom_bbox = c(ymin = 47.3025, ymax = 54.9831, xmin = 5.9887, xmax = 15.017),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_DEU_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "LUX" = list(
    name = "Luxembourg",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "LUX",
    iso2 = "LU",
    zoom_bbox = c(ymin = 49.4427, ymax = 50.1281, xmin = 5.6741, xmax = 6.2428),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_LUX_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "NLD" = list(
    name = "Netherlands",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "NLD",
    iso2 = "NL",
    zoom_bbox = c(ymin = 50.8037, ymax = 53.5104, xmin = 3.315, xmax = 7.0921),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NLD_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CHE" = list(
    name = "Switzerland",
    continent = "Europe",
    region = "Western Europe",
    iso3 = "CHE",
    iso2 = "CH",
    zoom_bbox = c(ymin = 45.7769, ymax = 47.8308, xmin = 6.0226, xmax = 10.4427),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CHE_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BHS" = list(
    name = "Bahamas",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "BHS",
    iso2 = "BS",
    zoom_bbox = c(ymin = 23.71, ymax = 27.04, xmin = -78.98, xmax = -77),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BHS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CUB" = list(
    name = "Cuba",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "CUB",
    iso2 = "CU",
    zoom_bbox = c(ymin = 19.8555, ymax = 23.1886, xmin = -84.9749, xmax = -74.178),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CUB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "DOM" = list(
    name = "Dominican Rep.",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "DOM",
    iso2 = "DO",
    zoom_bbox = c(ymin = 17.5986, ymax = 19.8849, xmin = -71.9451, xmax = -68.3179),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_DOM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "HTI" = list(
    name = "Haiti",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "HTI",
    iso2 = "HT",
    zoom_bbox = c(ymin = 18.031, ymax = 19.9157, xmin = -74.458, xmax = -71.6249),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_HTI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "District", L3 = "Commune")
  ),

  "JAM" = list(
    name = "Jamaica",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "JAM",
    iso2 = "JM",
    zoom_bbox = c(ymin = 17.7011, ymax = 18.5242, xmin = -78.3377, xmax = -76.1997),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_JAM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "PRI" = list(
    name = "Puerto Rico",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "PRI",
    iso2 = "PR",
    zoom_bbox = c(ymin = 17.9466, ymax = 18.5206, xmin = -67.2424, xmax = -65.591),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PRI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "TTO" = list(
    name = "Trinidad and Tobago",
    continent = "Amérique du Nord",
    region = "Caribbean",
    iso3 = "TTO",
    iso2 = "TT",
    zoom_bbox = c(ymin = 10, ymax = 10.89, xmin = -61.95, xmax = -60.895),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TTO_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "BLZ" = list(
    name = "Belize",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "BLZ",
    iso2 = "BZ",
    zoom_bbox = c(ymin = 15.8869, ymax = 18.5, xmin = -89.2291, xmax = -88.1068),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BLZ_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CRI" = list(
    name = "Costa Rica",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "CRI",
    iso2 = "CR",
    zoom_bbox = c(ymin = 8.225, ymax = 11.2171, xmin = -85.9417, xmax = -82.5462),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CRI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SLV" = list(
    name = "El Salvador",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "SLV",
    iso2 = "SV",
    zoom_bbox = c(ymin = 13.149, ymax = 14.4241, xmin = -90.0956, xmax = -87.7235),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SLV_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "Municipalité")
  ),

  "GTM" = list(
    name = "Guatemala",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "GTM",
    iso2 = "GT",
    zoom_bbox = c(ymin = 13.7353, ymax = 17.8193, xmin = -92.2292, xmax = -88.225),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GTM_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "Municipalité")
  ),

  "HND" = list(
    name = "Honduras",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "HND",
    iso2 = "HN",
    zoom_bbox = c(ymin = 12.9847, ymax = 16.0054, xmin = -89.3533, xmax = -83.1472),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_HND_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "Municipalité")
  ),

  "MEX" = list(
    name = "Mexico",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "MEX",
    iso2 = "MX",
    zoom_bbox = c(ymin = 14.5388, ymax = 32.7208, xmin = -117.1278, xmax = -86.812),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_MEX_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "NIC" = list(
    name = "Nicaragua",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "NIC",
    iso2 = "NI",
    zoom_bbox = c(ymin = 10.7268, ymax = 15.0163, xmin = -87.6685, xmax = -83.1472),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NIC_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "Municipalité")
  ),

  "PAN" = list(
    name = "Panama",
    continent = "Amérique du Nord",
    region = "Central America",
    iso3 = "PAN",
    iso2 = "PA",
    zoom_bbox = c(ymin = 7.2205, ymax = 9.6116, xmin = -82.9658, xmax = -77.2426),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PAN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "CAN" = list(
    name = "Canada",
    continent = "Amérique du Nord",
    region = "Northern America",
    iso3 = "CAN",
    iso2 = "CA",
    zoom_bbox = c(ymin = 41.6751, ymax = 83.2332, xmin = -140.9978, xmax = -52.6481),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CAN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "GRL" = list(
    name = "Greenland",
    continent = "Amérique du Nord",
    region = "Northern America",
    iso3 = "GRL",
    iso2 = "GL",
    zoom_bbox = c(ymin = 60.0368, ymax = 83.6451, xmin = -73.297, xmax = -12.2086),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GRL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "USA" = list(
    name = "United States of America",
    continent = "Amérique du Nord",
    region = "Northern America",
    iso3 = "USA",
    iso2 = "US",
    zoom_bbox = c(ymin = 18.9162, ymax = 71.3578, xmin = -171.7911, xmax = -66.9647),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_USA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "AUS" = list(
    name = "Australia",
    continent = "Océanie",
    region = "Australia and New Zealand",
    iso3 = "AUS",
    iso2 = "AU",
    zoom_bbox = c(ymin = -43.6346, ymax = -10.6682, xmin = 113.339, xmax = 153.5695),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_AUS_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "NZL" = list(
    name = "New Zealand",
    continent = "Océanie",
    region = "Australia and New Zealand",
    iso3 = "NZL",
    iso2 = "NZ",
    zoom_bbox = c(ymin = -46.6412, ymax = -34.4507, xmin = 166.5091, xmax = 178.5171),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NZL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "FJI" = list(
    name = "Fiji",
    continent = "Océanie",
    region = "Melanesia",
    iso3 = "FJI",
    iso2 = "FJ",
    zoom_bbox = c(ymin = -18.288, ymax = -16.0209, xmin = -180, xmax = 180),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_FJI_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "NCL" = list(
    name = "New Caledonia",
    continent = "Océanie",
    region = "Melanesia",
    iso3 = "NCL",
    iso2 = "NC",
    zoom_bbox = c(ymin = -22.4, ymax = -20.1056, xmin = 164.0296, xmax = 167.12),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_NCL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "PNG" = list(
    name = "Papua New Guinea",
    continent = "Océanie",
    region = "Melanesia",
    iso3 = "PNG",
    iso2 = "PG",
    zoom_bbox = c(ymin = -10.6525, ymax = -2.5, xmin = 141.0002, xmax = 156.02),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PNG_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "SLB" = list(
    name = "Solomon Is.",
    continent = "Océanie",
    region = "Melanesia",
    iso3 = "SLB",
    iso2 = "SB",
    zoom_bbox = c(ymin = -10.8264, ymax = -6.5993, xmin = 156.4914, xmax = 162.3986),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SLB_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "VUT" = list(
    name = "Vanuatu",
    continent = "Océanie",
    region = "Melanesia",
    iso3 = "VUT",
    iso2 = "VU",
    zoom_bbox = c(ymin = -16.5978, ymax = -14.6265, xmin = 166.6291, xmax = 167.8449),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_VUT_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "ARG" = list(
    name = "Argentina",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "ARG",
    iso2 = "AR",
    zoom_bbox = c(ymin = -55.25, ymax = -21.8323, xmin = -73.4154, xmax = -53.6283),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ARG_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "BOL" = list(
    name = "Bolivia",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "BOL",
    iso2 = "BO",
    zoom_bbox = c(ymin = -22.8729, ymax = -9.762, xmin = -69.5904, xmax = -57.4984),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BOL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Département", L2 = "Province", L3 = "District")
  ),

  "BRA" = list(
    name = "Brazil",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "BRA",
    iso2 = "BR",
    zoom_bbox = c(ymin = -33.7684, ymax = 5.2445, xmin = -73.9872, xmax = -34.73),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_BRA_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "CHL" = list(
    name = "Chile",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "CHL",
    iso2 = "CL",
    zoom_bbox = c(ymin = -55.6118, ymax = -17.58, xmin = -75.6444, xmax = -66.9599),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_CHL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "COL" = list(
    name = "Colombia",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "COL",
    iso2 = "CO",
    zoom_bbox = c(ymin = -4.2982, ymax = 12.4373, xmin = -78.9909, xmax = -66.8763),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_COL_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  ),

  "ECU" = list(
    name = "Ecuador",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "ECU",
    iso2 = "EC",
    zoom_bbox = c(ymin = -4.9591, ymax = 1.3809, xmin = -80.9678, xmax = -75.2337),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_ECU_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "FLK" = list(
    name = "Falkland Is.",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "FLK",
    iso2 = "FK",
    zoom_bbox = c(ymin = -52.3, ymax = -51.1, xmin = -61.2, xmax = -57.75),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_FLK_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "GUY" = list(
    name = "Guyana",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "GUY",
    iso2 = "GY",
    zoom_bbox = c(ymin = 1.2681, ymax = 8.367, xmin = -61.4103, xmax = -56.5394),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GUY_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "PRY" = list(
    name = "Paraguay",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "PRY",
    iso2 = "PY",
    zoom_bbox = c(ymin = -27.5485, ymax = -19.3427, xmin = -62.6851, xmax = -54.293),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PRY_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "PER" = list(
    name = "Peru",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "PER",
    iso2 = "PE",
    zoom_bbox = c(ymin = -18.348, ymax = -0.0572, xmin = -81.4109, xmax = -68.6651),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_PER_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "Province", L3 = "District")
  ),

  "SUR" = list(
    name = "Suriname",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "SUR",
    iso2 = "SR",
    zoom_bbox = c(ymin = 1.8177, ymax = 6.0253, xmin = -58.0447, xmax = -53.958),
    levels = 2,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_SUR_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "URY" = list(
    name = "Uruguay",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "URY",
    iso2 = "UY",
    zoom_bbox = c(ymin = -34.9526, ymax = -30.1097, xmin = -58.4271, xmax = -53.2096),
    levels = 3,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_URY_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District")
  ),

  "VEN" = list(
    name = "Venezuela",
    continent = "Amérique du Sud",
    region = "South America",
    iso3 = "VEN",
    iso2 = "VE",
    zoom_bbox = c(ymin = 0.7245, ymax = 12.1623, xmin = -73.305, xmax = -59.7583),
    levels = 4,
    url_pattern = "https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_VEN_[LEVEL].json.zip",
    level_names = c(L0 = "Pays", L1 = "Région", L2 = "District", L3 = "Arrondissement")
  )

)

# ==============================================================================
# FONCTIONS UTILITAIRES
# ==============================================================================

#' Lister tous les continents supportés
list_continents <- function() {
  sort(unique(sapply(GADM_WORLDWIDE, function(x) x$continent)))
}

#' Lister pays d'un continent → vecteur nommé c('Nom pays' = 'ISO3')
list_countries_by_continent <- function(continent) {
  entries <- Filter(function(x) x$continent == continent, GADM_WORLDWIDE)
  if (length(entries) == 0) return(character(0))
  codes <- names(entries)
  noms  <- sapply(entries, function(x) x$name)
  setNames(codes, noms)
}

#' Obtenir le nom complet d'un pays
get_country_name <- function(country_code) {
  if (!country_code %in% names(GADM_WORLDWIDE)) return(paste0('Inconnu (', country_code, ')'))
  GADM_WORLDWIDE[[country_code]]$name
}

#' Obtenir l'URL GADM pour un pays et niveau
get_gadm_url <- function(country_code, level) {
  if (!country_code %in% names(GADM_WORLDWIDE)) {
    stop(sprintf("Pays '%s' non configuré dans GADM_WORLDWIDE", country_code))
  }
  pattern <- GADM_WORLDWIDE[[country_code]]$url_pattern
  gsub('[LEVEL]', level, pattern, fixed = TRUE)
}

#' Vérifier qu'un pays est supporté
is_country_supported <- function(country_code) {
  country_code %in% names(GADM_WORLDWIDE)
}

#' Lister tous les pays supportés
list_all_countries <- function() {
  names(GADM_WORLDWIDE)
}

#' Obtenir les niveaux admin disponibles pour un pays
get_available_levels <- function(country_code) {
  if (!is_country_supported(country_code)) return(NULL)
  GADM_WORLDWIDE[[country_code]]$levels
}
