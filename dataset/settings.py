import os

# ---------------------- Filepaths ---------------------
PROJECT_PATH = "D:\\Heavy Data Science Data\\cours_supervise"
TRANSACTIONS_FILE_PATH = os.path.join(PROJECT_PATH, "transactions.npz")
FOYER_FISCAUX_FILE_PATH = os.path.join(PROJECT_PATH, "foyers_fiscaux.csv")
TAUX_ENDETTEMENT_FILE_PATH = os.path.join(PROJECT_PATH, "taux_endettement.csv")
ACTIFS_FINANCIERS_FILE_PATH = os.path.join(PROJECT_PATH, "actifs_financiers.csv")
TAUX_DINTERET_FILE_PATH = os.path.join(PROJECT_PATH, "taux_interet.csv")
FLUX_EMPRUNTS_FILE_PATH = os.path.join(PROJECT_PATH, "flux_nouveaux_emprunts.csv")
REGIONS_FILE_PATH = os.path.join(PROJECT_PATH, "departements-france.csv")
INDICE_REFERENCE_LOYERS = os.path.join(PROJECT_PATH, "indice_reference_loyers.csv")


# ---------------------- Column names -----------------s-
TRANSACTION_DATE = "date_transaction"
PRICE_PER_SQUARE_METER = "prix_m2"
NB_TRANSACTIONS_PER_MONTH = "nb_transactions_mois"

REGRESSION_TARGET = "prix"
CLASSIFICATION_TARGET = "en_dessous_du_marche"

random_state = 42
