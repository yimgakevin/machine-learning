Objectifs

Étudier l’évolution des prix de l’action Apple sur une période donnée.

Identifier les tendances et les variations saisonnières.

Construire des modèles de prévision (lissage exponentiel, ARIMA/SARIMA).

Évaluer la précision des prédictions pour aider à la prise de décision en bourse.

📊 Problématique métier

Un investisseur souhaite analyser l’évolution des prix des actions d’Apple afin d’anticiper leurs variations futures.
Votre mission est d'exploiter les séries temporelles pour identifier des tendances et proposer un modèle de prévision des prix.


📂 Base de données

Dataset : Apple Stock Price: https://www.kaggle.com/datasets/henryshan/apple-stock-price

Variables clés :  

Date : Date de l'enregistrement

Open : Prix d’ouverture
      
High : Prix le plus haut de la journée
      
Low : Prix le plus bas de la journée

Close : Prix de clôture (variable cible)
     
Volume : Volume d’échange

📝 Étapes du projet

1️⃣ Exploration et prétraitement des données

Charger et nettoyer les données (valeurs manquantes, formatage des dates).

Visualiser l’évolution du prix de clôture sur le temps.

Décomposer la série en tendance, saisonnalité et résidus.

Vérifier la stationnarité avec ADF et KPSS.

2️⃣ Modélisation et prévisions

🔹 Approche classique

Appliquer des moyennes mobiles pour lisser la série.

Désaisonnaliser avec régression linéaire et moyennes mobiles.

🔹 Approche lissage exponentiel

Lissage exponentiel simple.

Méthode de Holt-Winters pour intégrer la saisonnalité.

🔹 Approche ARIMA/SARIMA

Identifier les paramètres optimaux (p, d, q).

Construire et ajuster un modèle ARIMA.

Tester un SARIMA si une saisonnalité est détectée.







        
      Comparer les performances des modèles avec MSE, RMSE, MAPE.

        
      Identifier les périodes de forte volatilité.

        
      Formuler des recommandations d'investissement basées sur l'analyse des tendances.
