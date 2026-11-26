# 📚 Guide d'Utilisation — Module Z-scores OMS Expert

## Avant de commencer

Ce module calcule automatiquement les Z-scores WHO/OMS 2006 pour les enfants de **0-60 mois** selon les standards de croissance internationaux.

### Quoi mesurer
- **Poids** (en kg)
- **Taille/Longueur** (en cm)  
  - Couché : pour enfants < 24 mois
  - Debout : pour enfants ≥ 24 mois
- **Âge** (en mois complets)
- **Sexe** (Garçon/Fille)

---

## 🚀 Étapes d'utilisation

### Étape 1 : Importer les données
1. Cliquez sur **"Import & Mappeur"** dans la barre latérale
2. Téléchargez votre fichier CSV / Excel
3. Sélectionnez les colonnes correspondant à :
   - Poids
   - Taille/Longueur
   - Âge (mois)
   - Sexe

### Étape 2 : Aller à l'onglet Z-scores
1. Cliquez sur **"📐 Z-scores OMS 2006"**
2. Les colonnes sont **auto-détectées** (sinon, sélectionnez manuellement)
3. Spécifiez comment le **sexe est codé** :
   - Garçon : "M", "Male", "1", etc.
   - Fille : "F", "Female", "2", etc.
4. Cliquez **"Calculer les Z-scores"**

### Étape 3 : Interpréter les résultats

#### **Tableau de bord**
Vous verrez **3 grandes cartes** :

**🔴 Malnutrition Aiguë (WHZ < -2)**
- Voir le % et le badge OMS (CRITIQUE / SÉRIEUX / PAUVRE / ACCEPTABLE)
- Lire le texte narratif qui explique ce que cela signifie
- Vérifier le score de qualité (0-100)

**📏 Retard de Croissance (HAZ < -2)**
- Même structure

**⚖️ Insuffisance Pondérale (WAZ < -2)**
- Même structure

#### **Comprendre les badges OMS**

| Indicateur | Acceptable | Pauvre | Sérieux | Critique |
|-----------|-----------|--------|---------|----------|
| **GAM (WHZ)** | < 5% | 5-9% | 10-14% | ≥ 15% |
| **Stunting (HAZ)** | < 20% | 20-29% | 30-39% | ≥ 40% |
| **Underweight (WAZ)** | < 10% | 10-19% | 20-29% | ≥ 30% |

🟢 = Vert (Acceptable)  
🟡 = Jaune (Pauvre)  
🟠 = Orange (Sérieux)  
🔴 = Rouge (Critique)

### Étape 4 : Vérifier la qualité des données

Allez à l'onglet **"Qualité"** pour voir :

- **Score global** : 0-100
  - 90-100 : ✅ Excellent
  - 75-89 : ✅ Bon
  - 60-74 : ⚠️ Acceptable
  - < 60 : ❌ Problématique

- **10 critères détaillés** :
  1. Écart-type (attendu ~1.0)
  2. Digit preference (arrondir ?)
  3. Age heaping (clustering sur âges ronds ?)
  4. Sex ratio (équilibre M/F ?)
  5. Corrélations entre indicateurs
  6. Flags SMART (aberrances)
  7. Complétude (% non-manquants)
  8. Normalité de distribution
  9. Symétrie et kurtosis
  10. Homogénéité si plusieurs équipes

Si un critère est faible, lisez l'**explication** pour comprendre ce qui pourrait améliorer.

### Étape 5 : Télécharger le rapport

Allez à l'onglet **"Rapport Expert"** pour :
- Voir le rapport complet formaté
- **Télécharger en Markdown** (pour éditer, imprimer, partager)
- Copier-coller dans Word/Google Docs si besoin

Le rapport inclut :
- ✅ Résumé exécutif
- ✅ Classifications OMS
- ✅ Qualité des données
- ✅ Anomalies détectées
- ✅ Interprétation clinique
- ✅ Recommandations

---

## 📊 Les différents onglets

### 1. **Tableau de bord** (Vue d'ensemble)
- Les 3 KPI cards principales
- Narratif clinique auto-généré
- Tableau résumé statistique

### 2. **Distributions** (Comportement des données)
- Histogrammes montrant la distribution des Z-scores
- Courbe de référence N(0,1) en pointillé (ce qu'on attendrait)
- Zones ombrées = zones de sévérité OMS
- Optionnel : stratifier par sexe

**Comment lire :**
- Si beaucoup de cas à gauche (< -2) → malnutrition élevée
- Si distribution bien centrée sur 0 → situation normale
- Si beaucoup d'extrêmes (< -3 ou > 3) → vérifier qualité

### 3. **Classification** (Catégories)
- Donuts montrant % Normal vs Sévère vs Modéré
- Bar chart comparant WHZ, HAZ, WAZ côte à côte
- Tableau avec nombres exactes

### 4. **Par sexe** (Différences garçons/filles)
- Box plots pour voir si les garçons/filles ont des Z-scores différents
- Tableau comparatif avec moyennes
- **Test statistique inclus** (si différence significative, elle est notée)

### 5. **Données** (Tableau complet)
- Tous les Z-scores individuels
- Toutes les classifications
- Télécharger en CSV ou Excel

### 6. **Qualité** (Diagnostic)
- Score global détaillé
- Tableau des 10 critères
- Anomalies détectées avec explications
- Ce qu'il faudrait corriger

### 7. **Rapport Expert** (Synthèse)
- Rapport formaté Markdown
- Télécharger pour archiver/partager
- Contient tous les chiffres + interprétations

---

## ❓ Questions fréquentes

### Q: Quel est la différence entre WHZ, HAZ et WAZ ?

**WHZ (Poids-pour-Taille)** = Mesure la malnutrition **actuelle & aiguë**
- Change rapidement (jours/semaines)
- Sensible aux changements récents (derniers mois)
- Détecte malnutrition aiguë

**HAZ (Taille-pour-Âge)** = Mesure le retard de croissance **chronique**
- Change lentement (mois/années)
- Reflète l'état nutritionnel passé
- Détecte malnutrition chronique

**WAZ (Poids-pour-Âge)** = Mesure insuffisance pondérale **générale**
- Combine les deux au-dessus
- Moins spécifique mais complète

**En résumé :**
- **WHZ** = urgence actuelle
- **HAZ** = problème de longue durée
- **WAZ** = situation générale

### Q: Mon score de qualité est 65/100, c'est grave ?

**Réponse :** Dépend du critère problématique.

- ❌ Si **écart-type anormal** : les mesures peuvent être imprécises
- ❌ Si **digit preference** : les décimales sont arrondies
- ❌ Si **trop de flags** : il y a des valeurs aberrantes à vérifier
- ⚠️ Si **age heaping** : les enfants ont probablement des âges approximatifs

**Lire l'explication détaillée** pour savoir si c'est grave ou mineur.

### Q: Que signifie "CRITIQUE" ?

**CRITIQUE** = Situation d'urgence nutritionnelle selon OMS

- **GAM ≥ 15%** → 1 enfant sur 7 a une malnutrition aiguë
- **Stunting ≥ 40%** → 2 enfants sur 5 ont un retard de croissance sévère
- **Underweight ≥ 30%** → 1 enfant sur 3 a un poids insuffisant

**Actions recommandées :**
- Intervention nutritionnelle urgente
- Traitement des cas sévères
- Sensibilisation communautaire

### Q: Les Z-scores calculés sont-ils sauvegardés ?

**OUI.** Les colonnes `WHZ_calc`, `HAZ_calc`, `WAZ_calc` sont ajoutées à vos données et disponibles dans **tous les autres modules** de l'application.

Vous pouvez ensuite :
- Utiliser ces Z-scores pour faire des graphiques
- Les inclure dans vos exports
- Les croiser avec d'autres variables (région, sexe, etc.)

### Q: Puis-je exporter les Z-scores ?

**OUI, 3 façons :**

1. **Onglet Données** → télécharger CSV ou Excel
2. **Onglet Rapport Expert** → télécharger Markdown
3. Via les autres modules (qui ont accès aux Z-scores)

---

## 🎓 Bonus : Interpréter les anomalies

### "Digit preference détecté"
- **Signifie :** Les décimales ne sont pas uniformément distribuées
- **Cause probable :** Les mesures ont été arrondies (ex: 10.5 au lieu de 10.47)
- **Impact :** Les Z-scores sont moins précis
- **Solution :** Re-former les collecteurs sur la précision des mesures

### "Age heaping"
- **Signifie :** Beaucoup d'enfants ont des âges "ronds" (12, 24, 36, 48 mois)
- **Cause probable :** Les mères ne connaissent pas précisément l'âge
- **Impact :** HAZ et WAZ moins précis (basés sur l'âge)
- **Solution :** Améliorer la collecte d'âge (calendrier d'événements)

### "Sex ratio déséquilibré"
- **Signifie :** Beaucoup plus de garçons que de filles (ou vice-versa)
- **Cause probable :** Biais de sélection ou collecte
- **Impact :** Résultats biaisés par sexe
- **Solution :** Vérifier les critères d'inclusion

### "Écart-type anormal"
- **Signifie :** Trop ou pas assez de variabilité dans les mesures
- **Cause probable :** Erreurs de mesure ou données trop homogènes
- **Impact :** Distribution des Z-scores ne suit pas N(0,1)
- **Solution :** Réviser les protocoles de mesure

---

## 🚨 Quand demander de l'aide

Demandez à un expert si :
- Score de qualité < 60
- Plus de 3 anomalies détectées
- Résultats qui ne correspondent pas à votre contexte
- Doute sur les données ou les classifications

---

## 📞 Ressources OMS officielles

- WHO Child Growth Standards : https://www.who.int/tools/child-growth-standards
- Z-Score Implementation (ENA) : https://www.en-net.org/
- Standards de mesure : WHO guidelines

---

**Vous avez maintenant un module au niveau des meilleurs outils OMS/UNICEF.**

Bonne chance ! 📊
