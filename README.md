# Hyundai Used Car Price Analysis

![Status](https://img.shields.io/badge/Status-Active-success)
![Language](https://img.shields.io/badge/R-Tidyverse-blue)
![Data Analysis](https://img.shields.io/badge/Data%20Analysis-Regression-lightcoral)
![Visualization](https://img.shields.io/badge/Visualization-GGPlot2-orange)

---

## Aperçu du projet
![model1](www/logo.png)
Cette application **Shiny** fournit une interface interactive pour analyser les tendances et facteurs influençant le prix des voitures d’occasion Hyundai.  

Elle permet de :  

- Explorer les données brutes des véhicules  
- Résumer les caractéristiques clés à l’aide de statistiques descriptives  
- Visualiser les relations entre prix et variables telles que kilométrage, taille du moteur, type de carburant ou année de fabrication  
- Estimer le prix des véhicules selon leurs caractéristiques  

---

## Jeu de données

Nous utilisons le jeu de données **“Hyundai Used Car Listing”** disponible sur Kaggle :  [→ Accéder au dataset](https://www.kaggle.com/datasets/mysarahmadbhat/hyundai-used-car-listing)

- **Observations :** 4860 voitures  
- **Variables principales :**  
  - `Modèle` : i10, Tucson, etc.  
  - `Année` : année de fabrication  
  - `Prix` : prix de vente  
  - `Transmission` : manuelle / automatique  
  - `Kilométrage` : distance parcourue  
  - `Type de carburant` : essence, diesel, hybride, etc.  
  - `Taxe` : montant annuel  
  - `Consommation (MPG)` : consommation moyenne en miles par gallon  
  - `Taille du moteur` : capacité en litres  

---

## Plan de travail

### 1. Visualisation des données brutes
- Présentation sous forme de tableau interactif  
- Exploration des variables pour une vue globale  

### 2. Statistiques descriptives
- Moyenne, médiane, écart-type et quartiles pour les variables numériques  
- Identification des tendances générales et des valeurs extrêmes  

### 3. Graphiques
- **Nuages de points :** relation entre prix et variables numériques (kilométrage, moteur, consommation)  
- **Boxplots :** distribution des prix selon catégories (transmission, carburant)  
- **Diagrammes en barres :** relation entre prix et interaction de variables numériques et catégorielles  

### 4. Estimation des prix
- Modélisation par régression linéaire  
- Variables explicatives : année, kilométrage, taille du moteur, type de carburant  
- Prévision personnalisée du prix et identification des variables influentes  

---

### Lancer l’application Shiny

```R
shiny::runApp("app/")
