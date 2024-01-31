
# Load les packages -------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)


# Load les données --------------------------------------------------------


data <- readRDS("Data/data.rds")

vd_clean <- c(
  santegenerale = "Santé générale",
  santementale = "Santé mentale",
  consommationalcool = "Consommation d'alcool",
  tabac = "Tabac",
  douleursmusculaires = "Douleurs musculaires",
  douleursarticulaires = "Douleurs articulaires",
  troublesommeil = "Troubles du sommeil",
  maladiecardiovasculaire = "Maladie cardiovasculaire",
  maladiecardiovasculaire_hypertension = "Maladie cardiovasculaire - Hypertension",
  maladiecardiovasculaire_coronarienne = "Maladie cardiovasculaire - Coronarienne",
  maladiecardiovasculaire_fatigue = "Maladie cardiovasculaire - Fatigue",
  maladiechronique = "Maladie chronique",
  maladiechronique_colon = "Maladie chronique - Colon",
  maladiechronique_diabete = "Maladie chronique - Diabète",
  maladiechronique_rein = "Maladie chronique - Rein",
  maladiechronique_respiratoire = "Maladie chronique - Respiratoire",
  maladiechronique_genou = "Maladie chronique - Genou",
  maladiechronique_estomac = "Maladie chronique - Estomac",
  maladiechronique_neuro = "Maladie chronique - Neuro",
  maladiechronique_yeux = "Maladie chronique - Yeux",
  maladiechronique_dos = "Maladie chronique - Dos",
  maladiechronique_sciatique = "Maladie chronique - Sciatique",
  maladiechronique_rhumatisme = "Maladie chronique - Rhumatisme",
  maladiechronique_silicose = "Maladie chronique - Silicose",
  maladierespiratoire = "Maladie respiratoire",
  maladierespiratoire_asthme = "Maladie respiratoire - Asthme",
  maladierespiratoire_bronchite = "Maladie respiratoire - Bronchite",
  maladierespiratoire_rhume = "Maladie respiratoire - Rhume",
  maladierespiratoire_amygdalite = "Maladie respiratoire - Amygdalite",
  maladierespiratoire_yeux = "Maladie respiratoire - Yeux",
  problemepeau = "Problème de peau",
  problemepeau_demangeaisons = "Problème de peau - Démangeaisons",
  problemepeau_eruptions = "Problème de peau - Éruptions",
  problemepeau_allergie = "Problème de peau - Allergie",
  problemepeau_autre = "Problème de peau - Autre",
  problemepeau_squelette = "Problème de peau - Squelette",
  problemepeau_kyste = "Problème de peau - Kyste",
  problemeneuro = "Problème neuro",
  problemeneuro_maux_de_tete = "Problème neuro - Maux de tête",
  problemeneuro_vertige = "Problème neuro - Vertige",
  problemeneuro_parkinson = "Problème neuro - Parkinson",
  problemeneuro_oreille = "Problème neuro - Oreille",
  problemeneuro_syncope = "Problème neuro - Syncope",
  problemedigestif = "Problème digestif",
  problemedigestif_douleurabdo = "Problème digestif - Douleur abdominale",
  problemedigestif_nausees = "Problème digestif - Nausées",
  problemedigestif_vomissements = "Problème digestif - Vomissements",
  blessurerecentes = "Blessures récentes",
  blessurerecentes_main = "Blessures récentes - Main",
  blessurerecentes_jambe = "Blessures récentes - Jambe",
  blessurerecentes_epaule = "Blessures récentes - Épaule",
  blessurerecentes_dos = "Blessures récentes - Dos",
  blessurerecentes_yeux = "Blessures récentes - Yeux",
  blessurerecentes_oreille = "Blessures récentes - Oreille"
)

vd <- names(vd_clean) ## assignes les noms de variables cleaner à vd

data2 <- data[, vd] ## va chercher seulement les variables dépendantes dans le jeu de donnée

data2[is.na(data2)] <- 0 ## transforme les NA en 0 

pca <- PCA(X = data2)




# Loader des variables pour Une PCA des blessures récentes ----------------------------------------------------

data2$pca_blessurerecentes <- pull(data2 %>%
  select(starts_with('blessurerecentes')) %>%
  mutate(sum = rowSums(across(everything()))), sum)


# Loader des variables pour une PCA des problèmes digestifs -----------------

data2$pca_problemedigestif <- pull(data2 %>% 
  select(starts_with('problemedigestif')) %>% 
  mutate(sum = rowSums(across(everything()))), sum)


# Loader des variables pour une PCA des problèmes neuro -------------------

data2$pca_problemeneuro <- pull(data2 %>% 
  select(starts_with('problemeneuro')) %>% 
  mutate(sum = rowSums(across(everything()))), sum)


# Loader des variables pour une PCA des problèmes de peau -------------------

data2$pca_problemepeau <- pull(data2 %>% 
  select(starts_with('problemepeau')) %>% 
  mutate(sum = rowSums(across(everything()))), sum)


# Loader des variables pour une PCA des maladies respiratoires -------------------

data2$pca_maladierespiratoire <- pull(data2 %>% 
  select(starts_with('maladierespiratoire')) %>% 
  mutate(sum = rowSums(across(everything()))), sum)


# Loader des variables pour une PCA des maladies chroniques -------------------

data2$pca_maladiechronique <- pull(data2 %>% 
  select(starts_with('maladiechronique')) %>% 
  mutate(sum = rowSums(across(everything()))), sum)


# Loader des variables pour une PCA des maladies cardio-vasculaires -------------------

data2$pca_maladiecardiovasculaire <- pull(data2 %>% 
  select(starts_with('maladiecardiovasculaire')) %>% 
  mutate(sum = rowSums(across(everything()))), sum)



# Créer un nouveau dataframe pour la PCA ----------------------------------
pca_columns <- select(data2, starts_with('pca')) ## allez chercher dans la colonne ce qui commence avec pca

specific_columns <- select(data2, santegenerale, santementale, consommationalcool, tabac, douleursmusculaires, douleursarticulaires, troublesommeil) ## allez chercher les vecteurs spécifiques dans la base de donnée

vrai_pca <- cbind(pca_columns, specific_columns) ## assignez à vrai_pca les vecteurs de colonnes

pca2 <- PCA(X = vrai_pca, scale.unit = TRUE)

pca2
summary(pca2)
pca2[["eig"]] ## Garder aveec les eigenvalue > 1 

pca_dims <- pca[['var']][['coord']]
