
# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ade4) 


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

vd <- names(vd_clean)

data2 <- data[, vd]

data2[is.na(data2)] <- 0


data2 <- mutate_at(data2, vars(vd), as.factor)

data2 <- as.factor(data2)
data2

acm <- dudi.acm(data2, scannf = FALSE, nf = 4)









# Autre test d'ACM --------------------------------------------------------

library(corrr)
library(ggcorrplot)
library(ggplot2)
library(FactoMineR)
library(factoextra)

fviz_pca_var(data.pca, col.var = "black")


data2 <- as.numeric(data2)

pca_result <- prcomp(data2, scale. = TRUE)

# Extract PC scores
pc_scores <- as.data.frame(pca_result$x[, 1:2])

# Create a data frame with the variables and PC scores
pca_data <- cbind(data2, pc_scores)

# Plot the first two principal components
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "PCA Plot",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

