# Packages ----------------------------------------------------------------
library(tidyverse)

# Data --------------------------------------------------------------------
Data <- readRDS("Data/data.rds")


### Check each column, replace NA with 0 when appropriate

### VIS


### VDS
#"santegenerale"
#"consommationalcool"
#"tabac" 
#maladiecardiovasculaire"
#"maladiechronique"
#"maladierespiratoire"
#"problemepeau"
#"problemepeau_allergie"
#"problemepeau_autre"
#problemepeau_demangeaisons
#"problemepeau_eruptions"
#"problemepeau_squelette"
#"problemeneuro"
#"problemeneuro_parkinson"
#"problemeneuro_maux_de_tete"
#"problemeneuro_vertige"
#"problemedigestif"
#"problemedigestif_douleurabdo"
#"problemedigestif_nausees"
#"douleursmusculaires"
#"blessurerecentes_main"
#"blessurerecentes_jambe"
#blessurerecentes_epaule"
#"blessurerecentes_dos"
#blessurerecentes_yeux"
#"blessurerecentes_oreille"
#"troublesommeil"

table(Data$santegenerale)
table(Data$consommationalcool)
table(Data$tabac)
table(Data$maladiecardiovasculaire)
table(Data$maladiecardiovasculaire_hypertension)
table(Data$maladiecardiovasculaire_coronarienne)
table(Data$maladiecardiovasculaire_fatigue)
table(Data$maladiechronique)
table(Data$maladierespiratoire)
table(Data$problemepeau)
table(Data$problemepeau_allergie)
table(Data$problemepeau_autre)
table(Data$problemepeau_demangeaisons)
table(Data$problemepeau_eruptions)
table(Data$problemepeau_squelette)
table(Data$problemeneuro)
table(Data$problemeneuro_parkinson)
table(Data$problemeneuro_maux_de_tete)
table(Data$problemeneuro_vertige)
table(Data$problemedigestif)
table(Data$problemedigestif_douleurabdo)
table(Data$problemedigestif_nausees)
table(Data$douleursmusculaires)
table(Data$douleursarticulaires)
table(Data$blessurerecentes_main)
table(Data$blessurerecentes_jambe)
table(Data$blessurerecentes_epaule)
table(Data$blessurerecentes_dos)
table(Data$blessurerecentes_yeux)
table(Data$blessurerecentes_oreille)
table(Data$troublesommeil)
