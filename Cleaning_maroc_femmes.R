library(tidyverse)
library(readxl)

dataFemmes <- read_xlsx("Vrais_data_femmes.xlsx")

cleanDataF <- data.frame(id= dataH$`ID de la réponse`)

################################## Âge #################
table(dataFemmes$`Quel âge avez-vous ?`)

cleanDataF$ses_age <- NA
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "18 - 20 ans"] <- "18 à 20 ans"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "21 - 29 ans"] <- "21 à 29 ans"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "30 - 39 ans"] <- "30 à 39 ans"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "40-49"] <- "40 à 49 ans"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "50-59"] <- "50 à 59 ans"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "60 ou plus"] <- "60 ou plus"
table(cleanDataF$ses_age)

############### État civil #######

################ les données ne sont pas les mêmes ############
table(dataFemmes$`Quel est votre état civil ?`)

cleanDataF$ses_etatcivil <- NA

cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Célibataire"
] <- "Célibataire"
cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Divorcée/séparée"
] <- "Divorcée/séparée"
cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Mariée"
] <- "Mariée"
cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Veuve"
] <- "Veuve"

table(cleanDataF$ses_etatcivil)


##################### Lieu ######################
table(dataF$Lieu)

cleanDataF$ses_lieu <- NA
cleanDataF$ses_lieu[dataFemmes$Lieu == "Ahouli" | dataF$Lieu == "Ksar Ahouli"] <- "Ahouli"
cleanDataF$ses_lieu[dataFemmes$Lieu == "Mibladen"] <- "Mibladen"

table(cleanData$ses_lieu)


#################### Statut professionel actuel ##########
#################### est-ce qu'on inclue si invalide est seulement 2 ##########
table(dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`)

cleanDataF$statutTravail <- NA

cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Femme au foyer"] <- "Femme au foyer"
cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Travailleuse autonome"] <- "Travailleuse autonome"
cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Sans emploi - ne recherche pas d'emploi"] <- "Ne recherche pas d'emploi"
cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Sans emploi - recherche un emploi"] <- "Recherche un emploi"

table(cleanDataF$statutTravail)
