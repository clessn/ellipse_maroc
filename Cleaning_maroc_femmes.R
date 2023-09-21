library(tidyverse)
library(readxl)

dataF <- read_xlsx("Vrais_data_femmes.xlsx")

################################## Âge #################
table(dataF$`Quel âge avez-vous ?`)

cleanData$ses_age <- NA
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "18 - 20 ans"] <- "18 à 20 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "21 - 29 ans"] <- "21 à 29 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "30 - 39 ans"] <- "30 à 39 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "40-49"] <- "40 à 49 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "50-59"] <- "50 à 59 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "60 ou plus"] <- "60 ou plus"
table(cleanData$ses_age)

############### État civil #######
table(dataF$`Quel est votre état civil ?`)

cleanData$ses_etatcivil <- NA

cleanData$ses_etatcivil[dataF$`Quel est votre état civil ?` == "Célibataire"
] <- "Célibataire"
cleanData$ses_etatcivil[dataF$`Quel est votre état civil ?` == "Divorcée/séparée"
] <- "Divorcée/séparée"
cleanData$ses_etatcivil[dataF$`Quel est votre état civil ?` == "Mariée"
] <- "Mariée"
cleanData$ses_etatcivil[dataF$`Quel est votre état civil ?` == "Veuve"
] <- "Veuve"

table(cleanData$ses_etatcivil)


##################### Lieu ######################
table(dataF$Lieu)

cleanData$ses_lieu <- NA
cleanData$ses_lieu[dataF$Lieu == "Ahouli" | dataF$Lieu == "Ksar Ahouli"] <- "Ahouli"
cleanData$ses_lieu[dataF$Lieu == "Mibladen"] <- "Mibladen"

table(cleanData$ses_lieu)


#################### Statut professionel actuel ##########
table(dataF$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`)

cleanData$statutTravail <- NA
