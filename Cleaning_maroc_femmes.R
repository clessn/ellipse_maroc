library(tidyverse)
library(readxl)

dataFemmes <- read_xlsx("Vrais_data_femmes.xlsx")

cleanDataF <- data.frame(id= dataFemmes$`ID de la réponse`)

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


#################### Cmb d'enfants ####################
table(dataFemmes$`Combien d'enfants avez-vous ?`)

cleanDataF$ses_enfants <- NA

cleanDataF$ses_enfants[is.na(dataFemmes$`Combien d'enfants avez-vous ?`)] <- "0"
cleanDataF$ses_enfants[dataFemmes$`Combien d'enfants avez-vous ?` == "1"] <- "1"
cleanDataF$ses_enfants[dataFemmes$`Combien d'enfants avez-vous ?` == "2" |
                        dataFemmes$`Combien d'enfants avez-vous ?` == "3"] <- "2-3"
cleanDataF$ses_enfants[dataFemmes$`Combien d'enfants avez-vous ?` == "4" |
                        dataFemmes$`Combien d'enfants avez-vous ?` == "5"] <- "4-5"
cleanDataF$ses_enfants[dataFemmes$`Combien d'enfants avez-vous ?` == "6" |
                        dataFemmes$`Combien d'enfants avez-vous ?` == "10"] <- "6-10"

table(cleanDataF$ses_enfants)

##################### Provenance ###################
table(dataFemmes$`D'où venez vous ?`)

cleanDataF$lieu <- NA


########################### langues ##################
table(dataFemmes$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`)

cleanDataF$ses_lang_fr <- NA
cleanDataF$ses_lang_amaz <-NA
cleanDataF$ses_lang_darija <- NA
cleanDataF$ses_lang_arabe <- NA
cleanDataF$ses_lang_ang <- NA

cleanDataF$ses_lang_fr <- as.integer(grepl("Français", dataFemmes$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanDataF$ses_lang_fr)

cleanDataF$ses_lang_amaz <- as.integer(grepl("Amazigh", dataFemmes$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanDataF$ses_lang_amaz)

cleanDataF$ses_lang_darija <- as.integer(grepl("Darija", dataFemmes$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanDataF$ses_lang_amaz)

cleanDataF$ses_lang_ang <- as.integer(grepl("Anglais", dataFemmes$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanDataF$ses_lang_ang)

cleanDataF$ses_lang_arabe <- as.integer(grepl("Arabe", dataFemmes$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanDataF$ses_lang_arabe)

############################ niveau études ####################
table(dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?`)

cleanDataF$niveauetude <- NA

cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Sans diplôme"] <- "Sans diplôme"
cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Enseignement primaire"] <- "Enseignement primaire"
cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Enseignement secondaire"] <- "Enseignement secondaire"
cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Enseignement postsecondaire"] <- "Enseignement postsecondaire"

table(cleanDataF$niveauetude)


############################# logement principal ###################
table(dataFemmes$`Où est votre logement principal?`)




############################ Distance d'Ahouli #################
table(dataFemmes$`Distance d'Ahouli`)



########################## Membres familles ###################
table(dataFemmes$`Vivez-vous avec les membres de votre famille?`)

cleanDataF$membrefamilleensemble <- NA

cleanDataF$membrefamilleensemble[dataFemmes$`Vivez-vous avec les membres de votre famille?` ==
                                   "Non"] <- 0
cleanDataF$membrefamilleensemble[dataFemmes$`Vivez-vous avec les membres de votre famille?` ==
                                   "Oui"] <- 1

table(cleanDataF$membrefamilleensemble)


######################### rendre visite ###################
####################### nécessaire ?? ####################
table(dataFemmes$`Si vous ne vivez pas avec votre famille, combien de fois rendez-vous visite à votre famille?`)



######################### pièce habitable ###################
table(dataFemmes$`Quel est le nombre de pièce habitable de votre logement?`)

cleanDataF$nombrepiecehabitable <- NA

cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "1"] <- "1"
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "2"] <- "2"
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "3"] <- "3"
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "4"] <- "4"
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "5"] <- "5"
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "6"] <- "6"
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "7"] <- "7"

table(cleanDataF$nombrepiecehabitable)





############################# cmb de personnes vivent là ########################
table(dataFemmes$`Combien de personnes vivent dans le logement ?`)

cleanDataF$nbpersonnelogement <- NA 

cleanDataF$nbpersonnelogement[dataFemmes$`Combien de personnes vivent dans le logement ?` ==
                                "1" |
                                dataFemmes$`Combien de personnes vivent dans le logement ?`==
                                "2"] <- "1 - 2"
cleanDataF$nbpersonnelogement[dataFemmes$`Combien de personnes vivent dans le logement ?` ==
                                "3" |
                                dataFemmes$`Combien de personnes vivent dans le logement ?`==
                                "4"] <- "3 - 4"
cleanDataF$nbpersonnelogement[dataFemmes$`Combien de personnes vivent dans le logement ?` ==
                                "5" |
                                dataFemmes$`Combien de personnes vivent dans le logement ?`==
                                "6"] <- "5 - 6"
cleanDataF$nbpersonnelogement[dataFemmes$`Combien de personnes vivent dans le logement ?` ==
                                "7" |
                                dataFemmes$`Combien de personnes vivent dans le logement ?`==
                                "8"] <- "7 - 8"
cleanDataF$nbpersonnelogement[dataFemmes$`Combien de personnes vivent dans le logement ?` ==
                                "9" |
                                dataFemmes$`Combien de personnes vivent dans le logement ?` ==
                                "20"] <- "9 ou plus"

table(cleanDataF$nbpersonnelogement)
