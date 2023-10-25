# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)

# Data --------------------------------------------------------------------
dataH <- read_xlsx("raw_data/Data_hommes.xlsx",sheet = "Responses")

cleanData <- data.frame(id= dataH$`ID de la réponse`)


########################### Âge #################
table(dataH$`Quel âge avez-vous ?`)

cleanData$ses_age <- NA
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "18 - 20 ans"] <- "18_20"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "21 - 29 ans"] <- "21_29"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "30 - 39 ans"] <- "30_39"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "40-49"] <- "40_49"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "50-59"] <- "50_59"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "60 ou plus"] <- "60+"
table(cleanData$ses_age)
cleanData$ses_age <- factor(cleanData$ses_age,
                            ordered = TRUE,
                            levels = c("18_20", "21_29", "30_39",
                                       "40_49", "50_59", "60+"))

table(cleanData$ses_age)


########################## Lieu ###################
table(dataH$Lieu)

cleanData$ses_lieu <- NA
cleanData$ses_lieu[dataH$Lieu == "Ahou" | dataH$Lieu == "Ahouli" | dataH$Lieu ==
                     "AhouLi" | dataH$Lieu == "Ahouliy"] <- "Ahouli"
cleanData$ses_lieu[dataH$Lieu == "Mibladen"] <- "Mibladen"
cleanData$ses_lieu <- factor(cleanData$ses_lieu)
table(cleanData$ses_lieu)

########################### Revenu ###################
table(dataH$`Quel est le montant total de vos revenus annuels?`)

cleanData$ses_revenu <- NA
cleanData$ses_revenu[dataH$`Quel est le montant total de vos revenus annuels?` 
                     == "0 à 2000 dirhams" |
                       dataH$`Quel est le montant total de vos revenus annuels?`
                     == "2001 à 5000 dirhams"] <- "low"
cleanData$ses_revenu[dataH$`Quel est le montant total de vos revenus annuels?` 
                     == "5001 à 7000 dirhams" |
                       dataH$`Quel est le montant total de vos revenus annuels?`
                     == "7001 à 10 000 dirhams"] <- "mid"
cleanData$ses_revenu[dataH$`Quel est le montant total de vos revenus annuels?`==
                       "Plus de 10 000 dirhams"] <- "high"
cleanData$ses_revenu <- factor(cleanData$ses_revenu, ordered = TRUE,
                               levels = c("low", "mid", "high"))
table(cleanData$ses_revenu)
unique(cleanData$ses_revenu)


############ Qualité environnement travail ########
table(dataH$`En général, diriez-vous que la qualité de l'environnement de travail est:`)

cleanData$qualiteTravail <- NA
cleanData$qualiteTravail[dataH$`En général, diriez-vous que la qualité de l'environnement de travail est:`
                         == "Mauvaise"] <- 0
cleanData$qualiteTravail[dataH$`En général, diriez-vous que la qualité de l'environnement de travail est:`
                         == "Médiocre"] <- 0.33
cleanData$qualiteTravail[dataH$`En général, diriez-vous que la qualité de l'environnement de travail est:`
                         == "Moyenne"] <- 0.66
cleanData$qualiteTravail[dataH$`En général, diriez-vous que la qualité de l'environnement de travail est:`
                         == "Bonne"] <- 1

table(cleanData$qualiteTravail)


############## État civil #######
table(dataH$`Quel est votre état civil ?`)

cleanData$ses_etatcivil <- NA
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Célibataire"] <- "celib"
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Divorcé/séparé"] <- "divorce"
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Marié"] <- "marie"
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Veuf"] <- "veuf"
cleanData$ses_etatcivil <- factor(cleanData$ses_etatcivil)
table(cleanData$ses_etatcivil)

################## nb enfants ##################
table(dataH$`Combien d'enfants avez-vous ?`)

cleanData$ses_enfants <- NA

cleanData$ses_enfants <- as.numeric(dataH$`Combien d'enfants avez-vous ?`)
cleanData$ses_enfants[is.na(dataH$`Combien d'enfants avez-vous ?`)] <- 0

class(cleanData$ses_enfants)
table(cleanData$ses_enfants)


###################### Statut professionel actuel ####################
table(dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`)

cleanData$ses_statutTravail <- NA
cleanData$ses_statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                        == "Employé,Retraité"] <- "employe_retraite"
cleanData$ses_statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                       == "Retraité"] <- "retraite"
cleanData$ses_statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                       == "Travailleur autonome"] <- " autonome"
cleanData$ses_statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                       == "Travailleur autonome,Retraité"] <- "autonome_retraite"
cleanData$ses_statutTravail <- factor(cleanData$ses_statutTravail)
table(cleanData$ses_statutTravail)


####################### Provenance ############################

###################### laissez aux autres #######################
table(dataH$`D'où venez vous ?`)










###################### Langue ############################
table(dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`)

cleanData$ses_lang_fr <- NA
cleanData$ses_lang_amaz <-NA
cleanData$ses_lang_darija <- NA
cleanData$ses_lang_arabe <- NA
cleanData$ses_lang_ang <- NA
cleanData$ses_lang_espagnol <- NA
cleanData$ses_lang_allem <- NA

cleanData$ses_lang_fr <- as.integer(grepl("Français", dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanData$ses_lang_fr)

cleanData$ses_lang_amaz <- as.integer(grepl("Amazigh", dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanData$ses_lang_amaz)

cleanData$ses_lang_darija <- as.integer(grepl("Darija", dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanData$ses_lang_amaz)

cleanData$ses_lang_ang <- as.integer(grepl("Anglais", dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanData$ses_lang_ang)

cleanData$ses_lang_espagnol <- as.integer(grepl("Espagnol", dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanData$ses_lang_espagnol)

cleanData$ses_lang_arabe <- as.integer(grepl("Arabe", dataH$`Quelles sont les langues que vous maîtrisez ? (Plusieurs réponses possibles)`))
table(cleanData$ses_lang_arabe)

######################## Niveau d'étude ###########################
table(dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`)

cleanData$niveauetude <- NA
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Enseignement postsecondaire"] <- "postsecondaire"
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Enseignement primaire"] <- "primaire"
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Enseignement secondaire"] <- "secondaire"
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Sans diplôme"] <- "sans_diplome"
cleanData$niveauetude <- factor(cleanData$niveauetude, ordered = TRUE,
                                levels = c("sans_diplome", "primaire", "secondaire", "postsecondaire"))
table(cleanData$niveauetude)
unique(cleanData$niveauetude)


########################### Distance d'Ahouli ######################

########################### Problème avec deux données ######################
table(dataH$`Distance d'Ahouli`)
cleanData$distanceahouli <- NA
cleanData$distanceahouli <- gsub(",", ".", dataH$`Distance d'Ahouli`)
cleanData$distanceahouli <- gsub("[^0-9.]", "", cleanData$distanceahouli)
cleanData$distanceahouli <- as.numeric(cleanData$distanceahouli)
table(cleanData$distanceahouli)
hist(cleanData$distanceahouli)

######### Distance entre d'où venez vous et mine ########## 
table(dataH$`Calcul distance Mine d'Ahouli`)
cleanData$distanceahouli_provenance <- NA
cleanData$distanceahouli_provenance <- gsub(",", ".", dataH$`Calcul distance Mine d'Ahouli`)
cleanData$distanceahouli_provenance <- gsub("[^0-9.]", "", cleanData$distanceahouli_provenance)
cleanData$distanceahouli_provenance <- as.numeric(cleanData$distanceahouli_provenance)
table(cleanData$distanceahouli_provenance)
hist(cleanData$distanceahouli_provenance)


############################ Vivez vous avec les membres de votre famille ###############
table(dataH$`Vivez-vous avec les membres de votre famille?`)

cleanData$membrefamilleensemble <- NA

cleanData$membrefamilleensemble[dataH$`Vivez-vous avec les membres de votre famille?` ==
                                  "Non"] <- 0
cleanData$membrefamilleensemble[dataH$`Vivez-vous avec les membres de votre famille?` ==
                                  "Oui"] <- 1

table(cleanData$membrefamilleensemble)


########################## Nombre pièces habitables ##############
table(dataH$`Quel est le nombre de pièce habitable de votre logement?`)

cleanData$nombrepiecehabitable <- NA

cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "1"] <- 1
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "2"] <- 2
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "3"] <- 3
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "4"] <- 4
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "5"] <- 5
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "6"] <- 6
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "7"] <- 7

table(cleanData$nombrepiecehabitable)

########################## Personnes dans le logement ######################
table(dataH$`Combien de personnes vivent dans le logement ?`)

cleanData$nbpersonnelogement <- NA
cleanData$nbpersonnelogement <- as.numeric(dataH$`Combien de personnes vivent dans le logement ?`)
cleanData$nbpersonnelogement[is.na(dataH$`Combien de personnes vivent dans le logement ?`)] <- 0
 
class(cleanData$nbpersonnelogement)
table(cleanData$nbpersonnelogement)


########################## visite famille #########################
table(dataH$`Combien de fois rendez-vous visite à votre famille?`)

cleanData$visitefamille <- NA

cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Plusieurs fois par mois"] <- 0.25
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Plusieurs fois par semaine"] <- 0.75
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Tous les jours"] <- 1
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Une fois par mois"] <- 0
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Une fois par semaine"] <- 0.5

table(cleanData$visitefamille)


########################## réparation majeure #######################
table(dataH$`Est-ce que le logement a besoin de réparation majeure?`)

cleanData$reparationmajeure <- NA

cleanData$reparationmajeure[dataH$`Est-ce que le logement a besoin de réparation majeure?` ==
                              "Oui"] <- 1
cleanData$reparationmajeure[dataH$`Est-ce que le logement a besoin de réparation majeure?` ==
                              "Non"] <- 0

table(cleanData$reparationmajeure)


############################## réparation mineure ####################
table(dataH$`Est-ce que le logement a besoin de réparation mineure?`)

cleanData$reparationmineure <- NA

cleanData$reparationmineure[dataH$`Est-ce que le logement a besoin de réparation mineure?` ==
                              "Oui"] <- 1
cleanData$reparationmineure[dataH$`Est-ce que le logement a besoin de réparation mineure?` ==
                              "Non"] <- 0

table(cleanData$reparationmineure)


############################## eau courante #########################
table(dataH$`Est-ce que le logement a l'eau courante?`)

cleanData$eaucourante <- NA

cleanData$eaucourante[dataH$`Est-ce que le logement a l'eau courante?` == 
                        "Oui"] <- 1
cleanData$eaucourante[dataH$`Est-ce que le logement a l'eau courante?` == 
                        "Non"] <- 0

table(cleanData$eaucourante)

############################### toilette #######################
table(dataH$`Est-ce que le logement a des toilettes?`)

cleanData$toilette_in_logement <- NA

cleanData$toilette_in_logement[dataH$`Est-ce que le logement a des toilettes?` == 
                     "Non"] <- "no"
cleanData$toilette_in_logement[dataH$`Est-ce que le logement a des toilettes?` ==
                     "Oui, à l'extérieur"] <- "yes_outside"
cleanData$toilette_in_logement[dataH$`Est-ce que le logement a des toilettes?` ==
                     "Oui, dans la maison" |
                     dataH$`Est-ce que le logement a des toilettes?` ==
                     "Oui, dans la maison,Oui, à l'extérieur"] <- "yes"
cleanData$toilette_in_logement <- factor(cleanData$toilette_in_logement,
                                         ordered = TRUE,
                                         levels = c("no", "yes_outside",
                                                    "yes"))
table(cleanData$toilette_in_logement)
unique(cleanData$toilette_in_logement)


################################ revenus principal ####################
table(dataH$`Quelles sont vos sources de revenus principales?`)

# Agriculture
cleanData$revenuprincipal_agriculture <- NA
cleanData$revenuprincipal_agriculture <- as.integer(grepl("Agriculture", dataH$`Quelles sont vos sources de revenus principales?`))
table(cleanData$revenuprincipal_agriculture)

# Mines
cleanData$revenuprincipal_mines <- NA
cleanData$revenuprincipal_mines <- as.integer(grepl("Mines", dataH$`Quelles sont vos sources de revenus principales?`))
table(cleanData$revenuprincipal_mines)

# Artisans
cleanData$revenuprincipal_artisanat <- NA
cleanData$revenuprincipal_artisanat <- as.integer(grepl("Artisans", dataH$`Quelles sont vos sources de revenus principales?`))
table(cleanData$revenuprincipal_artisanat)

# Élevage
cleanData$revenuprincipal_elevage <- NA
cleanData$revenuprincipal_elevage <- as.integer(grepl("Élevage", dataH$`Quelles sont vos sources de revenus principales?`))
table(cleanData$revenuprincipal_elevage)

# Commerce
cleanData$revenuprincipal_commerce <- NA
cleanData$revenuprincipal_commerce <- as.integer(grepl("Commerce", dataH$`Quelles sont vos sources de revenus principales?`))
table(cleanData$revenuprincipal_commerce)

# Aides publiques ou privées
cleanData$revenuprincipal_aides <- NA
cleanData$revenuprincipal_aides <- as.integer(grepl("Aides publiques ou privées", dataH$`Quelles sont vos sources de revenus principales?`))
table(cleanData$revenuprincipal_aides)


##############################  accès services de santé #################
table(dataH$`Avez-vous accès à des services de santé?`)


cleanData$accessoinsante <- NA

cleanData$accessoinsante[dataH$`Avez-vous accès à des services de santé?` ==
                           "Non"] <- "Non"
cleanData$accessoinsante[dataH$`Avez-vous accès à des services de santé?` ==
                   "Non,Si non, à quelle distance? - 3 km" |
                     dataH$`Avez-vous accès à des services de santé?` ==
                     "Non,Si non, à quelle distance? - 5 km" |
                     dataH$`Avez-vous accès à des services de santé?` ==
                     "Non,Si non, à quelle distance? - 10 km" |
                     dataH$`Avez-vous accès à des services de santé?` ==
                     "Non,Si non, à quelle distance? - 15 km" |
                     dataH$`Avez-vous accès à des services de santé?` ==
                     "Oui,Si non, à quelle distance? - 15 km" |
                     dataH$`Avez-vous accès à des services de santé?` ==
        "Oui,Si non, à quelle distance? - 15 km à Midelt ( soins complets)" |
          dataH$`Avez-vous accès à des services de santé?` ==
          "Non,Si non, à quelle distance? - 15 km à Midelt" |
          dataH$`Avez-vous accès à des services de santé?` == 
          "Non,Si non, à quelle distance? - 15 km Midelt"] <- "Non, les soins les plus proches sont à une distance maximale de 15 km"

cleanData$accessoinsante[dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 20 km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 22 km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 25 km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 27 km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 30 km"] <- "Non, les soins les plus proches sont à une distance entre 16 et 30 km"

cleanData$accessoinsante[dataH$`Avez-vous accès à des services de santé?`==
                           "Non,Si non, à quelle distance? - 32 km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 32km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 43" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 43 km" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 60" |
                           dataH$`Avez-vous accès à des services de santé?` ==
                           "Non,Si non, à quelle distance? - 60 km"] <- "Non, les soins les plus proches sont à une distance de plus de 31 km"

cleanData$accessoinsante[dataH$`Avez-vous accès à des services de santé?` ==
                           "Oui"] <- "Oui"

table(cleanData$accessoinsante)

########################### Assurance maladie ##################
table(dataH$`Avez vous une assurance médicale?`)

cleanData$assurancemedicale <- NA

cleanData$assurancemedicale[dataH$`Avez vous une assurance médicale?` == 
                              "Oui"] <- 1
cleanData$assurancemedicale[dataH$`Avez vous une assurance médicale?` == 
                              "Non"] <- 0

table(cleanData$assurancemédicale)


############################ Assainissement ############
table(dataH$`Y a-t-il un système d'assainissement dans la communauté?`)

cleanData$assainissement <- NA

cleanData$assainissement[dataH$`Y a-t-il un système d'assainissement dans la communauté?` ==
                           "Oui"] <- 1
cleanData$assainissement[dataH$`Y a-t-il un système d'assainissement dans la communauté?` ==
                           "Non"] <- 0

table(cleanData$assainissement)


###################### école primaire ########################
table(dataH$`Y a-t-il une école primaire dans la communauté?`)

cleanData$ecoleprimaire <- NA

cleanData$ecoleprimaire[dataH$`Y a-t-il une école primaire dans la communauté?` ==
                   "Non"] <- "Non"
cleanData$ecoleprimaire[dataH$`Y a-t-il une école primaire dans la communauté?` ==
                          "Non,Si non, à quelle distance? - 15 km" |
                       dataH$`Y a-t-il une école primaire dans la communauté?`
                       == "Non,Si non, à quelle distance? - 3km" ] <-
  "Non, l'école est à une distance de maximum 15 km"
cleanData$ecoleprimaire[dataH$`Y a-t-il une école primaire dans la communauté?` ==
                          "Oui"] <- "Oui"

table(cleanData$ecoleprimaire)


######################### école secondaire ####################
table(dataH$`Y a-t-il une école secondaire dans la communauté?`)

cleanData$ecolesecondaire <- NA

cleanData$ecolesecondaire[dataH$`Y a-t-il une école secondaire dans la communauté?` == 
                            "Oui"] <- "Oui"
cleanData$ecolesecondaire[dataH$`Y a-t-il une école secondaire dans la communauté?` == 
                            "Non,Si non, à quelle distance? - 3km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 3 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 4 km" | 
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            " Non,Si non, à quelle distance? - 5 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 6 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 7 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 8 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 10 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Oui,Si non, à quelle distance? - 15 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 15 km"] <- "Non, l'école se trouve à une distance de maximum 15 km "

cleanData$ecolesecondaire[dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 20 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 22 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 25 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 27 km" |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 30 km " |
                            dataH$`Y a-t-il une école secondaire dans la communauté?` ==
                            "Non,Si non, à quelle distance? - 60 km"] <- "Non, l'école se trouve à une distance maximum de 60 km"


table(cleanData$ecolesecondaire)
######################### système de transport ###################
######################### difficile à coder ##################
table(dataH$`Y a-t-il un système de transport collectif dans la communauté?`)

cleanData$systemetransport <- NA

cleanData$systemetransport[dataH$`Y a-t-il un système de transport collectif dans la communauté?` ==
                             "Non"] <- "Non"
cleanData$systemetransport[dataH$`Y a-t-il un système de transport collectif dans la communauté?` ==
                             "Non"] <- "Non"


######################### Épicerie #######################
table(dataH$`Où allez vous pour acheter l'épicerie?`)
# Initialisation des variables à 0
cleanData$epicerie_midelt <- 0
cleanData$epicerie_boumia <- 0
cleanData$epicerie_nzala <- 0
cleanData$epicerie_rich <- 0
cleanData$epicerie_zaida <- 0
cleanData$epicerie_beram <- 0
cleanData$epicerie_missour <- 0

# Remplissage des variables en utilisant grepl()
cleanData$epicerie_midelt[grepl("Midelt", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1
cleanData$epicerie_boumia[grepl("Boumia", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1
cleanData$epicerie_nzala[grepl("Nzala", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1
cleanData$epicerie_rich[grepl("Rich", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1
cleanData$epicerie_zaida[grepl("Zaîda", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1
cleanData$epicerie_beram[grepl("Beram", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1
cleanData$epicerie_missour[grepl("Missour", dataH$`Où allez vous pour acheter l'épicerie?`)] <- 1

# Affichage des résultats
table(cleanData$epicerie_midelt)
table(cleanData$epicerie_boumia)
table(cleanData$epicerie_nzala)
table(cleanData$epicerie_rich)
table(cleanData$epicerie_zaida)
table(cleanData$epicerie_beram)
table(cleanData$epicerie_missour)

############################# Santé générale ####################
table(dataH$`Comment évalueriez-vous votre santé générale?`)

cleanData$santegenerale <- NA

cleanData$santegenerale[dataH$`Comment évalueriez-vous votre santé générale?` ==
                          "Mauvaise"] <- 0
cleanData$santegenerale[dataH$`Comment évalueriez-vous votre santé générale?` ==
                          "Médiocre"] <- 0.25
cleanData$santegenerale[dataH$`Comment évalueriez-vous votre santé générale?` ==
                          "Bonne"] <- 0.5
cleanData$santegenerale[dataH$`Comment évalueriez-vous votre santé générale?` ==
                          "Très bonne"] <- 0.75
cleanData$santegenerale[dataH$`Comment évalueriez-vous votre santé générale?` ==
                          "Excellente"] <- 1

table(cleanData$santegenerale)

############################ consommation alcool ##############
table(dataH$`Est-ce que vous consommez de l'alcool?`)

cleanData$consommationalcool <- NA

cleanData$consommationalcool[dataH$`Est-ce que vous consommez de l'alcool?` == 
                               "Non"] <- 0
cleanData$consommationalcool[dataH$`Est-ce que vous consommez de l'alcool?` == "Oui,Si oui, à quelle fréquence? (Rarement; Régulièrement) - Rarement"] <- 0.5
cleanData$consommationalcool[dataH$`Est-ce que vous consommez de l'alcool?` ==  "Oui,Si oui, à quelle fréquence? (Rarement; Régulièrement) - Régulièrement"] <- 1

table(cleanData$consommationalcool)

############################ Fumez Tabac ######################

########################### Me donne mal à la tête ça ##################
table(dataH$`Fumez-vous du tabac?`)

cleanData$tabac <- NA

cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Non"] <- 0
cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Non,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Occasionnellement" |
                  dataH$`Fumez-vous du tabac?`== "Oui,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Occasionnellement"] <- 0.5 
cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Non,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Quotidiennement" |
                  dataH$`Fumez-vous du tabac?` == "Oui,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Quotidiennement" |
                  dataH$`Fumez-vous du tabac?` == "Oui,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Quotidiennement ( par le nez ? )" |
                  dataH$`Fumez-vous du tabac?` == "Oui"] <- 1

table(cleanData$tabac)

############################ Maladie cardiovasculaire ################
table(dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)

cleanData$maladiecardiovasculaire <- NA

cleanData$maladiecardiovasculaire <- 1 - as.integer(grepl("Non", dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`))
table(cleanData$maladiecardiovasculaire)

cleanData$maladiecardiovasculaire_hypertension <- as.integer(grepl("Hypertension", dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`))
table(cleanData$maladiecardiovasculaire_hypertension)

cleanData$maladiecardiovasculaire_coronarienne <- as.integer(grepl("Maladie coronarienne", dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`))
table(cleanData$maladiecardiovasculaire_coronarienne)

cleanData$maladiecardiovasculaire_fatigue <- as.integer(grepl("Fatigue",dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)) 
table(cleanData$maladiecardiovasculaire_fatigue)


############################# Maladie chronique ######################

############################# créer plusieurs variables ? #################
table(dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`)

cleanData$maladiechronique <- NA
cleanData$maladiechronique_colon <- NA
cleanData$maladiechronique_genou <- NA
cleanData$maladiechronique_estomac <- NA
cleanData$maladiechronique_neuro <- NA
cleanData$maladiechronique_yeux <- NA
cleanData$maladiechronique_dos <- NA
cleanData$maladiechronique_sciatique <- NA
cleanData$maladiechronique_diabete <- NA
cleanData$maladiechronique_rein <- NA
cleanData$maladiechronique_respiratoire <- NA
cleanData$maladiechronique_rhumatisme <- NA
cleanData$maladiechronique_silicose <- NA

cleanData$maladiechronique <- 1 - as.integer(grepl("Non", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique_colon <- as.integer(grepl("Colon|colon", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_colon)

cleanData$maladiechronique_genou <- as.integer(grepl("Douleurs à genou", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_genou)

cleanData$maladiechronique_estomac <- as.integer(grepl("Estomac|estomac", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_estomac)

cleanData$maladiechronique_neuro <- as.integer(grepl("Neurologique", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_neuro)

cleanData$maladiechronique_yeux <- as.integer(grepl("Problème au yeux|Sensibilité au yeux", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_yeux)

cleanData$maladiechronique_dos <- as.integer(grepl("Problème de dos|Dos", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_dos)

cleanData$maladiechronique_sciatique <- as.integer(grepl("Sciatique", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_sciatique)

cleanData$maladiechronique_diabete <- as.integer(grepl("Diabète", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_diabete)

cleanData$maladiechronique_rein <- as.integer(grepl("Maladie rénales", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_rein)

cleanData$maladiechronique_respiratoire <- as.integer(grepl("Maladies respiratoires", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_respiratoire)

cleanData$maladiechronique_rhumatisme <- as.integer(grepl("rhumatisme|Rhumatisme", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_rhumatisme)

cleanData$maladiechronique_silicose <- as.integer(grepl("Silicose", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique_silicose)

############################## Problèmes respiratoires ##############
table(dataH$`Avez-vous des problèmes respiratoires?`)

cleanData$maladierespiratoire <- NA

cleanData$maladierespiratoire <- 1 - as.integer(grepl("Non", dataH$`Avez-vous des problèmes respiratoires?`))
table(cleanData$maladierespiratoire)

cleanData$maladierespiratoire_asthme <- as.integer(grepl("Asthme", dataH$`Avez-vous des problèmes respiratoires?`))
table(cleanData$maladierespiratoire)

cleanData$maladierespiratoire_yeux <- as.integer(grepl("Sensible aux yeux", dataH$`Avez-vous des problèmes respiratoires?`))
table(cleanData$maladierespiratoire)

cleanData$maladierespiratoire_bronchite <- as.integer(grepl("Bronchite", dataH$`Avez-vous des problèmes respiratoires?`))
table(cleanData$maladierespiratoire)

cleanData$maladierespiratoire_rhume <- as.integer(grepl("Rhume",dataH$`Avez-vous des problèmes respiratoires?`))
table(cleanData$maladierespiratoire_rhume)

cleanData$maladierespiratoire_amygdalite <- as.integer(grepl("Amygdalite", dataH$`Avez-vous des problèmes respiratoires?`))
table(cleanData$maladierespiratoire)


############################# problèmes de peau ####################
table(dataH$`Avez-vous des problèmes de peau?`)

cleanData$problemepeau <- NA
cleanData$problemepeau_allergie <- NA
cleanData$problemepeau_autre <- NA
cleanData$problemepeau_demangeaisons <- NA
cleanData$problemepeau_eruptions <- NA
cleanData$problemepeau_squelette <- NA

cleanData$problemepeau <- 1 - as.integer(grepl("Non",dataH$`Avez-vous des problèmes de peau?`))
table(cleanData$problemepeau)
cleanData$problemepeau_autre <- as.integer(grepl("Autre",dataH$`Avez-vous des problèmes de peau?`))
table(cleanData$problemepeau_autre)
cleanData$problemepeau_allergie <- as.integer(grepl("Allergie", dataH$`Avez-vous des problèmes de peau?`))
table(cleanData$problemepeau_allergie)
cleanData$problemepeau_demangeaisons <- as.integer(grepl("Démangeaisons",dataH$`Avez-vous des problèmes de peau?`))
table(cleanData$problemepeau)
cleanData$problemepeau_eruptions <- as.integer(grepl("Éruptions cutanées",dataH$`Avez-vous des problèmes de peau?`))
table(cleanData$problemepeau_eruptions)
cleanData$problemepeau_squelette <- as.integer(grepl("Squelette",dataH$`Avez-vous des problèmes de peau?`))
table(cleanData$problemepeau_squelette)


############################# problèmes neurologiques ##################
table(dataH$`Avez-vous des antécédents de problèmes neurologiques?`)

cleanData$problemeneuro <- NA
cleanData$problemeneuro_parkinson <- NA
cleanData$problemeneuro_maux_de_tete <- NA
cleanData$problemeneuro_vertige <- NA

cleanData$problemeneuro <- 1 - as.integer(grepl("Non", dataH$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanData$problemeneuro)
cleanData$problemeneuro_parkinson <- as.integer(grepl("Parkinson", dataH$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanData$problemeneuro_parkinson)
cleanData$problemeneuro_maux_de_tete <- as.integer(grepl("Maux de tête", dataH$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanData$problemeneuro_maux_de_tete)
cleanData$problemeneuro_vertige <- as.integer(grepl("Vertiges", dataH$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanData$problemeneuro_vertige)

################################ Problèmes digestifs #####################
table(dataH$`Avez-vous des problèmes digestifs?`)

cleanData$problemedigestif <- NA
cleanData$problemedigestif_douleurabdo <- NA
cleanData$problemedigestif_nausees <- NA

cleanData$problemedigestif <- 1 - as.integer(grepl("Non", dataH$`Avez-vous des problèmes digestifs?`))
table(cleanData$problemedigestif)
cleanData$problemedigestif_douleurabdo <- as.integer(grepl("Douleurs abdominales", dataH$`Avez-vous des problèmes digestifs?`))
table(cleanData$problemedigestif_douleurabdo)
cleanData$problemedigestif_vomissements <- as.integer(grepl("Vomissements", dataH$`Avez-vous des problèmes digestifs?`))
table(cleanData$problemedigestif_vomissements)
cleanData$problemedigestif_nausees <- as.integer(grepl("Nausées", dataH$`Avez-vous des problèmes digestifs?`))
table(cleanData$problemedigestif_nausees)

####################### Douleurs musculaires/articulaires ##################
table(dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`)

cleanData$douleursmusculaires <- NA
cleanData$douleursarticulaires <- NA

cleanData$douleursmusculaires <- as.integer(grepl("musculaires", dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`))
cleanData$douleursarticulaires <- as.integer(grepl("articulaires", dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`))

table(cleanData$douleursmusculaires)
table(cleanData$douleursarticulaires)

######################## Blessure dans les 12 dernier mois ###################

######################## petit hic de code ########################
table(dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`)

cleanData$blessurerecentes_main <- NA
cleanData$blessurerecentes_jambe <- NA
cleanData$blessurerecentes_epaule <- NA
cleanData$blessurerecentes_dos <- NA
cleanData$blessurerecentes_yeux <- NA
cleanData$blessurerecentes_oreille <- NA


cleanData$blessurerecentes <- 1 - as.integer(grepl("Non", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes)

cleanData$blessurerecentes_main <- as.integer(grepl("main", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes_main)

cleanData$blessurerecentes_jambe <- as.integer(grepl("jambe", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes_jambe)

cleanData$blessurerecentes_epaule <- as.integer(grepl("épaule", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes_epaule)

cleanData$blessurerecentes_dos <- as.integer(grepl("dos", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes_dos)

cleanData$blessurerecentes_yeux <- as.integer(grepl("yeux", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes_yeux)

cleanData$blessurerecentes_oreille <- as.integer(grepl("oreille", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes_oreille)

######################## Difficultés à dormir/insomnie ##################
table(dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?`)

cleanData$troublesommeil <- NA

cleanData$troublesommeil[dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` == 
                           "Non"] <- 0
cleanData$troublesommeil[dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` == 
"Oui, j'ai des difficultés à dormir" | dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` == 
                "Oui, je souffre d'insomnie"] <- 1

table(cleanData$troublesommeil)

########################## Où travaillez-vous ####################
table(dataH$`Travaillez-vous dans:`)

cleanData$lieudetravail_Ahouli <- NA
cleanData$lieudetravail_Mibladen <- NA

cleanData$lieudetravail_Ahouli <- as.integer(grepl("la mine d'Ahouli", dataH$`Travaillez-vous dans:`))
table(cleanData$lieudetravail_Ahouli)
cleanData$lieudetravail_Mibladen <- as.integer(grepl("la mine de Mibleden|la mine de Mibladen", dataH$`Travaillez-vous dans:`))
table(cleanData$lieudetravail_Mibladen)

####################### Année d'expérience #################
table(dataH$`Depuis combien de temps travaillez-vous dans la mine ?`)

cleanData$anneeexperience <- NA

cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "<1an" | 
                            dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "1-3 ans"] <- "0_3"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "4-6 ans"] <- "4_6"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "7-10 ans"] <- "7_10"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "10 - 15 ans"] <- "10_15"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "> 20 ans"] <- "20+"

cleanData$anneeexperience <- factor(cleanData$anneeexperience,
                            ordered = TRUE,
                            levels = c("0_3", "4_6", "7_10",
                                       "10_15", "20+"))
table(cleanData$anneeexperience)
unique(cleanData$anneeexperience)


######################## Perception du danger ###################
####################### à compléter ####################
table(dataH$`Connaissez-vous les dangers de l'extraction du plomb?`)

cleanData$perceptiondanger_accidents <- NA
cleanData$perceptiondanger_silicose <- NA
cleanData$perceptiondanger_mort <- NA
cleanData$perceptiondanger_respiratoire <- NA
cleanData$perceptiondanger_eboulement <- NA
cleanData$perceptiondanger_rhumatisme <- NA
cleanData$perceptiondanger_sciatique <- NA
cleanData$perceptiondanger_deterioration <- NA
cleanData$perceptiondanger_asthme <- NA
cleanData$perceptiondanger_neuro <- NA
cleanData$perceptiondanger_articulation <- NA
cleanData$perceptiondanger_hypertension <- NA
cleanData$perceptiondanger_vision <- NA
cleanData$perceptiondanger_audition <- NA
cleanData$perceptiondanger_estomac <- NA
cleanData$perceptiondanger_intestin <- NA
cleanData$perceptiondanger_rein <- NA
cleanData$perceptiondanger_toxicite <- NA
cleanData$perceptiondanger_colon <- NA

cleanData$perceptiondanger_accidents <- as.integer(grepl("Accident et blessures|risques des accidents et blessures|Blessures|accident et blessures|blessures et accidents ( éboulement)", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_accidents)

cleanData$perceptiondanger_silicose <- as.integer(grepl("Silicose|silicose", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_silicose)

cleanData$perceptiondanger_mort <- as.integer(grepl("mort", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_mort)

cleanData$perceptiondanger_respiratoire <- as.integer(grepl("problème respiratoire ( poumons)|problème de poumons|poussière de poumons|Problème de poumons|problème respiratoire", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_respiratoire)

cleanData$perceptiondanger_eboulement <- as.integer(grepl("risque d’éboulement|Éboulement|Risque d’éboulement|éboulement", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_eboulement)

cleanData$perceptiondanger_rhumatisme <- as.integer(grepl("rhumatisme", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_rhumatisme)

cleanData$perceptiondanger_sciatique <- as.integer(grepl("sciatique", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_sciatique)

cleanData$perceptiondanger_deterioration <- as.integer(grepl("détérioration de la santé", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_deterioration)

cleanData$perceptiondanger_asthme <- as.integer(grepl("asthme|Asthme", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_asthme)

cleanData$perceptiondanger_neuro <- as.integer(grepl("problème neurologique", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_neuro)

cleanData$perceptiondanger_articulation <- as.integer(grepl("problème d’articulation|problème articulations et musculaire|douleurs d’articulation|dos", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_articulation)

cleanData$perceptiondanger_hypertension <- as.integer(grepl("hypertension", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_hypertension)

cleanData$perceptiondanger_vision <- as.integer(grepl("perte de la vue|accident et blessures baisse de vue|perte de la vue accident et blessures", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_vision)

cleanData$perceptiondanger_audition <- as.integer(grepl("problème auditif", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_audition)

cleanData$perceptiondanger_estomac <- as.integer(grepl("estomac", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_estomac)

cleanData$perceptiondanger_intestin <- as.integer(grepl("instestins|intestin", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_intestin)

cleanData$perceptiondanger_rein <- as.integer(grepl("problème rénale|rénal", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_rein)

cleanData$perceptiondanger_toxicite <- as.integer(grepl("toxique|toxicité", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_toxicite)

cleanData$perceptiondanger_colon <- as.integer(grepl("colon", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger_colon)

####################### Suivi médical ####################
table(dataH$`Avez-vous accès à un suivi médical régulier pour détecter et traiter les problèmes de santé liés à l'exposition au plomb et aux conditions de travail dans la mine?`)

cleanData$suivimedical <- NA

cleanData$suivimedical[dataH$`Avez-vous accès à un suivi médical régulier pour détecter et traiter les problèmes de santé liés à l'exposition au plomb et aux conditions de travail dans la mine?` == 
                         "Non"] <- 0
cleanData$suivimedical[dataH$`Avez-vous accès à un suivi médical régulier pour détecter et traiter les problèmes de santé liés à l'exposition au plomb et aux conditions de travail dans la mine?` == 
                         "Oui"] <- 1

table(cleanData$suivimedical)

######################## Équipement de sécurité ################
table(dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`)

cleanData$equipement_casque <- NA
cleanData$equipement_lampe <- NA
cleanData$equipement_bottes <- NA
cleanData$equipement_tenue <- NA
cleanData$equipement_masque <- NA
cleanData$equipement_gants <- NA
cleanData$equipement_chapeau <- NA
cleanData$equipement_lunettes <- NA

cleanData$equipement_casque <- as.integer(grepl("Casque|casque", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_casque)

cleanData$equipement_lampe <- as.integer(grepl("Lampe|lamp|lampe|Lamp", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_lampe)

cleanData$equipement_bottes <- as.integer(grepl("Bottes|botte|bottes", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_bottes)

cleanData$equipement_tenue <- as.integer(grepl("tenu", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_tenue)

cleanData$equipement_masque <- as.integer(grepl("masque|Masque", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_masque)

cleanData$equipement_gants <- as.integer(grepl("gants|Gant|Gants", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_gants)

cleanData$equipement_chapeau <- as.integer(grepl("chapeau", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_chapeau)

cleanData$equipement_lunettes <- as.integer(grepl("lunettes", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement_lunettes)

##################### impacts long terme préoccupations ##################
table(dataH$`Selon vous, les impacts à long terme de l'extraction artisanale sur la santé des travailleurs sont-ils préoccupants?`)

cleanData$dangerimpactlongterme <- NA

cleanData$dangerimpactlongterme[dataH$`Selon vous, les impacts à long terme de l'extraction artisanale sur la santé des travailleurs sont-ils préoccupants?` ==
                                  "Oui"] <- 1

table(cleanData$dangerimpactlongterme)

#################### impacts long terme #####################
################### À cleaner comme du monde #################
table(dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`)

cleanData$impact_long_terme_respiratoire <- NA
cleanData$impact_long_terme_silicose <- NA
cleanData$impact_long_terme_rein <- NA
cleanData$impact_long_terme_rhumatisme <- NA
cleanData$impact_long_terme_sciatique <- NA
cleanData$impact_long_terme_accident <- NA
cleanData$impact_long_terme_vue <- NA
cleanData$impact_long_terme_audition <- NA
cleanData$impact_long_terme_parkinson <- NA
cleanData$impact_long_terme_sante <- NA
cleanData$impact_long_terme_sommeil <- NA
cleanData$impact_long_terme_rein <- NA
cleanData$impact_long_terme_digestif <- NA
cleanData$impact_long_terme_neuro <- NA
cleanData$impact_long_terme_dos <- NA
cleanData$impact_long_terme_articulation <- NA

cleanData$impact_long_terme_respiratoire <- as.integer(grepl("Asthme|Problème respiratoire|Problème de poumons|Maladie respiratoire|Risque sur les poumons|asthme", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_respiratoire)
cleanData$impact_long_terme_silicose <- as.integer(grepl("Silicose", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_silicose)
cleanData$impact_long_terme_rein <- as.integer(grepl("Problème rénale|Rénale|Problème rénal|Problème rénaux|Problème reine", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_rein)
cleanData$impact_long_terme_rhumatisme <- as.integer(grepl("Rhumatisme", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_rhumatisme)
cleanData$impact_long_terme_sciatique <- as.integer(grepl("Sciatique|sciatique", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_sciatique)
cleanData$impact_long_terme_accident <- as.integer(grepl("Accident et blessures|Accident/ blessures|Risque des accidents et blessures|Blessures", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_accident)
cleanData$impact_long_terme_vue <- as.integer(grepl("Perte de la vue|Perte de vue|Baisse de vue", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_vue)
cleanData$impact_long_terme_audition <- as.integer(grepl("Problème de auditif|Problème auditif|Problème de sens", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_audition)
cleanData$impact_long_terme_silicose <- as.integer(grepl("Parkinson|parkinson", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_silicose)
cleanData$impact_long_terme_sante <- as.integer(grepl("Courte espérance de vie|Détérioration de la santé|détérioration de la santé|Espérance de vie courte", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_sante)
cleanData$impact_long_terme_sommeil <- as.integer(grepl("Fatigue|Insomnie", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_sommeil)
cleanData$impact_long_terme_digestif <- as.integer(grepl("Estomac|Problème de la pareille digestive|estomac|intestin", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_digestif)
cleanData$impact_long_terme_neuro <- as.integer(grepl("Problème neurologique|Problème de la mémoire", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_neuro)
cleanData$impact_long_terme_dos <- as.integer(grepl("Problème de dos|Problème au dos|dos|Douleur de dos|Douleurs au dos|Le dos", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_dos)
cleanData$impact_long_terme_articulation <- as.integer(grepl("Problème d’articulation et musculaire|Problème d’a", dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`))
table(cleanData$impact_long_terme_articulation)
##################### problème santé à cause des mines ################
table(dataH$`Avez-vous ou l'un de vos proches soufferts d'accidents ou de problèmes de santé à cause du travail dans les mines?`)

cleanData$problemesantemine <- NA

cleanData$problemesantemine[dataH$`Avez-vous ou l'un de vos proches soufferts d'accidents ou de problèmes de santé à cause du travail dans les mines?` == 
                              "Non"] <- 0
cleanData$problemesantemine[dataH$`Avez-vous ou l'un de vos proches soufferts d'accidents ou de problèmes de santé à cause du travail dans les mines?` == 
                              "Oui"] <- 1

table(cleanData$problemesantemine)

###################### protocole #####################
table(dataH$`Existe-t-il un plan d'action ou un protocole pour gérer les situations d'urgence ou les accidents dans la mine?`)

cleanData$protocole <- NA

cleanData$protocole[dataH$`Existe-t-il un plan d'action ou un protocole pour gérer les situations d'urgence ou les accidents dans la mine?` ==
                      "Non"] <- 0
cleanData$protocole[dataH$`Existe-t-il un plan d'action ou un protocole pour gérer les situations d'urgence ou les accidents dans la mine?` ==
                      "Oui"] <- 1

table(cleanData$protocole)

####################### problème de santé courant communauté #############
###################### à cleaner sur le sens du monde #################
table(dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`)

cleanData$santecommunaute_accidents <- NA
cleanData$santecommunaute_mentale <- NA
cleanData$santecommunaute_chronique <- NA
cleanData$santecommunaute_respiratoire_cardio <- NA
cleanData$santecommunaute_rhumatisme <- NA
cleanData$santecommunaute_silicose <- NA
cleanData$santecommunaute_musculo_squelettique <- NA
cleanData$santecommunaute_sciatique <- NA
cleanData$santecommunaute_paralysie <- NA
cleanData$santecommunaute_parkinson <- NA
cleanData$santecommunaute_neurologique <- NA
cleanData$santecommunaute_cassure <- NA
cleanData$santecommunaute_mort <- NA
cleanData$santecommunaute_articulation <- NA
cleanData$santecommunaute_dos <- NA
cleanData$santecommunaute_hypertension <- NA
cleanData$santecommunaute_yeux <- NA
cleanData$santecommunaute_asthme <- NA
cleanData$santecommunaute_audition <- NA
cleanData$santecommunaute_colon <- NA
cleanData$santecommunaute_rein <- NA
cleanData$santecommunaute_estomac <- NA

cleanData$santecommunaute_accidents <- as.integer(grepl("Accidents et blessures", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_accidents)
cleanData$santecommunaute_mentale <- as.integer(grepl("Problèmes de santé mentale", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_mentale)
cleanData$santecommunaute_chronique <- as.integer(grepl("Autres problèmes de santé chroniques", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_chronique)
cleanData$santecommunaute_respiratoire_cardio <- as.integer(grepl("Problèmes respiratoires et cardiovasculaires", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_respiratoire_cardio)
cleanData$santecommunaute_rhumatisme <- as.integer(grepl("Rhumatisme|rhumatisme", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_rhumatisme)
cleanData$santecommunaute_silicose <- as.integer(grepl("Silicose|silicose", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_silicose)
cleanData$santecommunaute_musculo_squelettique <- as.integer(grepl("Troubles musculosquelettiques", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_musculo_squelettique)
cleanData$santecommunaute_sciatique <- as.integer(grepl("Sciatique|sciatique", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_sciatique)
cleanData$santecommunaute_paralysie <- as.integer(grepl("Paralysie", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_paralysie)
cleanData$problemesantecommunaute_parkinson <- as.integer(grepl("Parkinson", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_parkinson)
cleanData$santecommunaute_neurologique <- as.integer(grepl("problème neurologique", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_neurologique)
cleanData$santecommunaute_cassure <- as.integer(grepl("Casseurs", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_cassure)
cleanData$santecommunaute_mort <- as.integer(grepl("Mort", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_mort)
cleanData$santecommunaute_articulation <- as.integer(grepl("Problème d’articulation|Douleurs d’articulation|problème d’articulation", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_articulation) 
cleanData$santecommunaute_dos <- as.integer(grepl("dos", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_dos)
cleanData$santecommunaute_hypertension <- as.integer(grepl("hypertension", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_hypertension)
cleanData$santecommunaute_yeux <- as.integer(grepl("yeux|vue", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_yeux)
cleanData$santecommunaute_asthme <- as.integer(grepl("asthme|Asthme", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_asthme)
cleanData$santecommunaute_audition <- as.integer(grepl("problème auditif", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_audition)
cleanData$santecommunaute_colon <- as.integer(grepl("Colon|colon", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_colon)
cleanData$santecommunaute_rein <- as.integer(grepl("problème rénal|Problème rénal|Problème rénale", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_rein)
cleanData$santecommunaute_estomac <- as.integer(grepl("estomac|Estomac", dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`))
table(cleanData$santecommunaute_estomac)


###################### risque bétail #####################
table(dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]])

cleanData$impact_betail <- NA

cleanData$impact_betail[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                                                      "Mauvaise"] <- 0

cleanData$impact_betail[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                       "Médiocre"] <- 0.33

cleanData$impact_betail[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                       "Bonne"] <- 0.66

table(cleanData$impact_betail)

##################### risque qualité de l'air ###################
table(dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]])

cleanData$impact_qualiteair <- NA

cleanData$impact_qualiteair[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                              "Mauvaise"] <- 0
cleanData$impact_qualiteair[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                              "Médiocre"] <- 0.33
cleanData$impact_qualiteair[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                              "Bonne"] <- 0.66
cleanData$impact_qualiteair[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                              "Très bonne"] <- 1

table(cleanData$impact_qualiteair)


####################### qualité de l'eau ###################
table(dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]])

cleanData$impact_qualiteeau <- NA

cleanData$impact_qualiteeau[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Mauvaise"] <- 0
cleanData$impact_qualiteeau[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Médiocre"] <- 0.33
cleanData$impact_qualiteeau[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Bonne"] <- 0.66

table(cleanData$impact_qualiteeau)


####################### qualité alimentation ###################
table(dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]])


cleanData$qualite_alimentation <- NA

cleanData$qualite_alimentation[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                              "Mauvaise"] <- 0
cleanData$qualite_alimentation[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                 "Médiocre"] <- 0.33
cleanData$qualite_alimentation[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                 "Bonne"] <- 0.66
cleanData$qualite_alimentation[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                 "Très bonne"] <- 1

table(cleanData$qualite_alimentation)


######################## impact culture #####################
table(dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]])

cleanData$impact_culture <- NA

cleanData$impact_culture[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                                 "Mauvaise"] <- 0
cleanData$impact_culture[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                           "Médiocre"] <- 0.33
cleanData$impact_culture[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                           "Bonne"] <- 0.66
cleanData$impact_culture[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                           "Très bonne"] <- 1

table(cleanData$impact_culture)


################### qualité environnement social ################
table(dataH$`En général, diriez-vous que la qualité de l'environnement social est:`)

cleanData$qualitesocial <- NA

cleanData$qualitesocial[dataH$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                          "Mauvaise"] <- 0
cleanData$qualitesocial[dataH$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                          "Médiocre"] <- 0.33
cleanData$qualitesocial[dataH$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                          "Bonne"] <- 0.66
cleanData$qualitesocial[dataH$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                          "Très bonne"] <- 1

table(cleanData$qualitesocial)

################### qualité naturel #####################
table(dataH$`En général, diriez-vous que la qualité de l'environnement naturel est:`)

cleanData$qualitenaturel <- NA

cleanData$qualitenaturel[dataH$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                           "Mauvaise"] <- 0
cleanData$qualitenaturel[dataH$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                           "Médiocre"] <- 0.33
cleanData$qualitenaturel[dataH$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                           "Bonne"] <- 0.66
cleanData$qualitenaturel[dataH$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                           "Très bonne"] <- 1

table(cleanData$qualitenaturel)

######################### santé générale communauté ###############
table(dataH$`Selon vous, la santé générale de la communauté est :`)

cleanData$santegeneralecommunaute <- NA

cleanData$santegeneralecommunaute[dataH$`Selon vous, la santé générale de la communauté est :` ==
                                    "Mauvaise"] <- 0
cleanData$santegeneralecommunaute[dataH$`Selon vous, la santé générale de la communauté est :` ==
                                    "Moyenne"] <- 0.5
cleanData$santegeneralecommunaute[dataH$`Selon vous, la santé générale de la communauté est :` ==
                                    "Bonne"] <- 1

table(cleanData$santegeneralecommunaute)

######################## amélioration sanitaire communautée#################

###################### nuage de points ? #######################
table(dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`)

cleanData$ameliorationsituationeco_alteco <- NA
cleanData$ameliorationsituationeco_sante <- NA
cleanData$ameliorationsituationeco_ecole <- NA
cleanData$ameliorationsituationeco_transport <- NA
cleanData$ameliorationsituationeco_comms <- NA
cleanData$ameliorationsituationeco_travail <- NA
cleanData$ameliorationsituationeco_mine <- NA
cleanData$ameliorationsituationeco_tourisme <- NA
cleanData$ameliorationsituationeco_soutien_femme <- NA
cleanData$ameliorationsituationeco_public <- NA
cleanData$ameliorationsituationeco_cadre_mineur <- NA
cleanData$ameliorationsituationeco_dechets <- NA
cleanData$ameliorationsituationeco_rehab <- NA
cleanData$ameliorationsituationeco_elevage <- NA
cleanData$ameliorationsituationeco_propriete <- NA
cleanData$ameliorationsituationeco_vert <- NA
cleanData$ameliorationsituationeco_amusement_enfant <- NA
cleanData$ameliorationsituationeco_silicose <- NA
cleanData$ameliorationsituationeco_eau_potable <- NA
cleanData$ameliorationsituationeco_vente_mineraux <- NA
cleanData$ameliorationsituationeco_usine <- NA
cleanData$ameliorationsituationeco_bourse <- NA
cleanData$ameliorationsituationeco_securite <- NA
cleanData$ameliorationsituationeco_souk <- NA
cleanData$ameliorationsituationeco_puit <- NA
cleanData$ameliorationsituationeco_inondation <- NA

cleanData$ameliorationsituationeco_alteco <- as.integer(grepl("Alternative économique par des terres agricoles | Alternative économique ( redémarrage de la mine) |Alternative économique |pâturage|Panneaux solaires pour les agriculteurs", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_alteco)

cleanData$ameliorationsituationeco_sante <- as.integer(grepl("Hôpital|Ambulance|Médecin|médicaux|Médicaments|Première secours à Mibladen|Contrôle et suivie de silicose à Mibladen|Équipements médicaux à Mibladen|Équipements d'utilité publique|Dispensaire", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_sante)

cleanData$ameliorationsituationeco_ecole <- as.integer(grepl("École secondaire|Transport scolaire|Classe préscolaire", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_ecole)

cleanData$ameliorationsituationeco_transport <- as.integer(grepl("Route et ponts|Route|Pavage des rues|Aménagement de la route|Transport|Bus|Transport publique|Aménagement de la route Mibladen- Ahouli|Aménagement de la route Mibladen Ahouli|Pavage des rues", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_transport)

cleanData$ameliorationsituationeco_comms <- as.integer(grepl("Réseau de communication|Réseau et internet|Réseau de communication et internet à Ahouli|Électricité à Ahouli|Réseau", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_comms)

cleanData$ameliorationsituationeco_travail <- as.integer(grepl("Offres d’emploi|Offres d’emploi pour la jeunesse", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_travail)

cleanData$ameliorationsituationeco_mine <- as.integer(grepl("Redémarrage de la mine|redémarrage de la mine avec de bon conditions|Restauration du site Ahouli|muséologique|musélogique", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_mine)

cleanData$ameliorationsituationeco_tourisme <- as.integer(grepl("Promouvoir le tourisme à Mibladen|Promouvoir de tourisme Ahouli et Mibladen|Promouvoir de tourisme|Promouvoir de tourisme à Mibladen|Promouvoir de tourisme Ahouli|Promouvoir de tourisme à Mibladen et Ahouli|Complex tourisme", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_tourisme)

cleanData$ameliorationsituationeco_soutien_femme <- as.integer(grepl("Soutien des coopératives féminines|Promouvoir des coopératives féminines|Coopératives féminines", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_soutien_femme)

cleanData$ameliorationsituationeco_public <- as.integer(grepl("Plaque publique|Panneaux de publicité", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_public)

cleanData$ameliorationsituationeco_cadre_mineur <- as.integer(grepl("Permission minière au mineurs|Permission au mineurs|Protection juridique des mineurs|Cadre législatif|Soutien des mineurs|Cadre législatif des mineurs|Soutien aux mineurs par les airs compresseurs", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_cadre_mineur)

cleanData$ameliorationsituationeco_dechets <- as.integer(grepl("Conteneurs des déchets|Des conteneurs des déchets|Dépotoir|Gestion des déchets", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_dechets)

cleanData$ameliorationsituationeco_rehab <- as.integer(grepl("Réhabilitation|Réhabilitations des maisons", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_rehab)

cleanData$ameliorationsituationeco_elevage <- as.integer(grepl("Soutien des projets d’élevage|Soutie des éleveurs|Projet d’élevage", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_elevage)

cleanData$ameliorationsituationeco_propriete <- as.integer(grepl("Titre de propriété des maisons", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_propriete)

cleanData$ameliorationsituationeco_vert <- as.integer(grepl("Plantation et reboisement à Mibladen|Reboisement et plantation à Mibladen|Problème de forestiers|Plantation et reboisement des jardins de Mibladen|Jardin public", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_vert)

cleanData$ameliorationsituationeco_amusement_enfant <- as.integer(grepl("Parc de joue aux enfants|Pace de joue des enfants|Terrains de football pour les enfants|Maisons de jeunesses|Activités sportives au enfant", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_amusement_enfant)

cleanData$ameliorationsituationeco_silicose <- as.integer(grepl("Contrôle et suivie de silicose à Mibladen|Suivie et contrôle de silicose pour mineurs à Mibladen|Suivie et contrôle de silicose à Mibladen|Silicose", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_silicose)

cleanData$ameliorationsituationeco_eau_potable <- as.integer(grepl("L’eau potable|Traitement de l’eau potable|Eau potable|Filtration et traitement de l’eau potable|Problème de l’assainissement", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_eau_potable)

cleanData$ameliorationsituationeco_vente_mineraux <- as.integer(grepl("Vendre de plomb sans les intermédiaires|Vendre de produits sans les intermédiaires|Commercialisation de produit|Commercialisation de plomb sans les intermédiaires|Commercialisation de plomb et minéraux|Vendre de pb sans les intermédiaires", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_vente_mineraux)

cleanData$ameliorationsituationeco_usine <- as.integer(grepl("ouverture des usines|usines…|usines", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_usine)

cleanData$ameliorationsituationeco_bourse <- as.integer(grepl("Bourses des minéraux|Bourses des minéraux à Mibladen", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_bourse)

cleanData$ameliorationsituationeco_securite <- as.integer(grepl("Sécurité", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_securite)

cleanData$ameliorationsituationeco_souk <- as.integer(grepl("Suq hebdomadaire|Souq", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_souk)

cleanData$ameliorationsituationeco_puit <- as.integer(grepl("Puits irrigation alimenté par l’énergie solaire|Les canaux d’irrigations pour terrains agricoles|Canaux d’irrigations|Les canaux d’irrigation", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_puit)

cleanData$ameliorationsituationeco_inondation <- as.integer(grepl("Travaux d'aménagement contre d’inondations|Aménagement contre les inondations", dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanData$ameliorationsituationeco_inondation)

###################### stress travail ################
table(dataH$`Ressentez-vous du stress ou de l'anxiété liés à votre propre travail ou au travail de votre famille dans l'exploitation minière ?`)

cleanData$stresstravail <- NA

cleanData$stresstravail[dataH$`Ressentez-vous du stress ou de l'anxiété liés à votre propre travail ou au travail de votre famille dans l'exploitation minière ?` ==
                          "Non"] <- 0
cleanData$stresstravail[dataH$`Ressentez-vous du stress ou de l'anxiété liés à votre propre travail ou au travail de votre famille dans l'exploitation minière ?` ==
                          "Oui"] <- 1

table(cleanData$stresstravail)

######################## stress situation économique ##############
table(dataH$`Ressentez-vous du stress ou de l'anxiété en raison de votre situation économique ou de celle de votre famille?`)

cleanData$stresseconomique <- NA

cleanData$stresseconomique[dataH$`Ressentez-vous du stress ou de l'anxiété en raison de votre situation économique ou de celle de votre famille?` ==
                             "Non"] <- 0
cleanData$stresseconomique[dataH$`Ressentez-vous du stress ou de l'anxiété en raison de votre situation économique ou de celle de votre famille?` ==
                             "Oui"] <- 1

table(cleanData$stresseconomique)

####################### désespoir/dépression communauté ################
table(dataH$`Ressentez-vous un sentiment de désespoir ou de dépression lié aux conditions de travail et de vie dans la communauté?`)

cleanData$desespoircommunaute <- NA

cleanData$desespoircommunaute[dataH$`Ressentez-vous un sentiment de désespoir ou de dépression lié aux conditions de travail et de vie dans la communauté?` ==
                                "Non"] <- 0
cleanData$desespoircommunaute[dataH$`Ressentez-vous un sentiment de désespoir ou de dépression lié aux conditions de travail et de vie dans la communauté?` ==
                                "Oui"] <- 1

table(cleanData$desespoircommunaute)

##################### santé mentale ################
table(dataH$`Avez-vous le sentiment que votre santé mentale a été impactée par l'exploitation minière ?`)

cleanData$santementale <- NA

cleanData$santementale[dataH$`Avez-vous le sentiment que votre santé mentale a été impactée par l'exploitation minière ?` ==
                         "Non"] <- 0
cleanData$santementale[dataH$`Avez-vous le sentiment que votre santé mentale a été impactée par l'exploitation minière ?` ==
                         "Oui"] <- 1

table(cleanData$santementale)


cleanData$openprobsantecourant <- dataH$`Quelles sont les problèmes de santé les plus courant dans votre communauté`
cleanData$openameliorationeco <- dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`
cleanData$opendangerplomb <- dataH$`Connaissez-vous les dangers de l'extraction du plomb?`
cleanData$openimpactlongterme <- dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`

saveRDS(cleanData,"Data/hommes.rds")
