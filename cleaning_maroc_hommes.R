library(tidyverse)
library(readxl)

dataH <- read_xlsx("data_femmes.xlsx")


cleanData <- data.frame(id= dataH$`ID de la réponse`)


########################### Âge #################
table(dataH$`Quel âge avez-vous ?`)

cleanData$ses_age <- NA
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "18 - 20 ans"] <- "18 à 20 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "21 - 29 ans"] <- "21 à 29 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "30 - 39 ans"] <- "30 à 39 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "40-49"] <- "40 à 49 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "50-59"] <- "50 à 59 ans"
cleanData$ses_age[dataH$`Quel âge avez-vous ?` == "60 ou plus"] <- "60 ans ou plus"
table(cleanData$ses_age)


########################## Lieu ###################
table(dataH$Lieu)

cleanData$ses_lieu <- NA
cleanData$ses_lieu[dataH$Lieu == "Ahou" | dataH$Lieu == "Ahouli" | dataH$Lieu ==
                     "AhouLi" | dataH$Lieu == "Ahouliy"] <- "Ahouli"
cleanData$ses_lieu[dataH$Lieu == "Mibladen"] <- "Mibladen"
table((cleanData$ses_lieu))

########################### Revenu ###################
table(dataH$`Quel est le montant total de vos revenus annuels?`)

cleanData$ses_revenu <- NA
cleanData$ses_revenu[dataH$`Quel est le montant total de vos revenus annuels?` 
                     == "0 à 2000 dirhams" |
                       dataH$`Quel est le montant total de vos revenus annuels?`
                     == "2001 à 5000 dirhams"] <- "faible"
cleanData$ses_revenu[dataH$`Quel est le montant total de vos revenus annuels?` 
                     == "5001 à 7000 dirhams" |
                       dataH$`Quel est le montant total de vos revenus annuels?`
                     == "7001 à 10 000 dirhams"] <- "moyen"
cleanData$ses_revenu[dataH$`Quel est le montant total de vos revenus annuels?`==
                       "Plus de 10 000 dirhams"] <- "élevé"
table(cleanData$ses_revenu)


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

cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Célibataire"
                        ] <- "Célibataire"
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Divorcé/séparé"
] <- "Divorcé/séparé"
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Marié"
] <- "Marié"
cleanData$ses_etatcivil[dataH$`Quel est votre état civil ?` == "Veuf"
] <- "Veuf"

table(cleanData$ses_étatcivil)

################## nb enfants ##################
table(dataH$`Combien d'enfants avez-vous ?`)

cleanData$ses_enfants <- NA

cleanData$ses_enfants[is.na(dataH$`Combien d'enfants avez-vous ?`)] <- "0"
cleanData$ses_enfants[dataH$`Combien d'enfants avez-vous ?` == "1"] <- "1"

cleanData$ses_enfants[dataH$`Combien d'enfants avez-vous ?` == "2" |
                        dataH$`Combien d'enfants avez-vous ?` == "3"] <- "2-3"
cleanData$ses_enfants[dataH$`Combien d'enfants avez-vous ?` == "4" |
                        dataH$`Combien d'enfants avez-vous ?` == "5"] <- "4-5"
cleanData$ses_enfants[dataH$`Combien d'enfants avez-vous ?` == "6" |
                        dataH$`Combien d'enfants avez-vous ?` == "7"] <- "6-7"

table(cleanData$ses_enfants)

###################### Statut professionel actuel ####################
table(dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`)

cleanData$statutTravail <- NA

cleanData$statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                        == "Employé,Retraité"] <- "Employé maintenant retraité"
cleanData$statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                       == "Retraité"] <- "Retraité"
cleanData$statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                       == "Travailleur autonome"] <- " Travailleur autonome"
cleanData$statutTravail[dataH$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`
                       == "Travailleur autonome,Retraité"] <- "Travailleur
                                                  autonome maintenant retraité"

table(cleanData$statutTravail)


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

votre_vecteur <- c("Amar,Francais", "Francais", "Anglais et Francais", "Espagnol")

resultat <- as.integer(grepl("Francais", votre_vecteur))
resultat <- as.integer(grepl("Amazgigh", votre_vecteur))

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

######################## Niveau d'étude ###########################
table(dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`)

cleanData$niveauetude <- NA

cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Enseignement postsecondaire"] <- "Enseignement postsecondaire"
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Enseignement primaire"] <- "Enseignement primaire"
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Enseignement secondaire"] <- "Enseignement secondaire"
cleanData$niveauetude[dataH$`Quel est le plus haut niveau d'études que vous ayez atteint ?`
                      == "Sans diplôme"] <- "Sans diplôme"

table(cleanData$niveauetude)

######################### Logement principal ##########################

######################### Autres problèmes ######################
table(dataH$`Où est votre logement principal?`)

cleanData$logementprincipal <- NA

cleanData$logementprincipal[dataH$`Où est votre logement principal?`== 
   "Autre province,Précisez la commune - Missour" |
 dataH$`Où est votre logement principal?` == 
 "Province de Midelt,Précisez la commune - Missour"] <- "Province de Midelt, Missour"
cleanData$logementprincipal[dataH$`Où est votre logement principal?`== 
                       "Province de Midelt,Précisez la commune - Ait ayach" |
                         dataH$`Où est votre logement principal?` == 
 "Province de Midelt,Précisez la commune - Ait Aache"] <-"Province de Midelt, Ait ayach"
cleanData$logementprincipal[dataH$`Où est votre logement principal?`== 
                        "Province de Midelt,Précisez la commune - Ait izdey" |
dataH$`Où est votre logement principal?` == "Province de Midelt,Précisez la commune - Ait Izdeg"] <-
"Province de Midelt, Ait izdeg"
cleanData$logementprincipal[dataH$`Où est votre logement principal?`== 
                      "Province de Midelt,Précisez la commune - Ait oumghar" |
dataH$`Où est votre logement principal?` == "Provincede Midelt,Précisez la commune - Ait omghar"] <- 
 "Province de Midelt, Ait oumghar"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` ==
"Province de Midelt,Précisez la commune - Commune Tiaalaline"] <- "Province de Midelt, Tiaalaline"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` ==
"Province de Midelt,Précisez la commune - Kerando"] <- "Province de Midelt, Kerando"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` == 
                    "Province de Midelt,Précisez la commune - Miblden" |
                      dataH$`Où est votre logement principal?`== "Province de
       Midelt,Précisez la commune - Mibladen"] <- "Province de Midelt, Mibladen"  
cleanData$logementprincipal[dataH$`Où est votre logement principal?` == 
                          "Province de Midelt,Précisez la commune - Riche"] <-
                                                    "Province de Midelt, Riche"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` == 
                        "Province de Midelt,Précisez la commune - Amrssid"] <-
                                        "Province de Midelt, Amersid"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` == 
                        "Province de Midelt,Précisez la commune - Gerama"] <-
                                                "Province de Midelt, Gerama"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` == "Province
                            de Midelt,Précisez la commune - Nzala"] <- "Province
                                                              de Midelt, Nzala"
cleanData$logementprincipal[dataH$`Où est votre logement principal?` == "Province
          de Midelt,Précisez la commune - Zaida"] <- "Province de Midelt, Zaida"

table(cleanData$logementprincipal)                                                            


########################### Distance d'Ahouli ######################

########################### Problème avec deux données ######################
table(dataH$`Distance d'Ahouli`)
cleanData$distanceahouli <- NA
cleanData$distanceahouli <- gsub(",", ".", dataH$`Distance d'Ahouli`)
cleanData$distanceahouli <- gsub("[^0-9.]", "", cleanData$distanceahouli)
cleanData$distanceahouli <- as.numeric(cleanData$distanceahouli)
table(cleanData$distanceahouli)
hist(cleanData$distanceahouli)

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
                                 "1"] <- "1"
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "2"] <- "2"
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "3"] <- "3"
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "4"] <- "4"
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "5"] <- "5"
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "6"] <- "6"
cleanData$nombrepiecehabitable[dataH$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "7"] <- "7"

table(cleanData$nombrepiecehabitable)

########################## Personnes dans le logement ######################
table(dataH$`Combien de personnes vivent dans le logement ?`)

cleanData$nbpersonnelogement <- NA

cleanData$nbpersonnelogement[is.na(dataH$`Combien de personnes vivent dans le logement ?`)] <- "0"
cleanData$nbpersonnelogement[dataH$`Combien de personnes vivent dans le logement ?` ==
                               "1" |
                       dataH$`Combien de personnes vivent dans le logement ?` ==
                              "2" ] <- "1 - 2"
cleanData$nbpersonnelogement[dataH$`Combien de personnes vivent dans le logement ?` ==
                               "3" |
                               dataH$`Combien de personnes vivent dans le logement ?` ==
                               "4" ] <- "3 - 4"
cleanData$nbpersonnelogement[dataH$`Combien de personnes vivent dans le logement ?` ==
                               "5" |
                               dataH$`Combien de personnes vivent dans le logement ?` ==
                               "6" ] <- "5 - 6"
cleanData$nbpersonnelogement[dataH$`Combien de personnes vivent dans le logement ?` ==
                               "7" |
                               dataH$`Combien de personnes vivent dans le logement ?` ==
                               "8" ] <- "7 - 8"
cleanData$nbpersonnelogement[dataH$`Combien de personnes vivent dans le logement ?` ==
                               "9" |
                               dataH$`Combien de personnes vivent dans le logement ?` ==
                               "10" ] <- "9 - 10"
cleanData$nbpersonnelogement[dataH$`Combien de personnes vivent dans le logement ?` ==
                               "20"] <- "11 ou plus" 

table(cleanData$nbpersonnelogement)


########################## visite famille #########################
table(dataH$`Combien de fois rendez-vous visite à votre famille?`)

cleanData$visitefamille <- NA

cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Plusieurs fois par mois"] <- "Plusieurs fois par mois"
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Plusieurs fois par semaine"] <- "Plusieurs fois par semaine"
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Tous les jours"] <- "Tous les jours"
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Une fois par mois"] <- "Une fois par mois"
cleanData$visitefamille[dataH$`Combien de fois rendez-vous visite à votre famille?` ==
                          "Une fois par semaine"] <- "Une fois par semaine"

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

cleanData$toilette <- NA

cleanData$toilette[dataH$`Est-ce que le logement a des toilettes?` == 
                     "Non"] <- "Non"
cleanData$toilette[dataH$`Est-ce que le logement a des toilettes?` ==
                     "Oui, à l'extérieur"] <- "Oui, à l'extérieur"
cleanData$toilette[dataH$`Est-ce que le logement a des toilettes?` ==
                     "Oui, dans la maison"] <- "Oui, dans la maison"
cleanData$toilette[dataH$`Est-ce que le logement a des toilettes?` ==
                     "Oui, dans la maison,Oui, à l'extérieur"] <- "Oui, dans la maison et à l'extérieur"

table(cleanData$toilette)


################################ revenus principal ####################
table(dataH$`Quelles sont vos sources de revenus principales?`)

cleanData$revenuprincipal <- NA

cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Agriculture"] <- "Agriculture"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines,Agriculture"] <- "Mines,Agriculture"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines,Autre (veuillez spécifier) - Artisans"] <- "Mines,Artisanat"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Agriculture,Élevage"] <- "Agriculture,Élevage"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines,Agriculture,Élevage"] <- "Mines,Agriculture,Élevage"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines,Commerce"] <- "Mines,Commerce"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines"] <- "Mines"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines,Aides publiques ou privées"] <- "Mines,Aides publiques ou privées"
cleanData$revenuprincipal[dataH$`Quelles sont vos sources de revenus principales?` == 
                            "Mines,Élevage"] <- "Mines,Élevage"

table(cleanData$revenuprincipal)

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
table(dataH$`Y a-t-il un système de transport collectif dans la communauté?`)



######################### Épicerie #######################
table(dataH$`Où allez vous pour acheter l'épicerie?`)

cleanData$epicerie <- NA

cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == "Midelt"] <-
                                                                      "Midelt"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                      "Midelt,Si autre, laquelle? - Boumia"] <- "Midelt,Boumia"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Si autre, laquelle? - Nzala"] <- "Midelt,Nzala"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Si autre, laquelle? - Riche"] <- "Midelt,Rich"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Zaîda"] <- "Midelt,Zaîda"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Si autre, laquelle? - Boumia"] <- "Boumia"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Zaîda"] <- "Zaîda"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Si autre, laquelle? - Beram"] <- "Midelt,Beram"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Si autre, laquelle? - Missour"] <- "Midelt,Missour"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Si autre, laquelle? - Rich, Nzala" |
                     dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Si autre, laquelle? - Riche, Nzala"] <- "Midelt,Rich,Nzala"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Midelt,Zaîda,Si autre, laquelle? - Boumia"] <- 
                                                        "Midelt,Zaîda,Boumia"
cleanData$epicerie[dataH$`Où allez vous pour acheter l'épicerie?` == 
                     "Si autre, laquelle? - Rich"] <- "Rich"

table(cleanData$epicerie)

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
                               "Non"] <- "Non"
cleanData$consommationalcool[dataH$`Est-ce que vous consommez de l'alcool?` == 
     "Oui,Si oui, à quelle fréquence? (Rarement; Régulièrement) - Rarement"] <-
                                                             "Oui,rarement"
cleanData$consommationalcool[dataH$`Est-ce que vous consommez de l'alcool?` == 
"Oui,Si oui, à quelle fréquence? (Rarement; Régulièrement) - Régulièrement"] <-
                                                      "Oui,régulièrement"

table(cleanData$consommationalcool)

############################ Fumez Tabac ######################

########################### Me donne mal à la tête ça ##################
table(dataH$`Fumez-vous du tabac?`)

cleanData$tabac <- NA

cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Non"] <- "Non"
cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Non,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Occasionnellement" |
                  dataH$`Fumez-vous du tabac?`== "Oui,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Occasionnellement"] <- "Oui, occasionnellement"
cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Non,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Quotidiennement" |
                  dataH$`Fumez-vous du tabac?` == "Oui,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Quotidiennement" |
                  dataH$`Fumez-vous du tabac?` == "Oui,Si oui (à quelle fréquence : Occasionnellement; Quelques fois par semaine; Quotidiennement) - Quotidiennement ( par le nez ? )"] <- "Oui, quotidiennement"
cleanData$tabac[dataH$`Fumez-vous du tabac?` == "Oui"] <- "Oui"

table(cleanData$tabac)

############################ Maladie cardiovasculaire ################
table(dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)

cleanData$maladiecardiovasculaire <- NA

cleanData$maladiecardiovasculaire[dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                    "Non"] <- "Non"
cleanData$maladiecardiovasculaire[dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                    "Hypertension"] <- "Hypertension"
cleanData$maladiecardiovasculaire[dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                    "Hypertension,Maladie coronarienne"] <- 
                                      "Hypertension,maladie coronarienne"
cleanData$maladiecardiovasculaire[dataH$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                    "Maladie coronarienne"] <- "Maladie coronarienne"

table(cleanData$maladiecardiovasculaire)


############################# Maladie chronique ######################

############################# créer plusieurs variables ? #################
table(dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`)

cleanData$maladiechronique <- NA

cleanData$maladiechronique <- as.integer(grepl("Colon|colon", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Douleurs à genou", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Estomac|estomac", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Neurologique", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Problème au yeux|Sensibilité au yeux", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Problème de dos|Dos", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Sciatique", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Diabète", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Maladie rénales", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Maladies respiratoires", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("rhumatisme|Rhumatisme", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Silicose", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

cleanData$maladiechronique <- as.integer(grepl("Non", dataH$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanData$maladiechronique)

############################## Problèmes respiratoires ##############
table(dataH$`Avez-vous des problèmes respiratoires?`)

cleanData$maladierespiratoire <- NA

cleanData$maladierespiratoire[dataH$`Avez-vous des problèmes respiratoires?` ==
                                 "Non"] <- "Non"
cleanData$maladierespiratoire[dataH$`Avez-vous des problèmes respiratoires?` ==
                                "Asthme"] <- "Asthme"
cleanData$maladierespiratoire[dataH$`Avez-vous des problèmes respiratoires?` ==
       "Asthme,Autre (veuillez spécifier) - Sensible aux yeux ( poussière)"] <-
                "Asthme,sensibilité aux yeux causée par la poussière"
cleanData$maladierespiratoire[dataH$`Avez-vous des problèmes respiratoires?` ==
                                "Asthme,Bronchite"] <- "Asthme,bronchite"
cleanData$maladierespiratoire[dataH$`Avez-vous des problèmes respiratoires?` ==
                      "Autre (veuillez spécifier) - Amygdalite"] <- "Amygdalite"
cleanData$maladierespiratoire[dataH$`Avez-vous des problèmes respiratoires?` ==
                                "Bronchite"] <- "Bronchite"

table(cleanData$maladierespiratoire)


############################# problèmes de peau ####################
table(dataH$`Avez-vous des problèmes de peau?`)

cleanData$problemepeau <- NA

cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` == "Non"] <- "Non"
cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` ==
                         "Autre (veuillez spécifier)"] <- "Autre"
cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` == 
                         "Autre (veuillez spécifier) - Allergie"] <- "Allergie"
cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` == 
                         "Démangeaisons"] <- "Démangeaisons"
cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` == 
                         "Éruptions cutanées"] <- "Éruptions cutanées"
cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` == 
       "Éruptions cutanées,Démangeaisons"] <- "Éruptions cutanées,démangeaisons"
cleanData$problemepeau[dataH$`Avez-vous des problèmes de peau?` == 
"Éruptions cutanées,Démangeaisons,Autre (veuillez spécifier) - Squelette ( dos)"
] <- "Éruptions cutanées,démengeaisons,squellete (dos)"

table(cleanData$problèmepeau)

############################# problèmes neurologiques ##################
table(dataH$`Avez-vous des antécédents de problèmes neurologiques?`)

cleanData$problemeneuro <- NA

cleanData$problemeneuro[dataH$`Avez-vous des antécédents de problèmes neurologiques?` ==
                          "Non"] <- "Non"
cleanData$problemeneuro[dataH$`Avez-vous des antécédents de problèmes neurologiques?` ==
                   "Autre (veuillez spécifier) - Parkinson"] <- "Parkinson"
cleanData$problemeneuro[dataH$`Avez-vous des antécédents de problèmes neurologiques?` ==
                          "Maux de tête"] <- "Maux de tête"
cleanData$problemeneuro[dataH$`Avez-vous des antécédents de problèmes neurologiques?` ==
                          "Maux de tête,Vertiges" |
              dataH$`Avez-vous des antécédents de problèmes neurologiques?`==
    "Maux de tête,Vertiges,Autre (veuillez spécifier) - A cause de gaz"] <- 
                                            "Maux de tête,vertige"
cleanData$problemeneuro[dataH$`Avez-vous des antécédents de problèmes neurologiques?` ==
                          "Vertiges"] <- "Vertige"

table(cleanData$problemeneuro)

################################ Problèmes digestifs #####################
table(dataH$`Avez-vous des problèmes digestifs?`)

cleanData$problemedigestif <- NA

cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == "Non"] <- "Non"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
                             "Douleurs abdominales"] <- "Douleurs abdominales"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
                             "Nausées,Douleurs abdominales"] <- 
                                                  "Nausées,douleurs abdominales"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
"Nausées,Vomissements,Douleurs abdominales"] <- "Nausées,Vomissements,Douleurs abdominales"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
                             "Vomissements"] <- "Vomissements"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
                             "Nausées"] <- "Nausées"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
                  "Nausées,Vomissements"] <- "Nausées,vomissements"
cleanData$problemedigestif[dataH$`Avez-vous des problèmes digestifs?` == 
    "Vomissements,Douleurs abdominales"] <- "Vomissements,douleurs abdominales"

table(cleanData$problemedigestif)

####################### Douleurs musculaires/articulaires ##################
table(dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`)

cleanData$douleursmusculaires <- NA

cleanData$douleursmusculaires[dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
                                "Non"] <- "Non"
cleanData$douleursmusculaires[dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
"Oui, j'ai des douleurs articulaires fréquentes"] <- "Oui, j'ai des douleurs articulaires fréquentes"
cleanData$douleursmusculaires[dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
 "Oui, j'ai des douleurs musculaires  fréquentes"] <- "Oui, j'ai des douleurs musculaires  fréquentes"
cleanData$douleursmusculaires[dataH$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
"Oui, j'ai des douleurs musculaires  fréquentes,Oui, j'ai des douleurs articulaires fréquentes"] <- 
  "Oui, j'ai des douleurs musculaires et articulaires fréquentes"

table(cleanData$douleursmusculaires)


######################## Blessure dans les 12 dernier mois ###################

######################## petit hic de code ########################
table(dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`)

cleanData$blessurerecentes <- NA

cleanData$blessurerecentes <- as.integer(grepl("À la main|Casseur à jambe et main|Blessure à la main|Blessures à la main|Blessures à la main et jambe|Blessures à la main et tête|Blessures à la main, dos|Blessures à la main, jambe|Blessures à la main, jambe, tête, dos|Blessures à la main, tête, dos, jambe|Blessures à la main, tête, jambe|Blessures à la mine, tête, jambe", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes)

cleanData$blessurerecente <- as.integer(grepl("Casseur à jambe et main|Blessé à jambe|Blessé à la jambe|Blessures à jambe|Blessures à la jambe|Blessures à la main et jambe|Blessures à la main, jambe|Blessures à la main, jambe, tête, dos|Blessure à la main, tête, dos, jambe|Blessures à la main, tête, jambe|Blessure à la mine, tête, jambe|Casseur à la jambe et les blessures|Casseurs (jambe)", dataH$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanData$blessurerecentes)



######################## Difficultés à dormir/insomnie ##################
table(dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?`)

cleanData$troublesommeil <- NA

cleanData$troublesommeil[dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` == 
                           "Non"] <- "Non"
cleanData$troublesommeil[dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` == 
"Oui, j'ai des difficultés à dormir"] <- "Oui, j'éprouve des difficultés à dormir"
cleanData$troublesommeil[dataH$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` == 
                "Oui, je souffre d'insomnie"] <- "Oui, je souffre d'insomnie"

table(cleanData$troublesommeil)

########################## Où travaillez-vous ####################
table(dataH$`Travaillez-vous dans:`)

cleanData$lieudetravail <- NA

cleanData$lieudetravail[dataH$`Travaillez-vous dans:` == "la mine d'Ahouli"] <-
                                                            "La mine d'Ahouli"
cleanData$lieudetravail[dataH$`Travaillez-vous dans:` == "la mine d'Ahouli,la mine de Mibleden"] <-
  "La mine d'Ahouli et la mine de Mibladen"
cleanData$lieudetravail[dataH$`Travaillez-vous dans:` == "la mine de Mibleden"] <-
  "La mine de Mibladen"

table(cleanData$lieudetravail)


####################### Année d'expérience #################
table(dataH$`Depuis combien de temps travaillez-vous dans la mine ?`)

cleanData$anneeexperience <- NA

cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "<1an" | 
                            dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "1-3 ans"] <- "0 - 3 ans"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "4-6 ans"] <- "4 - 6 ans"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "7-10 ans"] <- "7 - 10 ans"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "10 - 15 ans"] <- "10 - 15 ans"
cleanData$anneeexperience[dataH$`Depuis combien de temps travaillez-vous dans la mine ?` ==
                            "> 20 ans"] <- "Plus de 20 ans"

table(cleanData$anneeexperience)



######################## Perception du danger ###################
table(dataH$`Connaissez-vous les dangers de l'extraction du plomb?`)

cleanData$perceptiondanger <- NA

cleanData$perceptiondanger <- as.integer(grepl("Accident et blessures|risques des accidents et blessures|Blessures|accident et blessures|blessures et accidents ( éboulement)", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("Silicose|silicose", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("mort", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("problème respiratoire ( poumons)|problème de poumons|poussière de poumons|Problème de poumons|problème respiratoire", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("risque d’éboulement|Éboulement|Risque d’éboulement|éboulement", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("rhumatisme", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("sciatique", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("détérioration de la santé", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("asthme", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("problème neurologique", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("problème d’articulation|problème articulations et musculaire|douleurs d’articulation", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("hypertension", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)

cleanData$perceptiondanger <- as.integer(grepl("perte de la vue|accident et blessures baisse de vue|perte de la vue accident et blessures", dataH$`Connaissez-vous les dangers de l'extraction du plomb?`))
table(cleanData$perceptiondanger)





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

cleanData$equipement <- NA

cleanData$equipement <- as.integer(grepl("Casque|casque", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("Lampe|lamp|lampe|Lamp", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("Bottes|botte|bottes", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("tenu", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("masque|Masque", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("gants|Gant|Gants", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("chapeau", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

cleanData$equipement <- as.integer(grepl("lunettes", dataH$`Utilisez-vous des équipements de sécurité ou de protection individuelle lors de votre travail dans la mine?`))
table(cleanData$equipement)

##################### impacts long terme préoccupations ##################
table(dataH$`Selon vous, les impacts à long terme de l'extraction artisanale sur la santé des travailleurs sont-ils préoccupants?`)

cleanData$dangerimpactlongterme <- NA

cleanData$dangerimpactlongterme[dataH$`Selon vous, les impacts à long terme de l'extraction artisanale sur la santé des travailleurs sont-ils préoccupants?` ==
                                  "Oui"] <- 1

table(cleanData$dangerimpactlongterme)

#################### impacts long terme #####################
################### À cleaner comme du monde #################
table(dataH$`Selon vous, quels sont les impacts à long terme de l’extraction artisanale sur la santé des travailleurs et de la communauté?`)


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


###################### risque bétail #####################
table(dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]])

cleanData$impact_betail <- NA

cleanData$impact_betail[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                                                      "Mauvaise"] <- 0

cleanData$impact_betail[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                       "Médiocre"] <- 0.5

cleanData$impact_betail[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                       "Bonne"] <- 1

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
                              "Médiocre"] <- 0.5
cleanData$impact_qualiteeau[dataH[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Bonne"] <- 1

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
###################### questions ouvertes rough ##################
table(dataH$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`)


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
