library(tidyverse)
library(readxl)

dataFemmes <- read_xlsx("Vrais_data_femmes_.xlsx", sheet = "Responses")

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
cleanDataF$ses_lieu[dataFemmes$Lieu == "Ahouli" | dataFemmes$Lieu == "Ahou" | dataFemmes$Lieu == "AhouLi" | dataFemmes$Lieu == "Ahouliy"] <- "Ahouli"
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

cleanDataF$logementprincipal <- NA

cleanDataF$logementprincipal[dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Amrssid" |
                               dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Amrssid Mibladen" |
                               dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Amrssid- Mibladen" |
                               dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Amrssid-Mibladen" |
                               dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Commune Amrssid" |
                               dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Commune Amrssid Mibladen" |
                               dataFemmes$`Où est votre logement principal?` ==
                               "Province de Midelt,Précisez la commune - Commune Mibladen" |
                               dataFemmes$`Où est votre logement principal?` ==
                               ""]


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

cleanDataF$visitefamille <- NA

cleanDataF$visitefamille[dataFemmes$`Si vous ne vivez pas avec votre famille, combien de fois rendez-vous visite à votre famille?` ==
                           "Plusieurs fois par an"] <- "Je visite ma famille plusieurs fois par années"
cleanDataF$visitefamille[dataFemmes$`Si vous ne vivez pas avec votre famille, combien de fois rendez-vous visite à votre famille?` ==
                           "Tous les jours"] <- "Je visite ma famille quotidiennement"
cleanDataF$visitefamille[dataFemmes$`Si vous ne vivez pas avec votre famille, combien de fois rendez-vous visite à votre famille?` ==
                           "Une fois par semaine"] <- "Je visite ma famille hebdomadairement"

table(cleanDataF$visitefamille)

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



######################### réparation majeure ####################
table(dataFemmes$`Est-ce que le logement a besoin de réparation majeure?`)

cleanDataF$reparationmajeure <- NA

cleanDataF$reparationmajeure[dataFemmes$`Est-ce que le logement a besoin de réparation majeure?` ==
                               "Non"] <- 0
cleanDataF$reparationmajeure[dataFemmes$`Est-ce que le logement a besoin de réparation majeure?` ==
                               "Oui"] <- 1

table(cleanDataF$reparationmajeure)

########################### réparation mineure ################
table(dataFemmes$`Est-ce que le logement a besoin de réparation mineure?`)

cleanDataF$reparationmineure <- NA

cleanDataF$reparationmineure[dataFemmes$`Est-ce que le logement a besoin de réparation mineure?` ==
                               "Non"] <- 0
cleanDataF$reparationmineure[dataFemmes$`Est-ce que le logement a besoin de réparation mineure?` ==
                               "Oui"] <- 1

table(cleanDataF$reparationmineure)

########################### eau courante #####################
table(dataFemmes$`Est-ce que le logement a l'eau courante?`)

cleanDataF$eaucourante <- NA

cleanDataF$eaucourante[dataFemmes$`Est-ce que le logement a l'eau courante?` ==
                         "Non"] <- 0
cleanDataF$eaucourante[dataFemmes$`Est-ce que le logement a l'eau courante?` ==
                         "Oui"] <- 1

table(cleanDataF$eaucourante)


############################ toilettes ###################
table(dataFemmes$`Est-ce que le logement a des toilettes?`)

cleanDataF$toilette <- NA

cleanDataF$toilette[dataFemmes$`Est-ce que le logement a des toilettes?` ==
                      "Non"] <- "Non"
cleanDataF$toilette[dataFemmes$`Est-ce que le logement a des toilettes?` ==
                      "Oui, à l'extérieur"] <- "Oui, à l'extérieur"
cleanDataF$toilette[dataFemmes$`Est-ce que le logement a des toilettes?` ==
                      "Oui, dans la maison"] <- "Oui, dans la maison"

table(cleanDataF$toilette)


########################## Revenus principals #####################
table(dataFemmes$`Quelles sont vos sources de revenus principales?`)

cleanDataF$revenusprincipales <- NA

cleanDataF$revenusprincipales <- as.integer(grepl("Agriculture", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)

cleanDataF$revenusprincipales <- as.integer(grepl("Aides publiques ou privées", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)

cleanDataF$revenusprincipales <- as.integer(grepl("Fils|Marinet fils|Marie et fils", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)


cleanDataF$revenusprincipales <- as.integer(grepl("Le mari|Le Mari|Mari|Marinet fils|Le Marie|Marie|Marie et fils", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)

cleanDataF$revenusprincipales <- as.integer(grepl("Nièce", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)

cleanDataF$revenusprincipales <- as.integer(grepl("Épargne retraite", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)

cleanDataF$revenusprincipales <- as.integer(grepl("Mines", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenusprincipales)

########################### Montant revenus annuels ##################
table(dataFemmes$`Quel est le montant total de vos revenus annuels?`)

cleanDataF$ses_revenu <- NA

cleanDataF$ses_revenu[dataFemmes$`Quel est le montant total de vos revenus annuels?` ==
                        "0 à 2000 dirhams"] <- "0 à 2000 dirhams"
cleanDataF$ses_revenu[dataFemmes$`Quel est le montant total de vos revenus annuels?` ==
                        "2001 à 5000 dirhams"] <- "2001 à 5000 dirhams"

table(cleanDataF$ses_revenu)

############################# services de santé #################
table(dataFemmes$`Avez-vous accès à des services de santé?`)

cleanDataF$accessoinsante <- NA

cleanDataF$accessoinsante[dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Non"] <- "Non"
cleanDataF$accessoinsante[dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Oui"] <- "Oui"
cleanDataF$accessoinsante[dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Non,Si non, à quelle distance? - 15km" |
                            dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Oui,Si non, à quelle distance? - 15 km" |
                            dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Non,Si non, à quelle distance? - 15 km"] <- "Non, les services les plus proches sont à une distance de 15 km"
cleanDataF$accessoinsante[dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Non,Si non, à quelle distance? - 30 km" |
                            dataFemmes$`Avez-vous accès à des services de santé?` ==
                            "Non,Si non, à quelle distance? - 30km"] <- "Non, les services les plus proches sont à une distance de 30 km"


table(cleanDataF$accessoinsante)
############################ assurance médicale #######################
table(dataFemmes$`Avez vous une assurance médicale?`)

cleanDataF$assurancemedicale <- NA

cleanDataF$assurancemedicale[dataFemmes$`Avez vous une assurance médicale?` ==
                               "Non"] <- 0
cleanDataF$assurancemedicale[dataFemmes$`Avez vous une assurance médicale?` ==
                               "Oui"] <- 1

table(cleanDataF$assurancemedicale)


############################# système d'assainissement ################
table(dataFemmes$`Y a-t-il un système d'assainissement dans la communauté?`)

cleanDataF$assainisement <- NA

cleanDataF$assainisement[dataFemmes$`Y a-t-il un système d'assainissement dans la communauté?` ==
                           "Non"] <- 0
cleanDataF$assainisement[dataFemmes$`Y a-t-il un système d'assainissement dans la communauté?` ==
                           "Oui"] <- 1

table(cleanDataF$assainisement)


############################### école primaire #######################
table(dataFemmes$`Y a-t-il une école primaire dans la communauté?`)

cleanDataF$ecoleprimaire <- NA

cleanDataF$ecoleprimaire[dataFemmes$`Y a-t-il une école primaire dans la communauté?` ==
                           "Oui"] <- 1

table(cleanDataF$ecoleprimaire)


############################### école secondaire ######################
table(dataFemmes$`Y a-t-il une école secondaire dans la communauté?`)

cleanDataF$ecolesecondaire <- NA

cleanDataF$ecolesecondaire[dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Non"] <- "Non"
cleanDataF$ecolesecondaire[dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Oui"] <- "Oui"
cleanDataF$ecolesecondaire[dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Non,Si non, à quelle distance? - 15km" |
                             dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Oui,Si non, à quelle distance? - 15 km" |
                             dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Non,Si non, à quelle distance? - 15 km"] <- "Non, l'école se trouve à une distance de 15 km"
cleanDataF$ecolesecondaire[dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Non,Si non, à quelle distance? - 30km" |
                             dataFemmes$`Y a-t-il une école secondaire dans la communauté?` ==
                             "Non,Si non, à quelle distance? - 30 km" |
                             dataFemmes$`Y a-t-il une école secondaire dans la communauté?` == 
                             "Non,Si non, à quelle distance? - 39 km"] <- "Non, l'école se trouve à une distance d'entre 16 à 39 km"

table(cleanDataF$ecolesecondaire)


############################### système de transport collectif ################
table(dataFemmes$`Y a-t-il un système de transport collectif dans la communauté?`)

cleanDataF$systemetransport <- NA

cleanDataF$systemetransport <- as.integer(grepl("Non", dataFemmes$`Y a-t-il un système de transport collectif dans la communauté?`))
table(cleanDataF$systemetransport)
cleanDataF$systemetransport <- as.integer(grepl("15 km", dataFemmes$`Y a-t-il un système de transport collectif dans la communauté?`))
table(cleanDataF$systemetransport)
cleanDataF$systemetransport <- as.integer(grepl("30 km|30km", dataFemmes$`Y a-t-il un système de transport collectif dans la communauté?`))
table(cleanDataF$systemetransport)
cleanDataF$systemetransport <- as.integer(grepl("Fréquent|Fréquente|fr", dataFemmes$`Y a-t-il un système de transport collectif dans la communauté?`))
table(cleanDataF$systemetransport)

############################## Épicerie #################
table(dataFemmes$`Où allez vous pour acheter l'épicerie?`)

cleanDataF$epicerie <- NA

cleanDataF$epicerie[dataFemmes$`Où allez vous pour acheter l'épicerie?` ==
                      "Midelt"] <- "Midelt"

table(cleanDataF$epicerie)

########################## santé générale ###################
table(dataFemmes$`Comment évalueriez-vous votre santé générale?`)

cleanDataF$ses_santegenerale <- NA

cleanDataF$ses_santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Mauvaise"] <- 0
cleanDataF$ses_santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Médiocre"] <- 0.25
cleanDataF$ses_santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Bonne"] <- 0.5
cleanDataF$ses_santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Très bonne"] <- 0.75
cleanDataF$ses_santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Excellente"] <- 1

table(cleanDataF$santegenerale)


############################## consulté médecin ####################
table(dataFemmes$`Avez-vous consulté un médecin ou un autre professionnel de santé au cours des 12 derniers mois?`)

cleanDataF$consultationmedicale <- NA

cleanDataF$consultationmedicale[dataFemmes$`Avez-vous consulté un médecin ou un autre professionnel de santé au cours des 12 derniers mois?` ==
                                  "Non"] <- 0
cleanDataF$consultationmedicale[dataFemmes$`Avez-vous consulté un médecin ou un autre professionnel de santé au cours des 12 derniers mois?` ==
                                  "Oui"] <- 1

table(cleanDataF$consultationmedicale)


########################## Maladie cardiovasculaire ###################
table(dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)

cleanDataF$maladiecardiovasculaire[dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                     "Non"] <- "Non"
cleanDataF$maladiecardiovasculaire[dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                     "Hypertension"] <- "Hypertension"
cleanDataF$maladiecardiovasculaire[dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                     "Hypertension,Maladie coronarienne"] <- "Hypertension,maladie coronarienne"
cleanDataF$maladiecardiovasculaire[dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                     "Autre (veuillez spécifier) - Anémie"] <- "Anémie"
cleanDataF$maladiecardiovasculaire[dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?` ==
                                     "Hypertension,Autre (veuillez spécifier) - Fatigue"] <- "Hypertension,fatigue"

table(cleanDataF$maladiecardiovasculaire)


########################### maladies chroniques ########################
table(dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`)

cleanDataF$maladiechronique <- NA

cleanDataF$maladiechronique <- as.integer(grepl("Colon", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique <- as.integer(grepl("Hémorroïde", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique <- as.integer(grepl("Kyste sébacé", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique <- as.integer(grepl("Diabète", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique <- as.integer(grepl("Maladies respiratoires", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique <- as.integer(grepl("Maladie rénales", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique <- as.integer(grepl("Non", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)


############################ problèmes respiratoires ####################
table(dataFemmes$`Avez-vous des problèmes respiratoires?`)

cleanDataF$maladierespiratoire <- NA

cleanDataF$maladierespiratoire[dataFemmes$`Avez-vous des problèmes respiratoires?` ==
                                 "Non"] <- "Non"
cleanDataF$maladierespiratoire[dataFemmes$`Avez-vous des problèmes respiratoires?` ==
                                 "Asthme"] <- "Asthme"
cleanDataF$maladierespiratoire[dataFemmes$`Avez-vous des problèmes respiratoires?` ==
                                 "Bronchite"] <- "Bronchite"
cleanDataF$maladierespiratoire[dataFemmes$`Avez-vous des problèmes respiratoires?` ==
                                 "Autre (veuillez spécifier) - Rhume"] <- "Rhume"

table(cleanDataF$maladierespiratoire)

############################### problèmes de peau #######################
table(dataFemmes$`Avez-vous des problèmes de peau?`)

cleanDataF$problemepeau <- NA

cleanDataF$problemepeau[dataFemmes$`Avez-vous des problèmes de peau?` ==
                          "Non"] <- "Non"
cleanDataF$problemepeau[dataFemmes$`Avez-vous des problèmes de peau?` ==
                          "Démangeaisons"] <- "Démangeaisons"
cleanDataF$problemepeau[dataFemmes$`Avez-vous des problèmes de peau?` ==
                          "Éruptions cutanées"] <- "Éruptions cutanées"
cleanDataF$problemepeau[dataFemmes$`Avez-vous des problèmes de peau?` ==
                          "Éruptions cutanées,Démangeaisons"] <- "Éruptions cutanées,démangeaisons"
cleanDataF$problemepeau[dataFemmes$`Avez-vous des problèmes de peau?` ==
                          "Éruptions cutanées,Autre (veuillez spécifier) - Kyste sébacé"] <- "Éruptions cutanées,kyste sébacé"


table(cleanDataF$problemepeau)


############################ problèmes neurologiques ##################
table(dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`)

cleanDataF$problemeneurologique <- NA

cleanDataF$problemeneurologique <- as.integer(grepl("Maux de tête", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneurologique)

cleanDataF$problemeneurologique <- as.integer(grepl("Vertiges", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneurologique)

cleanDataF$problemeneurologique <- as.integer(grepl("Douleurs dans les oreilles", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneurologique)

cleanDataF$problemeneurologique <- as.integer(grepl("Syncope", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneurologique)

cleanDataF$problemeneurologique <- as.integer(grepl("Non", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneurologique)

######################## problèmes digestifs #######################
table(dataFemmes$`Avez-vous des problèmes digestifs?`)

cleanDataF$problemedigestif <- NA

cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
                              "Non"] <- "Non"
cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
                              "Douleurs abdominales"] <- "Douleurs abdominales"
cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
                              "Nausées,Vomissements"] <- "Nausées,vomissements"
cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
                              "Vomissements,Douleurs abdominales"] <- "Vomissements,douleurs abdominales"
cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
                              "Nausées,Douleurs abdominales"] <- "Nausées,douleurs abdominales"
cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
                              "Vomissements"] <- "Vomissements"
cleanDataF$problemedigestif[dataFemmes$`Avez-vous des problèmes digestifs?` ==
             "Nausées,Vomissements,Douleurs abdominales"] <- "Nausées,vomissements,douleurs abdominales"

table(cleanDataF$problemedigestif)


############################# douleurs articulaires/musculaires #############
table(dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`)

cleanDataF$douleursmusculaires <- NA

cleanDataF$douleursmusculaires[dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
                                 "Non"] <- "Non"
cleanDataF$douleursmusculaires[dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
"Oui, j'ai des douleurs articulaires fréquentes"] <- "Oui, j'ai des douleurs articulaires fréquentes"
cleanDataF$douleursmusculaires[dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
"Oui, j'ai des douleurs musculaires  fréquentes"] <- "Oui, j'ai des douleurs musculaires  fréquentes"
cleanDataF$douleursmusculaires[dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?` ==
"Oui, j'ai des douleurs musculaires  fréquentes,Oui, j'ai des douleurs articulaires fréquentes"] <- "Oui, j'ai des douleurs musculaires et articulaires fréquentes"


table(cleanDataF$douleursmusculaires)


############################### blessures accidents dans 12 derniers mois
table(dataFemmes$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`)

cleanDataF$blessurerecentes <- NA

cleanDataF$blessurerecentes[dataFemmes$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?` ==
                              "Non"] <- "Non"
cleanDataF$blessurerecentes[dataFemmes$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?` ==
                              "Oui, j'ai eu un accident au cours des 12 derniers mois,Si oui veuillez préciser quel type de blessure ou d'accident: - Chute de blocs au fond" |
                              dataFemmes$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?` ==
                              "Oui, j'ai eu une blessure au cours des 12 derniers mois,Si oui veuillez préciser quel type de blessure ou d'accident: - Chute de bloc au fond"
                            ] <- "Oui, une chute de blocs au fond"


table(cleanDataF$blessurerecentes)


############################### Difficultés à dormir ####################
table(dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?`)

cleanDataF$troublesommeil <- NA

cleanDataF$troublesommeil[dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` ==
                            "Non"] <- "Non"
cleanDataF$troublesommeil[dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` ==
                            "Oui, je souffre d'insomnie"] <- "Oui, je souffre d'insomnie"
cleanDataF$troublesommeil[dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` ==
                            "Oui, j'ai des difficultés à dormir"] <- "Oui, j'ai des difficultés à dormir"



table(cleanDataF$troublesommeil)


################################### problème santé communauté ################
table(dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté`)

cleanDataF$santecommunaute <- NA

cleanDataF$santecommunaute <- as.integer(grepl("Accidents et blessures|ccidents et blessures", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Hypertension", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("hémorroïde|Hémorroïdes", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Problème digestif|Problème digestifs", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Problèmes respiratoires et cardiovasculaires", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Rhumatisme|rhumatisme", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Troubles musculosquelettiques", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Allergie|allergie", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Fatigue", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Problèmes de santé mentale", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Accidents et blessures|ccidents et blessures", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Autres problèmes de santé chroniques", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Maux de tête", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("Problème neurologique|Maladie neurologique", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("problème rénal|Rénales", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("silicose|Silicose", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("détérioration de la santé", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)

cleanDataF$santecommunaute <- as.integer(grepl("sciatique", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute)


####################################### problème qualité eau ################
table(dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]])

cleanDataF$impact_qualiteeau <- NA

cleanDataF$impact_qualiteeau[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Mauvaise"] <- 0
cleanDataF$impact_qualiteeau[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Médiocre"] <- 0.33
cleanDataF$impact_qualiteeau[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Bonne"] <- 0.66
cleanDataF$impact_qualiteeau[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’eau]"]] ==
                              "Très bonne"] <- 1

table(cleanDataF$impact_qualiteeau)

################################# qualité l'air ######################
table(dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]])

cleanDataF$impact_qualiteair <- NA

cleanDataF$impact_qualiteair[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                              "Mauvaise"] <- 0
cleanDataF$impact_qualiteair[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                               "Médiocre"] <- 0.33
cleanDataF$impact_qualiteair[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                               "Bonne"] <- 0.66
cleanDataF$impact_qualiteair[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’air]"]] ==
                               "Très bonne"] <- 1

table(cleanDataF$impact_qualiteair)


############################### qualité alimentation ################
table(dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]])

cleanDataF$qualite_alimentation <- NA

cleanDataF$qualite_alimentation[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                 "Mauvaise"] <- 0
cleanDataF$qualite_alimentation[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                  "Médiocre"] <- 0.33
cleanDataF$qualite_alimentation[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                  "Bonne"] <- 0.66
cleanDataF$qualite_alimentation[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Qualité de l’alimentation]"]] ==
                                  "Très bonne"] <- 1

table(cleanDataF$impact_qualiteair)

############################# qualité impact bétail #####################
table(dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]])

cleanDataF$impact_betail <- NA

cleanDataF$impact_betail[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                          "Mauvaise"] <- 0
cleanDataF$impact_betail[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                           "Médiocre"] <- 0.33
cleanDataF$impact_betail[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                           "Bonne"] <- 0.66
cleanDataF$impact_betail[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur le bétail]"]] == 
                           "Très bonne"] <- 1

table(cleanDataF$impact_betail)

############################## impact culture ######################
table(dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]])

cleanDataF$impact_culture <- NA

cleanDataF$impact_culture[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                           "Mauvaise"] <- 0
cleanDataF$impact_culture[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                            "Médiocre"] <- 0.33
cleanDataF$impact_culture[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                            "Bonne"] <- 0.66
cleanDataF$impact_culture[dataFemmes[["Quelles sont les risques en santé environnementale qui pourraient être liés à la mine?  \r\n[Impact sur les cultures]"]] ==
                            "Très bonne"] <- 1

table(cleanDataF$impact_culture)


############################# qualité environnement de travail #################
table(dataFemmes$`En général, diriez-vous que la qualité de l'environnement de travail est:`)

cleanDataF$qualitetravail <- NA

cleanDataF$qualitetravail[dataFemmes$`En général, diriez-vous que la qualité de l'environnement de travail est:` ==
                            "Mauvaise"] <- 0
cleanDataF$qualitetravail[dataFemmes$`En général, diriez-vous que la qualité de l'environnement de travail est:` ==
                            "Médiocre"] <- 0.5
cleanDataF$qualitetravail[dataFemmes$`En général, diriez-vous que la qualité de l'environnement de travail est:` ==
                            "Moyenne"] <- 1

table(cleanDataF$qualitetravail)



############################## environnement social #################
table(dataFemmes$`En général, diriez-vous que la qualité de l'environnement social est:`)

cleanDataF$qualitesocial <- NA

cleanDataF$qualitesocial[dataFemmes$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                           "Mauvaise"] <- 0
cleanDataF$qualitesocial[dataFemmes$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                           "Médiocre"] <- 0.33
cleanDataF$qualitesocial[dataFemmes$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                           "Bonne"] <- 0.66
cleanDataF$qualitesocial[dataFemmes$`En général, diriez-vous que la qualité de l'environnement social est:` ==
                           "Très bonne"] <- 1

table(cleanDataF$qualitesocial)


############################# environnement naturel ####################
table(dataFemmes$`En général, diriez-vous que la qualité de l'environnement naturel est:`)

cleanDataF$qualitenaturel <- NA

cleanDataF$qualitenaturel[dataFemmes$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                            "Mauvaise"] <- 0
cleanDataF$qualitenaturel[dataFemmes$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                            "Médiocre"] <- 0.33
cleanDataF$qualitenaturel[dataFemmes$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                            "Bonne"] <- 0.66
cleanDataF$qualitenaturel[dataFemmes$`En général, diriez-vous que la qualité de l'environnement naturel est:` ==
                            "Très bonne"] <- 1

table(cleanDataF$qualitenaturel)


############################# santé générale ####################
table(dataFemmes$`Selon vous, la santé générale de la communauté est :`)

cleanDataF$santegeneralecommunaute <- NA

cleanDataF$santegeneralecommunaute[dataFemmes$`Selon vous, la santé générale de la communauté est :` ==
                                     "Mauvaise"] <- 0
cleanDataF$santegeneralecommunaute[dataFemmes$`Selon vous, la santé générale de la communauté est :` ==
                                     "Moyenne"] <- 0.25
cleanDataF$santegeneralecommunaute[dataFemmes$`Selon vous, la santé générale de la communauté est :` ==
                                     "Bonne"] <- 0.5
cleanDataF$santegeneralecommunaute[dataFemmes$`Selon vous, la santé générale de la communauté est :` ==
                                     "Très bonne"] <- 0.75
cleanDataF$santegeneralecommunaute[dataFemmes$`Selon vous, la santé générale de la communauté est :` ==
                                     "Excellente"] <- 1

table(cleanDataF$santegeneralecommunaute)

########################### amélioration socioéconomique ###############
table(dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`)


########################### formation risque ##################
table(dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?`)

cleanDataF$formationrisque <- NA

cleanDataF$formationrisque[dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?` ==
                             "Non"] <- "Non"
cleanDataF$formationrisque[dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?` ==
"Oui,Si oui, par qui et comment? - Étudiants universitaires"] <- "Oui, par des étudiants universitaires"
cleanDataF$formationrisque[dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?` ==
"Oui,Si oui, par qui et comment? - Gens de la région "] <- "Oui, par des gens de la région"
cleanDataF$formationrisque[dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?` ==
"Oui,Si oui, par qui et comment? - Gens de Mibladen"] <- "Oui, par des gens de Mibladen"
cleanDataF$formationrisque[dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?` ==
"Oui,Si oui, par qui et comment? - Presse ( interview)" |
  dataFemmes$`Avez-vous déjà reçu une formation ou des informations sur les risques sanitaires associés au travail dans la mine, spécifiquement pour les femmes (par exemple, la manipulation de plomb lorsqu'on est enceinte)?` == 
"Oui,Si oui, par qui et comment? - Presse et interview"] <- "Oui, par des conférences de presse ou des interviews"

table(cleanDataF$formationrisque)


################################# préoccupation maternité ###################
table(dataFemmes$`Si vous êtes enceinte ou prévoyez de l'être, avez-vous des préoccupations concernant l'exposition aux substances (et aux conditions de travail) liées à l'exploitation minière ?`)

cleanDataF$preoccupationmaternite <- NA

cleanDataF$preoccupationmaternite[dataFemmes$`Si vous êtes enceinte ou prévoyez de l'être, avez-vous des préoccupations concernant l'exposition aux substances (et aux conditions de travail) liées à l'exploitation minière ?` ==
                                    "Non"] <- 0
cleanDataF$preoccupationmaternite[dataFemmes$`Si vous êtes enceinte ou prévoyez de l'être, avez-vous des préoccupations concernant l'exposition aux substances (et aux conditions de travail) liées à l'exploitation minière ?` ==
                                    "Oui"] <- 1

table(cleanDataF$preoccupationmaternite)

############################ complication maternité #####################
table(dataFemmes$`Si vous êtes enceinte ou avez été enceinte, avez-vous eu des complications pendant la grossesse ou avec la santé de votre enfant depuis que vous travaillez ou vivez près de la mine ?`)

cleanDataF$complicationmaternite <- NA

cleanDataF$complicationmaternite[dataFemmes$`Si vous êtes enceinte ou avez été enceinte, avez-vous eu des complications pendant la grossesse ou avec la santé de votre enfant depuis que vous travaillez ou vivez près de la mine ?` ==
                                   "Non"] <- 0
cleanDataF$complicationmaternite[dataFemmes$`Si vous êtes enceinte ou avez été enceinte, avez-vous eu des complications pendant la grossesse ou avec la santé de votre enfant depuis que vous travaillez ou vivez près de la mine ?` ==
                                   "Oui"] <- 1

table(cleanDataF$complicationmaternite)

############################ problèmes menstruels #################
table(dataFemmes$`Avez-vous eu des problèmes menstruels depuis que vous travaillez ou vivez près de la mine ?`)

cleanDataF$problememenstruel <- NA

cleanDataF$problememenstruel[dataFemmes$`Avez-vous eu des problèmes menstruels depuis que vous travaillez ou vivez près de la mine ?` ==
                               "Non"] <- 0
cleanDataF$problememenstruel[dataFemmes$`Avez-vous eu des problèmes menstruels depuis que vous travaillez ou vivez près de la mine ?` ==
                               "Oui"] <- 1

table(cleanDataF$problememenstruel)

############################ problème fertilité ##################
table(dataFemmes$`Avez-vous déjà rencontré des problèmes de fertilité ou de grossesse vous soupçonnez être liés à l'exposition à l'exploitation minière ?`)

cleanDataF$problemefertilite <- NA

cleanDataF$problemefertilite[dataFemmes$`Avez-vous déjà rencontré des problèmes de fertilité ou de grossesse vous soupçonnez être liés à l'exposition à l'exploitation minière ?` ==
                               "Non"] <- 0
cleanDataF$problemefertilite[dataFemmes$`Avez-vous déjà rencontré des problèmes de fertilité ou de grossesse vous soupçonnez être liés à l'exposition à l'exploitation minière ?` ==
                               "Oui"] <- 1

table(cleanDataF$problemefertilite)


############################ stress travail #################
table(dataFemmes$`Ressentez-vous du stress ou de l'anxiété liés à votre propre travail ou au travail de votre famille dans l'exploitation minière ?`)

cleanDataF$stresstravail <- NA

cleanDataF$stresstravail[dataFemmes$`Ressentez-vous du stress ou de l'anxiété liés à votre propre travail ou au travail de votre famille dans l'exploitation minière ?` ==
                           "Non"] <- 0
cleanDataF$stresstravail[dataFemmes$`Ressentez-vous du stress ou de l'anxiété liés à votre propre travail ou au travail de votre famille dans l'exploitation minière ?` ==
                           "Oui"] <- 1

table(cleanDataF$stresstravail)


############################ stress économie ##################
table(dataFemmes$`Ressentez-vous du stress ou de l'anxiété en raison de votre situation économique ou de celle de votre famille?`)

cleanDataF$stresseconomique <- NA

cleanDataF$stresseconomique[dataFemmes$`Ressentez-vous du stress ou de l'anxiété en raison de votre situation économique ou de celle de votre famille?` ==
                              "Non"] <- 0
cleanDataF$stresseconomique[dataFemmes$`Ressentez-vous du stress ou de l'anxiété en raison de votre situation économique ou de celle de votre famille?` ==
                              "Oui"] <- 1

table(cleanDataF$stresseconomique)

########################## sentiment dépression ####################
table(dataFemmes$`Ressentez-vous un sentiment de désespoir ou de dépression lié aux conditions de travail et de vie dans la communauté?`)

cleanDataF$desespoircommunaute <- NA

cleanDataF$desespoircommunaute[dataFemmes$`Ressentez-vous un sentiment de désespoir ou de dépression lié aux conditions de travail et de vie dans la communauté?` ==
                                 "Non"] <- 0
cleanDataF$desespoircommunaute[dataFemmes$`Ressentez-vous un sentiment de désespoir ou de dépression lié aux conditions de travail et de vie dans la communauté?` ==
                                 "Oui"] <- 1

table(cleanDataF$desespoircommunaute)

########################### santé mentale ##################
table(dataFemmes$`Avez-vous le sentiment que votre santé mentale a été impactée par l'exploitation minière ?`)

cleanDataF$santementale <- NA

cleanDataF$santementale[dataFemmes$`Avez-vous le sentiment que votre santé mentale a été impactée par l'exploitation minière ?` ==
                          "Non"] <- 0
cleanDataF$santementale[dataFemmes$`Avez-vous le sentiment que votre santé mentale a été impactée par l'exploitation minière ?` ==
                          "Oui"] <- 1


########################### soutien mentale ####################
table(dataFemmes$`Avez-vous accès à des ressources de soutien en santé mentale dans votre communauté ?`)

cleanDataF$soutienmentale <- NA

cleanDataF$soutienmentale[dataFemmes$`Avez-vous accès à des ressources de soutien en santé mentale dans votre communauté ?` ==
                            "Non"] <- 0
cleanDataF$soutienmentale[dataFemmes$`Avez-vous accès à des ressources de soutien en santé mentale dans votre communauté ?` ==
                            "Oui"] <- 1

table(cleanDataF$soutienmentale)


########################## services santé reproductive ###############
table(dataFemmes$`Avez-vous accès à des services de santé reproductive dans votre communauté (par exemple, le contrôle des naissances, le dépistage du cancer du sein et du col de l'utérus, les soins prénatals et postnataux)?`)

cleanDataF$servicereproductif <- NA

cleanDataF$servicereproductif[dataFemmes$`Avez-vous accès à des services de santé reproductive dans votre communauté (par exemple, le contrôle des naissances, le dépistage du cancer du sein et du col de l'utérus, les soins prénatals et postnataux)?` ==
                                "Non"] <- "Non"
cleanDataF$servicereproductif[dataFemmes$`Avez-vous accès à des services de santé reproductive dans votre communauté (par exemple, le contrôle des naissances, le dépistage du cancer du sein et du col de l'utérus, les soins prénatals et postnataux)?` ==
"Non,Si non, où accédez-vous à ces services? - 15 km"] <- "Non, les services les plus près sont à 15 km"
cleanDataF$servicereproductif[dataFemmes$`Avez-vous accès à des services de santé reproductive dans votre communauté (par exemple, le contrôle des naissances, le dépistage du cancer du sein et du col de l'utérus, les soins prénatals et postnataux)?` ==
"Non,Si non, où accédez-vous à ces services? - Midelt" |
  dataFemmes$`Avez-vous accès à des services de santé reproductive dans votre communauté (par exemple, le contrôle des naissances, le dépistage du cancer du sein et du col de l'utérus, les soins prénatals et postnataux)?` ==
  "Oui,Si non, où accédez-vous à ces services? - Midelt"] <- "Non, les services les plus près sont à Midelt"

table(cleanDataF$servicereproductif)


####################### accès produits d'hygiène #################
table(dataFemmes$`Avez-vous accès à des produits d'hygiène féminine de base (par exemple, serviettes hygiéniques, tampons) dans votre communauté?`)

cleanDataF$accesproduithygiene <- NA

cleanDataF$accesproduithygiene[dataFemmes$`Avez-vous accès à des produits d'hygiène féminine de base (par exemple, serviettes hygiéniques, tampons) dans votre communauté?` ==
                                 "Non"] <- 0

table(cleanDataF$accesproduithygiene)

####################### victime violence sexiste #################
table(dataFemmes$`Avez-vous ou une autre femme de votre famille été victime de violence ou de discrimination en raison de votre sexe dans votre communauté?`)

cleanDataF$victimesexisme <- NA

cleanDataF$victimesexisme[dataFemmes$`Avez-vous ou une autre femme de votre famille été victime de violence ou de discrimination en raison de votre sexe dans votre communauté?` == 
                            "Non"] <- 0
cleanDataF$victimesexisme[dataFemmes$`Avez-vous ou une autre femme de votre famille été victime de violence ou de discrimination en raison de votre sexe dans votre communauté?` == 
                            "Oui"] <- 1

table(cleanDataF$victimesexisme)

####################### soutien violence sexiste #################
table(dataFemmes$`Avez-vous accès à des services de soutien en cas de violence sexiste (par exemple, un centre de conseil, un abri, des services juridiques) dans votre communauté?`)

cleanDataF$soutienviolencesexiste <- NA

cleanDataF$soutienviolencesexiste[dataFemmes$`Avez-vous accès à des services de soutien en cas de violence sexiste (par exemple, un centre de conseil, un abri, des services juridiques) dans votre communauté?` ==
                                    "Non"] <- "Non"
cleanDataF$soutienviolencesexiste[dataFemmes$`Avez-vous accès à des services de soutien en cas de violence sexiste (par exemple, un centre de conseil, un abri, des services juridiques) dans votre communauté?` ==
"Non,Si non, où accédez-vous à ces services? - 15 km"] <- "Non, les services les plus près sont à 15 km"
cleanDataF$soutienviolencesexiste[dataFemmes$`Avez-vous accès à des services de soutien en cas de violence sexiste (par exemple, un centre de conseil, un abri, des services juridiques) dans votre communauté?` ==
"Non,Si non, où accédez-vous à ces services? - Midelt"] <- "Non, les services les plus près sont à Midelt"


table(cleanDataF$soutienviolencesexiste)



###################### opportunités égales #####################
table(dataFemmes$`Pensez-vous que l'exploitation minière offre des opportunités égales pour les femmes et les hommes ?`)

cleanDataF$opportunitesegales <- NA

cleanDataF$opportunitesegales[dataFemmes$`Pensez-vous que l'exploitation minière offre des opportunités égales pour les femmes et les hommes ?` ==
                                "Non"] <- 0
cleanDataF$opportunitesegales[dataFemmes$`Pensez-vous que l'exploitation minière offre des opportunités égales pour les femmes et les hommes ?` ==
                                "Oui"] <- 1

table(cleanDataF$opportunitesegales)


####################### emplois femmes #######################
table(dataFemmes$`Quelle est votre opinion sur les opportunités d'emploi pour les femmes dans votre communauté ?`)

cleanDataF$opportunitesemploisfemmes <- NA

cleanDataF$opportunitesemploisfemmes[dataFemmes$`Quelle est votre opinion sur les opportunités d'emploi pour les femmes dans votre communauté ?` ==
                                       "Très insatisfaisante"] <- 0
cleanDataF$opportunitesemploisfemmes[dataFemmes$`Quelle est votre opinion sur les opportunités d'emploi pour les femmes dans votre communauté ?` ==
                                       "Insatisfaisante"] <- 0.5
cleanDataF$opportunitesemploisfemmes[dataFemmes$`Quelle est votre opinion sur les opportunités d'emploi pour les femmes dans votre communauté ?` ==
                                       "Satisfaisante"] <- 1

table(cleanDataF$opportunitesemploisfemmes)

######################### participation décisions économiques ################à
table(dataFemmes$`Participez-vous aux décisions économiques de votre foyer (par exemple, comment dépenser l'argent, combien d'argent économiser)?`)

cleanDataF$participationeconomique <- NA

cleanDataF$participationeconomique[dataFemmes$`Participez-vous aux décisions économiques de votre foyer (par exemple, comment dépenser l'argent, combien d'argent économiser)?` ==
                                     "Non"] <- 0
cleanDataF$participationeconomique[dataFemmes$`Participez-vous aux décisions économiques de votre foyer (par exemple, comment dépenser l'argent, combien d'argent économiser)?` ==
                                     "Oui"] <- 1

table(cleanDataF$participationeconomique)

