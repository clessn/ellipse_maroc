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


############################## Épicerie #################
table(dataFemmes$`Où allez vous pour acheter l'épicerie?`)

cleanDataF$epicerie <- NA

cleanDataF$epicerie[dataFemmes$`Où allez vous pour acheter l'épicerie?` ==
                      "Midelt"] <- "Midelt"

table(cleanDataF$epicerie)

########################## santé générale ###################
table(dataFemmes$`Comment évalueriez-vous votre santé générale?`)

cleanDataF$santegenerale <- NA

cleanDataF$santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Mauvaise"] <- 0
cleanDataF$santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Médiocre"] <- 0.25
cleanDataF$santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Bonne"] <- 0.5
cleanDataF$santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
                           "Très bonne"] <- 0.75
cleanDataF$santegenerale[dataFemmes$`Comment évalueriez-vous votre santé générale?` ==
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
########################### grepl ? ##########################
table(dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`)


######################## problèmes digestifs #######################
table(dataFemmes$`Avez-vous des problèmes digestifs?`)
