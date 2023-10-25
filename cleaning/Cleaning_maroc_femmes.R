library(tidyverse)
library(readxl)

dataFemmes <- read_xlsx("raw_data/Vrais_data_femmes_.xlsx", sheet = "Responses")

cleanDataF <- data.frame(id= dataFemmes$`ID de la réponse`)

################################## Âge #################
table(dataFemmes$`Quel âge avez-vous ?`)

cleanDataF$ses_age <- NA
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "18 - 20 ans"] <- "18_20"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "21 - 29 ans"] <- "21_29"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "30 - 39 ans"] <- "30_39"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "40-49"] <- "40_49"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "50-59"] <- "50_59"
cleanDataF$ses_age[dataFemmes$`Quel âge avez-vous ?` == "60 ou plus"] <- "60+"

cleanDataF$ses_age <- factor(cleanDataF$ses_age,
                            ordered = TRUE,
                            levels = c("18_20", "21_29", "30_39",
                                       "40_49", "50_59", "60+"))

table(cleanDataF$ses_age)


############### État civil #######
table(dataFemmes$`Quel est votre état civil ?`)

cleanDataF$ses_etatcivil <- NA

cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Célibataire"
] <- "celib"
cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Divorcée/séparée"
] <- "divorcee"
cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Mariée"
] <- "mariee"
cleanDataF$ses_etatcivil[dataFemmes$`Quel est votre état civil ?` == "Veuve"
] <- "veuve"

table(cleanDataF$ses_etatcivil)


##################### Lieu ######################
table(dataFemmes$Lieu)

cleanDataF$ses_lieu <- NA
cleanDataF$ses_lieu[dataFemmes$Lieu == "Ahouli" | dataFemmes$Lieu == "Ahou" | dataFemmes$Lieu == "AhouLi" | dataFemmes$Lieu == "Ahouliy"] <- "Ahouli"
cleanDataF$ses_lieu[dataFemmes$Lieu == "Mibladen"] <- "Mibladen"

table(cleanDataF$ses_lieu)


#################### Statut professionel actuel ##########
#################### est-ce qu'on inclue si invalide est seulement 2 ##########
table(dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?`)

cleanDataF$statutTravail <- NA

cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Femme au foyer"] <- "femme_foyer"
cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Travailleuse autonome"] <- "travailleuse_autonome"
cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Sans emploi - ne recherche pas d'emploi"] <- "cherche_pas_emploi"
cleanDataF$statutTravail[dataFemmes$`Parmi les catégories suivantes, laquelle décrit le mieux votre statut professionnel actuel ?` ==
                           "Sans emploi - recherche un emploi"] <- "cherche_emploi"

table(cleanDataF$statutTravail)


#################### Cmb d'enfants ####################
table(dataFemmes$`Combien d'enfants avez-vous ?`)

cleanDataF$ses_enfants <- NA

cleanDataF$ses_enfants <- as.numeric(dataFemmes$`Combien d'enfants avez-vous ?`)
cleanDataF$ses_enfants[is.na(dataFemmes$`Combien d'enfants avez-vous ?`)] <- 0

class(cleanDataF$ses_enfants)
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
                         "Sans diplôme"] <- "sans_diplome"
cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Enseignement primaire"] <- "primaire"
cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Enseignement secondaire"] <- "secondaire"
cleanDataF$niveauetude[dataFemmes$`Quel est le plus haut niveau d'études que vous ayez atteint ?` ==
                         "Enseignement postsecondaire"] <- "postsecondaire"

cleanDataF$niveauetude <- factor(cleanDataF$niveauetude, ordered = TRUE,
                                levels = c("sans_diplome", "primaire", "secondaire", "postsecondaire"))

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
                           "Plusieurs fois par an"] <- 0
cleanDataF$visitefamille[dataFemmes$`Si vous ne vivez pas avec votre famille, combien de fois rendez-vous visite à votre famille?` ==
                           "Tous les jours"] <- 1
cleanDataF$visitefamille[dataFemmes$`Si vous ne vivez pas avec votre famille, combien de fois rendez-vous visite à votre famille?` ==
                           "Une fois par semaine"] <- 0.5

table(cleanDataF$visitefamille)

######################### pièce habitable ###################
table(dataFemmes$`Quel est le nombre de pièce habitable de votre logement?`)

cleanDataF$nombrepiecehabitable <- NA

cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "1"] <- 1
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "2"] <- 2
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "3"] <- 3
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "4"] <- 4
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "5"] <- 5
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "6"] <- 6
cleanDataF$nombrepiecehabitable[dataFemmes$`Quel est le nombre de pièce habitable de votre logement?` ==
                                 "7"] <- 7

table(cleanDataF$nombrepiecehabitable)





############################# cmb de personnes vivent là ########################
table(dataFemmes$`Combien de personnes vivent dans le logement ?`)

cleanDataF$nbpersonnelogement <- NA 

cleanDataF$nbpersonnelogement <- as.numeric(dataFemmes$`Combien de personnes vivent dans le logement ?`)
#cleanDataF$nbpersonnelogement[is.na(dataFemmes$`Combien de personnes vivent dans le logement ?`)] <- 0

class(cleanDataF$nbpersonnelogement)
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

cleanDataF$toilette_in_logement <- NA

cleanDataF$toilette_in_logement[dataFemmes$`Est-ce que le logement a des toilettes?` == 
                                 "Non"] <- "no"
cleanDataF$toilette_in_logement[dataFemmes$`Est-ce que le logement a des toilettes?` ==
                                 "Oui, à l'extérieur"] <- "yes_outside"
cleanDataF$toilette_in_logement[dataFemmes$`Est-ce que le logement a des toilettes?` ==
                                 "Oui, dans la maison" |
                                 dataFemmes$`Est-ce que le logement a des toilettes?` ==
                                 "Oui, dans la maison,Oui, à l'extérieur"] <- "yes"
cleanDataF$toilette_in_logement <- factor(cleanDataF$toilette_in_logement,
                                         ordered = TRUE,
                                         levels = c("no", "yes_outside",
                                                    "yes"))
table(cleanDataF$toilette_in_logement)
unique(cleanDataF$toilette_in_logement)


########################## Revenus principals #####################
table(dataFemmes$`Quelles sont vos sources de revenus principales?`)

cleanDataF$revenuprincipal_agriculture <- NA
cleanDataF$revenuprincipal_aides <- NA
cleanDataF$revenuprincipal_famille <- NA
cleanDataF$revenuprincipal_epargne <- NA
cleanDataF$revenuprincipal_mines <- NA

cleanDataF$revenuprincipal_agriculture <- as.integer(grepl("Agriculture", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenuprincipal_agriculture)

cleanDataF$revenuprincipal_aides <- as.integer(grepl("Aides publiques ou privées", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenuprincipal_aides)

cleanDataF$revenuprincipal_famille <- as.integer(grepl("Le mari|Le Mari|Mari|Marinet fils|Le Marie|Marie|Marie et fils|Nièce", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenuprincipal_famille)

cleanDataF$revenuprincipal_epargne <- as.integer(grepl("Épargne retraite", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenuprincipal_epargne)

cleanDataF$revenuprincipal_mines <- as.integer(grepl("Mines", dataFemmes$`Quelles sont vos sources de revenus principales?`))
table(cleanDataF$revenuprincipal_mines)

########################### Montant revenus annuels ##################
table(dataFemmes$`Quel est le montant total de vos revenus annuels?`)

cleanDataF$ses_revenu <- NA

cleanDataF$ses_revenu[dataFemmes$`Quel est le montant total de vos revenus annuels?` ==
                        "0 à 2000 dirhams"] <- "low"
cleanDataF$ses_revenu[dataFemmes$`Quel est le montant total de vos revenus annuels?` ==
                        "2001 à 5000 dirhams"] <- "mid"

cleanDataF$ses_revenu <- factor(cleanDataF$ses_revenu, ordered = TRUE,
                               levels = c("low", "mid", "high"))
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

cleanDataF$assainissement <- NA

cleanDataF$assainissement[dataFemmes$`Y a-t-il un système d'assainissement dans la communauté?` ==
                           "Non"] <- 0
cleanDataF$assainissement[dataFemmes$`Y a-t-il un système d'assainissement dans la communauté?` ==
                           "Oui"] <- 1

table(cleanDataF$assainissement)


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

cleanDataF$maladiecardiovasculaire <- NA
cleanDataF$maladiecardiovasculaire_hypertension <- NA
cleanDataF$maladiecardiovasculaire_coronarienne <- NA
cleanDataF$maladiecardiovasculaire_fatigue <- NA

cleanDataF$maladiecardiovasculaire <- as.integer(grepl("Non",dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)) 
table(cleanDataF$maladiecardiovasculaire)    

cleanDataF$maladiecardiovasculaire_hypertension <- as.integer(grepl("Hypertension",dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)) 
table(cleanDataF$maladiecardiovasculaire_hypertension)    

cleanDataF$maladiecardiovasculaire_coronarienne <- as.integer(grepl("Maladie coronarienne|Anémie",dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)) 
table(cleanDataF$maladiecardiovasculaire_coronarienne)    

cleanDataF$maladiecardiovasculaire_fatigue <- as.integer(grepl("Fatigue",dataFemmes$`Avez-vous déjà été diagnostiqué avec une maladie cardiovasculaire?`)) 
table(cleanDataF$maladiecardiovasculaire_fatigue)    


########################### maladies chroniques ########################
table(dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`)

cleanDataF$maladiechronique <- NA
cleanDataF$maladiechronique_colon <- NA
cleanDataF$maladiechronique_hemorroide <- NA
cleanDataF$maladiechronique_kyste <- NA
cleanDataF$maladiechronique_diabete <- NA
cleanDataF$maladiechronique_respiratoire <- NA
cleanDataF$maladiechronique_rein <- NA

cleanDataF$maladiechronique <- 1 - as.integer(grepl("Non", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique)

cleanDataF$maladiechronique_colon <- as.integer(grepl("Colon", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique_colon)

cleanDataF$maladiechronique_hemorroide <- as.integer(grepl("Hémorroïde", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique_hemorroide)

cleanDataF$maladiechronique_kyste <- as.integer(grepl("Kyste sébacé", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique_kyste)

cleanDataF$maladiechronique_diabete <- as.integer(grepl("Diabète", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique_diabete)

cleanDataF$maladiechronique_respiratoire <- as.integer(grepl("Maladies respiratoires", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique_respiratoire)

cleanDataF$maladiechronique_rein <- as.integer(grepl("Maladie rénales", dataFemmes$`Avez-vous déjà été diagnostiqué avec une autre maladie chronique?`))
table(cleanDataF$maladiechronique_rein)


############################ problèmes respiratoires ####################
table(dataFemmes$`Avez-vous des problèmes respiratoires?`)

cleanDataF$maladierespiratoire <- NA
cleanDataF$maladierespiratoire_asthme <- NA
cleanDataF$maladierespiratoire_bronchite <- NA
cleanDataF$maladierespiratoire_rhume <- NA

cleanDataF$maladierespiratoire <- 1 - as.integer(grepl("Non",dataFemmes$`Avez-vous des problèmes respiratoires?`))
table(cleanDataF$maladierespiratoire)
cleanDataF$maladierespiratoire_asthme <- as.integer(grepl("Asthme",dataFemmes$`Avez-vous des problèmes respiratoires?`))
table(cleanDataF$maladierespiratoire_asthme)
cleanDataF$maladierespiratoire_bronchite <- as.integer(grepl("Bronchite",dataFemmes$`Avez-vous des problèmes respiratoires?`))
table(cleanDataF$maladierespiratoire_bronchite)
cleanDataF$maladierespiratoire_rhume <- as.integer(grepl("Rhume",dataFemmes$`Avez-vous des problèmes respiratoires?`))
table(cleanDataF$maladierespiratoire_rhume)
cleanDataF$maladierespiratoire_amygdalite <- as.integer(grepl("Amygdalite", dataFemmes$`Avez-vous des problèmes respiratoires?`))
table(cleanDataF$maladierespiratoire)


############################### problèmes de peau #######################
table(dataFemmes$`Avez-vous des problèmes de peau?`)

cleanDataF$problemepeau <- NA
cleanDataF$problemepeau_demangeaisons <- NA
cleanDataF$problemepeau_eruptions <- NA
cleanDataF$problemepeau_kyste <- NA

cleanDataF$problemepeau <- 1 - as.integer(grepl("Non",dataFemmes$`Avez-vous des problèmes de peau?`))
table(cleanDataF$problemepeau)
cleanDataF$problemepeau_demangeaisons <- as.integer(grepl("Démangeaisons",dataFemmes$`Avez-vous des problèmes de peau?`))
table(cleanDataF$problemepeau_demangeaisons)
cleanDataF$problemepeau_eruptions <- as.integer(grepl("Éruptions cutanées",dataFemmes$`Avez-vous des problèmes de peau?`))
table(cleanDataF$problemepeau_eruptions)
cleanDataF$problemepeau_kyste <- as.integer(grepl("Kyste sébacé",dataFemmes$`Avez-vous des problèmes de peau?`))
table(cleanDataF$problemepeau_kyste)


############################ problèmes neurologiques ##################
table(dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`)

cleanDataF$problemeneuro <- NA
cleanDataF$problemeneuro_maux_de_tete <- NA
cleanDataF$problemeneuro_vertige <- NA
cleanDataF$problemeneuro_oreille <- NA
cleanDataF$problemeneuro_syncope <- NA


cleanDataF$problemeneuro <- 1 - as.integer(grepl("Non", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneuro)

cleanDataF$problemeneuro_maux_de_tete <- as.integer(grepl("Maux de tête", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneuro_maux_de_tete)

cleanDataF$problemeneuro_vertige <- as.integer(grepl("Vertiges", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneuro_vertige)

cleanDataF$problemeneuro_oreille <- as.integer(grepl("Douleurs dans les oreilles", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneuro_oreille)

cleanDataF$problemeneuro_syncope <- as.integer(grepl("Syncope", dataFemmes$`Avez-vous des antécédents de problèmes neurologiques?`))
table(cleanDataF$problemeneuro_syncope)

######################## problèmes digestifs #######################
table(dataFemmes$`Avez-vous des problèmes digestifs?`)

cleanDataF$problemedigestif <- NA
cleanDataF$problemedigestif_douleurabdo <- NA
cleanDataF$problemedigestif_vomissements <- NA
cleanDataF$problemedigestif_nausees <- NA

cleanDataF$problemedigestif <- 1 - as.integer(grepl("Non",dataFemmes$`Avez-vous des problèmes digestifs?`))
table(cleanDataF$problemedigestif)
cleanDataF$problemedigestif_douleurabdo <- as.integer(grepl("Douleurs abdominales",dataFemmes$`Avez-vous des problèmes digestifs?`))
table(cleanDataF$problemedigestif_douleurabdo)
cleanDataF$problemedigestif_vomissements <- as.integer(grepl("Vomissements",dataFemmes$`Avez-vous des problèmes digestifs?`))
table(cleanDataF$problemedigestif_vomissements)
cleanDataF$problemedigestif_nausees <- as.integer(grepl("Nausées",dataFemmes$`Avez-vous des problèmes digestifs?`))
table(cleanDataF$problemedigestif_nausees)


############################# douleurs articulaires/musculaires #############
table(dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`)

cleanDataF$douleursmusculaires <- NA
cleanDataF$douleursarticulaires <- NA

cleanDataF$douleursmusculaires <- as.integer(grepl("musculaires",   dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`))
cleanDataF$douleursarticulaires <- as.integer(grepl("articulaires", dataFemmes$`Avez-vous des douleurs musculaires ou articulaires fréquentes?`))

table(cleanDataF$douleursmusculaires)
table(cleanDataF$douleursarticulaires)


############################### blessures accidents dans 12 derniers mois ####
table(dataFemmes$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`)

cleanDataF$blessurerecentes <- NA

cleanDataF$blessurerecentes <- 1 - as.integer(grepl("Non", dataFemmes$`Avez-vous eu une blessure ou un accident au cours des 12 derniers mois?`))
table(cleanDataF$blessurerecentes)


############################### Difficultés à dormir ####################
table(dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?`)

cleanDataF$troublesommeil <- NA

cleanDataF$troublesommeil[dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` ==
                            "Non"] <- 0
cleanDataF$troublesommeil[dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` ==
                            "Oui, je souffre d'insomnie" |dataFemmes$`Avez-vous des difficultés à dormir ou souffrez-vous d'insomnie?` ==
                            "Oui, j'ai des difficultés à dormir"] <- 1

table(cleanDataF$troublesommeil)


################################### problème santé communauté ################
table(dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté`)

cleanDataF$santecommunaute_accidents <- NA
cleanDataF$santecommunaute_hypertension <- NA
cleanDataF$santecommunaute_hemorroide <- NA
cleanDataF$santecommunaute_digestif <- NA
cleanDataF$santecommunaute_respiratoire_cardio <- NA
cleanDataF$santecommunaute_rhumatisme <- NA
cleanDataF$santecommunaute_musculo_squelettique <- NA
cleanDataF$santecommunaute_allergie <- NA
cleanDataF$santecommunaute_fatigue <- NA
cleanDataF$santecommunaute_mentale <- NA
cleanDataF$santecommunaute_chronique <- NA
cleanDataF$santecommunaute_maux_de_tete <- NA
cleanDataF$santecommunaute_neurologique <- NA
cleanDataF$santecommunaute_rein <- NA
cleanDataF$santecommunaute_silicose <- NA
cleanDataF$santecommunaute_diminution <- NA
cleanDataF$santecommunaute_sciatique <- NA

cleanDataF$santecommunaute_accidents <- as.integer(grepl("Accidents et blessures|ccidents et blessures", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_accidents)

cleanDataF$santecommunaute_hypertension <- as.integer(grepl("Hypertension", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_hypertension)

cleanDataF$santecommunaute_hemorroide <- as.integer(grepl("hémorroïde|Hémorroïdes", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_hemorroide)

cleanDataF$santecommunaute_digestif <- as.integer(grepl("Problème digestif|Problème digestifs", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_digestif)

cleanDataF$santecommunaute_respiratoire_cardio <- as.integer(grepl("Problèmes respiratoires et cardiovasculaires", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_respiratoire_cardio)

cleanDataF$santecommunaute_rhumatisme <- as.integer(grepl("Rhumatisme|rhumatisme", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_rhumatisme)

cleanDataF$santecommunaute_musculo_squelettique <- as.integer(grepl("Troubles musculosquelettiques", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_musculo_squelettique)

cleanDataF$santecommunaute_allergie <- as.integer(grepl("Allergie|allergie", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_allergie)

cleanDataF$santecommunaute_fatigue <- as.integer(grepl("Fatigue", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_fatigue)

cleanDataF$santecommunaute_mentale <- as.integer(grepl("Problèmes de santé mentale", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_mentale)

cleanDataF$santecommunaute_chronique <- as.integer(grepl("Autres problèmes de santé chroniques", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_chronique)

cleanDataF$santecommunaute_maux_de_tete <- as.integer(grepl("Maux de tête", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_maux_de_tete)

cleanDataF$santecommunaute_neurologique <- as.integer(grepl("Problème neurologique|Maladie neurologique", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_neurologique)

cleanDataF$santecommunaute_rein <- as.integer(grepl("problème rénal|Rénales", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_rein)

cleanDataF$santecommunaute_silicose <- as.integer(grepl("silicose|Silicose", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_silicose)

cleanDataF$santecommunaute_diminution <- as.integer(grepl("détérioration de la santé", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_diminution)

cleanDataF$santecommunaute_sciatique <- as.integer(grepl("sciatique", dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté` ))
table(cleanDataF$santecommunaute_sciatique)


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
                            "Médiocre"] <- 0.33
cleanDataF$qualitetravail[dataFemmes$`En général, diriez-vous que la qualité de l'environnement de travail est:` ==
                            "Moyenne"] <- 0.66

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

cleanDataF$ameliorationsituationeco_alteco <- NA
cleanDataF$ameliorationsituationeco_sante <- NA
cleanDataF$ameliorationsituationeco_ecole <- NA
cleanDataF$ameliorationsituationeco_transport <- NA
cleanDataF$ameliorationsituationeco_comms <- NA
cleanDataF$ameliorationsituationeco_travail <- NA
cleanDataF$ameliorationsituationeco_soutien_femme <- NA
cleanDataF$ameliorationsituationeco_public <- NA
cleanDataF$ameliorationsituationeco_dechets <- NA
cleanDataF$ameliorationsituationeco_rehab <- NA
cleanDataF$ameliorationsituationeco_propriete <- NA
cleanDataF$ameliorationsituationeco_vert <- NA
cleanDataF$ameliorationsituationeco_amusement_enfant <- NA
cleanDataF$ameliorationsituationeco_eau_potable <- NA
cleanDataF$ameliorationsituationeco_securite <- NA
cleanDataF$ameliorationsituationeco_souk <- NA
cleanDataF$ameliorationsituationeco_installation <- NA
cleanDataF$ameliorationsituationeco_supermarche <- NA
cleanDataF$ameliorationsituationeco_alphabetisation <- NA

cleanDataF$ameliorationsituationeco_alteco <- as.integer(grepl("Aide financière", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_alteco)

cleanDataF$ameliorationsituationeco_sante <- as.integer(grepl("Hôpital|Ambulance|Médecin|médicaux|Médicaments|Première secours à Mibladen|Contrôle et suivie de silicose à Mibladen|Équipements médicaux à Mibladen|Équipements d'utilité publique|Dispensaire", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_sante)

cleanDataF$ameliorationsituationeco_ecole <- as.integer(grepl("École secondaire|Transport scolaire|Classe préscolaire", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_ecole)

cleanDataF$ameliorationsituationeco_transport <- as.integer(grepl("Route et ponts|Route|Pavage des rues|Aménagement de la route|Transport|Bus|Transport publique|Aménagement de la route Mibladen- Ahouli|Aménagement de la route Mibladen Ahouli|Pavage des rues", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_transport)

cleanDataF$ameliorationsituationeco_comms <- as.integer(grepl("Réseau de communication|Réseau et internet|Réseau de communication et internet à Ahouli|Électricité à Ahouli|Réseau", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_comms)

cleanDataF$ameliorationsituationeco_travail <- as.integer(grepl("Offres d’emploi|Offres d’emploi pour la jeunesse", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_travail)

cleanDataF$ameliorationsituationeco_soutien_femme <- as.integer(grepl("Soutien des coopératives féminines|Promouvoir des coopératives féminines|Coopératives féminines", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_soutien_femme)

cleanDataF$ameliorationsituationeco_public <- as.integer(grepl("Plaque publique|Panneaux de publicité", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_public)

cleanDataF$ameliorationsituationeco_dechets <- as.integer(grepl("Conteneurs des déchets|Des conteneurs des déchets|Dépotoir|Gestion des déchets", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_dechets)

cleanDataF$ameliorationsituationeco_rehab <- as.integer(grepl("Réhabilitation|Réhabilitations des maisons", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_rehab)

cleanDataF$ameliorationsituationeco_propriete <- as.integer(grepl("Titre de propriété des maisons", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_propriete)

cleanDataF$ameliorationsituationeco_vert <- as.integer(grepl("Plantation et reboisement à Mibladen|Reboisement et plantation à Mibladen|Problème de forestiers|Plantation et reboisement des jardins de Mibladen|Jardin public", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_vert)

cleanDataF$ameliorationsituationeco_amusement_enfant <- as.integer(grepl("Parc de joue aux enfants|Pace de joue des enfants|Terrains de football pour les enfants|Maisons de jeunesses|Activités sportives au enfant|Centre de loisirs", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_amusement_enfant)

cleanDataF$ameliorationsituationeco_eau_potable <- as.integer(grepl("L’eau potable|Traitement de l’eau potable|Eau potable|Filtration et traitement de l’eau potable|Problème de l’assainissement|l’eau", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_eau_potable)

cleanDataF$ameliorationsituationeco_securite <- as.integer(grepl("Sécurité|gendarmerie|police", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_securite)

cleanDataF$ameliorationsituationeco_souk <- as.integer(grepl("Suq hebdomadaire|Souq", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_souk)

cleanDataF$ameliorationsituationeco_installation <- as.integer(grepl("Hamame publique|prière", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_installation)

cleanDataF$ameliorationsituationeco_supermarche <- as.integer(grepl("Supermarchés", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_supermarche)

cleanDataF$ameliorationsituationeco_alphabetisation <- as.integer(grepl("Alphabétisation|Analphabétisme|alphabétisation", dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?`))
table(cleanDataF$ameliorationsituationeco_alphabetisation)

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


cleanDataF$openameliorationeco <- dataFemmes$`Qu'est ce qui pourrait  améliorer la situation socioéconomique et sanitaire de la communauté?` 

cleanDataF$openprobsantecourant <- dataFemmes$`Quelles sont les problèmes de santé les plus courant dans votre communauté`

saveRDS(cleanDataF,"Data/femmes.rds")



######### question ouvertes ######