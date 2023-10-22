
##### Load packages #####
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(wordcloud2)
library(svglite)
library(webshot)
library(htmlwidgets)
### Load Data ####
data <- readRDS("Data/data.rds")


# Analyse -----------------------------------------------------------------


## logement ----------------------------------------------------------------



### reparation_majeure --------------------------------------------------------------
table(data$reparationmajeure)
table(data$ses_revenu)

graph <- data %>% 
  group_by(ses_revenu) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nrepmajeure = sum(reparationmajeure, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nrepmajeure/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_revenu, y = prop * 100, fill = ses_revenu)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Nécessité de réparation majeure dans le logement\nen fonction du revenu", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  scale_x_discrete(labels = c("0 à 5000 dirhams", "5001 à 7000 dirhams", "Plus de 10 000 dirhams"))

ggsave("graphs/logement/repmajeureXrevenu.png",
       width = 9,height = 7)

### reparation mineur -------------------------------------------------------
table(data$reparationmineure)
table(data$ses_revenu)

graph <- data %>% 
  group_by(ses_revenu) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nrepmineure = sum(reparationmineure, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nrepmineure/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

ggplot(graph, aes(x = ses_revenu, y = prop * 100)) +
  geom_bar(stat = "identity") +
  clessnverse::theme_clean_light()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_revenu, y = prop * 100, fill = ses_revenu)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Nécessité de réparation mineure dans le logement\nen fonction du revenu", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  scale_x_discrete(labels = c("0 à 5000 dirhams", "5001 à 7000 dirhams", "Plus de 10 000 dirhams"))

ggsave("graphs/logement/repmineureXrevenu.png",
       width = 9,height = 7)



### toilette ----------------------------------------------------------------
table(data$toilette_in_logement)
table(data$ses_revenu)

graph <- data %>% 
  mutate(ses_revenu = recode(ses_revenu, 
                             "low" = "0 à 5000 dirhams",
                             "mid" = "5001 à 7000 dirhams",
                             "high" = "Plus de 10 000 dirhams")) %>% 
  group_by(ses_revenu, toilette_in_logement) %>% # prépare les prochaines données #
  summarise(ntoilette = n()) %>% # nb de personnes qui ont mis 1 dans la case #
  group_by(ses_revenu) %>% 
  mutate(ngroup = sum(ntoilette),
         prop = ntoilette/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()


colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = toilette_in_logement, y = prop * 100, fill = ses_revenu)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~ses_revenu) +
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Accès à une toilette dans le logement\nen fonction du revenu", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0))  # Centrer le titre du graphique


ggsave("graphs/logement/toiletteXrevenu.png",
       width = 9,height = 7)


### eau courante ------------------------------------------------------------
table(data$eaucourante)
table(data$ses_revenu)

graph <- data %>% 
  group_by(ses_revenu) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            neau = sum(eaucourante, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = neau/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()


colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B", "#E377C2","#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_revenu, y = prop * 100, fill = ses_revenu)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Accès à l'eau courante\nen fonction du revenu", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  scale_x_discrete(labels = c("0 à 5000 dirhams", "5001 à 7000 dirhams", "Plus de 10 000 dirhams"))

ggsave("graphs/logement/eaucouranteXrevenu.png",
       width = 9,height = 7)



### revenumineXsex ----------------------------------------------------------
table(data$revenuprincipal_mines)
table(data$ses_sex)
############## Variable de contrôle ? #################
graph <- data %>% 
  group_by(ses_sex) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nrevenuprincip_mine = sum(revenuprincipal_mines, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nrevenuprincip_mine/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_sex, y = prop * 100, fill = ses_sex)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Revenu provenant de la \nmîne selon le sexe", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  
  ggsave("graphs/Revenus/revenuXsex.png",
         width = 9,height = 7)


## service -----------------------------------------------------------------


### assurance med -----------------------------------------------------------
table(data$assurancemedicale)
table(data$niveauetude)

graph <- data %>% 
  group_by(niveauetude) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nassmed = sum(assurancemedicale, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nassmed/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()


colors <- c("#8C564B", "#E377C2","#BCBD22", "#17BECF")

ggplot(graph, aes(x = niveauetude, y = prop * 100, fill = niveauetude)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Possession d'une assurance médicale\nen fonction du niveau d'études", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique

ggsave("graphs/services/assmedXetude.png",
       width = 9,height = 7)



### assainissement ----------------------------------------------------------
table(data$assainissement)


## sante -------------------------------------------------------------------


### sante generale ----------------------------------------------------------
table(data$santegenerale)
table(data$ses_age)

graph <- data %>% 
  group_by(ses_age) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nsantegen = sum(santegenerale, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nsantegen/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_age, y = prop * 100, fill = ses_age)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Santé générale selon l'âge", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique

ggsave("graphs/sante/santegenXage.png",
       width = 9,height = 7)


### sommeil -----------------------------------------------------------------
table(data$troublesommeil)
table(data$ses_age)

graph <- data %>% 
  group_by(ses_age) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            ntroublesom = sum(troublesommeil, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = ntroublesom/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_age, y = prop * 100, fill = ses_age)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Souffrir de trouble du sommeil\nselon l'âge", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  
  ggsave("graphs/sante/troublesomXage.png",
         width = 9,height = 7)
#####################################################
table(data$troublesommeil)
table(data$stresseconomique)

graph <- data %>% 
  group_by(stresseconomique) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            ntroublesom = sum(troublesommeil, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = ntroublesom/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = stresseconomique, y = prop * 100, fill = stresseconomique)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Souffrir de trouble du sommeil\nselon le niveau de stress de son économie", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  
  ggsave("graphs/sante/troublesomXstresseco.png",
         width = 9,height = 7)


### probcourantXsex ---------------------------------------------------------
table(data_long$santecommunaute_accidents)
table(data$ses_age)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("santecommunaute"),
    names_to = "problemecourantcommunaute",
    names_prefix = "problemecourantcommunaute_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(ses_age, problemecourantcommunaute) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nproblemecourantcommunaute = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = nproblemecourantcommunaute/ngroup,
         ses_age = paste0(ses_age, "\nn = ", ngroup)) 

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(problemecourantcommunaute, prop), fill = ses_age, label = ses_age, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~ses_age) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "L'âge du répondant selon sa perception\ndes problèmes de santé courant de la communauté", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0))

ggsave("graphs/sante/probcourantXage.png",
       width = 15,height = 13)

## perception --------------------------------------------------------------


### danger_accidents --------------------------------------------------------
table(data_long$santecommunaute_accidents)
table(data$niveauetude)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("perceptiondanger"),
    names_to = "dangerextractionplomb",
    names_prefix = "dangerextractionplomb_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(niveauetude, dangerextractionplomb) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            ndangerextractionplomb = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = ndangerextractionplomb/ngroup,
         niveauetude = paste0(niveauetude, "\nn = ", ngroup))

graph <- data(
  accidents = cleanData$perceptiondanger_accidents,
  silicose = cleanData$perceptiondanger_silicose,
  mort = cleanData$perceptiondanger_mort,
  respiratoire = cleanData$perceptiondanger_respiratoire,
  articulation = cleanData$perceptiondanger_articulation,
  asthme = cleanData$perceptiondanger_asthme,
  audition = cleanData$perceptiondanger_audition,
  colon = cleanData$perceptiondanger_colon,
  deterioration = cleanData$perceptiondanger_deterioration,
  eboulement = cleanData$perceptiondanger_eboulement,
  estomac = cleanData$perceptiondanger_estomac,
  hypertension = cleanData$perceptiondanger_hypertension,
  intestin = cleanData$perceptiondanger_intestin,
  neuro = cleanData$perceptiondanger_neuro,
  rein = cleanData$perceptiondanger_rein,
  rhumatisme = cleanData$perceptiondanger_rhumatisme,
  sciatique = cleanData$perceptiondanger_sciatique,
  toxicite = cleanData$perceptiondanger_toxicite,
  vision = cleanData$perceptiondanger_vision)

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(dangerextractionplomb, prop), fill = niveauetude, label = niveauetude, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~niveauetude) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Le niveau d'étude du répondant\nselon sa connaisance des dangers\nde l'extraction du plomb", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0))

ggsave("graphs/perceptions/dangerplombXetude.png",
       width = 20,height = 17)

### amelioration_travail ----------------------------------------------------
table(data$ameliorationsituationeco_travail)
table(data$ses_age)

graph <- data %>% 
  group_by(ses_age) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nameliotravail = sum(ameliorationsituationeco_travail, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nameliotravail/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ameliorationsituationeco_travail, y = prop * 100, fill = ameliorationsituationeco_travail)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Vouloir une meilleure condition de travail ou de meilleurs offres d'emploi\nselon la classe d'âge", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  
  ggsave("graphs/perceptions/amelioecoXses_age.png",
         width = 9,height = 7)




### amelio_feminine ---------------------------------------------------------
table(data$ameliorationsituationeco_ecole)
table(data$ses_age)

graph <- data %>% 
  group_by(ses_age) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            namelioeco_ecole = sum(ameliorationsituationeco_ecole, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = namelioeco_ecole/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")


ggplot(graph, aes(x = ses_age, y = prop * 100, fill = ses_age)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Vouloir des avancés dans la sphère académique\nen fonction de l'âge", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique

ggsave("graphs/perceptions/amelioeco_ecoleXage.png",
       width = 15,height = 13)

### desespoir  --------------------------------------------------------------
table(data$desespoircommunaute)
table(data$ses_age)

graph <- data %>% 
  group_by(ses_age) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            ndesespoir = sum(desespoircommunaute, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = ndesespoir/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = ses_age, y = prop * 100, fill = ses_age)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Sentiment de désespoir dans la \ncommunauté selon l'âge", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
        axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique
  
  ggsave("graphs/perceptions/desespoirXage.png",
         width = 9,height = 7)

### perception danger -------------------------------------------------------
table(data$perceptiondanger_accidents)
table(data$perceptiondanger_respiratoire)
table(data$perceptiondanger_articulation)
table(data$perceptiondanger_asthme)
table(data$perceptiondanger_audition)
table(data$perceptiondanger_colon)
table(data$perceptiondanger_deterioration)
table(data$perceptiondanger_eboulement)
table(data$perceptiondanger_estomac)
table(data$perceptiondanger_hypertension)
table(data$perceptiondanger_intestin)
table(data$perceptiondanger_mort)
table(data$perceptiondanger_neuro)
table(data$perceptiondanger_rein)
table(data$perceptiondanger_rhumatisme)
table(data$perceptiondanger_sciatique)
table(data$perceptiondanger_silicose)
table(data$perceptiondanger_toxicite)
table(data$perceptiondanger_vision)


table(data$ses_age)

graph <- data(
  age = cleanData$ses_age,
  accidents = cleanData$perceptiondanger_accidents,
  silicose = cleanData$perceptiondanger_silicose,
  mort = cleanData$perceptiondanger_mort,
  respiratoire = cleanData$perceptiondanger_respiratoire,
  articulation = cleanData$perceptiondanger_articulation,
  asthme = cleanData$perceptiondanger_asthme,
  audition = cleanData$perceptiondanger_audition,
  colon = cleanData$perceptiondanger_colon,
  deterioration = cleanData$perceptiondanger_deterioration,
  eboulement = cleanData$perceptiondanger_eboulement,
  estomac = cleanData$perceptiondanger_estomac,
  hypertension = cleanData$perceptiondanger_hypertension,
  intestin = cleanData$perceptiondanger_intestin,
  neuro = cleanData$perceptiondanger_neuro,
  rein = cleanData$perceptiondanger_rein,
  rhumatisme = cleanData$perceptiondanger_rhumatisme,
  sciatique = cleanData$perceptiondanger_sciatique,
  toxicite = cleanData$perceptiondanger_toxicite,
  vision = cleanData$perceptiondanger_vision)


table(data_long$ameliorationsituationeco)
table(data$ses_age)
data_long <- data %>%
  pivot_longer(
    cols = starts_with("ameliorationsituationeco"),
    names_to = "ameliorationsituationeco",
    names_prefix = "ameliorationsituationeco_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(ses_age, ameliorationsituationeco) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nameliorationsituationeco = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = nameliorationsituationeco/ngroup,
         ses_age = paste0(ses_age, "\nn = ", ngroup)) # créer une autre colonne avec la proportion #

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(ameliorationsituationeco, prop), fill = ses_age, label = ses_age, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~ses_age) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "L'âge du répondant selon sa perception\ndes améliorations nécessaires dans sa communauté", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0))

  
  ggsave("graphs/perceptions/ameliosituationecoXage.png",
         width = 15,height = 13)
  

# continue ----------------------------------------------------------------
table(data$distanceahouli_provenance)
table(data$ses_age)

ggplot(data, aes(x = distanceahouli_provenance, y = ses_age)) +
  geom_point() +  # Ajouter les points
  labs(x = "Distance à Ahouli (provenance)", y = "Âge",  # Définir les titres des axes
       title = "Graphique à Nuage de Points",
       caption = "Source: Votre Source de Données")  # Définir la légende