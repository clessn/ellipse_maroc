
##### Load packages #####
library(tidyverse)


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
            nstresseco = sum(stresseconomique, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  mutate(prop = nstresseco/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

ggplot(graph, aes(x = stresseconomique, y = prop * 100, fill = stresseconomique)) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  geom_text(aes(label = paste0("n = ",ngroup)), vjust = -0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
  scale_fill_manual(values = colors) +
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
## perception --------------------------------------------------------------


### danger_accidents --------------------------------------------------------
table(data$perceptiondanger_accidents)
table(data$ses)



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



