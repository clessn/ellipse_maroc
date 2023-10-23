##### Load packages #####
library(tidyverse)
library(RColorBrewer)

### Load Data ####
data <- readRDS("Data/data.rds")

# Marges d'erreur ---------------------------------------------------------

z <- 2.58
samples <- table(data$ses_lieu, data$ses_sex)
pops <- matrix(data = c(70, 60, 300, 250),
               nrow = 2, ncol = 2,
               byrow = TRUE,
               dimnames = list(c("Ahouli", "Mibladen"),
                               c("female", "male")))
samples["Ahouli","male"]
pops["Ahouli","male"]

adjust <- function(lieu,
                   sex,
                   ns = samples,
                   bigns = pops){
  n <-ns[lieu, sex]
  bign <- bigns[lieu, sex]
  coef <- sqrt((bign - n) / (bign - 1))
  return(coef)
}

sds_matrix <- function(vd,
                          d = data){
  sds_table <- d %>%
    group_by(ses_lieu, ses_sex) %>%
    summarise(sd = sd(eval(parse(text = vd)), na.rm = TRUE)) %>%
    drop_na() %>%
    pivot_wider(id_cols = "ses_lieu",
                values_from = "sd",
                names_from = "ses_sex")
  # Convertir le dataframe en matrice
  sds_matrix <- as.matrix(sds_table[,-1])
  rownames(sds_matrix) <- c("Ahouli", "Mibladen")
  colnames(sds_matrix) <- c("female", "male")
  return(sds_matrix)
}

calculate_me <- function(vd,
                         lieu,
                         sex,
                         d = data,
                         z = 2.58,
                         ns = samples,
                         bigns = pops){
  ## get sd of strat
  sd <- sds_matrix(vd)[lieu, sex]
  n <- ns[lieu, sex]
  me <- z * (sd / sqrt(n)) * adjust(lieu, sex)
  return(me) 
}

calculate_me(vd = "santegenerale",
             lieu = "Mibladen",
             sex = "female")


### sante generale ----------------------------------------------------------
table(data$santegenerale)
table(data$ses_sex)


### Code dplyr classique
graph <- data %>% 
  group_by(ses_lieu, ses_sex, santegenerale) %>% # prépare les prochaines données #
  summarise(nvalue = n(), .groups = "drop_last") %>% # nb de personnes qui ont mis 1 dans la case #
  ungroup() %>% 
  complete(ses_lieu, ses_sex, santegenerale, fill = list(nvalue = 0)) %>%
  group_by(ses_lieu, ses_sex) %>% 
  mutate(ngroup = sum(nvalue),
         prop = nvalue/ngroup) %>% # créer une autre colonne avec la proportion #
  drop_na()


### Ajouter la marge d'erreur à la dataframe
graph$me <- NA
for (i in 1:nrow(graph)){
  graph$me[i] <- calculate_me(vd = "santegenerale", ### ici, changer la vd
                              lieu = graph$ses_lieu[i],
                              sex = graph$ses_sex[i])
}

#### Soustraire et additionner la marge d'erreur à la proportion
graph <- graph %>% 
  mutate(low = prop - me,
         high = prop + me)

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")


ggplot(graph, aes(x = santegenerale, y = prop*100, fill = ses_sex)) +
  facet_grid(rows = vars(ses_lieu),
             cols = vars(ses_sex),
             switch = "y") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0("n = ", ngroup)), x = 0.75, y = 75, color = "black", size = 3) +
  scale_fill_manual(values = colors) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion des répondants (%)",
       x = "Évaluation de la santé général de chacun",
       title = "Santé générale des répondants selon\n le sexe et le lieu de résidence",
       caption = "Source : Données Ahouli") +
  theme(axis.title.x = element_text(vjust = 0.5),
        axis.text.x = element_text(angle=0, vjust = 0.5, hjust = 0.5),
        axis.title.y =element_text(hjust=0.5),
        plot.title = element_text(hjust = 0)) +
  geom_point() +
  geom_segment(aes(yend = high*100, y = low*100, xend = santegenerale), color = "black")
  theme(strip.placement = "outside") 
  
ggsave("graphs/perceptions/santegenXlieuXsex.png",
       width = 15, height = 13)
  
  
    geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
    geom_text(aes(label = paste0("n = ",ngroup)), vjust = 0.5, color = "black", size = 3) +  # Ajoute le nombre de répondants au-dessus des barres
    scale_fill_manual(values = colors) +
    clessnverse::theme_clean_light() +
    labs(y = "Proportion de répondants (%)", # Titre de l'axe Y
         title = "Revenu provenant de la \nmîne selon le sexe", # Titre du graphique
         caption = "Source: Données Ahouli") + # Légende en bas à droite
    theme(axis.title.x=element_blank(), # Supprimer le titre de l'axe X
          axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5), # Rotation des labels de l'axe X
          axis.title.y=element_text(hjust=0.5), # Centrer le titre de l'axe Y
          plot.title = element_text(hjust = 0)) + # Centrer le titre du graphique  

# santegenerale X revenu --------------------------------------------------

### Code dplyr classique
graph <- data %>% 
    group_by(ses_lieu, ses_sex, ses_revenu, maladiecardiovasculaire) %>% # prépare les prochaines données #
    summarise(nvalue = n(), .groups = "drop_last") %>% # nb de personnes qui ont mis 1 dans la case #
    ungroup() %>% 
    complete(ses_lieu, ses_sex, ses_revenu, maladiecardiovasculaire,
             fill = list(nvalue = 0)) %>%
    drop_na() %>% 
    group_by(ses_lieu, ses_sex, ses_revenu) %>% 
    mutate(ngroup = sum(nvalue),
           prop = nvalue/ngroup) %>% # créer une autre colonne avec la proportion #
    filter(maladiecardiovasculaire == 1)
  
### Ajouter la marge d'erreur à la dataframe
graph$me <- NA
for (i in 1:nrow(graph)){
    graph$me[i] <- calculate_me(vd = "maladiecardiovasculaire", ### ici, changer la vd
                                lieu = graph$ses_lieu[i],
                                sex = graph$ses_sex[i])
}
  
#### Soustraire et additionner la marge d'erreur à la proportion
graph <- graph %>% 
    mutate(low = prop - me,
           high = prop + me)
  
ggplot(graph, aes(x = ses_revenu, y = prop)) +
    facet_grid(rows = vars(ses_lieu),
               cols = vars(ses_sex),
               switch = "y") +
    geom_bar(stat = "identity") +
    geom_point() +
    geom_segment(aes(yend = high, y = low, xend = ses_revenu)) +
  coord_cartesian(ylim = c(0,1)) +
  theme(strip.placement = "outside")
  