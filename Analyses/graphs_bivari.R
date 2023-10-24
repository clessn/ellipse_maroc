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
         high = prop + me,
         ses_sex = ifelse(ses_sex == "male", "Hommes", "Femmes"))

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")


breaks <- c(0.0, 0.25, 0.5, 0.75,1)

breaks <- c(0.0, 0.25, 0.5, 0.75, 1)

labels <- c("Médiocre", "Mauvaise", "Moyenne", "Bonne", "Très bonne")
names(labels) <- breaks

ggplot(graph, aes(x = santegenerale, y = prop*100, fill = ses_sex)) +
  facet_grid(rows = vars(ses_lieu),
             cols = vars(ses_sex),
             switch = "y") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0("n = ", ngroup)), x = 0.75, y = 75, color = "black", size = 3) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),
                     limits = c(-30, 100)) +
  clessnverse::theme_clean_light() +
  labs(y = "Proportion des répondants (%)",
       x = "\nÉvaluation de la santé général de chacun",
       title = "Santé générale des répondants selon\n le sexe et le lieu de résidence",
       caption = "La marge d'erreur a été calculée pour chaque combinaison de sexe (homme/femme) et\nde lieu (mine d'Ahouli/mine de Mibladen) afin d'évaluer la précision des estimations.") +
  theme(axis.title.x = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=0, vjust = 0.5, hjust = 0.5),
        axis.title.y =element_text(hjust=0.5),
        plot.title = element_text(hjust = 0),
        panel.spacing = unit(1, "cm")) +
  geom_point(show.legend = FALSE) +
  geom_segment(aes(yend = high*100, y = low*100, xend = santegenerale), color = "black")

ggsave("graphs/perceptions/santegenXlieuXsex.png",
       width = 12, height = 10)


# santegenerale X revenu --------------------------------------------------

### Code dplyr classique
graph <- data %>% 
    group_by(ses_lieu, ses_sex, ses_revenu, maladiecardiovasculaire) %>% # prépare les prochaines données #
    summarise(nvalue = n(), .groups = "drop_last") %>% # nb de personnes qui ont mis 1 dans la case #
    ungroup() %>% 
    complete(ses_lieu, ses_sex, ses_revenu, maladiecardiovasculaire,
             fill = list(nvalue = 0)) %>%
    drop_na() %>% 
    group_by(ses_lieu, ses_sex) %>% 
    mutate(ngroup = sum(nvalue),
           prop = nvalue/ngroup,
           prop = ifelse(is.nan(prop), 0, prop)) %>% # créer une autre colonne avec la proportion #
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
           high = prop + me,
           ses_sex = ifelse(ses_sex == "male", "Hommes", "Femmes"))
  

colors <- c("#1F77B4", "#FF7F0E", "#2CA02C","#8C564B","#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")


ggplot(graph, aes(x = ses_revenu, y = prop*100, fill = ses_sex)) +
    facet_grid(rows = vars(ses_lieu),
               cols = vars(ses_sex),
               switch = "y") +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = paste0("n = ", ngroup)), x = "high", y = 100, color = "black", size = 3) +
    scale_fill_manual(values = colors) +
    clessnverse::theme_clean_light() +
    scale_y_continuous(breaks = seq(0, 100, by = 10),
                     limits = c(-70, 150)) +
    labs(y = "Proportion des répondants qui\nont eu des maladies cardiovasculaires (%)",
       x = "\nNiveau de revenu",
       title = "Incidence de maladies cardiovasculaires des répondants selon\n le sexe, le lieu de résidence et le revenu",
       caption = "La marge d'erreur a été calculée pour chaque combinaison de sexe (homme/femme) et\nde lieu (mine d'Ahouli/mine de Mibladen) afin d'évaluer la précision des estimations.") +
    theme(axis.title.x = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=0, vjust = 0.5, hjust = 0.5),
        axis.title.y =element_text(hjust=0.5),
        plot.title = element_text(hjust = 0),
        panel.spacing = unit(1, "cm")) +
    geom_point(show.legend = FALSE) +
    scale_x_discrete(labels = c("0 à 5000\ndirhams", "5001 à 7000\ndirhams", "Plus de 10 000\ndirhams")) +
    geom_segment(aes(yend = high*100, y = low*100, xend = ses_revenu), color = "black") + 
    theme(strip.placement = "outside")
  
ggsave("graphs/perceptions/maladiecardioXrevenu.png",
       width = 12, height = 10)
