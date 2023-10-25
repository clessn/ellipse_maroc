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



### bivarié -----------------------------------------------------------------
text <- data$opendangerplomb
text <- gsub("[[:punct:]]", " ", text)
text <- tolower(text)
table(data$ses_lieu)
# Ajoutez des étiquettes pour les hommes et les femmes
text_ahouli <- data$openprobsantecourant[data$ses_lieu == "Ahouli"]
text_mibladen <- data$openprobsantecourant[data$ses_lieu == "Mibladen"]
# Combinez les textes avec des étiquettes
text_ahouli <- gsub("[[:punct:]]", " ", text_ahouli)
docs_ahouli <- Corpus(VectorSource(text_ahouli))
docs_ahouli <- docs_ahouli %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problèmes",'troubles'))

text_mibladen <- gsub("[[:punct:]]", " ", text_mibladen)
docs_mibladen <- Corpus(VectorSource(text_mibladen))
docs_mibladen <- docs_mibladen %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problèmes','troubles'))

dtm_ahouli <- TermDocumentMatrix(docs_ahouli)
matrix_ahouli <- as.matrix(dtm_ahouli)
words_ahouli <- sort(rowSums(matrix_ahouli), decreasing = TRUE)
df_ahouli <- data.frame(word = names(words_ahouli), freq = words_ahouli) %>% 
  mutate(lieu = "Ahouli")

dtm_mibladen <- TermDocumentMatrix(docs_mibladen)
matrix_mibladen <- as.matrix(dtm_mibladen)
words_mibladen <- sort(rowSums(matrix_mibladen), decreasing = TRUE)
df_mibladen <- data.frame(word = names(words_mibladen), freq = words_mibladen) %>% 
  mutate(lieu = "Mibladen")

df <- rbind(df_ahouli, df_mibladen)

ggplot(df, aes(label = word, size = freq, fill = lieu)) +
  geom_bar(stat = "identity")+
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la connaissance des dangers de l'extraction\ndu plomb, selon le lieu de résidence") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 18) +
  facet_wrap(~lieu, nrow = 2) +
  theme(strip.text = element_text(size = 25))

ggsave("graphs/cloudword/OpendangerplombXlieu.png",
       width = 15, height = 13)




### graph/prob/sex ----------------------------------------------------------
table(data_long$santecommunaute_accidents)
table(data$ses_sex)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("santecommunaute"),
    names_to = "problemecourantcommunaute",
    names_prefix = "problemecourantcommunaute_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(ses_sex, problemecourantcommunaute) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nproblemecourantcommunaute = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = nproblemecourantcommunaute/ngroup,
         ses_sex = paste0(ses_sex, "\nn = ", ngroup)) %>% 
  mutate(problemecourantcommunaute = gsub("problemecourantcommunaute_","", problemecourantcommunaute)) %>% 
  mutate(problemecourantcommunaute = gsub("santecommunaute_","", problemecourantcommunaute))


colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(problemecourantcommunaute, prop), fill = ses_sex, label = ses_sex, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~ses_sex) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Le sexe du répondant selon sa perception\ndes problèmes de santé courant de la communauté", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0))

ggsave("graphs/sante/probcourantXses.png",
       width = 15,height = 13)



### graph/probsante/lieu ----------------------------------------------------
table(data_long$santecommunaute_accidents)
table(data$ses_lieu)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("santecommunaute"),
    names_to = "problemecourantcommunaute",
    names_prefix = "problemecourantcommunaute_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(ses_lieu, problemecourantcommunaute) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            nproblemecourantcommunaute = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = nproblemecourantcommunaute/ngroup,
         ses_lieu = paste0(ses_lieu, "\nn = ", ngroup)) %>% 
  mutate(problemecourantcommunaute = gsub("problemecourantcommunaute_","", problemecourantcommunaute)) %>% 
  mutate(problemecourantcommunaute = gsub("santecommunaute_","", problemecourantcommunaute))


colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(problemecourantcommunaute, prop), fill = ses_lieu, label = ses_lieu, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~ses_lieu) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Le lieu de résidence du répondant selon sa perception\ndes problèmes de santé courant de la communauté", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0))

ggsave("graphs/sante/probcourantXlieu.png",
       width = 15,height = 13)


### graph/danger/sex --------------------------------------------------------
table(data_long$santecommunaute_accidents)
table(data$ses_sex)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("perceptiondanger"),
    names_to = "dangerextractionplomb",
    names_prefix = "dangerextractionplomb_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(ses_sex, dangerextractionplomb) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            ndangerextractionplomb = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = ndangerextractionplomb/ngroup,
         ses_sex = paste0(ses_sex, "\nn = ", ngroup)) %>% 
  mutate(dangerextractionplomb = gsub("perceptiondanger_","",dangerextractionplomb))


colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(dangerextractionplomb, prop), fill = ses_sex, label = ses_sex, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Le sexe du répondant\nselon sa connaisance des dangers\nde l'extraction du plomb", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) 

ggsave("graphs/previous/perceptions/dangerplombXsex.png",
       width = 15, height = 13)



# graphs/danger/lieu ------------------------------------------------------
table(data_long$santecommunaute_accidents)
table(data$ses_lieu)

data_long <- data %>%
  pivot_longer(
    cols = starts_with("perceptiondanger"),
    names_to = "dangerextractionplomb",
    names_prefix = "dangerextractionplomb_",
    values_to = "valeur")

graph <- data_long %>% 
  group_by(ses_lieu, dangerextractionplomb) %>% # prépare les prochaines données #
  summarise(ngroup = n(), # va chercher le nb de répondants de chque grps #
            ndangerextractionplomb = sum(valeur, na.rm = TRUE)) %>% # nb de personnes qui ont mis 1 dans la case #
  drop_na() %>% 
  mutate(prop = ndangerextractionplomb/ngroup,
         ses_lieu = paste0(ses_lieu, "\nn = ", ngroup)) %>% 
  mutate(dangerextractionplomb = gsub("perceptiondanger_","",dangerextractionplomb))


colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#a6bddb", "#d95f02", "#006837", "#74c476", "#c7e9c0", "#a50026", "#d73027","#f46d43", "#fdae61", "#54278f", "#6a51a3", "#9e9ac8", "#cbc9e2", "#636363", "#9ebcda", "#e0ecf4", "#fee0d2", "#525252", "#969696", "#bdbdbd", "#d9d9d9")

ggplot(graph, aes(x = prop * 100, y = reorder(dangerextractionplomb, prop), fill = ses_lieu, label = ses_lieu, stat(count))) +
  geom_bar(stat = "identity",show.legend = FALSE) + # Couleur des barres
  facet_wrap(~ses_lieu) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(x = "Proportion de répondants (%)", # Titre de l'axe Y
       title = "Le lieu de résidence du répondant\nselon sa connaisance des dangers\nde l'extraction du plomb", # Titre du graphique
       caption = "Source: Données Ahouli") + # Légende en bas à droite
  theme(axis.title.y=element_blank(), # Supprimer le titre de l'axe X
        axis.text.y=element_text(angle=0, vjust=0, hjust=0.5), # Rotation des labels de l'axe X
        axis.title.x=element_text(hjust=0.5), # Centrer le titre de l'axe Y
        plot.title = element_text(hjust = 0)) 

ggsave("graphs/previous/perceptions/dangerplombXlieu.png",
       width = 15, height = 13)

