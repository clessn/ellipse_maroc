##### Load packages #####
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(wordcloud2)
library(svglite)
library(webshot)
library(htmlwidgets)
library(ggwordcloud)
library(writexl)

### Load Data ####
data <- readRDS("Data/data.rds")


# CloudWord ---------------------------------------------------------------


## CloudWord/probsante -----------------------------------------------------

text <- c(data$openprobsantecourant)

text <- gsub("[[:punct:]]", " ", text)

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("french"), 'veuillez',"autre","grand","spécifier","autres","santé", "problèmes",'troubles'))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light() +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la question des problèmes de\nsanté courants dans la communauté") +
 theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 23) 

ggsave("graphs/cloudword/probsantecourant.png",
       width = 15 ,height = 15)


#### openprobsante homme/femme -----------------------------------------------------
text_hommes <- data$openprobsantecourant[data$ses_sex == "male"]
text_femmes <- data$openprobsantecourant[data$ses_sex == "female"]

# Prétraitement du texte pour les hommes
text_hommes <- gsub("[[:punct:]]", " ", text_hommes)
docs_hommes <- Corpus(VectorSource(text_hommes))
docs_hommes <- docs_hommes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problèmes",'troubles'))

# Prétraitement du texte pour les femmes
text_femmes <- gsub("[[:punct:]]", " ", text_femmes)
docs_femmes <- Corpus(VectorSource(text_femmes))
docs_femmes <- docs_femmes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problèmes','troubles'))

# Créez le nuage de mots pour les hommes
dtm_hommes <- TermDocumentMatrix(docs_hommes)
matrix_hommes <- as.matrix(dtm_hommes)
words_hommes <- sort(rowSums(matrix_hommes), decreasing = TRUE)
df_hommes <- data.frame(word = names(words_hommes), freq = words_hommes) %>% 
  mutate(sex = "Hommes")

# Créez le nuage de mots pour les femmes
dtm_femmes <- TermDocumentMatrix(docs_femmes)
matrix_femmes <- as.matrix(dtm_femmes)
words_femmes <- sort(rowSums(matrix_femmes), decreasing = TRUE)
df_femmes <- data.frame(word = names(words_femmes), freq = words_femmes) %>% 
  mutate(sex = "Femmes")

df <- rbind(df_hommes, df_femmes)

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la question des problèmes de santé courants\ndans la communauté, selon le sexe") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 13) +
  facet_wrap(~sex, nrow = 2) +
  theme(strip.text = element_text(size = 25))

ggsave("graphs/cloudword/OpenprobsanteXsex.png",
       width = 15, height = 13)




# Prétraitez le texte et ajoutez des étiquettes pour les hommes et les femmes
text <- data$openprobsantecourant
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
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problèmes","troubles"))

text_mibladen <- gsub("[[:punct:]]", " ", text_mibladen)
docs_mibladen <- Corpus(VectorSource(text_mibladen))
docs_mibladen <- docs_mibladen %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problèmes',"troubles"))

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

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la question des problèmes de santé courants\ndans la communauté, selon le lieu de résidence\n") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 13) +
  facet_wrap(~lieu, nrow = 2) +
  theme(strip.text = element_text(size = 25))

ggsave("graphs/cloudword/OpenprobsanteXlieu.png",
       width = 15, height = 13)

### cloudword/danger --------------------------------------------------------
text <- c(data$opendangerplomb)

text <- gsub("[[:punct:]]", " ", text)

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("french"),"oui", "savez", "dire", "pouvez", "risque",'problème','troubles'))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light() +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la connaissance des dangers de l'extraction du plomb") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 23) 

ggsave("graphs/cloudword/opendanger.png",
       width = 15,height = 13)



### open/danger/sex ---------------------------------------------------------
text_hommes <- data$opendangerplomb[data$ses_sex == "male"]
text_femmes <- data$opendangerplomb[data$ses_sex == "female"]

# Prétraitement du texte pour les hommes
text_hommes <- gsub("[[:punct:]]", " ", text_hommes)
docs_hommes <- Corpus(VectorSource(text_hommes))
docs_hommes <- docs_hommes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problème","oui","dire","savez","pouvez",'troubles'))

# Prétraitement du texte pour les femmes
text_femmes <- gsub("[[:punct:]]", " ", text_femmes)
docs_femmes <- Corpus(VectorSource(text_femmes))
docs_femmes <- docs_femmes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problème',"oui","dire","savez","pouvez",'troubles'))

# Créez le nuage de mots pour les hommes
dtm_hommes <- TermDocumentMatrix(docs_hommes)
matrix_hommes <- as.matrix(dtm_hommes)
words_hommes <- sort(rowSums(matrix_hommes), decreasing = TRUE)
df_hommes <- data.frame(word = names(words_hommes), freq = words_hommes) %>% 
  mutate(sex = "Hommes")

# Créez le nuage de mots pour les femmes
dtm_femmes <- TermDocumentMatrix(docs_femmes)
matrix_femmes <- as.matrix(dtm_femmes)
words_femmes <- sort(rowSums(matrix_femmes), decreasing = TRUE)
df_femmes <- data.frame(word = names(words_femmes), freq = words_femmes) %>% 
  mutate(sex = "Femmes")

df <- rbind(df_hommes, df_femmes)

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la connaissance des dangers de l'extraction\ndu plomb, selon le sexe") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 23) +
  facet_wrap(~sex, nrow = 2) +
  theme(strip.text = element_text(size = 25))

ggsave("graphs/cloudword/OpendangerplombXsex.png",
       width = 15, height = 13)


### open/dangerplomb/lieu ---------------------------------------------------
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

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la connaissance des dangers de l'extraction\ndu plomb, selon le lieu de résidence") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 18) +
  facet_wrap(~lieu, nrow = 2) +
  theme(strip.text = element_text(size = 25))

ggsave("graphs/cloudword/OpendangerplombXlieu.png",
       width = 15, height = 13)



### open/ameliosocio --------------------------------------------------------
text <- c(data$openameliorationeco)

text <- gsub("[[:punct:]]", " ", text)

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("french"), "ahouli", "mibladen","Ahouli","Mibladen","hebdomadaire","publique","intermédiaires","aide","eau"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words) %>% 
  mutate(category = category[match(word, names(category))])


category <- c(
  "médecin" = "Santé",
  "hôpital" = "Santé",
  "dispensaire" = "Santé",
  "suivie" = "Santé",
  "médicaux" = "Santé",
  "traitement" = "Santé",
  "plomb" = "Santé",
  "ambulance" = "Santé",
  "médicaments" = "Santé",
  "salaires" = "Santé",
  "inondations" = "Santé",
  "irrigations" = "Santé",
  "contrôle" = "Santé",
  "hamame" = "Santé",
  "médicinale" = "Santé",
  "médecins" = "Santé",
  "médicinal" = "Santé",
  "réhabilitation" = "Santé",
  "réhabilitations" = "Santé",
  "silicose" = "Santé",
  "médicinales" = "Santé",
  "assainissement" = "Santé",
  "équipements" = "Santé",
  "equipements" = "Santé",
  "généraliste" = "Santé",
  "médoc" = "Santé",
  "école" = "Éducation",
  "écoles" = "Éducation",
  "préscolaires" = "Éducation",
  "élèves" = "Éducation",
  "enfants" = "Éducation",
  "alphabétisation" = "Éducation",
  "analphabétisme" = "Éducation",
  "analphabétismes" = "Éducation",
  "classe" = "Éducation",
  "scolaire" = "Éducation",
  "projets" = "Éducation",
  "bourses" = "Éducation",
  "jeunesse" = "Éducation",
  "apprentissage" = "Éducation",
  "collège" = "Éducation",
  "enfant" = "Éducation",
  "préscolaire" = "Éducation",
  "secondaire" = "Éducation",
  "route" = "Transport",
  "réseau" = "Transport",
  "communication" = "Transport",
  "transport" = "Transport",
  "parc" = "Transport",
  "bus" = "Transport",
  "titre" = "Transport",
  "canaux" = "Transport",
  "site" = "Transport",
  "voiture" = "Transport",
  "pont" = "Transport",
  "voie" = "Transport",
  "rues" = "Transport",
  "taxi" = "Transport",
  "ruelle" = "Transport",
  "terres agricoles" = "Environnement",
  "aménagement" = "Environnement",
  "eau potable" = "Environnement",
  "minière" = "Environnement",
  "dépotoir" = "Environnement",
  "déchets" = "Environnement",
  "potable" = "Environnement",
  "conteneurs" = "Environnement",
  "énergies solaires" = "Environnement",
  "reboisement" = "Environnement",
  "puits" = "Environnement",
  "période" = "Environnement",
  "énergie solaire" = "Environnement",
  "arbres" = "Environnement",
  "forestiers" = "Environnement",
  "ruelles" = "Environnement",
  "électricité" = "Environnement",
  "nettoyage" = "Environnement",
  "poubelles" = "Environnement",
  "économique" = "Économie et Emploi",
  "emploi" = "Économie et Emploi",
  "bourse" = "Économie et Emploi",
  "offres" = "Économie et Emploi",
  "coopératives" = "Économie et Emploi",
  "soutien" = "Économie et Emploi",
  "promouvoir" = "Économie et Emploi",
  "redémarrage" = "Économie et Emploi",
  "usines" = "Économie et Emploi",
  "usine" = "Économie et Emploi",
  "titre" = "Économie et Emploi",
  "alternative" = "Économie et Emploi",
  "travail" = "Économie et Emploi",
  "financière" = "Économie et Emploi",
  "financier" = "Économie et Emploi",
  "produits" = "Économie et Emploi",
  "logement" = "Économie et Emploi",
  "agricoles" = "Économie et Emploi",
  "agriculteurs" = "Économie et Emploi",
  "vente" = "Économie et Emploi",
  "vendre" = "Économie et Emploi",
  "intérêt" = "Économie et Emploi",
  "mine" = "Économie et Emploi",
  "interêt" = "Économie et Emploi",
  "création" = "Économie et Emploi",
  "restauration" = "Économie et Emploi",
  "commercialisation" = "Économie et Emploi",
  "revenu" = "Économie et Emploi",
  "minéraux" = "Économie et Emploi",
  "supermarché" = "Économie et Emploi",
  "supermarchés" = "Économie et Emploi",
  "opportunité" = "Économie et Emploi",
  "mineurs" = "Économie et Emploi",
  "tourisme" = "Culture et Loisirs",
  "muséologique" = "Culture et Loisirs",
  "musée" = "Culture et Loisirs",
  "artisans" = "Culture et Loisirs",
  "plantes" = "Culture et Loisirs",
  "romarin" = "Culture et Loisirs",
  "publicité" = "Culture et Loisirs",
  "loisirs" = "Culture et Loisirs",
  "jardin" = "Culture et Loisirs",
  "musique" = "Culture et Loisirs",
  "naissance" = "Culture et Loisirs",
  "langues" = "Culture et Loisirs",
  "musical" = "Culture et Loisirs",
  "terres" = "Culture et Loisirs",
  "touristique" = "Culture et Loisirs", 
  "titre" = "Juridique",
  "permission" = "Juridique",
  "propriété" = "Juridique",
  "maisons" = "Juridique",
  "légal" = "Juridique",
  "juridique" = "Juridique",
  "autorisation" = "Juridique",
  "contrôle" = "Juridique",
  "droit" = "Juridique",
  "loi" = "Juridique",
  "justice" = "Juridique",
  "terrains" = "Juridique",
  "gendarmerie" = "Juridique",
  "législatif" = "Juridique",
  "protection" = "Juridique",
  "sécurité" = "Juridique",
  "police" = "Juridique",
  "centre" = "Juridique",
  "femmes" = "Social",
  "gens" = "Social",
  "famille" = "Social",
  "social" = "Social",
  "citoyen" = "Social",
  "femme" = "Social",
  "veufs" = "Social",
  "internet" = "Social",
  "personnels" = "Social",
  "personnes" = "Social",
  "communauté" = "Social",
  "jeunes" = "Social",
  "souq" = "Social",
  "suq" = "Social",
  "pavage" = "Social",
  "commune" = "Social",
  "locales" = "Social",
  "collectives" = "Social",
  "association" = "Social",
  "féminines" = "Social",
  "veuves" = "Social",
  "société" = "Social"
)



ggplot(df, aes(label = word, size = freq, color = category)) +
  geom_text_wordcloud() +
  scale_color_manual(values = rainbow(length(unique(df$category)))) +
  clessnverse::theme_clean_light() +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la question des améliorations \nsocio-sanitaires de la communauté") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 30) 

ggsave("graphs/cloudword/Openameliosani.png",
       width = 15, height = 13)

write_xlsx(df,"graphs/cloudword/Openameliosani.xlsx")

### Open/amelio/sex ---------------------------------------------------------
text_hommes <- data$openameliorationeco[data$ses_sex == "male"]
text_femmes <- data$openameliorationeco[data$ses_sex == "female"]

# Prétraitement du texte pour les hommes
text_hommes <- gsub("[[:punct:]]", " ", text_hommes)
docs_hommes <- Corpus(VectorSource(text_hommes))
docs_hommes <- docs_hommes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problème","oui","dire","savez","pouvez","ahouli",'mibladen',"hebdomadaire","alternative"))

# Prétraitement du texte pour les femmes
text_femmes <- gsub("[[:punct:]]", " ", text_femmes)
docs_femmes <- Corpus(VectorSource(text_femmes))
docs_femmes <- docs_femmes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problème',"oui","dire","savez","pouvez","ahouli","mibladen","hebdomadaire","alternative"))

# Créez le nuage de mots pour les hommes
dtm_hommes <- TermDocumentMatrix(docs_hommes)
matrix_hommes <- as.matrix(dtm_hommes)
words_hommes <- sort(rowSums(matrix_hommes), decreasing = TRUE)
df_hommes <- data.frame(word = names(words_hommes), freq = words_hommes) %>% 
  mutate(sex = "Hommes")

# Créez le nuage de mots pour les femmes
dtm_femmes <- TermDocumentMatrix(docs_femmes)
matrix_femmes <- as.matrix(dtm_femmes)
words_femmes <- sort(rowSums(matrix_femmes), decreasing = TRUE)
df_femmes <- data.frame(word = names(words_femmes), freq = words_femmes) %>% 
  mutate(sex = "Femmes")

category <- c(
  "médecin" = "Santé",
  "hôpital" = "Santé",
  "dispensaire" = "Santé",
  "suivie de silicose" = "Santé",
  "équipements médicaux" = "Santé",
  "traitement" = "Santé",
  "plomb" = "Santé",
  "ambulance" = "Santé",
  "médicaments" = "Santé",
  "salaires" = "Santé",
  "inondations" = "Santé",
  "Puits irrigations" = "Social",
  "hamame" = "Santé",
  "médicinale" = "Santé",
  "médecins" = "Santé",
  "médicinal" = "Santé",
  "réhabilitation" = "Santé",
  "réhabilitations" = "Santé",
  "médicinales" = "Santé",
  "assainissement" = "Santé",
  "equipements" = "Santé",
  "médecin généraliste" = "Santé",
  "médoc" = "Santé",
  "école secondaire" = "Éducation",
  "écoles" = "Éducation",
  "classe préscolaire" = "Éducation",
  "élèves" = "Éducation",
  "Terrain de football pour les enfants" = "Éducation",
  "alphabétisation" = "Éducation",
  "analphabétisme" = "Éducation",
  "analphabétismes" = "Éducation",
  "transport scolaire" = "Transport",
  "projet de pâturage" = "Social",
  "bourse des minéraux" = "Économie et Emploi",
  "Maisons de jeunesses" = "Culture et Loisirs",
  "apprentissage" = "Éducation",
  "collège" = "Éducation",
  "enfant" = "Éducation",
  "route" = "Transport",
  "Route Ahouli Mibladen" = "Transport",
  "réseau de communication" = "Transport",
  "transport" = "Transport",
  "parc de jeux aux enfants" = "Culture et Loisirs",
  "bus" = "Transport",
  "canaux" = "Transport",
  "site" = "Transport",
  "voiture" = "Transport",
  "pont" = "Transport",
  "voie" = "Transport",
  "Pavage des rues" = "Transport",
  "taxi" = "Transport",
  "Pavage des ruelles" = "Transport",
  "terres agricoles" = "Environnement",
  "aménagement de la piscine" = "Environnement",
  "eau potable" = "Environnement",
  "Permission minière" = "Économie et Emploi",
  "dépotoir" = "Environnement",
  "conteneurs des déchets" = "Environnement",
  "énergies solaires" = "Environnement",
  "panneaux solaires" = "Environnement",
  "reboisement et plantation" = "Environnement",
  "période" = "Environnement",
  "énergie solaire" = "Environnement",
  "arbres" = "Environnement",
  "problème de forestiers" = "Environnement",
  "électricité" = "Environnement",
  "nettoyage" = "Environnement",
  "poubelles" = "Environnement",
  "alternative économique" = "Économie et Emploi",
  "offres d'emplois" = "Économie et Emploi",
  "opportunité d'emploi" = "Économie et Emploi",
  "coopératives féminines" = "Social",
  "soutien de la femme" = "Économie et Emploi",
  "promouvoir" = "Économie et Emploi",
  "usines" = "Économie et Emploi",
  "usine" = "Économie et Emploi",
  "titre" = "Économie et Emploi",
  "travail" = "Économie et Emploi",
  "aide financière" = "Économie et Emploi",
  "financier" = "Économie et Emploi",
  "commercialisation de produit" = "Économie et Emploi",
  "logement" = "Économie et Emploi",
  "terres agricoles" = "Économie et Emploi",
  "agriculteurs" = "Économie et Emploi",
  "vente" = "Économie et Emploi",
  "vendre" = "Économie et Emploi",
  "intérêt" = "Économie et Emploi",
  "mine" = "Économie et Emploi",
  "interêt" = "Économie et Emploi",
  "création" = "Économie et Emploi",
  "restauration" = "Économie et Emploi",
  "revenu" = "Économie et Emploi",
  "supermarché" = "Économie et Emploi",
  "supermarchés" = "Économie et Emploi",
  "mineurs" = "Économie et Emploi",
  "tourisme" = "Culture et Loisirs",
  "parc muséologique" = "Culture et Loisirs",
  "musée" = "Culture et Loisirs",
  "artisans" = "Culture et Loisirs",
  "plantes" = "Culture et Loisirs",
  "romarin" = "Culture et Loisirs",
  "publicité" = "Culture et Loisirs",
  "loisirs" = "Culture et Loisirs",
  "jardin à Mibladen" = "Culture et Loisirs",
  "musique" = "Culture et Loisirs",
  "naissance" = "Culture et Loisirs",
  "langues" = "Culture et Loisirs",
  "centre musical" = "Culture et Loisirs",
  "complexes touristiques" = "Culture et Loisirs", 
  "titre" = "Juridique",
  "permission minière" = "Juridique",
  "propriété des maisons" = "Juridique",
  "légal" = "Juridique",
  "protection juridique" = "Juridique",
  "autorisation" = "Juridique",
  "droit" = "Juridique",
  "loi" = "Juridique",
  "justice" = "Juridique",
  "gendarmerie" = "Juridique",
  "cadre législatif" = "Juridique",
  "sécurité" = "Juridique",
  "police" = "Juridique",
  "femmes" = "Social",
  "gens" = "Social",
  "famille" = "Social",
  "social" = "Social",
  "citoyen" = "Social",
  "femme" = "Social",
  "veufs" = "Social",
  "internet" = "Social",
  "personnels" = "Social",
  "personnes" = "Social",
  "communauté" = "Social",
  "jeunes" = "Social",
  "souq hebdomadaire" = "Social",
  "suq" = "Social",
  "commune" = "Social",
  "locales" = "Social",
  "collectives" = "Social",
  "association" = "Social",
  "veuves" = "Social",
  "société" = "Social"
)

df <- rbind(df_hommes, df_femmes) %>% 
  mutate(category = category[match(word, names(category))])

df$category <- factor(df$category)

ggplot(df, aes(label = word, size = freq, color = category)) +
  geom_text_wordcloud() +
  scale_color_manual(values = rainbow(length(unique(df$category)))) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la question des améliorations \nsocio-sanitaires de la communauté, selon le sexe\n") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 20) +
  facet_wrap(~sex, nrow = 2) +
  theme(strip.text = element_text(size = 25)) +
  guides(color = guide_legend(title = "Catégorie"))

ggsave("graphs/cloudword/OpenameliosaniXsex.png",
       width = 15, height = 13)

write_xlsx(df,"graphs/cloudword/Excel_amelioXSex.xlsx")


### Open/amelio/lieu --------------------------------------------------------
text <- data$openameliorationeco
text <- gsub("[[:punct:]]", " ", text)
text <- tolower(text)
table(data$ses_lieu)
# Ajoutez des étiquettes pour les hommes et les femmes
text_ahouli <- data$openameliorationeco[data$ses_lieu == "Ahouli"]
text_mibladen <- data$openameliorationeco[data$ses_lieu == "Mibladen"]
# Combinez les textes avec des étiquettes
text_ahouli <- gsub("[[:punct:]]", " ", text_ahouli)
docs_ahouli <- Corpus(VectorSource(text_ahouli))
docs_ahouli <- docs_ahouli %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problèmes","ahouli","mibladen","alternative","hebdomadaire"))

text_mibladen <- gsub("[[:punct:]]", " ", text_mibladen)
docs_mibladen <- Corpus(VectorSource(text_mibladen))
docs_mibladen <- docs_mibladen %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problèmes',"ahouli","mibladen","alternative","hebdomadaire"))

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

df <- rbind(df_ahouli, df_mibladen) %>% 
  mutate(category = category[match(word, names(category))])

category <- c(
  "médecin" = "Santé",
  "hôpital" = "Santé",
  "dispensaire" = "Santé",
  "suivie" = "Santé",
  "médicaux" = "Santé",
  "traitement" = "Santé",
  "plomb" = "Santé",
  "ambulance" = "Santé",
  "médicaments" = "Santé",
  "salaires" = "Santé",
  "inondations" = "Santé",
  "irrigations" = "Santé",
  "contrôle" = "Santé",
  "hamame" = "Santé",
  "médicinale" = "Santé",
  "médecins" = "Santé",
  "médicinal" = "Santé",
  "réhabilitation" = "Santé",
  "réhabilitations" = "Santé",
  "silicose" = "Santé",
  "médicinales" = "Santé",
  "assainissement" = "Santé",
  "équipements" = "Santé",
  "equipements" = "Santé",
  "généraliste" = "Santé",
  "médoc" = "Santé",
  "école" = "Éducation",
  "écoles" = "Éducation",
  "préscolaires" = "Éducation",
  "élèves" = "Éducation",
  "enfants" = "Éducation",
  "alphabétisation" = "Éducation",
  "analphabétisme" = "Éducation",
  "analphabétismes" = "Éducation",
  "classe" = "Éducation",
  "scolaire" = "Éducation",
  "projets" = "Éducation",
  "bourses" = "Éducation",
  "jeunesse" = "Éducation",
  "apprentissage" = "Éducation",
  "collège" = "Éducation",
  "enfant" = "Éducation",
  "préscolaire" = "Éducation",
  "secondaire" = "Éducation",
  "route" = "Transport",
  "réseau" = "Transport",
  "communication" = "Transport",
  "transport" = "Transport",
  "parc" = "Transport",
  "bus" = "Transport",
  "titre" = "Transport",
  "canaux" = "Transport",
  "site" = "Transport",
  "voiture" = "Transport",
  "pont" = "Transport",
  "voie" = "Transport",
  "rues" = "Transport",
  "taxi" = "Transport",
  "ruelle" = "Transport",
  "terres agricoles" = "Environnement",
  "aménagement" = "Environnement",
  "eau potable" = "Environnement",
  "minière" = "Environnement",
  "dépotoir" = "Environnement",
  "déchets" = "Environnement",
  "potable" = "Environnement",
  "conteneurs" = "Environnement",
  "énergies solaires" = "Environnement",
  "reboisement" = "Environnement",
  "puits" = "Environnement",
  "période" = "Environnement",
  "énergie solaire" = "Environnement",
  "arbres" = "Environnement",
  "forestiers" = "Environnement",
  "ruelles" = "Environnement",
  "électricité" = "Environnement",
  "nettoyage" = "Environnement",
  "poubelles" = "Environnement",
  "économique" = "Économie et Emploi",
  "emploi" = "Économie et Emploi",
  "bourse" = "Économie et Emploi",
  "offres" = "Économie et Emploi",
  "coopératives" = "Économie et Emploi",
  "soutien" = "Économie et Emploi",
  "promouvoir" = "Économie et Emploi",
  "redémarrage" = "Économie et Emploi",
  "usines" = "Économie et Emploi",
  "usine" = "Économie et Emploi",
  "titre" = "Économie et Emploi",
  "alternative" = "Économie et Emploi",
  "travail" = "Économie et Emploi",
  "financière" = "Économie et Emploi",
  "financier" = "Économie et Emploi",
  "produits" = "Économie et Emploi",
  "logement" = "Économie et Emploi",
  "agricoles" = "Économie et Emploi",
  "agriculteurs" = "Économie et Emploi",
  "vente" = "Économie et Emploi",
  "vendre" = "Économie et Emploi",
  "intérêt" = "Économie et Emploi",
  "mine" = "Économie et Emploi",
  "interêt" = "Économie et Emploi",
  "création" = "Économie et Emploi",
  "restauration" = "Économie et Emploi",
  "commercialisation" = "Économie et Emploi",
  "revenu" = "Économie et Emploi",
  "minéraux" = "Économie et Emploi",
  "supermarché" = "Économie et Emploi",
  "supermarchés" = "Économie et Emploi",
  "opportunité" = "Économie et Emploi",
  "mineurs" = "Économie et Emploi",
  "tourisme" = "Culture et Loisirs",
  "muséologique" = "Culture et Loisirs",
  "musée" = "Culture et Loisirs",
  "artisans" = "Culture et Loisirs",
  "plantes" = "Culture et Loisirs",
  "romarin" = "Culture et Loisirs",
  "publicité" = "Culture et Loisirs",
  "loisirs" = "Culture et Loisirs",
  "jardin" = "Culture et Loisirs",
  "musique" = "Culture et Loisirs",
  "naissance" = "Culture et Loisirs",
  "langues" = "Culture et Loisirs",
  "musical" = "Culture et Loisirs",
  "terres" = "Culture et Loisirs",
  "touristique" = "Culture et Loisirs", 
  "titre" = "Juridique",
  "permission" = "Juridique",
  "propriété" = "Juridique",
  "maisons" = "Juridique",
  "légal" = "Juridique",
  "juridique" = "Juridique",
  "autorisation" = "Juridique",
  "contrôle" = "Juridique",
  "droit" = "Juridique",
  "loi" = "Juridique",
  "justice" = "Juridique",
  "terrains" = "Juridique",
  "gendarmerie" = "Juridique",
  "législatif" = "Juridique",
  "protection" = "Juridique",
  "sécurité" = "Juridique",
  "police" = "Juridique",
  "centre" = "Juridique",
  "femmes" = "Social",
  "gens" = "Social",
  "famille" = "Social",
  "social" = "Social",
  "citoyen" = "Social",
  "femme" = "Social",
  "veufs" = "Social",
  "internet" = "Social",
  "personnels" = "Social",
  "personnes" = "Social",
  "communauté" = "Social",
  "jeunes" = "Social",
  "souq" = "Social",
  "suq" = "Social",
  "pavage" = "Social",
  "commune" = "Social",
  "locales" = "Social",
  "collectives" = "Social",
  "association" = "Social",
  "féminines" = "Social",
  "veuves" = "Social",
  "société" = "Social"
)

ggplot(df, aes(label = word, size = freq, color = category)) +
  geom_text_wordcloud() +
  scale_color_manual(values = rainbow(length(unique(df$category)))) +
  clessnverse::theme_clean_light(base_size = 15) +
  labs(title = "Mots les plus fréquents dans les réponses \nsur la question des améliorations socio-sanitaires\nde la communauté, selon le lieu de résidence\n") +
  theme(plot.title = element_text(hjust = 0.5, size = 35)) +
  scale_size_area(max_size = 20) +
  facet_wrap(~lieu, nrow = 2) +
  theme(strip.text = element_text(size = 25))

ggsave("graphs/cloudword/OpenameliosaniXlieu.png",
       width = 15, height = 13)

write_xlsx(df,"graphs/cloudword/Excel_amelioXlieu.xlsx")
