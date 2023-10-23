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
docs <- tm_map(docs, removeWords, c(stopwords("french"), 'veuillez',"autre","grand","spécifier","autres","santé", "problèmes"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

ggplot(df, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_color_gradient(low = "darkgrey", high = "darkblue") +
  clessnverse::theme_clean_light() +
  scale_size_area(max_size = 18) 

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
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres', "problèmes"))

# Prétraitement du texte pour les femmes
text_femmes <- gsub("[[:punct:]]", " ", text_femmes)
docs_femmes <- Corpus(VectorSource(text_femmes))
docs_femmes <- docs_femmes %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords("french"), 'veuillez', 'autre', 'grand', 'spécifier', 'autres','problèmes'))

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
text_combined <- c(text_ahouli, text_mibladen)

# Créez un corpus avec les textes combinés
docs <- Corpus(VectorSource(text_combined))

# Prétraitez le texte
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, c(stopwords("french"), "veuillez",'autre','grand',"spécifier","autres"))
docs <- tm_map(docs, stripWhitespace)

# Créez le nuage de mots
dtm <- DocumentTermMatrix(docs)
words <- as.data.frame(as.table(dtm))
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

colnames(words) <- c("Word", "Frequency")

# Générez le nuage de mots
wordcloud2(words, color = "random-light")


### cloudword/danger --------------------------------------------------------
text <- c(data$opendangerplomb)

text <- gsub("[[:punct:]]", " ", text)

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("french"),"oui", "savez", "dire", "pouvez", "risque"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud3 <- wordcloud2(df, shape = "pentagon")

ggsave("graphs/cloudword/opendanger.png",
       width = 15,height = 13)




### open/ameliosocio --------------------------------------------------------
text <- c(data$openameliorationeco)

text <- gsub("[[:punct:]]", " ", text)

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("french"), "Ahouli", "Mibladen"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud2(df, shape = "diamond")




