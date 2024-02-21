library(tm)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(RWeka)
library(readxl)
library(tokenizers)

my_data <-read_xlsx("raw_data/Excel_bigram.xlsx")

my_corpus <- Corpus(VectorSource(my_data$Données))

# Subset data for Ahouli
text_ahouli <- my_data$Données[my_data$Lieu == "Ahouli"]


# Remove punctuation, numbers, and transform to lowercase
docs_ahouli <- Corpus(VectorSource(text_ahouli))
docs_ahouli <- tm_map(docs_ahouli, content_transformer(tolower))
docs_ahouli <- tm_map(docs_ahouli, removeNumbers)
docs_ahouli <- tm_map(docs_ahouli, removePunctuation)

# Create a function to generate bigrams
createBigrams <- function(text){
  words <- unlist(strsplit(text, "\\s+"))
  bigrams <- paste(words[1:(length(words)-1)], words[2:length(words)], sep = " ")
  return(bigrams)
}
# Apply the function to each document
docs_ahouli <- lapply(docs_ahouli, function(x) createBigrams(as.character(x)))
docs_ahouli <- Corpus(VectorSource(docs_ahouli))

# Create term-document matrix
dtm_ahouli <- DocumentTermMatrix(docs_ahouli)

# Convert to matrix and calculate word frequencies
matrix_ahouli <- as.matrix(dtm_ahouli)
words_ahouli <- sort(rowSums(matrix_ahouli), decreasing = TRUE)

# Create data frame with word frequencies and location
df_ahouli <- data.frame(word = names(words_ahouli), freq = words_ahouli) %>% 
  mutate(Lieu = "Ahouli")


## Text cleaning ##

# Remove numbers
my_corpus <- tm_map(my_corpus, removeNumbers)

# Remove punctuations
my_corpus <- tm_map(my_corpus, removePunctuation)

# Remove Stopwords
my_corpus <- tm_map(my_corpus, removeWords, c("agricoles Ouverture","NA NA", "tourisme Coopératives","féminines tapis","agricoles Coopératives", "agricoles Projets", "Projets délevage", "	
usines Coopératives", "féminines Plantes", "agricoles Promouvoir", "mine Promouvoir", "agricoles Réouverture", "féminines Ouverture", "féminines Valorisations"))

# Remove white spaces 
my_corpus <- tm_map(my_corpus, stripWhitespace)

as.character(joint_coprus[[1]])

# Bigrams

minfreq_bigram <- 2 ### mets la fréquence minimum des mots à 2

##### Version chatGPT #####


# Create bigrams
createBigrams <- function(text){
  words <- unlist(strsplit(text, "\\s+"))
  bigrams <- paste(words[1:(length(words)-1)], words[2:length(words)], sep = " ")
  return(bigrams)
}

# Apply bigram creation function to corpus
my_bigrams <- lapply(my_corpus, createBigrams)

# Flatten the list of bigrams
all_bigrams <- unlist(my_bigrams)

# Count frequency of bigrams
bigram_freq <- table(all_bigrams)

# Convert to data frame
bigram_freq_df <- data.frame(bigram = names(bigram_freq), freq = as.numeric(bigram_freq))

# Sort by frequency
bigram_freq_df <- bigram_freq_df[order(bigram_freq_df$freq, decreasing = TRUE), ]


# Generate word cloud
wordcloud(words = bigram_freq_df$bigram, freq = bigram_freq_df$freq, scale = c(1, 0.35), 
          colors = brewer.pal(8, "Dark2"), random.order = FALSE, max.words = 40)

# CHAT  -------------------------------------------------------------------


# Create bigrams for Ahouli and Mibladen
bigram_freq_df_Ahouli <- createBigrams(my_corpus[my_data$Lieu == "Ahouli"], "Ahouli")
bigram_freq_df_Mibladen <- createBigrams(my_corpus[my_data$Lieu == "Mibladen"], "Mibladen")

# Combine bigram dataframes
bigram_freq_df_combined <- rbind(bigram_freq_df_Ahouli, bigram_freq_df_Mibladen)

# Generate word cloud
wordcloud(words = bigram_freq_df_combined$bigram, freq = bigram_freq_df_combined$freq, 
          scale = c(1, 0.35), colors = brewer.pal(8, "Dark2"), random.order = FALSE, max.words = 40)
