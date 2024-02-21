library(tm)
library(wordcloud)
library(RColorBrewer)

# Load data
my_data <- read_xlsx("raw_data/Excel_bigram.xlsx")

# Check data loading
print(head(my_data))

# Check unique locations
print(unique(my_data$Genre))

# Clean and preprocess text data
my_corpus <- Corpus(VectorSource(my_data$Données[my_data$Genre == "Male"]))
my_corpus <- tm_map(my_corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeWords, c("d’emploi Médecin", "publique Bus", "d’emploi La", "féminines tapis", "NA NA"))
my_corpus <- tm_map(my_corpus, stripWhitespace)

# Create bigrams function
createBigrams <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  bigrams <- paste(words[1:(length(words) - 1)], words[2:length(words)], sep = " ")
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
bigram_alt_Male <- wordcloud(words = bigram_freq_df$bigram, freq = bigram_freq_df$freq, scale = c(1, 0.35), 
                               colors = brewer.pal(8, "Dark2"), random.order = FALSE, max.words = 50)


# Clean and preprocess text data
my_corpus2 <- Corpus(VectorSource(my_data$Données[my_data$Genre == "Female"]))
my_corpus2 <- tm_map(my_corpus2, content_transformer(tolower))
my_corpus2 <- tm_map(my_corpus2, removeNumbers)
my_corpus2 <- tm_map(my_corpus2, removePunctuation)
my_corpus2 <- tm_map(my_corpus2, removeWords, c("d’emploi Médecin", "publique Bus", "d’emploi La", "féminines tapis"))
my_corpus2 <- tm_map(my_corpus2, stripWhitespace)

# Create bigrams function
createBigrams2 <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  bigrams <- paste(words[1:(length(words) - 1)], words[2:length(words)], sep = " ")
  return(bigrams)
}

# Apply bigram creation function to corpus
my_bigrams2 <- lapply(my_corpus2, createBigrams2)

# Flatten the list of bigrams
all_bigrams2 <- unlist(my_bigrams2)

# Count frequency of bigrams
bigram_freq2 <- table(all_bigrams2)

# Convert to data frame
bigram_freq_df2 <- data.frame(bigram = names(bigram_freq2), freq = as.numeric(bigram_freq2))

# Sort by frequency
bigram_freq_df2 <- bigram_freq_df2[order(bigram_freq_df2$freq, decreasing = TRUE), ]

# Generate word cloud
bigram_alt_Female <- wordcloud(words = bigram_freq_df2$bigram, freq = bigram_freq_df2$freq, scale = c(1, 0.35), 
                                 colors = brewer.pal(8, "Dark2"), random.order = FALSE, max.words = 50)
