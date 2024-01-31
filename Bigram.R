library(tm)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(RWeka)
library(readxl)

my_data <-read_xlsx("raw_data/Données Amelio_sani.xlsx")

my_corpus <- Corpus(VectorSource(my_data$Données))
## Text cleaning ##

# Remove numbers
my_corpus <- tm_map(my_corpus, removeNumbers)

# Remove punctuations
my_corpus <- tm_map(my_corpus, removePunctuation)

# Remove white spaces 
my_corpus <- tm_map(my_corpus, stripWhitespace)

as.character(my_corpus[[1]])

# Bigrams

minfreq_bigram <- 2 

token_delim <- "\\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(my_corpus,Weka_control(min = 2, max = 2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing = TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order = FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words = 1000)

View(wordcloud)
