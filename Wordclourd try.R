library(worldcloud)
library(tidyverse)
library(wordcloud2)
library(RColorBrewer)
library(tm)



text <- c(data$)

docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 
  
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("french",))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud2(df)


table(data$openameliorationeco)

text <- c(data$openprobsantecourant)

text <- gsub("[[:punct:]]", " ", text)

docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) 
  
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("french"), 'veuillez',"autre","grand","spécifier","autres"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

wordcloud2(df)
