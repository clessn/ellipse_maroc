    library(tm)
    library(ggplot2)
    library(wordcloud)
    library(RColorBrewer)
    library(RWeka)
    library(readxl)
    
    
    my_data <-read_xlsx("raw_data/Excel_bigram.xlsx")
    
    my_corpus <- Corpus(VectorSource(my_data$Données))
    ## Text cleaning ##
    
    my_corpus <- tm_map(my_corpus, content_transformer(tolower))
    
    # Remove numbers
    my_corpus <- tm_map(my_corpus, removeNumbers)
    
    # Remove punctuations
    my_corpus <- tm_map(my_corpus, removePunctuation)
    
    # Remove Stopwords
    
    my_corpus <- tm_map(my_corpus, removeWords, c("d’emploi Médecin","publique Bus","d’emploi La","féminines tapis"))
    
    # Remove white spaces 
    my_corpus <- tm_map(my_corpus, stripWhitespace)
    
    as.character(my_corpus[[1]])
    
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
    
    bigram_freq_df <- bigram_freq_df[bigram_freq_df$freq > 2, ]
    
    bigram_freq_df <- bigram_freq_df[!(bigram_freq_df$bigram %in% c('NA NA','féminines encourager','usines promouvoir','agricutlure réouverture','tourisme réouverture','tourisme restauration','féminines promouvoir','agriculture Coopératives','agricoles coopératives','agricoles ouverture','agricoles promouvoir', 'féminines plantation','mine terre','usines coopératives','agricoles projets','féminines ouverture','mine promouvoir','agricole promouvoir','tourisme coopératives','agriculture promouvoir','agriculture ouverture','agriculture coopératives' )),]
    
    
    # Generate word cloud
    wordcloud(words = bigram_freq_df$bigram, freq = bigram_freq_df$freq, scale = c(1, 0.35), 
              colors = brewer.pal(8, "Dark2"), random.order = FALSE, max.words = 40)
  
    ggplot(bigram_freq_df, aes(label = bigram, size = freq, color = freq)) +
      geom_text_wordcloud() +
      scale_color_gradient(low = "darkgrey", high = "darkgreen") +
      clessnverse::theme_clean_light(base_size = 15) +
      labs(title = "Mots les plus fréquents dans les réponses \nsur la question des alternatives économiques") +
      theme(plot.title = element_text(hjust = 0.5,vjust = 0.5, size = 30)) +
      scale_size_area(max_size = 13) +
      theme(strip.text = element_text(size = 25)) 
    
    ggsave("graphs/Bigram/alt_eco_all.png",
           width = 12, height = 8)
    