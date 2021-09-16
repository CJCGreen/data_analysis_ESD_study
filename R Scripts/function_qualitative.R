# C Green 01/09/2021
# Function to create Word clouds, horizontal bar graphs and bigram/trigram lists from qualitative data
# Used in R script for thesis results Chapters 5 and 6
######################################################################################################

# install.packages(c("tm", "SnowballC", "RColorBrewer", "wordcloud", "RWeka"))
# Note: tm = text mining

# Load packages for Word clouds
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")

# Load packages for ngrams (bigrams and trigrams)
library(RWeka)

# Function to create word clouds, horizontal bar plots and/or bi/trigrams from contents of a text file
analyseFreqWords <- function(textfile, myStopWords, minWordFreq, stem="False", rewriteStems=NULL, 
                             wordcloud="True", barplot="True", ngrams="True") {
  # Arguments:
  # textfile: source file for the words to visualise
  # myStopWords: extra words to exclude, as well as the standard English stop words
  # minWordFreq: minimum word frequency to include in the word cloud
  # stem: optional argument, default is False. Whether to conduct word stemming
  # wordcloud: optional argument, default is True. Whether to create a word cloud
  # barplot: optional argument, default is True. Whether to create a horizontal bar plot
  # ngrams: optional argument, default is True. Whether to create a frequency table of bigram/trigrams
  
  # Load text
  text <- readLines(textfile)
  docs <- Corpus(VectorSource(text))
  
  # Transform
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Clean
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english")) # Exclude common English words
  docs <- tm_map(docs, removeWords, myStopWords) 
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  # Optional word stemming:
  # If there are a lot of similar words such as death/s, birth/s,
  # similar words should be grouped together to show true frequency.
  if(stem=="True") docs <- tm_map(docs, stemDocument)

  # Build a document matrix
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  # Optional: if word stemming was done:
  # After wordstemming, d contains word parts, e.g.:
  head(d)
  # Result:
  #        word freq
  # popul popul   14
  # veget veget   14
  # rate   rate   11
  # death death    9
  # birth birth    8
  # equal equal    5
  if(stem=="True"&& !is.null(rewriteStems)) {
    d <- d[d$freq>2,]
    # Replace the stemmed words with the root words in full
    d$word <- rewriteStems
  }

  # Optional: Generate word cloud
  if(wordcloud=="True") {
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = minWordFreq,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  }
  
  # Optional: Generate horizontal bar plot for words with frequency more than 2, 
  # in descending order of frequency
  if(barplot=="True") {
    d <- d[d$freq>2,]
    d <- d[order(d$freq),]
    par(mar = c(5, 5, 4, 2)) # Adjust margins
    barplot(d$freq, horiz=TRUE, names.arg = d$word, las=1, mar=c(5,8,4,2), 
            cex.names=0.8, col=brewer.pal(nrow(d), "Set2"))
  }
  # Optional: Generate frequency table of bi/trigrams
  if(ngrams=="True") {
    token_delim <- " \\t\\r\\n.!?,;\"()"
    bitritoken <- NGramTokenizer(docs,Weka_control(min=2,max=3, delimiters= token_delim))
    two_three_word <- data.frame(table(bitritoken))
    two_three_word <- two_three_word[two_three_word$Freq>1,]
    sort_two_three <- two_three_word[order(two_three_word$Freq, decreasing = TRUE),]
    sort_two_three
  }
}