setwd("~/Documents/spatialProofs/")
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
texto <- read.delim("spatialProofs.txt", header = FALSE)
# Load the data as a corpus
docs <- Corpus(VectorSource(texto$V1))
#replace special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#cleaning the text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)


#https://www.tidytextmining.com/ngrams.html
library(tidyverse)
library(tidytext)

bookName <- NULL
bookName[1:length(unlist(docs))] <- "Spatial Imaginings"
spacetext <- tibble(text=unlist(docs),bookName)
#you can change the ngram tokens by changing the value of 'n'
space_ngrams <- spacetext %>% unnest_tokens(ngram, text, token = "ngrams", n = 2)

ngramList <- space_ngrams %>%
  count(ngram, sort = TRUE)
