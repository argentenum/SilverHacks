library(tidyverse)
library(tidytext)



bookName <- NULL
bookName[1:length(unlist(docs))] <- "Spatial Imaginings"
spacetext <- tibble(text=unlist(docs),bookName)
space_bigrams <- spacetext %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigramList <- space_bigrams %>%
  count(bigram, sort = TRUE)

#

