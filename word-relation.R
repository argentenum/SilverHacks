setwd("~/Dropbox/Arjun/01-teaching/01-iitd/hss-pg-planning/coursewords/")
library(dplyr)
library(tidytext)
filenames <- c("hsl756", "hsl782", "hsl853", "hsl878", "hsv771", "hsl731", "hsl771",
              "hsl783", "hsl854", "hsl879", "hsv773", "hsl732", "hsl772", "hsl831", 
              "hsl856", "hsl881", "hsv774", "hsl733", "hsl773", "hsl832", "hsl857",
              "hsl882", "hsv781", "hsl734", "hsl774", "hsl833", "hsl871", "hsl883",
              "hsl751", "hsl775", "hsl834", "hsl872", "hsl884", "hsl752", "hsl776",
              "hsl835", "hsl873", "hsl885", "hsl753", "hsl777", "hsl836", "hsl874",
              "hsv731", "hsl754", "hsl778", "hsl851", "hsl875", "hsv734", "hsl755",
              "hsl779", "hsl852", "hsl877", "hsv735")
# extracting separate words from the text file
allcourses <- scan("allcourses.txt", character(), quote = "")
# creating tibble for all words; flat across courses
text_df <- tibble(line = 1, text = allcourses)
# unnesting the tibble
text_df <- text_df %>%
  unnest_tokens(word, text)
# removing stopwords
tidy_courses <- text_df %>%
  anti_join(stop_words)
# creating a lost of unwanted words to be removed from the corpus
unwanted_words <- c("student", "students")
unwanted_tibble <- tibble(word = unwanted_words, lexicon = "SMART")
# removing unwanted words
tidy_courses <- tidy_courses %>%
  anti_join(unwanted_tibble)
# counting frequencies of the words
tidy_count <- tidy_courses %>%
  count(word, sort = TRUE)
# plotting top frequencies
library(ggplot2)
tidy_count %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
# plotting wordcloud
library(wordcloud)
tidy_courses %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))
# trying the above steps with ngrams 2
text_ng <- tibble(line = 1, text = allcourses)
text_ng <- text_ng %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)
tidy_count <- text_ng %>%
  count(word, sort = TRUE)
tidy_count %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
# removing uniteresting terms
library(tidyr)
bigrams_separated <- text_ng %>%
  separate(word, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered <- bigrams_filtered %>%
  filter(!word1 %in% unwanted_tibble$word) %>%
  filter(!word2 %in% unwanted_tibble$word)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigram_counts <- bigrams_united %>% 
  count(bigram, sort = TRUE)
bigram_counts %>%
  filter(n > 2) %>%
  mutate(word = reorder(bigram, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
#network of bigrams
library(igraph)
library(ggraph)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
