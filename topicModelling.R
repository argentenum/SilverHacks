install.packages("tm")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("SnowballC")
install.packages("textclean")
install.packages("reshape2")

# Load the required libraries
library(tm)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(SnowballC)
library(textclean)
library(reshape2)

# Load the text file
# Replace "your_text_file.txt" with the path to your text file
text_data <- readLines("C:/Users/HP/Downloads/castro.txt")

# Ensure any trailing newlines are properly handled
text_data <- gsub("\r?\n$", "", text_data)

# Create a Corpus
#corpus <- Corpus(VectorSource(text_data))

# Create a VCorpus instead of SimpleCorpus
corpus <- VCorpus(VectorSource(text_data))

# Text Preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms to reduce noise
#dtm <- removeSparseTerms(dtm, 0.99)

# Check for empty rows in the DTM and remove them (if any)
row_totals <- apply(dtm, 1, sum)

# Remove rows with all zero entries (empty documents)
dtm <- dtm[row_totals > 0, ]


# Perform Latent Dirichlet Allocation (LDA) for Topic Modeling
# Set the number of topics (k) as needed
k <- 5  # You can change this to the desired number of topics
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# View the top terms in each topic
topics <- tidy(lda_model, matrix = "beta")

# Display the top 10 terms for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Print the top terms in each topic
print(top_terms)

# Visualize the top terms in each topic using ggplot2
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic",
       x = NULL, y = "Beta") +
  theme_minimal()

# Optionally, you can also view the distribution of topics in each document
# This will give you an idea of which topics dominate which documents
doc_topics <- tidy(lda_model, matrix = "gamma")

# Print the topic distribution for each document
print(doc_topics)

# Save the results to a CSV file if needed
write.csv(top_terms, "top_terms_per_topic.csv", row.names = FALSE)
