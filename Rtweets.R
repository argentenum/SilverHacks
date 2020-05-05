setwd("~/Documents/twitter/twitter-scraping")
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

## store api keys (these are fake example values; replace with your own keys)
api_key <- ""
api_secret_key <- ""
access_token <- ""
access_token_secret <- ""

## authenticate via web browser
token <- create_token(
  app = "Rjuntwitter",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# post a tweet from R
#post_tweet("Look, i'm learning to use RTweets")
## your tweet has been posted!

## search for 500 tweets using the #rstats hashtag
rstats_tweets <- search_tweets(q = "(from:htTweets)",
                               n = 18000, retryonratelimit = TRUE, include_rts = FALSE)
write_as_csv(rstats_tweets,"RT.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

tweets <- rstats_tweets %>%
  select(user_id,status_id,created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url)

# view the first 3 rows of the dataframe
head(rstats_tweets, n = 3)

# what users are tweeting with #rstats
users <- search_users("#earthquake",
                      n = 18000)
# how many locations are represented
length(unique(users$location))
## [1] 299


users %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Twitter users - unique locations ")

users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")


users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

users %>% na.omit() %>%
  ggplot(aes(time_zone)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Time Zone",
       title = "Twitter users - unique time zones ")


tweet_data <- rstats_tweets
# flood start date sept 13 - 24 (end of incident)
start_date <- as.POSIXct('2013-09-13 00:00:00')
end_date <- as.POSIXct('2013-09-24 00:00:00')

# cleanup
flood_tweets <- tweet_data %>%
  mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  filter(created_at >= start_date & created_at <= end_date ) %>%
  mutate(text = gsub("http://*|https://*)", "", text))

data("stop_words")

# get a list of words
flood_tweet_clean <- flood_tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co"))


# plot the top 15 words -- notice any issues?
flood_tweet_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
