setwd("~/ownCloud/twitter-scraping")
#1) Do this whenever you need to start a session#
library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)

# 2) Get access to Twitter#
consumerKey = ""  
consumerSecret = ""
accessToken = ""
accessSecret = ""
options(httr_oauth_cache=TRUE)
setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

#3.1) Scrape a user's tweets 
obamatweets<- userTimeline("potus44", n = 3200)

obamatweets_df <- tbl_df(map_df(obamatweets, as.data.frame))

write.csv(obamatweets_df, "obamatweets.csv")

#3.2) Search for a hashtag
yeswecan <- searchTwitter("#yeswecan exclude:retweets", n=3200)

yeswecan_df <- tbl_df(map_df(yeswecan, as.data.frame))

write.csv(yeswecan_df, "yeswecan.csv")

#3.3) Search for all tweets directed to a user
tweetstoobama <- searchTwitter("@potus44 exclude:retweets", n=3200)

tweetstoobama_df <- tbl_df(map_df(tweetstoobama, as.data.frame))

write.csv(tweetstoobama_df, "tweetstoobama.csv")



voicekashmir <- searchTwitter("#aVoiceFromKashmir exclude:retweets", n=1034)

voicekashmir_df <- tbl_df(map_df(voicekashmir, as.data.frame))

write.csv(voicekashmir_df, "voicekashmir.csv")


vktr <- read.csv("voicekashmir.csv", header = TRUE)
voicekashmir <- searchTwitter("#aVoiceFromKashmir exclude:retweets", n=1034)
voicekashmir_df <- tbl_df(map_df(voicekashmir, as.data.frame))
bindktr <- rbind(vktr, voicekashmir_df)
write.csv(bindktr, "voicekashmir.csv", append = FALSE, row.names = FALSE)
