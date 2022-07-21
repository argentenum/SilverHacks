library(WikipediaR)
medi <- userContribs(domain = "en", user.name = "TrangaBellam",
             ucprop = "ids|title|timestamp|comment|sizediff|flags")
library(RCurl)
library(pageviews)
library(wikipediatrend)
library(WikipediR)

#pageviews package
article_pageviews(
  project = "en.wikipedia",
  article = "R (programming language)",
  platform = "all",
  user_type = "all",
  start = "2015100100",
  end = NULL,
  reformat = TRUE,
  granularity = "daily")
obama_pageviews <- article_pageviews(article = "Barack_Obama")
old_pageviews(
  project = "en.wikipedia",
  article = "Gyanvapi_Mosque",
  platform = "all",
  granularity = "hourly",
  start = "2015100100",
  end = "2015100200",
  reformat = TRUE)


topTest <- top_articles(
  project = "bn.wikipedia",
  platform = "all",
  start = as.Date("2022-01-01"),
  end = as.Date("2022-01-02"),
  granularity = "daily",
  reformat = TRUE)


#WikipediaR package

UserContribs <- userContribs(user.name = "TrangaBellam", domain = "en", ucprop = "ids|title|timestamp|comment|sizediff|flags",
                         start = as.Date("2022-01-01"),
                         end = as.Date("2022-01-02"))
backLinks <- backLinks(page = "Barack_Obama", domain = "en")
contribs <- contribs(page = "Gyanvapi_Mosque", domain = "en")
linksinPage <- links("Gyanvapi_Mosque")
bobInfo <- userInfo(user.name = "TrangaBellam", domain = "en")

UserPageContribs <- userContribs(user.name = "TrangaBellam", 
                                 domain = "en", 
                                 ucprop = "ids|title|timestamp|comment|sizediff|flags")
#WikipediR
pageContent <- page_content("en","wikipedia", page_name = "Aaron Halfaker")
revisionContent <- revision_content("en","wikipedia", revisions = 1083646797)
revisionDiff <- revision_diff("en","wikipedia", revisions = 552373187, direction = "next")
recentChanges <- recent_changes("en","wikipedia",properties = c("user"))


#httr
sample1 <- GET("https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA")
apiUrl <- "https://en.wikipedia.org/w/api.php"
apiRequest <- httr::GET(apiUrl)
apiContent <- httr::content(apiRequest, as = 'text')
#https://httr.r-lib.org/articles/quickstart.html
r <- GET("http://httpbin.org/get")
content(r, "text", encoding = "ISO-8859-1")
str(content(r, "parsed"))
headers(r)
headers(r)$date
#https://bookdown.org/paul/apis_for_social_scientists/mediawiki-action-api.html

#https://rpubs.com/Sergio_Garcia/working_with_web_data_in_r
trying <- httr::GET("https://en.wikipedia.org/w/api.php", query = list(page = "Barak Obama", domain = "en", prop = "info"))
tryingContent <- content(trying, "parsed")
#follow this .... this can lead somewhere

library(httr)
trialQuery <- GET("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=revisions&titles=Shivaji&rvlimit=500&rvprop=timestamp%7Cuser%7Ccomment")
trialQuery <- GET("https://www.mediawiki.org/w/api.php",
                  query = list(format = "json",
                               titles = "Shivaji",
                               prop = "info"))
trialQuery <- GET("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=info&titles=Shivaji")
trialContent <- content(trialQuery, as = 'parsed')
trialContent$query$pages
