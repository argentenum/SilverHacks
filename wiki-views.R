setwd("~/ownCloud/iaclals-scraping")
library(pageviews)
# http://www.r-datacollection.com/blog/Using-wikipediatrend/
# the first package is obsolete so using second package
# https://cran.r-project.org/web/packages/pageviews/vignettes/Accessing_Wikimedia_pageviews.html
page_views <- article_pageviews(project = "en.wikipedia", article = "Delhi", platform = "all", user_type = "all", start = "2015100100", end = as.Date("2018-11-02"), reformat = TRUE)
page_views
library(ggplot2)

ggplot(page_views, aes(x=date, y=views)) + 
  geom_line(size=1.5, colour="steelblue") + 
  geom_smooth(method="loess", colour="#00000000", fill="#001090", alpha=0.1) +
  scale_y_continuous( breaks=seq(5e6, 50e6, 5e6) , 
                      label= paste(seq(5,50,5),"M") ) +
  theme_bw()
