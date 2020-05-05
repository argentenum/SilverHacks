#reading from "What links here" page
library(rvest)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Special:WhatLinksHere/Ghatotkacha&limit=500&hidetrans=1&hideredirs=1"
InBoundPage <- read_html(url)
AllBound <- html_nodes(InBoundPage,'a')
alllinks <- NULL
for (BUN in 1:length(AllBound))
{
  matched <- str_match_all(AllBound[BUN], "<a href=\"(.*?)\"")
  links <- matched[[1]][, 2]
  alllinks <- c(alllinks,links)
}
#finding links to citations and removing them
CiteLinks <- which(substring(alllinks,1,1)=="#")
if (length(CiteLinks)>0) {alllinks <- alllinks[-which(substring(alllinks,1,1)=="#")]}
alllinks <- gsub("/wiki/","",alllinks)
#finding links to Citation Needed and other editorial statements and removing them
EditLinks <- grep(":",alllinks)
if (length(EditLinks)>0) {alllinks <- alllinks[-grep(":",alllinks)]}
QuesLinks <- grep("/w/index.php",alllinks)
if (length(QuesLinks)>0) {alllinks <- alllinks[-grep("/w/index.php",alllinks)]}
ShopLinks <- grep("wikimedia",alllinks)
if (length(QuesLinks)>0) {alllinks <- alllinks[-grep("wikimedia",alllinks)]}
MainLinks <- grep("Main_Page",alllinks)
if (length(QuesLinks)>0) {alllinks <- alllinks[-grep("Main_Page",alllinks)]}

