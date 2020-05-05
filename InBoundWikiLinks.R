#extracts links from current version of a page
setwd("~/Documents/wiki-links-network/")
PageList <- read.csv("MasterList.csv", stringsAsFactors = FALSE)
library(rvest)
library(stringr)
library(xml2)
Source <- NULL
Target <- NULL
#extracting links for each page in the MasterList
for (HIMA in 1:length(PageList$WikiPage))
{
  Pageurl <- "https://en.wikipedia.org/w/index.php?title=Special:WhatLinksHere/BBLLAANNKK&limit=5000&hidetrans=1&hideredirs=1"
  Pageurl <- sub("BBLLAANNKK",PageList$WikiPage[HIMA],Pageurl)
  PageHtml <- read_html(Pageurl)
  PageMatter <- html_nodes(PageHtml,'a') #reading links
  alllinks <- NULL
  for (PARA in 1:length(PageMatter))
  {
    matched <- str_match_all(PageMatter[PARA], "<a href=\"(.*?)\"")
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
  SourcePage <- NULL
  SourcePage[1:length(alllinks)] <- PageList$WikiPage[HIMA]
  Source <- c(Source,SourcePage)
  Target <- c(Target,alllinks)
}
LinkEdges <- data.frame(Target,Source, row.names = NULL) #reversed as this is inbound
names(LinkEdges) <- c("Source","Target")
write.csv(LinkEdges, file = "inboundDump.csv", append = FALSE)
#preparing data for NetworkD3 forceNetwork
library(networkD3)
simpleNetwork(LinkEdges, opacity = 1, zoom = T, nodeColour = "#1f547a")

