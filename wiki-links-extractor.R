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
  Pageurl <- "https://en.wikipedia.org/wiki/BBLLAANNKK"
  Pageurl <- sub("BBLLAANNKK",PageList$WikiPage[HIMA],Pageurl)
  PageHtml <- read_html(Pageurl)
  PageMatter <- html_nodes(PageHtml,'p') #reading paragraph sections of the page
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
  SourcePage <- NULL
  SourcePage[1:length(alllinks)] <- PageList$WikiPage[HIMA]
  Source <- c(Source,SourcePage)
  Target <- c(Target,alllinks)
}
LinkEdges <- data.frame(Source,Target, row.names = NULL)
names(LinkEdges) <- c("Source","Target")
write.csv(LinkEdges, file = "outboundDump.csv", append = FALSE)
#preparing data for NetworkD3 forceNetwork
library(networkD3)
simpleNetwork(LinkEdges, opacity = 1, zoom = T, nodeColour = "#1f547a")

                