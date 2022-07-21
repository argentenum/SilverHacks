#create a network graph of editors starting from a given wikipage
startPage <- readline(prompt = "Input start page title: ")
startWikiEdit <- "https://xtools.wmflabs.org/authorship/en.wikipedia.org/BBLLAANNKK/0?format=wikitext"
startWikiUrl <- sub("BBLLAANNKK",startPage,startWikiEdit)
levelOneEditors <- readLines(startWikiUrl)
levelOneEditors <- levelOneEditors[12:(length(levelOneEditors)-1)]
editorName <- NULL
editCharCount <- NULL
editPercentContri <- NULL
userID <- NULL
for (LINE in 1:(length(levelOneEditors)/6))
{
  editorName[LINE] <- levelOneEditors[((LINE-1)*6)+2]
  if(grepl("/",editorName[LINE])) 
  {editorName[LINE] <- substr(editorName[LINE],27,(26+((nchar(editorName[LINE])-29)/2))) 
  userID[LINE] <- LINE
  } 
  else 
  {editorName[LINE] <- substr(editorName[LINE],10,(9+((nchar(editorName[LINE])-12)/2)))  
  }
  editCharRough <- levelOneEditors[((LINE-1)*6)+4]
  editCharCount[LINE] <-  as.numeric(gsub(",","",substr(editCharRough,3,nchar(editCharRough))))
  editPercentRough <- levelOneEditors[((LINE-1)*6)+5]
  editPercentContri[LINE] <- as.numeric(substr(editPercentRough,3,(nchar(editPercentRough)-1)))
  userID[LINE] <- LINE
}
levelOneDF <- data.frame(editorName,editCharCount,editPercentContri,userID)
levelOneDF[5] <- "editor"
#extracted usernames and contribution details of all editors at level one
#level two data on pages edited by first level editors is available as json
library(jsonlite)
library("tidyr")
library("dplyr")
library(httr)
#creating loop for extracting data of each editor and adding them to a dataframe
#columnNames <- c("page_namespace","page_title","page_is_redirect","count","pa_class","page_title_ns""editorName")
levelTwoDF <- NULL
userEditURL <- "https://xtools.wmflabs.org/api/user/top_edits/en.wikipedia.org/BBLLAANNKK/0"
userTXTURL <- "https://xtools.wmflabs.org/topedits/en.wikipedia.org/BBLLAANNKK/0?format=wikitext"
for (EDIT in 1:length(editorName))
{
  userSubURL <- gsub("BBLLAANNKK",editorName[EDIT],userEditURL)
  #getting data using httr::GET
  userEditJson <- GET(userSubURL)
  userEditContent <- content(userEditJson, as = 'parsed')
  #check if the user has not records trhough the text file
  if (names(userEditContent[1]) != "error") {
    userEditDF <- data.frame(t(sapply(userEditContent$top_edits$`0`,c)))
    userEditDF[7] <- editorName[EDIT]
    #writiing the ditor's details to the DF
    levelTwoDF <- rbind(userEditDF,levelTwoDF)
  }
  print(EDIT)
}  
colnames(levelTwoDF)[7] <- "editorName"
#mapping the network
#D3 network requires table without count, have to prepare DF without count and each link repeated "count" times
#do that looping the loop
#the nodes-edges network doesn't load if it is too heavy, so setting threshold of edit counts
THRESHOLD <- 10
editDF <- NULL
for (REM in 1:nrow(levelTwoDF))
{
  if (as.numeric(levelTwoDF$count[REM])>THRESHOLD) {
  for (ROM in 1:as.numeric(levelTwoDF$count[REM]) )
  {
    rowDF <- data.frame(as.character(levelTwoDF$page_title[REM]),as.character(levelTwoDF$editorName[REM]),
                        stringsAsFactors = FALSE)
    editDF <- rbind(editDF, rowDF)
  }
  print(paste(REM,"out of",nrow(levelTwoDF),levelTwoDF$page_title[REM],levelTwoDF$editorName[REM], sep = " "))
}
}
names(editDF) <- c("pageTitle","editor")
library(networkD3)
p <- simpleNetwork(editDF, height="100px", width="100px")
#preparing nodes and edges for gephi
#preparing nodes - id, label, category
pageNodes <- unlist(unique(levelTwoDF$page_title))
#to ensure ID not repeated add editor number to the first page number to shift the numbering of pages
pageIDs <- c((length(editorName)+1):(length(pageNodes)+length(editorName)))
nodeDF <- data.frame(pageIDs,pageNodes, stringsAsFactors = FALSE)
nodeDF[3] <- "page"
names(nodeDF) <- c("id","label","category")
editorDF <- levelOneDF[,c(4,1,5)]
names(editorDF) <- names(nodeDF)
nodeDF <- rbind(editorDF,nodeDF)
#preparing edges - source, target, weight
targetLabel <- unlist(levelTwoDF$page_title)
sourceLabel <- levelTwoDF$editorName
weight <- as.numeric(levelTwoDF$count)
sourceID <- NULL
targetID <- NULL
for (ZORO in 1:length(targetLabel))
{
  sourceID[ZORO] <- nodeDF$id[which(nodeDF$label==sourceLabel[ZORO])]
  targetID[ZORO] <- nodeDF$id[which(nodeDF$label==targetLabel[ZORO])]
}
edgesDF <- data.frame(sourceID, targetID, weight, sourceLabel, targetLabel)
names(edgesDF) <- c("source", "target", "weight", "sourceLabel", "targetLabel")
write.csv(nodeDF, file = "AmarKutirNodes.csv", row.names = FALSE)
write.csv(edgesDF, file = "AmarKutirEdges.csv", row.names = FALSE)
