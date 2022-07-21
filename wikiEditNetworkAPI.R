library(httr)
library(WikipediaR)
startPage <- "Nupur_Sharma_(politician)" #getting start page from user
#edits of the start page - latest 500 edits
contribs <- contribs(page = startPage, domain = "en")
#UserContribs <- userContribs(user.name = "TrangaBellam", domain = "en", ucprop = "ids|title|timestamp|comment|sizediff|flags")
levelOneContribs <- as.character(unique(contribs$contribs$user)) #list of editors
#preparing to get contributions to Wikipedia by all these editors - latest 500 edits
petUserContribsUrl <- "https://en.wikipedia.org/w/api.php?action=query&format=json&list=usercontribs&ucuser=BBLLAANNKK&uclimit=500&ucshow=!minor"
contribsContentDFAll <- NULL
for(RAW in 1:length(levelOneContribs)) #getting contribution of each editor
{
  subUserContribsUrl <- gsub("BBLLAANNKK",levelOneContribs[RAW],petUserContribsUrl)
  contribs1 <- GET(subUserContribsUrl)
print(paste(levelOneContribs[RAW],"out of",length(levelOneContribs)))
contribsContent1 <- content(contribs1, as = 'parsed')
contribsContentDF1 <- NULL #some have 10 columns
contribsContentDF2 <- NULL #some have 11 columns
for (ROW in 1:length(contribsContent1$query$usercontribs))
{
  contribROW <- unlist(contribsContent1$query$usercontribs[ROW])
  if (length(contribROW)==10)
  {contribsContentDF1 <- rbind(contribsContentDF1,contribROW)}
  else {contribsContentDF2 <- rbind(contribsContentDF2,contribROW)}
}
contribsContentDF <- rbind(contribsContentDF1,contribsContentDF2[,c(1,2,3,4,5,6,7,8,10,11)])
#contribsContentDF <- data.frame(contribsContentDF, stringsAsFactors = FALSE)
unwantedEdits <- c("Talk:", "User talk:", "User:", "Wikipedia talk:", "Wikipedia:")
for (REM in 1:5)
{
  if (length(contribsContentDF)==10) #check to make sure the DF exists
  {contribsContentDF <- contribsContentDF[-grep(unwantedEdits[REM],contribsContentDF[,7]),]}
}
contribsContentDFAll <- rbind(contribsContentDFAll,contribsContentDF)
}
allDates <- as.Date(contribsContentDFAll[,8])
years <- strftime(allDates, "%Y")
print(paste("Date range: from ",(sort(unique(allDates[order(format(as.Date(allDates),"%m%d"))]))[1])," to ",
sort(unique(allDates[order(format(as.Date(allDates),"%m%d"))]), decreasing = TRUE)[1]))
#END OF PART ONE
#PART TWO : subseting data according to date
#specifying date range
startDate <- "2020-06-14"
endDate <- "2022-06-11"
contribsContentDFChosen <- contribsContentDFAll[which(as.Date(contribsContentDFAll[,8])>=startDate & as.Date(contribsContentDFAll[,8])<=endDate),]
#preparing nodes for Gephi
userids <-  as.numeric(contribsContentDFChosen[,1])
usernames <- contribsContentDFChosen[,2]
pageids <- as.numeric(contribsContentDFChosen[,3])
pagetitles <- contribsContentDFChosen[,7]
usersDF <- data.frame(userids,usernames, stringsAsFactors = FALSE)
usersDF <- usersDF[!duplicated(usersDF),]
usersDF[3] <- "editor"
names(usersDF) <- c("id","label","category")
pageDF <- data.frame(pageids, pagetitles, stringsAsFactors = FALSE)
pageDF <- pageDF[!duplicated(pageDF),]
pageDF[3] <- "article"
names(pageDF) <- c("id","label","category")
pageDF <- pageDF[-grep(":",pageDF$label),]
nodesDF <- rbind(usersDF,pageDF)
#to prepare edges calculate the adjacency
#two loops - first editor/source; second page/target
edgesDF <- NULL
for(USER in 1:nrow(usersDF))
{
  for(PAGE in 1:nrow(pageDF))
  {
    count <- length(which(contribsContentDFChosen[,2]==usersDF$label[USER] & contribsContentDFChosen[,7]==pageDF$label[PAGE]))
    if(count > 0){print(paste(usersDF$id[USER],pageDF$id[PAGE],count,usersDF$label[USER],pageDF$label[PAGE],sep = " "))
      edgesDF <- rbind(edgesDF,
                       data.frame(as.numeric(usersDF$id[USER]),as.numeric(pageDF$id[PAGE]),as.numeric(count),
                                  usersDF$label[USER],pageDF$label[PAGE]))}
  }
}
names(edgesDF) <- c("source", "target", "weight", "sourceLabel", "targetLabel")
write.csv(nodesDF,file = paste(startPage,"Nodes",startDate,endDate,".csv", sep = "-"),row.names = FALSE)
write.csv(edgesDF,file = paste(startPage,"Edges",startDate,endDate,".csv", sep = "-"),row.names = FALSE)
