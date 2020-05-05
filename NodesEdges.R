#preparing nodes and edges for gephi
setwd("~/Documents/wiki-fresh/")
AllFiles <- list.files(path = ".")
RevHistFiles <- AllFiles[grep("RevHistData-",AllFiles)]
#creating Page Nodes
PageNodes <- sub("_"," ",sub(".csv","",sub("RevHistData-","",RevHistFiles)))
PageNodesDF <- data.frame(c(1:length(PageNodes)),PageNodes,rep("Page", each=length(PageNodes)),stringsAsFactors = FALSE)
names(PageNodesDF) <- c("id","label","category")
#Creating Editor Nodes
UEditors <- NULL
for (NOW in 1:length(RevHistFiles)){
  ReadRevHist <- read.csv(file = RevHistFiles[NOW], stringsAsFactors = FALSE)
  UEditors <- c(UEditors,unique(ReadRevHist$Editor))
}
UEditors <- unique(UEditors)
EditNodesDF <- data.frame(c(100001:(length(UEditors)+100000)),UEditors,
                            rep("Editor", each=length(UEditors)), stringsAsFactors = FALSE)
names(EditNodesDF) <- c("id","label","category")
#combining all nodes
NodesDF <- rbind(PageNodesDF,EditNodesDF)
#creating Edges
EdgesDF <- NULL
for (THEN in 1:length(RevHistFiles)){
  ReadRevHist <- read.csv(file = RevHistFiles[THEN], stringsAsFactors = FALSE)
  EditorCount <- table(ReadRevHist$Editor)
  ThisPage <- NULL
  ThisPage[1:length(names(EditorCount))] <- sub("_"," ",sub(".csv","",sub("RevHistData-","",RevHistFiles[THEN])))
  StartID <- NULL
  StartID[1:length(names(EditorCount))] <- NodesDF$id[which(NodesDF$label==ThisPage[1])]
  EndID <- NULL
  for (SMALL in 1:length(names(EditorCount))){
    EndID[SMALL] <- NodesDF$id[which(NodesDF$label==names(EditorCount)[SMALL])]
}
TheseEdgesDF <- data.frame(StartID,EndID,as.numeric(EditorCount),ThisPage,names(EditorCount), stringsAsFactors = FALSE)
  names(TheseEdgesDF) <- c("source","target","weight","sourceLabel","targetLabel")
EdgesDF <- rbind(EdgesDF,TheseEdgesDF)
}
write.csv(NodesDF , file = "WikiNodes.csv", append = FALSE, row.names = FALSE)
write.csv(EdgesDF , file = "WikiEdges.csv", append = FALSE, row.names = FALSE)
