#Trying to organise the dataset
#using existing TOP
EditorinFocusSet <-  which(RevHistDF$Editor == EdCountFreq$Editor[TOP])
EditorinFocusSet <- EditorinFocusSet +2 #adding 2 for the first 2 columns of WordHistDF
#EditorinFocusPrevious <- EditorinFocusSet -1

#EditorinFocusSet <- c(1,EditorinFocusSet) #adding the 1st column containing words
#EditorinFocusDF <- df1[,EditorinFocusSet]
#extracting the set won't work as we need to check each set with the previous one
#placing it in a loop
EdChangeCount <- NULL
for (OUTER in 1:length(dftop$word)) {
  Magic <- 0
    for (INNER in 1:length(EditorinFocusSet)) {
    if (EditorinFocusSet[INNER] == 1) {Previous <- 0} else {Previous <- dftop[OUTER,(EditorinFocusSet[INNER]-1)]}
    LastCount <- dftop[OUTER,EditorinFocusSet[INNER]] - Previous
    Magic <- Magic + LastCount
  }
  EdChangeCount[OUTER] <- Magic
}
MagicDF <- data.frame(dftop$word,EdChangeCount, stringsAsFactors = FALSE)
names(MagicDF) <- c("word","n")
AddedDF <- MagicDF[which(sign(MagicDF$n)==1),]
RemovedDF <- MagicDF[which(sign(MagicDF$n)==-1),]
#plotting both wordclouds along with Editor behaviour
library(ggwordcloud)
library(RColorBrewer)
setwd("~/Documents/wiki-fresh/")
pdf("Editors.pdf")
ZeroDF <- data.frame(word = " ", n = 1, stringsAsFactors = FALSE)
if (length(AddedDF$word)>0) {WordDF1 <- AddedDF} else {WordDF1 <- ZeroDF}
EditWord1 <-ggplot(WordDF1, aes(label = word, size = n,color = n)) +
  geom_text_wordcloud(shape = "square") +
  scale_size() +
  ggtitle("Added words (from Top100 words)")+
  theme_minimal()
if (length(RemovedDF$word)>0) {WordDF2 <- RemovedDF} else {WordDF2 <- ZeroDF}
EditWord2 <-ggplot(WordDF2, aes(label = word, size = abs(n),color = n)) +
  geom_text_wordcloud(shape = "square") +
  scale_size() +
  ggtitle("Deleted words (from Top100 words)")+
  theme_minimal()
library(gridExtra)
grid.arrange(EditWord1, EditWord2)
dev.off()
#Decision on whether to place all graphs on a single page will be taken later