#accessing all versions of a wiki page
setwd("~/Documents/wiki-fresh/")
input.pagename <- readline(prompt="Enter url ending with undescore: ") #input pagename
#getting total number of edits
#unable to get total number of edits, TRY LATER, skipping step
#input total number of edits
PageEditStat <- readline((prompt="How many times has this page been edited?"))
#wikipedia allows a maximum Revision History display for 5000 revisions
if (PageEditStat > 5000) {PageEditStat <- 5000}
library('rvest')
library("WikipediaR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("wikipediatrend", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("WikipediR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library(purrr)
library(dplyr)
#defining Revision History Url to insert Edits Stats
RevHistVariableUrl <- "https://en.wikipedia.org/w/index.php?title=NNAAMMEE&offset=&limit=BBLLAANNKK&action=history"
RevHistVariableUrl <-sub("NNAAMMEE",input.pagename,RevHistVariableUrl)
RevHistUrl <- sub("BBLLAANNKK",PageEditStat,RevHistVariableUrl)
RevisionHistory <- read_html(RevHistUrl)
# RevisionHistory contain the html file containing Revision History of the page
#We need to create a data table with the Old id, username, size, change
library(data.table)
library(XML)
#Creating vector containing all date and time of revisions
OldidDateHtml <- html_nodes(RevisionHistory,'.mw-changeslist-date')
OldidDate <- html_text(OldidDateHtml)
#Creating vector containing all sizes of revisions
OldidSizeHtml <- html_nodes(RevisionHistory,'.history-size')
OldidSize <- html_text(OldidSizeHtml) #extracting the numerical size
OldidSize <- gsub(",","",OldidSize,fixed = TRUE)
OldidSize <- as.numeric(gsub(" bytes","",OldidSize,fixed = TRUE))
OldidSize[which(is.na(OldidSize))] <- 0 #replacing NAs with zero
#it is difficult to pick out the difference in size
#Computing difference - using loop to leave out the first revison
DiffinSize <- NULL
for (DIFF in 1:length(OldidSize))
{DiffinSize[DIFF] <- (OldidSize[DIFF]-OldidSize[DIFF+1])
if (is.na(DiffinSize[DIFF])) {DiffinSize[DIFF] <- 0}
}
#Creating vector containing all sizes of revisions
OldidCommentHtml <- html_nodes(RevisionHistory,'.comment--without-parentheses')
OldidComment <- html_text(OldidCommentHtml)
#This does not contain revisions without any comment
#Inserting revisions without comments
ReadAll <- readLines(RevHistUrl) #reading the Revision History page as lines
ReadHistLines <- grep("<li data-mw-revid=",ReadAll) #creating a total html vector for all revisions
#the max min allows us to markout revision html lines from the rest of the page
LineNoswithComment <- grep(".comment--without-parentheses",ReadHistLines)
LineNoswithComment <- LineNoswithComment - (min(LineNoswithComment)-1) #renumbering lines remving those preceeding revison info
CommentsAll <- NULL
PageEditNum <- as.numeric(PageEditStat)
CommentsAll[1:PageEditNum] <- ""
for (DUD in 1:length(LineNoswithComment))
{CommentsAll[LineNoswithComment[DUD]] <- OldidComment[DUD]
}

#Creating vector containing oldids of revisions
#working with the ReadLines result
LinesWithId <- grep("li data-mw-revid=", ReadAll ) #identifying lines with the oldids
CodeWithId <- ReadAll[LinesWithId] #subsetting the lines with oldids
#since the length of oldids is variable
#reading numerals in in the string section containing the old id
OldIds <- as.numeric(gsub("([0-9]+).*$", "\\1", substr(CodeWithId,20,32))) #creating the vector
#Creating vector containing all users/ editors of revisions
OldidUsersHtml <- html_nodes(RevisionHistory,'bdi')
OldidUsers <- html_text(OldidUsersHtml)
#problem - some lines have no user record!!
#Solving the problem - identifying Lines with usernames
LineNoswithUsers <- grep("<bdi>",ReadAll)
LineNoswithUsers <- LineNoswithUsers - (min(LineNoswithUsers)-1)
AllLineNos <- c(1:PageEditStat)
LinesMissUsers <- AllLineNos[!(AllLineNos %in% LineNoswithUsers)] #Line nos with IP removed
#sometimes a very long line is read by the ReadHtml as two lines posing skewed results
#need to check if it indeed is a linewithout a username by checking the opening expression
LinesMissUsers <- c(631,632,633,634)
for (YEP in LinesMissUsers) {
  if (is.na(pmatch("<li data-mw-revid=",ReadHistLines[YEP]))) {LinesMissUsers <- LinesMissUsers[!LinesMissUsers %in% YEP]}
}
if (length(LinesMissUsers)>0)
{
  for (TAT in 1:length(LinesMissUsers))
  {OldidUsers <- append(OldidUsers, "IP Not Available", after = (LinesMissUsers[TAT]-1))
  }
}  
# creating the master dataframe
RevHistDF <- data.frame(OldIds, OldidDate, OldidUsers, OldidSize, DiffinSize, CommentsAll, stringsAsFactors = FALSE) 
#since all edits don't have comments that cannot be appended to this
names(RevHistDF) <- c("OldId", "TimeStamp","Editor", "Bytes","Diff Bytes","Comments")
write.csv(RevHistDF, file = paste("RevHistData","-",input.pagename,".csv",sep = ""), append = FALSE, row.names = FALSE)
#the basic dataframe is ready
#modifications desireable - parse the timestamp
#PART 2 : WORD COUNTING
#create a loop for oldid vector
for (NOW in 1:length(OldIds))
{
  Blankurl <- "https://en.wikipedia.org/w/index.php?title=NNAAMMEE&oldid=BBLLAANNKK"
  PageUrl <- sub("BBLLAANNKK",OldIds[NOW], Blankurl)
  PageUrl <- sub("NNAAMMEE",input.pagename, PageUrl)
  PageHtml <- read_html(PageUrl)
  PageMatter <- html_nodes(PageHtml,'p') #reading paragraph sections of the page
  PageText <- html_text(PageMatter) #stripping html markup from paras
  PageText = tolower(PageText) #make it lower case
  PageText = gsub('[[:punct:]]', '', PageText) #remove punctuation
  PageText <- paste(PageText, collapse = '') #collapsing to a single character
  PageText = gsub("[[:digit:]]", " ", PageText) #replacing numbers with blank
  library(dplyr)
  library(tidytext)
  PageText <- strsplit(PageText, ' ')[[1]] #breaking text into single words
  PageText <- PageText[PageText != ""] #removing blank strings
  # creating tibble for all words; flat across courses
  text_df <- tibble(line = 1, text = PageText)
  # unnesting the tibble
  text_df <- text_df %>%
    unnest_tokens(word, text)
  # removing stopwords
  tidy_courses <- text_df %>%
    anti_join(stop_words)
  # if you want to remove more unwanted words refer to word-relation.R for hss pg courses
  unwanted_words <- c("was", "an")
  unwanted_tibble <- tibble(word = unwanted_words, lexicon = "SMART")
  # removing unwanted words
  tidy_courses <- tidy_courses %>%
    anti_join(unwanted_tibble)
  
  # counting frequencies of the words
  tidy_count <- tidy_courses %>%
    count(word, sort = TRUE)
  assign(paste0("OldidWordCount", NOW), tidy_count)
  print(Sys.time())
  print(length(OldIds)-NOW)
  print(OldidDate[NOW])
}
#This has recorded tibbles with word counts under a variable variable name
# Problem is if earlier run had higher no. of verisons the old values for counts remain in memory
#that should not bother us as it gets reassigned each time
#challenge is to read the variables
#for (NOW in 1:length(OldIds)) {
#  print(paste0("OldidWordCount", NOW))
#  print(get(paste0("OldidWordCount", NOW)))
#}
#works! BUt we don't need this bit now. We can apply it below.
#Part 3 - adding counts of all words across the corpus
#Loop through Oldids and aggregate two tibbles at a time
#the aggregate is stored as a data frame
#the aggregate dataframe updates itself till the end
setone <- OldidWordCount1
df1 <- data.frame(setone$word,setone$n)
names(df1) <- c("word","n")
for (NOW in 2:(length(OldIds))) {
  print(paste0("To add", (length(OldIds)-NOW)))
  settwo <- get(paste0("OldidWordCount", NOW))
  df2 <- data.frame(settwo$word,settwo$n)
  names(df2) <- c("word","n")
  
  melt(list(df1, df2), id.vars = "word")
  
  df1<- dcast(melt(mget(ls(pattern = "df\\d+")), id.vars = "word"), 
              word ~ variable, value.var = "value", fun.aggregate = sum)
}
#df1 gives the total count of all words across the history of the page
# Part 4 - Adding columns to the data frame with word counts for each version
# Work with two loops - Outer loop runs through OldIds - inner loop runs through df1$word
for (OUTER in 1:length(OldIds)) {
  CountTibble <- get(paste0("OldidWordCount", OUTER)) #getting the relevant tibble
  WordColumn <- CountTibble$word 
  CountColumn <- CountTibble$n
  SpecificCount <- NULL
  for (INNER in 1:length(df1$word)) {
    WordPosition <- which(as.character(df1$word[INNER]) == WordColumn) #idenifying position of the word
    if (length(WordPosition) > 0) {SpecificCount[INNER] <- CountColumn[WordPosition]
    } else {
      SpecificCount[INNER] <- 0
    } 
  }
  #assign(paste0("Count", as.character(OldIds[OUTER])), SpecificCount)
  df1[,(paste0("Count", as.character(OldIds[OUTER])))] <- SpecificCount
  print(paste0("To list", (length(OldIds)-OUTER)))
}
write.csv(df1, file = paste("WordHist","-",input.pagename,".csv",sep = ""), append = FALSE, row.names = FALSE)
# so the basic word count data frame is ready. we can now move on to plotting
# Part 5 - Plotting
library(ggplot2)
#Create new data frame for each plot
#Need to transform the date to R format
TransDate <- as.POSIXct(strptime(RevHistDF$TimeStamp, "%H:%M, %d %B %Y", tz = "GMT"))
#PLOT plot size history
pdf(paste0("Plots",input.pagename,".pdf"))
WikiFrame <- data.frame(TransDate, RevHistDF$Bytes, stringsAsFactors = FALSE)
names(WikiFrame) <- c("Date","Bytes")
SizePlot <- ggplot(data = WikiFrame, aes(Date, Bytes))+
  geom_line(color = "#0D7195", size = 1)
print(SizePlot + scale_x_datetime(date_labels = "%Y")  + ggtitle(input.pagename, subtitle = "Wikipedia Page Size History"))
#PLOT  Ploting top word - 100 or less
#sort WordHist(df1) data frame according to n
dfsorted <- df1[order(-df1$n),]
#subset to top 100 owrds or less
if (length(dfsorted$n) > 100) {
  dftop <- dfsorted[c(1:100),]
} else {dftop <- dfsorted}
#plotting using loop (OUTER - word) within loop (INNER - Oldid)
for (OUTER in 1:length(dftop$word)){
  WordFrame <- data.frame(TransDate, as.numeric(dftop[OUTER,c(3:length(dftop))]), stringsAsFactors = FALSE)
  names(WordFrame) <- c("Date","Count")
  #WordPlot <- ggplot(data = WordFrame, aes(Date, Count))+
  #  geom_line(color = "#0D7195", size = 1)
  #print(WordPlot + scale_x_datetime(date_labels = "%Y")  + ggtitle(input.pagename, subtitle = dftop$word[OUTER]))
  #combining page date with word data
  dt <- data.frame(WikiFrame$Date,WikiFrame$Bytes,WordFrame$Count, stringsAsFactors = FALSE)
  names(dt) <- c("Date","Size","Count")
  WordPlot <- ggplot(data = dt)+
    geom_line(mapping=aes(x=Date,y=Size),color = "grey", size = 2)+
    geom_area(mapping=aes(x=Date,y=Size), fill="grey", alpha=1.)+
    geom_line(mapping = aes(x=Date,y=Count*(max(dt$Size)/max(dt$Count))), size = 1, color = "blue") + 
    coord_cartesian(ylim = c(0, (1.1*max(dt$Size)))) +
    scale_x_datetime(date_labels = "%Y")  + ggtitle(input.pagename, subtitle = dftop$word[OUTER])+
    scale_y_continuous(name = "Bytes", 
                       sec.axis = sec_axis(~./(max(dt$Size)/max(dt$Count)), name = "Word Count"))
  print(WordPlot)
}
#place the total word count in the background for perspective
#PLOT plotting pie of users
#creating a dataframe with frequency of editors
EdCountFreq <- data.frame(table(RevHistDF$Editor), stringsAsFactors = FALSE)
EdCountFreq %>% mutate_if(is.factor, as.character) -> EdCountFreq
names(EdCountFreq) <- c("Editor","Frequency")
EdCountFreq <- EdCountFreq[order(-EdCountFreq$Frequency),]
#need to consolidate the smaller contributions
#consolidate all contributors below 1%
Others <- sum(EdCountFreq$Frequency[which(EdCountFreq$Frequency < ((as.numeric(PageEditStat)*0.01)))])
OthersDF <- data.frame(Editor = "Others", Frequency = Others)
#Subsetting Editors with >= 1% edits and adding others
EdCountFreq <- EdCountFreq[which(EdCountFreq$Frequency >= ((as.numeric(PageEditStat)*0.01))),]
EdCountFreq <- rbind(EdCountFreq, OthersDF)
#Plotting Pie
bp<- ggplot(EdCountFreq, aes(x="", y=Frequency, fill=Editor))+
  geom_bar(width = 1, stat = "identity", color = 'black')
pie <- bp + coord_polar("y", start=0) + ggtitle(input.pagename, subtitle = "Editors by Edits")
print(pie)
write.csv(EdCountFreq, file = paste("EdCount","-",input.pagename,".csv",sep = ""), append = FALSE, row.names = FALSE)
#PLOT Edit behaviour of top Editors
for (TOP in 1:(length(EdCountFreq$Editor)-1)) {
  EditorRows <- which(RevHistDF$Editor == EdCountFreq$Editor[TOP]) #identifying editor's rows
  EditorDiff <- RevHistDF$`Diff Bytes`[EditorRows]
  EditBehavDF <- data.frame((1:length(EditorDiff)),EditorDiff, stringsAsFactors = FALSE)
  names(EditBehavDF) <- c("Seq","Bytes")
  p1 <- ggplot(data = EditBehavDF, aes(x = Seq, y = Bytes)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept=0, color = "red") +
    ggtitle(input.pagename, subtitle = paste("Edit Behaviour of ",EdCountFreq$Editor[TOP],
                                             "\nContribution",
                                             round(((EdCountFreq$Frequency[TOP]/as.numeric(PageEditStat))*100), digits = 2)
                                             ,"%"))
  print(p1)
}
for (TOP in 1:(length(EdCountFreq$Editor)-1)) {
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
  ZeroDF <- data.frame(word = " ", n = 1, stringsAsFactors = FALSE)
  if (length(AddedDF$word)>0) {WordDF1 <- AddedDF} else {WordDF1 <- ZeroDF}
  EditWord1 <-ggplot(WordDF1, aes(label = word, size = n, color = n)) +
    geom_text_wordcloud(shape = "square") +
    scale_size() +
    ggtitle(paste("Words added by",EdCountFreq$Editor[TOP],"to",input.pagename,sep = " "))+
    theme_minimal()
  if (length(RemovedDF$word)>0) {WordDF2 <- RemovedDF} else {WordDF2 <- ZeroDF}
  EditWord2 <-ggplot(WordDF2, aes(label = word, size= abs(n), color = n)) +
    geom_text_wordcloud(shape = "square") +
    scale_size() +
    ggtitle(paste("Words removed by",EdCountFreq$Editor[TOP],"to",input.pagename,sep = " "))+
    theme_minimal()
  library(gridExtra)
  grid.arrange(EditWord1,EditWord2)
}
#PLOT Controversy Factor for the Page
#Counting the frequency of additions, no change and removal
Intervention <- sign(RevHistDF$`Diff Bytes`)
ControTable <- table(Intervention)
ControFactor <- as.numeric(round((ControTable[1]/as.numeric(PageEditStat)), digits = 3))
ControTable <- data.frame(ControTable, stringsAsFactors = FALSE)
ControTable %>% mutate_if(is.factor, as.character) -> ControTable
#preparing the data frame for plotting
if (length(which(ControTable$Intervention == -1))>0){
  CF1 <- data.frame(Change="Removal",freq="Freq",len=(ControTable$Freq[which(ControTable$Intervention == -1)]),
                    stringsAsFactors = FALSE)
} else {
  CF1 <- data.frame(Change="Removal",freq="Freq",len=0, stringsAsFactors = FALSE)
}
if (length(which(ControTable$Intervention == 0))>0){
  CF2 <- data.frame(Change="No Change",freq="Freq",len=(ControTable$Freq[which(ControTable$Intervention == 0)]),
                    stringsAsFactors = FALSE)
} else {
  CF2 <- data.frame(Change="No Change",freq="Freq",len=0, stringsAsFactors = FALSE)
}
if (length(which(ControTable$Intervention == 1))>0){
  CF3 <- data.frame(Change="Addition",freq="Freq",len=(ControTable$Freq[which(ControTable$Intervention == 1)]),
                    stringsAsFactors = FALSE)
} else {
  CF3 <- data.frame(Change="Addition",freq="Freq",len=0, stringsAsFactors = FALSE)
}
ControDF <- rbind(CF1,CF2,CF3)
#plotting stacked bar
ControBar <- ggplot(data=ControDF, aes(x=freq, y=len, fill=Change)) +
  geom_bar(stat="identity", width = 0.3, ) +
  scale_fill_manual(values = c("Blue","Grey","Red")) +
  geom_text(aes(label = len), size = 3.5, color = "White", position = position_stack(vjust = 0.5)) +
  #guides(fill=FALSE) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle(input.pagename, subtitle = paste("Controversy Factor ",ControFactor)) +
  coord_flip()
print(ControBar)
#PLOT Controversy Factor for each word
#Prepare data frame for controversy data for each word
ChooseByTop <- length(which(dftop$n > dftop$n[1]/10))
ChooseByFraction <- round((0.4*length(dftop$word)), digits = 0)
Choice <- sort(c(ChooseByFraction,ChooseByTop))
ChoiceDF <- dftop[(1:Choice[1]),]
#ChoiceDF gives a further subset of the top words
#identifying the Additions, No Change and Removals for each of these words
#cgreating a MaxControDF with Additions, No Change, Removals and ControFactor
MaxControDF <- NULL
AllWordControDF <- NULL
for (OUTER in 1: length(ChoiceDF$word)) {
  ChangeVec <- NULL
  for (INNER in 4:length(ChoiceDF)) {
    ChangeVec[INNER] <- ChoiceDF[OUTER,INNER] - ChoiceDF[OUTER,(INNER-1)]
  }
  ChangeVec <- sign(ChangeVec[4:length(ChoiceDF)])
  Addition <- length(which(ChangeVec==1))
  NoChange <- length(which(ChangeVec==0))
  Removal <- length(which(ChangeVec==-1))
  WordControFac <- as.numeric(round((Removal/(Addition +  NoChange + Removal)), digits = 3))
  WordControDF <- data.frame(ChoiceDF$word[OUTER] ,Addition,NoChange,Removal, WordControFac, stringsAsFactors = FALSE )
  names(WordControDF) <- c("Word" ,"Addition","No Change", "Removal", "WordControFac")
  AllWordControDF <- rbind(AllWordControDF,WordControDF)
  print(paste(OUTER,INNER, sep = " "))
}
AllWordControDF <- AllWordControDF[order(-AllWordControDF$WordControFac),]
#subsetting words with ControFac > 2.5%
if (length(which(AllWordControDF$WordControFac>0.025))==0) {
  TopWordControDF <- AllWordControDF[which(AllWordControDF$WordControFac==max(AllWordControDF$WordControFac)),]} else {
    TopWordControDF <- AllWordControDF[which(AllWordControDF$WordControFac>0.025),]}
#to plot create new DF each time
for (WORD in 1:length(TopWordControDF$Word)) {
  ControDF <- data.frame(c("Removal","Addition"),
                         c("Freq","Freq"),
                         c(AllWordControDF$Removal[WORD],AllWordControDF$Addition[WORD]),
                         stringsAsFactors = FALSE)
  names(ControDF) <- c("Change","freq","len")
  CDF <- ggplot(data=ControDF, aes(x=freq, y=len, fill=Change)) +
    geom_bar(stat="identity", width = 0.3, ) +
    scale_fill_manual(values = c("Blue","Red")) +
    geom_text(aes(label = len), size = 3.5, color = "White", position = position_stack(vjust = 0.5)) +
    #guides(fill=FALSE) +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle(input.pagename, subtitle = paste("Controversy Factor for",TopWordControDF$Word[WORD],":",
                                             TopWordControDF$WordControFac[WORD], sep = " ")) +
    coord_flip()
  print(CDF)
}
print(PageEditStat)
print(Sys.time())
dev.off()
#All is done