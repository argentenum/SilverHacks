setwd("~/Documents/Covind")
library(jsonlite)
rawJson <- fromJSON("https://api.covid19india.org/raw_data.json", flatten=TRUE)
write.csv(rawJson, file = "Covid19TrackerRaw.csv")
#preparing statewise list
StatesUTList <- unique(rawJson$raw_data$detectedstate)
StatesUTList <- StatesUTList[StatesUTList != ""]
AllDates <- as.Date(rawJson$raw_data$dateannounced, "%d/%m/%y")
AllDates <- na.omit(AllDates)
DatesList <- seq(min(AllDates), max(AllDates), by="days")
#Creating an empty data frame with states as rows and dates as columns
#creating a vector with zero values for length(StatesUTList)*length(DatesList)
BlankVec <- NULL
BlankVec[1:(length(StatesUTList)*length(DatesList))] <- 0
#now distributing BlankVec into a matrix
BlankMatrix <- matrix(BlankVec, nrow = length(StatesUTList), ncol = length(DatesList))
#in original matrix dates appear as strings - so convert them to dates
NoStringDates <- as.Date(rawJson$raw_data$dateannounced, "%d/%m/%y")
NewDF <- rawJson$raw_data
NewDF$dateannounced <- NoStringDates
NoStringDates2 <- as.Date(rawJson$raw_data$statuschangedate, "%d/%m/%y")
NewDF$statuschangedate <- NoStringDates2
# This is the dataframe to be used for all calculations - later convert the status changed dates as well
#Read respective values and replace in blank matrix
DailyStates <- BlankMatrix
for (DIN in 1:length(DatesList))
{
  for (RAJ in 1:length(StatesUTList))
  {
    Gona <- sum(NewDF$dateannounced == DatesList[DIN] & NewDF$detectedstate == StatesUTList[RAJ])
    DailyStates[RAJ,DIN] <- Gona
  }
}
DailyDF <- data.frame(StatesUTList,DailyStates)
#preparing Cumulative Matrix
CumuStates <- BlankMatrix
#no addition in the first date - first column
CumuStates[,1] <- DailyStates[,1]
for (DIN in 2:length(DatesList))
{
  for (RAJ in 1:length(StatesUTList))
  {
    CumuStates[RAJ,DIN] <- CumuStates[RAJ,(DIN-1)] + DailyStates[RAJ,DIN]
  }
}
#Adding State names to Matrix
CumuDF <- data.frame(CumuStates)
rownames(CumuDF) <- StatesUTList
#sorting in descending order according to current total cases
TopStates <-  CumuDF[order(-CumuDF[,ncol(CumuDF)]),]
#plotting area chart for top 10 states
#need to reqrite CumuDF in new format with Date, Cumulative Count, State
#StackedCumuDF <- NULL
#StackedCumuDF <- data.frame(Date = numeric(),          # Specify empty vectors in data.frame
#                     Count = numeric(),
#                     State = factor(),
#                     stringsAsFactors = TRUE)
StackedDate <- NULL
StackedCount <- NULL
StackedState <- NULL
Rwo <- 1
for (DIN in 30:length(DatesList))
{
  for (RAJ in 1:length(StatesUTList))
  {
    StackedDate[Rwo] <- as.Date(DatesList[DIN], origin = "1970-01-01")
    StackedCount[Rwo] <- CumuDF[RAJ,DIN]
    StackedState[Rwo] <- StatesUTList[RAJ]
    #NewRow <- c(DIN,CumuDF[RAJ,DIN],StatesUTList[RAJ])
    #StackedCumuDF <- rbind(StackedCumuDF,NewRow)
    Rwo <- Rwo + 1
  }
}
StackedCumuDF <- data.frame(as.Date(StackedDate, origin = "1970-01-01"),StackedCount,StackedState)
names(StackedCumuDF) <- c("Day","Count","State")
library(ggplot2)
library(dplyr)
library(scales)
pdf("covindExport.pdf",  
    title="State and Age statistics for Covid in India",
    paper = "a4r")
print("State and Age Statistics for Covid in India",justify=right)
#sftp://arjunghosh@ssh2.iitd.ernet.in/home/hss/faculty/arjunghosh/public_html/covind
StackedCumuDF$State <- factor(StackedCumuDF$State , levels= rownames(TopStates)[1:10] )
ggplot(StackedCumuDF, aes(x=Day, y=Count, fill=State)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  labs(title = "Top 10 Covid Affected States in India", 
     subtitle = "", 
     caption = paste("Data from www.covid19india.org ",Sys.time()))


#Stacking graph by percentage
StackedCumuDF <- StackedCumuDF  %>%
  group_by(Day, State) %>%
  summarise(n = sum(Count)) %>%
  mutate(percentage = n / sum(n))
ggplot(StackedCumuDF, aes(x=Day, y=percentage, fill=State)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  labs(title = paste("Share of Top 10 Covid Affected States in India"), 
     subtitle = paste(""), 
     caption = paste("Data from www.covid19india.org ",Sys.time()))
#plotting daily distribution for top 10 states

RAJ <- 1
POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                       Many=DailyStates[POSIT,30:length(DatesList)], 
                       More=CumuStates[POSIT,30:length(DatesList)])
BarState$Date <- as.POSIXct(BarState$Date)

ggplot(BarState, aes(Date)) + 
  geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
  geom_line(aes(y = More, group = 1, color = "Cumulative"), size = 1)  +
  scale_fill_manual("",values="tan1")+
  scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
  scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
  ylab("Cumulative Cases")+
  theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
  scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
  theme(legend.key=element_blank(),
        legend.title=element_blank(),
        legend.position = "bottom")+
  labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
       subtitle = paste("Rank ",RAJ), 
       caption = paste("Data from www.covid19india.org ",Sys.time()))

  
  RAJ <- 2
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  RAJ <- 3
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))

  RAJ <- 4
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  RAJ <- 5
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  RAJ <- 6
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  
  RAJ <- 7
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  
  RAJ <- 8
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  
  RAJ <- 9
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  
  RAJ <- 10
  POSIT <- which(StatesUTList == rownames(TopStates)[RAJ])
  BarState <- data.frame(Date=as.Date(DatesList[30:length(DatesList)], origin = "1970-01-01"), 
                         Many=DailyStates[POSIT,30:length(DatesList)], 
                         More=CumuStates[POSIT,30:length(DatesList)])
  BarState$Date <- as.POSIXct(BarState$Date)
  
  ggplot(BarState, aes(Date)) + 
    geom_bar(aes(y = Many*(max(BarState$More)/max(BarState$Many)), color = "Daily"), stat="identity", fill = "tan1") +
    geom_line(aes(y = More, group = 1, color = "Cumulative"),, size = 1)  +
    scale_fill_manual("",values="tan1")+
    scale_colour_manual("", values=c("Daily" = "tan1", "Cumulative" = "purple"))+
    scale_y_continuous(sec.axis = sec_axis(~./(max(BarState$More)/max(BarState$Many)), name = "Daily Count of Cases")) +
    ylab("Cumulative Cases")+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom")+
    labs(title = paste("Growth of Covid Cases in ", StatesUTList[POSIT]), 
         subtitle = paste("Rank ",RAJ), 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
#Plotting Agewise distribution of confirmed
  AllAge <- as.numeric(NewDF$agebracket)


  FalseAgeIndex <- which(is.na(AllAge))
  AllAgeIndex <- c(1:length(AllAge))
  TrueAgeIndex <- AllAgeIndex[-FalseAgeIndex]
  AgeDF <- data.frame(as.numeric(NewDF$agebracket[TrueAgeIndex]), stringsAsFactors = FALSE)
  names(AgeDF) <- c("Age")
  ggplot(AgeDF, aes(x=Age)) + 
    geom_histogram(binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
    ylab("Confirmed Cases")+
    ggtitle(paste("Agewise distribution of Covid cases in India"))

  #Scatterplot of Age by Date
  AgeByDate <- data.frame(as.Date(NewDF$dateannounced[TrueAgeIndex]), origin = "1970-01-01",
                          as.numeric(NewDF$agebracket[TrueAgeIndex]), stringsAsFactors = FALSE)
  names(AgeByDate) <- c("Date","Origin","Age")
  ShortAgeByDate <- data.frame(AgeByDate$Date[AgeByDate$Date > DatesList[30]],
                               AgeByDate$Age[AgeByDate$Date > DatesList[30]])
  names(ShortAgeByDate) <- c("Date","Age")
  ShortAgeByDate$Date <- as.POSIXct(ShortAgeByDate$Date)
  
  ggplot(ShortAgeByDate, aes(x=Date, y=Age)) + 
    geom_point(
      color="black",
      fill="#69b3a2",
      shape=22,
      alpha=0.5,
      size=2,
      stroke = 1
    )+
    theme(axis.text.x = element_text( angle = 90, color="black", size=8, face=1))+
    scale_x_datetime(date_breaks = "2 day", labels = date_format("%d %b"))+
    labs(title = "Age Distribution of Covid Cases in India", 
         subtitle = "By Date", 
         caption = paste("Data from www.covid19india.org ",Sys.time()))
  
  
  
      
  dev.off()