library(stringr)
library(maps)
data(world.cities)
setwd("~/Documents/Covind")
#Discovering the mentions of travel information in Notes
TrvFrmIndex <- grep("Travelled from ",NewDF$notes, ignore.case=TRUE)
TrvToIndex <- grep("Travelled to ",NewDF$notes, ignore.case=TRUE)
#extractting the relevant rows for each substring
#may be helpful to put in a loop. will have to think about it
TrvFrmDF <- NewDF[TrvFrmIndex,]
names(TrvFrmDF) <- names(NewDF)
TrvToDF <- NewDF[TrvToIndex,]
names(TrvToDF) <- names(NewDF)
#working with TrvToDF
Puppy <- strsplit(TrvToDF$notes, "ravelled to ")
Puppy <- purrr::map( .x = Puppy, .f = 2 ) %>%   unlist()
Puppy <- strsplit(Puppy, " ")
Puppy <- 
Puppy <- gsub('[[:punct:]]',"",Puppy)
TrvToDF$notes <- Puppy

#working with TrvFrmDF
Puppy <- strsplit(TrvFrmDF$notes, "ravelled from ")
Puppy <- purrr::map( .x = Puppy, .f = 2 ) %>%   unlist()
Puppy <- strsplit(Puppy, " ")
Puppy <- purrr::map( .x = Puppy, .f = 1 ) %>%   unlist()
Puppy <- gsub('[[:punct:]]',"",Puppy)
TrvFrmDF$notes <- Puppy

#from here on we need to merge the two dataframes
TravelDF <- rbind(TrvFrmDF,TrvToDF)
#need to replace place values which are not in the worldcities data set
#prepare a database of the various mistakes historically and replace values in a loop
ReplaceMapDF <- read.csv("NotInMap.csv", header = TRUE, stringsAsFactors = FALSE)
ReplaceMapIndex <- which(TravelDF$notes %in% ReplaceMapDF$Mistake)
if (ReplaceMapIndex > 0) {
  for (jix in 1:length(ReplaceMapIndex))
  {
  TravelDF$notes[ReplaceMapIndex[jix]] <- ReplaceMapDF$Correct[which(ReplaceMapDF$Mistake==TravelDF$notes[ReplaceMapIndex[jix]])]
  }
}
NotInMap <- unique(TravelDF$notes[-unique(c(which(TravelDF$notes %in% world.cities$name), which(TravelDF$notes %in% world.cities$country.etc)))])
print(paste("Not in Map ",length(NotInMap)))
#need to replace INdian cities with States from a database that is maintaned
ReplaceCitiesDF <- read.csv("CityToState.csv", header = TRUE, stringsAsFactors = FALSE)
ReplaceCitiesIndex <- which(TravelDF$notes %in% ReplaceCitiesDF$City)
if (ReplaceCitiesIndex>0){
  for (jix in 1:length(ReplaceCitiesIndex))
  {
  TravelDF$notes[ReplaceCitiesIndex[jix]] <- ReplaceCitiesDF$State[which(ReplaceCitiesDF$City==TravelDF$notes[ReplaceCitiesIndex[jix]])]
    #ReplaceCitiesDF$State[which(ReplaceCitiesDF$City==ReplaceCitiesIndex[jix])]
  }
}
#sort out India values - cities with states - for India we are keeping to States
IndianCities <- TravelDF$notes[which(TravelDF$notes %in% world.cities$name[world.cities$country.etc=="India"])]
#further reducing any cities/ UTs which are in StatesUTList
IndianCities<- IndianCities[-which(IndianCities %in% StatesUTList)]
print(paste("Not in Map ",length(IndianCities)))

#some of the places are cities, need to convert them to countries, except for Indian cities
#now there is a Canadian Delhi which clashes with Indian Delhi - first remove it from worldcities
world.cities.rev <- world.cities[-which(world.cities$name=="Delhi"&world.cities$country.etc=="Canada"),]
#finding mentions of non-Indian cities
NonIndianIndex <- which(TravelDF$notes %in% world.cities.rev$name[-which(world.cities.rev$country.etc=="India")])
#Replace them with their respective country names
if (NonIndianIndex>0){
  for (jix in 1:length(NonIndianIndex))
  {
    TravelDF$notes[jix] <- world.cities.rev$country.etc[which(world.cities.rev$name == TravelDF$notes[NonIndianIndex[jix]])]
  }
}
#there are still a few blank entries in the notes. just get rid of them
TravelDF <- TravelDF[-which(TravelDF$notes==""),]
#preparing nodes - redundant
#nodes <- data.frame(unique(TravelDF$notes), stringsAsFactors = FALSE)
#names(nodes) <- c("name")
#now get the links ready and we are ready to go
#the source is "notes" and target is "detected state"
#to do this easily first concatenate "notes" and "detected state" with a removable string
Concat <- paste(TravelDF$notes,"@@",TravelDF$detectedstate)
ConcateTable <- data.frame(table(Concat), stringsAsFactors = FALSE)
ConcateTable$Concat <- levels(droplevels(ConcateTable$Concat))
ConcateSplit <- strsplit(ConcateTable$Concat," @@ ")
Source <- purrr::map( .x = ConcateSplit, .f = 1 ) %>%   unlist()
Target <- purrr::map( .x = ConcateSplit, .f = 2 ) %>%   unlist()
links <- data.frame(
  source=Source, 
  target=Target, 
  value=ConcateTable$Freq
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

library(networkD3)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

# save the widget
library(htmlwidgets)
saveWidget(p, file="sankeyCovind.html")

# Add a 'group' column to the nodes data frame:
#nodes$group <- as.factor(c("Middle East","India South","Australia","Carribean","Middle East",
#                           "South Asia","South America","South America","North America","India East",
#                           "Asia","India North","Europe","Middle East","Africa","Europe","Europe",
#                           "Asia","India South","India South","India West","Middle East","Europe",
#                           "South East Asia","India North","Middle East","India North","Middle East",
#                           "South East Asia","Europe","South Asia","Europe","Europe",
#                           "South East Asia","Carribean","Europe","Europe","Middle East",
#                           "North America","India North","India East","Carribean","India South",
#                           "India East","India West","India East","India East","India North",
#                           "India South","India South","India North","India West","India North",
#                           "India West","India North","India East","India East","India North",
#                           "India North","India South"))
nodes$group <- as.factor(c("Middle East","India","Australia","Carribean","Middle East",
                           "South Asia","South America","South America","North America","India",
                           "Asia","India","Europe","Middle East","Africa","Europe","Europe",
                           "Asia","India","India","India","Middle East","Europe",
                           "South East Asia","India","Middle East","India","Middle East",
                           "South East Asia","Europe","South Asia","Europe","Europe",
                           "South East Asia","Carribean","Europe","Europe","Middle East",
                           "North America","India","India","Carribean","India",
                           "India","India","India","India","India",
                           "India","India","India","India","India",
                           "India","India","India","India","India",
                           "India","India"))

# Give a color for each group:
#my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["wheat4","deepskyblue","yellow2","magenta",
#            "sienna","hotpink","darkorchid","darkblue","lightslategrey","cornflowerblue","green4","tan2",
#            "darkcyan","tomato"])'

my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["wheat4","deepskyblue","yellow2","magenta",
            "sienna","hotpink","darkorchid","lightslategrey","green4","tan2","tomato"])'

# Make the Network
q <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, NodeGroup="group")
q
