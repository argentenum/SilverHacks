#examine incompletedata.csv
setwd("~/ownCloud/iaclals-scraping")
incompletedata <- read.csv(file = "incompletedata.csv", stringsAsFactors = FALSE, header = TRUE)
print(sum(is.na(incompletedata$Birth.Place)))
print(sum(is.na(incompletedata$Birth.Date)))
print(sum(is.na(incompletedata$Death.Place)))
print(sum(is.na(incompletedata$Death.Date)))
#extracting birth and death years
for (fis in 1:length(incompletedata$Name))
     {incompletedata$Birth.Date <- substr(incompletedata$Birth.Date,1,4)
       incompletedata$Death.Date <- substr(incompletedata$Death.Date,1,4) 
}
#creating complete list of birth
completedata <- read.csv(file = "cleanoutput.csv", stringsAsFactors = FALSE, header = TRUE)
birthcomplete <- data.frame(completedata$Name, completedata$Birth.Place, completedata$birth.year, completedata$Field)
names(birthcomplete) <- c("Name", "Birth.Place", "Birth.Year", "Field")
birthincomplete <- data.frame(incompletedata$Name, incompletedata$Birth.Place, incompletedata$Birth.Date, incompletedata$Field)
names(birthincomplete) <- c("Name", "Birth.Place", "Birth.Year", "Field")
birthtotal <- rbind(birthcomplete, birthincomplete)
birthtotal <- na.omit(birthtotal)
birthtotal <- birthtotal[order(birthtotal$Birth.Year),]

#creating complete list of death
deathcomplete <- data.frame(completedata$Name, completedata$Death.Place, completedata$death.year, completedata$Field)
names(deathcomplete) <- c("Name", "Death.Place", "Death.Year", "Field")
deathincomplete <- data.frame(incompletedata$Name, incompletedata$Death.Place, incompletedata$Death.Date, incompletedata$Field)
names(deathincomplete) <- c("Name", "Death.Place", "Death.Year", "Field")
deathtotal <- rbind(deathcomplete, deathincomplete)
#getting incoplete death cases
deathNA <- deathtotal[!complete.cases(deathtotal),]
#extracting complete death cases
deathtotal <- na.omit(deathtotal)
deathtotal <- deathtotal[order(deathtotal$Death.Year),]

#extracting cases where there is a possibility of the person living - no death data
nodeath <- subset(deathNA, is.na(deathNA$Death.Place))
nodeath <- subset(nodeath, is.na(nodeath$Death.Year))
#checking if place of residence is available
resplace <- NULL
for (tiy in 1:length(nodeath$Name))
  {blankurl <- "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=DESCRIBE%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FBBLLAANNKK%3E&format=text%2Fcsv"
  newurl <- sub("BBLLAANNKK",nodeath$Name[tiy],blankurl)
  resfile <- read.csv(url(newurl),stringsAsFactors = FALSE)
  resiplace <- NA
  indexresplace <- which(resfile$predicate=="http://dbpedia.org/ontology/residence")
  #extracting place of residence; only first mentioned place except India
  if (length(indexresplace) > 0) {resiplace <- resfile$object[indexresplace]}
  if (length(indexresplace) == 0) {} else 
   {if (length(indexresplace) == 1) {if (resiplace[1] == "http://dbpedia.org/resource/India") {resiplace <- NA}} else
    {if (resiplace[1] == "http://dbpedia.org/resource/India") {resiplace <- resiplace[2]}}}
  resplace[tiy] <-   sub("http://dbpedia.org/resource/","",resiplace[1:length(resiplace)])
}
#writing dataframe of the living with place of residence
living <- data.frame(nodeath$Name, nodeath$Field, resplace )
living <- living[complete.cases(living),]
names(living) <- c("Name", "Field", "Residence" )
write.csv(living, file = "living.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
#matching living to birth as that would lead to a far richer migration plot
living.again <- read.csv(file = "living.csv", stringsAsFactors = FALSE, header = TRUE)
livbirth <- merge(birthtotal,living.again, by = "Name"  )
livbirth <- unique(livbirth)
forced.death <- NULL
forced.death[1:nrow(livbirth)] <- 2019
livbirth <- data.frame(livbirth$Name, livbirth$Birth.Place, livbirth$Birth.Year, livbirth$Residence,  forced.death, livbirth$Field.x)
names(livbirth) <- c("Name", "Birth.Place", "birth.year", "Death.Place",  "death.year", "Field")
head(livbirth)

# use additionalplacedata.R to update allpacedate.csv and allcoords.csv

# adding location from updated allcoords.csv
allcoords <- read.csv(file = "allcoords.csv", stringsAsFactors = FALSE, header = TRUE)

birthlon <- NULL
birthlat <- NULL
deathlon <- NULL
deathlat <- NULL
bio.length <- nrow(livbirth)
for (t in 1:bio.length)
{
  birloc.place <- livbirth$Birth.Place[t]
  birthrow <- which(allcoords$places==birloc.place)
  birthlon[t] <- allcoords$lon[birthrow]
  birthlat[t] <- allcoords$lat[birthrow]
  
  dthloc.place <- livbirth$Death.Place[t]
  deathrow <- which(allcoords$places==dthloc.place)
  deathlon[t] <- allcoords$lon[deathrow]
  deathlat[t] <- allcoords$lat[deathrow]
}


birthlat <- as.numeric(birthlat)
birthlon <- as.numeric(birthlon)
deathlat <- as.numeric(deathlat)
deathlon <- as.numeric(deathlon)
newoutput <- data.frame(livbirth$Birth.Place, livbirth$Death.Place, birthlon, birthlat, deathlon, deathlat, livbirth$Name, livbirth$Field,
                   livbirth$birth.year, livbirth$death.year)
# newoutput <- na.omit(newoutput)
# colnames(newoutput)<- c("Birth Place","Death Place","birth.place","death.place","birth.year","death.year","bir.lon",
#                       "bir.lat","dth.lon","dth.lat")
write.csv(newoutput,file = "newcleanoutput.csv", , sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)


