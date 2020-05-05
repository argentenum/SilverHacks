# organising manually cleaned bio data to a matrix
# query and add location data
setwd("~/ownCloud/iaclals-scraping")
bioinfo <- read.csv(file = "cleanrecord.csv", stringsAsFactors = FALSE, header = TRUE)
allcoords <- read.csv(file = "allcoords.csv", stringsAsFactors = FALSE, header = TRUE)
# bioinfo.vector <- c(bioinfo$V2," ")
bioutput <- matrix(bioinfo$V2, ncol = 7, byrow = TRUE)
bioutput <- bioutput[,-7]
colnames(bioutput)<- c("Name","Birth Place","Birth Date","Death Place","Death Date","Field")
write.csv(bioutput, file = "bioutput.csv")
#removing any row with NA value
bioNA <- bioutput[!complete.cases(bioutput),]
write.csv(bioNA,file="incompletedata.csv")
bioutput <- na.omit(bioutput)
#extracting and writing birth and death years
birth.year <- substr(bioutput[,3],1,4)
death.year <- substr(bioutput[,5],1,4)
bioutput <- cbind(bioutput,birth.year,death.year)
#extracting location info and writing to csv
birthlon <- NULL
birthlat <- NULL
deathlon <- NULL
deathlat <- NULL
bio.length <- length(bioutput[,1])
for (t in 1:bio.length)
    {
  birloc.place <- bioutput[t,2]
  birthrow <- which(allcoords[,1]==birloc.place)
  birthlon[t] <- allcoords[birthrow,2]
  birthlat[t] <- allcoords[birthrow,3]
  
  dthloc.place <- bioutput[t,4]
  deathrow <- which(allcoords[,1]==dthloc.place)
  deathlon[t] <- allcoords[deathrow,2]
  deathlat[t] <- allcoords[deathrow,3]
}
#there is a mistake in writing lat and lon. it has been reversed in the allcords.csv

birthlat <- as.numeric(birthlat)
birthlon <- as.numeric(birthlon)
deathlat <- as.numeric(deathlat)
deathlon <- as.numeric(deathlon)
bioutput <- cbind(bioutput[,2, drop=FALSE],bioutput[,4, drop=FALSE],birthlon,birthlat,deathlon,deathlat,
                  bioutput[,1, drop=FALSE],bioutput[,6, drop=FALSE],bioutput[,7, drop=FALSE],bioutput[,8, drop=FALSE])
bioutput <- na.omit(bioutput)
#colnames(bioutput)<- c("Birth Place","Death Place","birth.place","death.place","birth.year","death.year","bir.lon",
#                       "bir.lat","dth.lon","dth.lat")
write.csv(bioutput,file = "cleanoutput.csv", append = FALSE)

