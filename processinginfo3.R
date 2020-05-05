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
    {blankurl <- "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=DESCRIBE%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FBBLLAANNKK%3E&format=text%2Fcsv"
  birloc.url <- sub("BBLLAANNKK",bioutput[t,2],blankurl)
  birthfile <- read.csv(url(birloc.url),stringsAsFactors = FALSE)
  indexbirthlon<- which(birthfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#long")
  birthlon[t] <- birthfile$object[indexbirthlon]
  indexbirthlat<- which(birthfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#lat")
  birthlat[t] <- birthfile$object[indexbirthlat]
  dthloc.url <- sub("BBLLAANNKK",bioutput[t,4],blankurl)
  deathfile <- read.csv(url(dthloc.url),stringsAsFactors = FALSE)
  indexdeathlon<- which(deathfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#long")
  deathlon[t] <- deathfile$object[indexdeathlon]
  indexdeathlat<- which(deathfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#lat")
  deathlat[t] <- deathfile$object[indexdeathlat]
}
birthlat <- as.numeric(birthlat)
birthlon <- as.numeric(birthlon)
deathlat <- as.numeric(deathlat)
deathlon <- as.numeric(deathlon)
bioutput <- cbind(bioutput[,2, drop=FALSE],bioutput[,4, drop=FALSE],birthlon,birthlat,deathlon,deathlat,
                  bioutput[,1, drop=FALSE],bioutput[,6, drop=FALSE],bioutput[,7, drop=FALSE],bioutput[,8, drop=FALSE])
#colnames(bioutput)<- c("Birth Place","Death Place","birth.place","death.place","birth.year","death.year","bir.lon",
#                       "bir.lat","dth.lon","dth.lat")
write.csv(bioutput,file = "cleanoutput.csv", append = FALSE)

