setwd("~/ownCloud/iaclals-scraping")
# adding place names to null-places.csv
terra.all <- read.csv("biorecord.csv", stringsAsFactors = FALSE, header = FALSE)
null.places <- read.csv("null-places.csv", stringsAsFactors = FALSE, header = TRUE)
terra.birth <- which(terra.all$V1=="Birth Place")
terra.death <- which(terra.all$V1=="Death Place")
terra.rows <- c(terra.birth,terra.death)
terra.places <- unique(terra.all$V2[terra.rows])
terra.places <- terra.places[complete.cases(terra.places)]
#removing place names with NA
bioNA <- bioutput[!complete.cases(bioutput),]
#removing place names already on the null-places.csv
terra.places <- terra.places[!terra.places %in% null.places$null.places]
# removing place names already on the allcoords.csv
oldcoords <- read.csv("allcoords.csv", stringsAsFactors = FALSE, header = TRUE)
terra.places <- terra.places[!terra.places %in% oldcoords$places]
terra.delete <- NULL
terra.coords <- NULL
for (vr in 1:length(terra.places))
  {area <- 0
  blankurl <- "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=DESCRIBE%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FBBLLAANNKK%3E&format=text%2Fcsv"
  terloc.url <- sub("BBLLAANNKK",terra.places[vr],blankurl)
  terfile <- read.csv(url(terloc.url),stringsAsFactors = FALSE)
  # removing large area
  indexarea<- which(terfile$predicate=="http://dbpedia.org/ontology/PopulatedPlace/areaTotal")
  if (length(indexarea) != 0) {area <- terfile$object[indexarea]}
  area <- as.numeric(area)
  if (area > 7100) {terra.delete <- c(terra.delete,terra.places[vr])}
  # removing places without coordinates
  indexdeathlon<- which(terfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#long")
  if (length(indexdeathlon) == 0) {terra.delete <- c(terra.delete,terra.places[vr])}
  # extracting location coords for valid places
  if (length(indexdeathlon) != 0) {
    indexlon<- which(terfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#long")
    lon <- terfile$object[indexlon]
    indexlat<- which(terfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#lat")
    lat <- terfile$object[indexlat]
    terra.coords <- rbind(terra.coords,c(terra.places[vr],lon, lat))
  }
}
# merge the old and new coords
frame.coords <- data.frame(terra.coords[,1], terra.coords[,2], terra.coords[,3], stringsAsFactors = FALSE)
names(frame.coords) <- c("places","lon","lat")
mergedcoords <- rbind(oldcoords,frame.coords)
names(mergedcoords) <- c("places","lon","lat")
terra.frame <- data.frame(terra.delete)
names(terra.frame) <- c("null.places")
newframe <- rbind(null.places,terra.frame)
write.csv(newframe, file = "null-places.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
write.csv(mergedcoords, file = "allcoords.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
