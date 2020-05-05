allplacedata <- read.csv("allplacedata.csv", stringsAsFactors = FALSE, header = TRUE)



terra.places <- setdiff(livbirth$Death.Place, allplacedata$terra.places)
#removing cases of http error
library(httr)
hterr <- NULL
for (fr in 1:length(terra.places))
{blankurl <- "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=DESCRIBE%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FBBLLAANNKK%3E&format=text%2Fcsv"
terloc.url[fr] <- sub("BBLLAANNKK",terra.places[fr],blankurl)
if (http_error(terloc.url[fr])) {hterr <- c(hterr,fr)} 
}
terra.places <- terra.places[-hterr]
areaTotal <- NULL
populationTotal <- NULL
populationTotalRanking <- NULL
PopulatedPlace <- NULL
type <- NULL
hypernym <- NULL
longd <- NULL
latd <- NULL
long <- NULL
lat <- NULL
point <- NULL
country <- NULL
utcOffset <- NULL

areaTotal[1:length(terra.places)] <- NA
populationTotal[1:length(terra.places)] <- NA
populationTotalRanking[1:length(terra.places)] <- NA
PopulatedPlace[1:length(terra.places)] <- NA
type[1:length(terra.places)] <- NA
hypernym[1:length(terra.places)] <- NA
longd[1:length(terra.places)] <- NA
latd[1:length(terra.places)] <- NA
long[1:length(terra.places)] <- NA
lat[1:length(terra.places)] <- NA
point[1:length(terra.places)] <- NA
country[1:length(terra.places)] <- NA
utcOffset[1:length(terra.places)] <- NA
for (vr in 1:length(terra.places))
{blankurl <- "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=DESCRIBE%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FBBLLAANNKK%3E&format=text%2Fcsv"
terloc.url <- sub("BBLLAANNKK",terra.places[vr],blankurl)
terfile <- read.csv(url(terloc.url),stringsAsFactors = FALSE)
# getting areaTotal
indexareatotal<- which(terfile$predicate=="http://dbpedia.org/ontology/PopulatedPlace/areaTotal")
if (length(indexareatotal) > 0) {area <- terfile$object[indexareatotal]
areaTotal[vr] <- as.numeric(area)}
#getting populationTotal
indexpopulationTotal <- which(terfile$predicate=="http://dbpedia.org/ontology/populationTotal")
if(length(indexpopulationTotal) > 0) {populationTotal[vr] <- as.numeric(terfile$object[indexpopulationTotal])}
#getting populationTotalRanking
indexpopulationTotalRanking<- which(terfile$predicate=="http://dbpedia.org/ontology/populationTotalRanking")
if (length(indexpopulationTotalRanking) > 0) {populationTotalRanking[vr] <- as.numeric(terfile$object[indexpopulationTotalRanking])}
#getting PopulatedPlace
indexPopulatedPlace <- which(terfile$predicate=="http://dbpedia.org/ontology/PopulatedPlace/areaTotal")
if (length(indexPopulatedPlace) > 0) {PopulatedPlace[vr] <- as.numeric(terfile$object[indexPopulatedPlace])}
#getting type
indextype <- which(terfile$predicate=="http://dbpedia.org/ontology/type")
if (length(indextype) >0) {type[vr] <- terfile$object[indextype]}
#getting hypernym
indexhypernym <- which(terfile$predicate=="http://purl.org/linguistics/gold/hypernym")
if (length(indexhypernym) > 0) {hypernym[vr] <- terfile$object[indexhypernym]}
#getting longd
indexlongd <- which(terfile$predicate=="http://dbpedia.org/property/longd")
if (length(indexlongd) > 0) {longd[vr] <- as.numeric(terfile$object[indexlongd])}
#getting latd
indexlatd <- which(terfile$predicate=="http://dbpedia.org/property/latd")
if (length(indexlatd) > 0) {latd[vr] <- as.numeric(terfile$object[indexlatd])}
#getting long
indexlong <- which(terfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#long")
if (length(indexlong) > 0) {long[vr] <- as.numeric(terfile$object[indexlong])}
#getting lat
indexlat <- which(terfile$predicate=="http://www.w3.org/2003/01/geo/wgs84_pos#lat")
if (length(indexlat) > 0) {lat[vr] <- as.numeric(terfile$object[indexlat])}
#getting point
indexpoint <- which(terfile$predicate=="http://www.georss.org/georss/point")
if (length(indexpoint) > 0) {point[vr] <- terfile$object[indexpoint]}
#getting country
indexcountry <- which(terfile$predicate=="http://dbpedia.org/ontology/country")
if (length(indexcountry) > 0)  {country[vr] <- terfile$object[indexcountry]}
#getting utcOffset
indexutcOffset <- which(terfile$predicate=="http://dbpedia.org/ontology/utcOffset")
if (length(indexutcOffset) >0) {utcOffset[vr] <- terfile$object[indexutcOffset]}
}
newplacedata <- data.frame(terra.places, areaTotal, populationTotal, populationTotalRanking, PopulatedPlace, type, hypernym, longd,
                           latd, long, lat, point, country, utcOffset)
write.csv(newplacedata, file = "newplacedata.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
