# extract infobox data from dbpedia
setwd("~/ownCloud/iaclals-scraping")
# begin by creating a blank file with 
# creating blank csv file to write data
# write.csv(as.matrix(t(c("Label","Data"))), file="biorecord.csv",row.names=FALSE, col.names=FALSE)
#reading listofpersons.csv
person.withextra <- read.csv(file = "listofpersons.csv", stringsAsFactors = FALSE)
# deduplicating listofpersons.csv
person.list <- person.withextra[!duplicated(person.withextra$Person),]
#read person name and process in loop
lamba <- length(person.list$Person)
for (z in 1:lamba)
  {person.name <- person.list$Person[z]
  #beginning fresh person
  #person.name <- "Satyajit_Ray"
  #writing person name to csv
  personmatrix <- c("Person Name",person.name)
  personmatrix <- as.matrix(t(personmatrix))
  write.table(personmatrix, file = "biorecord.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  #reading the csv file from DBpedia
  blankurl <- "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=DESCRIBE%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2FBBLLAANNKK%3E&format=text%2Fcsv"
  newurl <- sub("BBLLAANNKK",person.name,blankurl)
  fullfile <- read.csv(url(newurl),stringsAsFactors = FALSE)
  indexdeathplace <- which(fullfile$predicate=="http://dbpedia.org/ontology/deathPlace")
  #extracting and appending birth place
  indexbirthplace <- which(fullfile$predicate=="http://dbpedia.org/ontology/birthPlace")
  birthplace <- fullfile$object[indexbirthplace]
  birthplace <- sub("http://dbpedia.org/resource/","",birthplace[1:length(birthplace)])
  birthplacerow <- NULL
  birthplacerow[1:length(birthplace)]<- "Birth Place"
  birthplaceframe <- data.frame(birthplacerow, birthplace, stringsAsFactors = FALSE)
  write.table(birthplaceframe,file="biorecord.csv", sep = ",", append = TRUE, quote = FALSE, row.names=FALSE, col.names=FALSE)
  #extracting and appending birthdate
  indexbirthdate<- which(fullfile$predicate=="http://dbpedia.org/ontology/birthDate")
  birthdate <- fullfile$object[indexbirthdate]
  if (length(indexbirthdate) == 0) {birthdate = "NA"}
  birthdaterow <- NULL
  birthdaterow[1:length(birthdate)] <- "Birth Date"
  birthdateframe <- data.frame(birthdaterow, birthdate, stringsAsFactors = FALSE)
  firstbirthdate <- c(birthdaterow[1],birthdate[1])
  firstbirthdate <- as.matrix(t(firstbirthdate))
  write.table(firstbirthdate, file = "biorecord.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  #extracting and appending death place
  deathplace <- fullfile$object[indexdeathplace]
  deathplace <- sub("http://dbpedia.org/resource/","",deathplace[1:length(deathplace)])
  deathrow <- NULL
  deathrow[1:length(deathplace)] <- "Death Place"
  deathframe <- data.frame(deathrow,deathplace, stringsAsFactors = FALSE)
  write.table(deathframe,file="biorecord.csv", sep = ",", append = TRUE, quote = FALSE, row.names=FALSE, col.names=FALSE)
  #extracting and appending deathdate
  indexdeathdate<- which(fullfile$predicate=="http://dbpedia.org/ontology/deathDate")
  deathdate <- fullfile$object[indexdeathdate]
  deathdaterow <- NULL
  deathdaterow[1:length(deathdate)] <- "Death Date"
  #deathdateframe <- data.frame(deathdaterow, deathdate, stringsAsFactors = FALSE)
  firstdeathdate <- c(deathdaterow[1],deathdate[1])
  firstdeathdate <- as.matrix(t(firstdeathdate))
  write.table(firstdeathdate, file = "biorecord.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  #Writing person Field to csv
  field.name <- person.list$Field[z]
  fieldmatrix <- c("Field",field.name)
  fieldmatrix <- as.matrix(t(fieldmatrix))
  write.table(fieldmatrix, file = "biorecord.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  #appending a blank row
  blankrow <- c("","")
  blankrow <- as.matrix(t(blankrow))
  write.table(blankrow, file = "biorecord.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}

