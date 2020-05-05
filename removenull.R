setwd("~/ownCloud/iaclals-scraping")
with.null <- read.csv("biorecord.csv", stringsAsFactors = FALSE, header = FALSE)
allcoords <- read.csv(file = "allcoords.csv", stringsAsFactors = FALSE, header = TRUE)
#removing all places not in allcoords.csv
terra.birth <- which(with.null$V1=="Birth Place")
terra.death <- which(with.null$V1=="Death Place")
terra.rows <- c(terra.birth,terra.death)
for (ete in 1:length(terra.rows))
{if (with.null$V2[terra.rows[ete]] %in% allcoords$places) {} else {with.null$V2[terra.rows[ete]] <- "NA"}
}
# null.places <- read.csv("null-places.csv", stringsAsFactors = FALSE, header = FALSE)
# removing rows with names of larger places as per manually prepared list
# without.null <- with.null[ !with.null$V2 %in% null.places$V1, ]

#this is modified cleanrecord which does not reove any place names on account of being large area

#retaining only the first mentioned place of birth and death
#how to retain the most precise place name? may be can be done if allcoords.csv can be ranked - to do
without.null <- with.null
person.row <- which(without.null$V1=="Person Name")
for.removal <- NULL
for (pr in 1:(length(person.row)-1))
     {batch.removal <- NULL
      birth.sum <- which(without.null$V1[person.row[pr]:person.row[(pr+1)]]=="Birth Place")
      death.sum <- which(without.null$V1[person.row[pr]:person.row[(pr+1)]]=="Death Place")
      if (length(birth.sum) > 1) {batch.removal <- c(batch.removal,birth.sum[-1])}
      if (length(death.sum) > 1) {batch.removal <- c(batch.removal,death.sum[-1])}
      for.removal <- c(for.removal, (batch.removal+person.row[pr]))
}
# need to do it for the last infobox
# modify the statements in the for loop
batch.removal <- NULL
birth.sum <- which(without.null$V1[person.row[pr+1]:length(without.null$V1)]=="Birth Place")
death.sum <- which(without.null$V1[person.row[pr+1]:length(without.null$V1)]=="Death Place")
if (length(birth.sum) > 1) {batch.removal <- c(batch.removal,birth.sum[-1])}
if (length(death.sum) > 1) {batch.removal <- c(batch.removal,death.sum[-1])}
for.removal <- c(for.removal, (batch.removal+person.row[pr]))
for.removal <- for.removal-1
without.double <- data.frame(without.null$V1[-for.removal],without.null$V2[-for.removal], stringsAsFactors = FALSE)
names(without.double) <- c("V1","V2")       
# if birth or death place row is missing add row with NA value
person.row <- which(without.double$V1=="Person Name")
person.names <- without.double$V2[person.row]
birth.force <- data.frame("Birth Place","NA")
names(birth.force) <- c("V1","V2")
death.force <- data.frame("Death Place","NA")
names(death.force) <- c("V1","V2")
for (qr in 1:length(person.names))
  {sr <- length(without.double$V2)
    birth.insert <- 0
    name.row <- which(without.double$V2 == person.names[qr])
      if (without.double$V1[name.row+1]!="Birth Place") {birth.insert <- 1}
      if (without.double$V1[name.row+3]!="Death Place") {death.insert <- 1}
      if (birth.insert == 1) {without.double <- rbind(without.double[1:name.row,],birth.force[1,],without.double[(name.row+1):sr,], stringsAsFactors = FALSE )}  
    
}

for (tr in 1:length(person.names))
  {ur <- length(without.double$V2)
  death.insert <- 0
  name.row <- which(without.double$V2 == person.names[tr])
    if (without.double$V1[name.row+1]!="Birth Place") {birth.insert <- 1}
    if (without.double$V1[name.row+3]!="Death Place") {death.insert <- 1}
    if (death.insert == 1) {without.double <- rbind(without.double[1:(name.row+2),],death.force[1,],without.double[(name.row+3):ur,], stringsAsFactors = FALSE)} 
   
}

write.csv(without.null, file = "without.null.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
write.csv(without.double, file = "cleanrecord.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
