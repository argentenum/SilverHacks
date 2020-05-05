#to check if cleanrecord.csv has 7 rows per person
setwd("~/ownCloud/iaclals-scraping")
debugrecord <- read.csv(file = "cleanrecord.csv", stringsAsFactors = FALSE, header = TRUE)
locateperson <- which(debugrecord$V1=="Person Name")
probindex <- NULL
for (yup in 1:(length(locateperson)-1))
  {if ((locateperson[yup+1] - locateperson[yup])!=7) {probindex <- c(probindex,locateperson[yup])}
}
probnames <- debugrecord$V2[probindex]
#probindex contains the locations where the problematic personname occurs
#removing the problematic personnames
delete.rowsall <- NULL
for (tup in 1:length(probindex))
  {nextindex <- which(debugrecord$V1[(probindex[tup]+1):length(debugrecord$V1)] == "Person Name")
  delete.rows <- c(probindex[tup]:(probindex[tup]+(nextindex[1]-1)) )
  delete.rowsall <- c(delete.rowsall,delete.rows)
}
if (length(delete.rowsall)>0) {freshrecord <- debugrecord[-delete.rowsall,]
write.csv(freshrecord, file = "cleanrecord.csv", sep = ",", append = FALSE, 
          quote = FALSE, col.names = FALSE, row.names = FALSE)
cat("This may require further cleaning")}
if (length(delete.rowsall)==0){cat("This is a clean file")}
