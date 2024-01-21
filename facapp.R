#do this the first time you run this script
install.packages("pdftools")
install.packages("dplyr")

#Henceforth just run from here on. You may delete the previous lines
setwd("~/facapp2302/OTHERS/") #set the path to the folder where you have downloaded
#the applications
#change the date in line 63 to the last date of applications
library(pdftools)
library(dplyr)



file.vector <- list.files(path = "~/facapp2302/OTHERS/")
pdf.list <- file.vector[grepl(".pdf",file.vector)]

reg.number <- NULL
name <- NULL
postapp <- NULL
email <- NULL
better.dob <- NULL
agenow <- NULL
categ <- NULL
nation <- NULL
phyh <- NULL
gender <- NULL
phd.inst <- NULL
phd.dt <- NULL
ba.perct <- NULL
ba.class <- NULL
ma.perct <- NULL
ma.class <- NULL
specialization <- NULL
current <- NULL
j.india <- NULL
proc.india <- NULL
j.int <- NULL
proc.int <- NULL
conf.india <- NULL
conf.int <- NULL
book.ch <- NULL
book <- NULL
journal <- NULL

for (ton in 1:length(pdf.list))
{
ready1 <- pdf_text(pdf.list[ton])
cover1 <- grep("G. Referees", ready1)
ready2 <- ready1[1:cover1[1]]
ready3 <- ready2 %>% strsplit(split = "\n")
ready4 <- unlist(ready3)
reg.number[ton] <- substr(ready4[1],unlist(gregexpr(pattern ='IITD/',ready4[1])),nchar(ready4[1]))
name.pos <- grep("  Name:                                           ", ready4)
name[ton] <- trimws(gsub("  Name:                                          ","",ready4[name.pos]))
post.pos <- grep("Post Applied For:                              ", ready4)
postapp[ton] <- trimws(gsub("Post Applied For:                            ","",ready4[post.pos]))
email.pos <- grep("Email Id:                                      ", ready4)
email[ton] <- trimws(gsub("Email Id:                                      ","",ready4[email.pos]))
dob.pos <- grep("Date of Birth:                                  ", ready4)
dob <- trimws(gsub("Date of Birth:","",ready4[dob.pos]))
dob.clean <- gsub("/","",dob)
better.dob[ton] <- as.Date(dob.clean, format = "%d %b %Y")
agenow[ton] <- round(((as.numeric(as.Date("2023-12-31")  - better.dob[ton]) )/365.25), digits = 1)
categ.pos <- grep("Category:                                      ", ready4)
categ[ton] <- trimws(gsub("Category:                                      ","",ready4[categ.pos]))
nation.pos <- grep("Nationality:                                   ", ready4)
nation[ton] <- trimws(gsub("Nationality:                                   ","",ready4[nation.pos]))
phyh.pos <- grep("Physically Handicapped:", ready4)
phyh[ton] <-  trimws(gsub("Physically Handicapped:","",ready4[phyh.pos]))
gender.pos <- grep("Gender:", ready4)
gender[ton] <- trimws(gsub("Gender:","",ready4[gender.pos]))
phd.pos <- grep("Academic Record-PhD", ready4)
phd.gre <- gregexpr(pattern = "University/Institute  ", ready4[phd.pos+1])
phd.inst[ton] <- substr(ready4[phd.pos+3],as.numeric(phd.gre), (as.numeric(phd.gre)+22))
phd.dot <- gregexpr(pattern = "Date of Defence of", ready4[phd.pos+1])
phd.dt[ton] <- substr(ready4[phd.pos+3],as.numeric(phd.dot), (as.numeric(phd.dot)+18))
otr.pos <- grep("Academic Record-Others", ready4)
perct.dot <- gregexpr("Marks/C", ready4[otr.pos+2])
ba.pos <- grep("Bachelor of Arts - B.A.", ready4)
ma.pos <- grep("Master of Arts - M.A.|M.Sc", ready4)
mphil.pos <- grep("Master of Philosophy", ready4)
if (length(ba.pos) > 0) {ba.perct[ton] <- as.numeric(substr(ready4[ba.pos],as.numeric(perct.dot),(as.numeric(perct.dot)+7)))}
    else {ba.perct[ton] <- "NA"}
if (length(ma.pos) > 0) {ma.perct[ton] <- as.numeric(substr(ready4[ma.pos],as.numeric(perct.dot),(as.numeric(perct.dot)+7)))}
    else {ma.perct[ton] <- "NA"}
class.dot <- gregexpr("Division/Grad", ready4[otr.pos+1])
if (length(ba.pos) > 0) {ba.class[ton] <- substr(ready4[ba.pos],as.numeric(class.dot),(as.numeric(class.dot)+13))}
    else {ba.class[ton] <- "NA"}
if (length(ma.pos) > 0) {ma.class[ton] <- substr(ready4[ma.pos],as.numeric(class.dot),(as.numeric(class.dot)+13))}
    else {ma.class[ton] <- "NA"}
special.pos <- grep("Area of Specialization: ", ready4)
specialization[ton] <- gsub("Area of Specialization: ","",ready4[special.pos])
curspe.pos <- grep("Current Area of Research: ", ready4)
current[ton] <- gsub("Current Area of Research: ","",ready4[curspe.pos])
pub.pos <- grep("E. Publications", ready4)
j.india[ton] <- as.numeric(substr(ready4[pub.pos+2],60,70))
proc.india[ton] <- as.numeric(substr(ready4[pub.pos+3],60,70))
conf.india[ton] <- as.numeric(substr(ready4[pub.pos+5],60,70))
j.int[ton] <- as.numeric(substr(ready4[pub.pos+2],86,99))
proc.int[ton] <- as.numeric(substr(ready4[pub.pos+3],86,99))
conf.int[ton] <- as.numeric(substr(ready4[pub.pos+5],86,99))
book.ch[ton] <- as.numeric(substr(ready4[pub.pos+4],133,150))
bkst.pos <- grep("E.2. Books", ready4)
bken.pos <- grep("F. EXPERIENCE", ready4)
if ((bken.pos - bkst.pos)>2) {
for (bk in (bkst.pos+2):(bken.pos-1)) {book[ton] <- paste(book[ton],substr(ready4[bk],43,66),collapse = '')}
} else {book[ton] <- 0}

jnst.pos <- grep("E.1. Best Papers", ready4)
jnen.pos <- grep("E.2. Books", ready4)
if ((jnen.pos - jnst.pos)>2) {
  for (jn in (jnst.pos+2):(jnen.pos-1)) {journal[ton] <- paste(journal[ton],substr(ready4[jn],78,114),collapse = '')}
} else {journal[ton] <- 0}

}
df <- data_frame(pdf.list, reg.number, name,postapp,email,agenow, categ,nation,phyh,
                 gender,phd.inst,phd.dt, ba.perct,ba.class,ma.perct,
                 ma.class,specialization,current,j.india,proc.india,j.int,proc.int,conf.india,conf.int,book.ch,
                 book, journal)
names(df) <- c("File Name", "Reg", "Name","Position", "Email", "Age",  "Category", "Nationality", "PH", "Gender", "PhD. Institution",
              "PhD Defence", "BA %", "BA Class", "MA%", "MA Class", "Specialization", "Current Research",
              "Journal India", "Proceedings India", "Journal International", "Proceedings INternational", 
              "Conferences India", "Proceedings International", "Book Chapters", "Books", "Journals")
write.csv(df,"all.csv")
