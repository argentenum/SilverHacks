setwd("~/ownCloud/fb-scraping")
directory <- "~/ownCloud/fb-scraping/ritwik-naskar.txt"
fb.lines <- readLines(directory)
page.owner <- fb.lines[1]
today <- fb.lines[2]
yesterday <- fb.lines[3]
lms <- as.Date(c("2018-01-01","2018-05-30"))
#if limits are change enter change in ggplot x_scale_date beloe
current.year <- "2018"
#check position of header of each post from the name of the page owner
post.position <- grep(page.owner,fb.lines)
post.position <- c(post.position,grep(" others.",fb.lines))
post.position <- unique(post.position)
#extract post header
post.head <- fb.lines[post.position]
post.head <- trimws(post.head)
#extract post time date and place if any
post.moment <- fb.lines[post.position+1]
post.moment <- trimws(post.moment)
#removing any row where the page owner is tagged in a post, thus false row
#this is done through checking for the presence of the seperator "·" in post.moment
true.row <- NULL
for (fed in 1:length(post.moment))
{if (grepl("·",post.moment[fed])) {true.row <- c(true.row,fed)}
}
post.position <- post.position[true.row]
post.head <- post.head[true.row]
post.moment <- post.moment[true.row]
#removing any row where the page owner has commented on a post
false.row <- NULL
for (red in 1:length(post.moment))
{if (grepl(paste(page.owner," ·"),post.moment[red])) {false.row <- c(false.row,red)}
}
post.position <- post.position[-false.row]
post.head <- post.head[-false.row]
post.moment <- post.moment[-false.row]
length(fb.lines)
all.posts <- NULL
shares <- NULL
position.sq <- c(1:(length(post.position)-1))
for (x in position.sq)
{ start.search <- post.position[x]
  end.search <- post.position[x+1]
  search.zone <- fb.lines[start.search:end.search]
  end.zone <- grep("Like", search.zone)
  if (length(end.zone)==0) {end.zone<- c(4)}  
  post.vector <- fb.lines[(start.search+3):(start.search+end.zone[1]-1)]
  all.posts[x] <- paste(post.vector, collapse = ' ')
}
remaining.post <- fb.lines[(post.position[x+1]):length(fb.lines)]
all.posts[x+1] <- paste(remaining.post, collapse = ' ')
#count no. of shares
for (y in position.sq)
{ shares[y] <- 0
  start.search <- post.position[y]
  end.search <- post.position[y+1]
  search.zone <- fb.lines[start.search:end.search]
  comments.button <- grep("Comments",search.zone)
  if (length(comments.button) == 0) {comments.button <- 2}
  share.phrase <- search.zone[comments.button-1]
  share.phrase <- trimws(share.phrase)
  if (grepl("share",share.phrase)) {shares[y] <- as.numeric(substring(share.phrase,1,(regexpr("share", share.phrase)-2)))}
}
shares[y+1] <- 0
comments.last <- grep("Comments",remaining.post)
share.last <- remaining.post[comments.button-1]
if (grepl("share",share.last)) {shares[y+1] <- as.numeric(substring(share.last,1,(regexpr("share", share.last)-2)))}
#check type of post
post.identity <- read.csv("~/Documents/fb-scraping/post.identity.csv", header = TRUE)
post.label <- NULL
post.label[1:length(post.position)] <- "selfpost"
for (z in 1:length(post.position))
{ if (grepl("added",post.head[z])) {post.label[z] <- "photos"}
  if (grepl("shared a",post.head[z])) {post.label[z] <- "share"}
  if (grepl("like",post.head[z])) {post.label[z] <- "advertisement"}
  if (grepl("is with",post.head[z])) {post.label[z] <- "taggedpost"}
  if (grepl("shared a photo to ",post.head[z])) {post.label[z] <- "taggedpost"}
  if (grepl("shared a video to ",post.head[z])) {post.label[z] <- "taggedpost"}
  if (grepl("feeling",post.head[z])) {post.label[z] <- "feeling"}
  if (grepl("updated",post.head[z])) {post.label[z] <- "profileupdate"}
  if (grepl("is at",post.head[z])) {post.label[z] <- "checkin"}
  if (grepl("is eating",post.head[z])) {post.label[z] <- "restaurant"}
  if (grepl("travelling to",post.head[z])) {post.label[z] <- "travel"}
  if (grepl("was live",post.head[z])) {post.label[z] <- "live video"}
}
#splitting date and place
post.datetime <- NULL
post.location <- NULL
for (k in 1:length(post.position))
{ dateplace <- strsplit(as.character(post.moment[k]),"·")
  vec.dateplace <- unlist(dateplace)
  post.datetime[k] <- vec.dateplace[1]
  post.location[k] <- trimws(vec.dateplace[2])
}
post.day <- NULL
post.time <- NULL
for (m in 1:length(post.position))
{ day <- unlist(strsplit(as.character(post.datetime[m])," at "))
  post.day[m] <- day[1]
  post.time[m] <- trimws(day[2], which = "right")
}
temp.date <- NULL
post.date <- NULL
post.month <- NULL
post.year <- NULL
temp.date[1:length(post.position)] <- "Nil"
post.date[1:length(post.position)] <- "Nil"
post.month[1:length(post.position)] <- "Nil"
post.year[1:length(post.position)] <- "Nil"
for (n in 1:length(post.position))
{ if (grepl("hrs",post.day[n])) {temp.date[n] <- today
  date.convert <- unlist(strsplit(temp.date[n]," "))
  post.date[n] <- date.convert[1]
  post.month[n] <- date.convert[2]
  post.year[n] <- date.convert[3]
  }
  if (grepl("Yesterday",post.day[n])) {temp.date[n] <- yesterday
  date.convert <- unlist(strsplit(temp.date[n]," "))
  post.date[n] <- date.convert[1]
  post.month[n] <- date.convert[2]
  post.year[n] <- date.convert[3]
  }
  if (temp.date[n]=="Nil") {date.convert <- unlist(strsplit(post.day[n]," "))
  post.date[n] <- date.convert[1]
  post.month[n] <- date.convert[2]
  post.year[n] <- date.convert[3]
  if (is.na(post.year[n])) {post.year[n] <- current.year}
  }
}
month.number <- NULL
temp.string <- NULL
month.number[1:length(post.position)] <- "00"
temp.string[1:length(post.position)] <- "00"
for (p in 1:length(post.position))
{ if (post.month[p]==month.name[1]) {month.number[p] <- "01"}
  if (post.month[p]==month.name[2]) {month.number[p] <- "02"}
  if (post.month[p]==month.name[3]) {month.number[p] <- "03"}
  if (post.month[p]==month.name[4]) {month.number[p] <- "04"}
  if (post.month[p]==month.name[5]) {month.number[p] <- "05"}
  if (post.month[p]==month.name[6]) {month.number[p] <- "06"}
  if (post.month[p]==month.name[7]) {month.number[p] <- "07"}
  if (post.month[p]==month.name[8]) {month.number[p] <- "08"}
  if (post.month[p]==month.name[9]) {month.number[p] <- "09"}
  if (post.month[p]==month.name[10]) {month.number[p] <- "10"}
  if (post.month[p]==month.name[11]) {month.number[p] <- "11"}
  if (post.month[p]==month.name[12]) {month.number[p] <- "12"}
  temp.string[p] <- paste(post.year[p],month.number[p],post.date[p], sep = "-")
}
date.numeric <- as.numeric(as.POSIXlt(temp.string, format="%Y-%m-%d"))
numeric.small <- date.numeric/100000000
date.month <- format(as.Date(as.POSIXct(date.numeric, origin="1970-01-01")), "%Y-%m-%d")
scraped.sheet <- data.frame(post.position,post.head,post.moment,all.posts,shares,post.label,post.day,post.date,post.month,post.year,post.time,date.numeric,date.month,post.location,numeric.small)
write.csv(scraped.sheet, file = paste(substring(basename(directory),1,(nchar(basename(directory))-4)),"-sheet.csv",sep = ""), row.names = FALSE)
hist(scraped.sheet$date.numeric)
selfposts <- subset(scraped.sheet, scraped.sheet$post.label == "selfpost")
taggedposts <- subset(scraped.sheet, scraped.sheet$post.label == "taggedpost")
all.together <- data.frame(scraped.sheet$date.month,taggedposts$date.month,selfposts$date.month)
library(ggplot2)
#sec.axis = sec_axis(~.*1, labels = format(as.Date(as.POSIXct(date.numeric, origin="1970-01-01")), "%Y-%b")))
#mapping by numeric.small
ggplot() +
  ggtitle(paste(page.owner," ",today)) +
  geom_histogram(mapping = aes(scraped.sheet$numeric.small), bins = 100) +
  scale_x_continuous(name = "Numeric Date/100000000")
#mapping by month
library(scales)
ggplot() +
  ggtitle(paste(page.owner)) +
  labs(x = "Month", y = "No. of Posts") +
  geom_histogram(mapping = aes(as.Date(scraped.sheet$date.month)), binwidth = 3, fill="red", colour="red", position = "dodge") +
  geom_histogram(mapping = aes(as.Date(taggedposts$date.month)), binwidth =2, fill="blue", colour="blue", position = "dodge") +
  geom_histogram(mapping = aes(as.Date(selfposts$date.month)), binwidth =1, fill="orange", colour="orange", position = "dodge") +
  scale_colour_manual(name="group", values=c("r" = "red", "b"="blue", "o"="orange"), labels=c("b"="blue values", "r"="red values", "o"="orange")) +
  scale_fill_manual(name="group", values=c("r" = "red", "b"="blue", "o"="orange"), labels=c("b"="blue values", "r"="red values", "o"="orange")) +
  scale_x_date(labels = date_format("%m-%Y"), limits = as.Date(c("2018-01-01","2018-05-30")))
ggsave(paste(substring(basename(directory),1,(nchar(basename(directory))-4)),"-bar.pdf",sep = ""))
#mapping types of posts
lbls <- paste(names(table(post.label)),",",table(post.label))
png(filename = paste(substring(basename(directory),1,(nchar(basename(directory))-4)),"-pie.png",sep = ""), width = 960, height = 960, units = "px")
pie(table(post.label), labels = lbls, main = paste(page.owner," ",today," \n Types of Posts"))
dev.off()
#extracting tagging relationships
tag.rel <- data.frame(taggedposts$post.head,taggedposts$date.month,taggedposts$date.numeric,
                      taggedposts$post.time,taggedposts$shares, stringsAsFactors = FALSE)
names(tag.rel) <- c("post.head","date.month","date.numeric","post.time","shares")
tag.rel$post.head <- as.character(tag.rel$post.head)
post.by <- NULL
tag.friend <- NULL
post.by[1:length(tag.rel$post.head)] <- " "
tag.friend[1:length(tag.rel$post.head)] <- page.owner
for (s in 1:length(tag.rel$post.head))
{ temp.grab1 <- unlist(strsplit(tag.rel$post.head[s]," is with | shared a photo to | shared a video to "))
  temp.grab2 <- unlist(strsplit(temp.grab1[2]," and "))
  temp.grab2 <- unlist(strsplit(temp.grab2[1]," in "))
  temp.grab2 <- unlist(strsplit(temp.grab2[1]," at "))
  post.by[s] <- temp.grab1[1]
  if (post.by[s]==page.owner) {tag.friend[s] <- temp.grab2[1]}
}
tag.rel <- data.frame(tag.rel,post.by,tag.friend)
tag.in <- subset(tag.rel, !(tag.rel$post.by == page.owner))
#creating adjacency table
library(igraph)
dat <- data.frame(tag.in$post.by,tag.in$tag.friend, row.names = NULL)
names(dat) <- c("post.by","tag.friend")
who.tag.whom <- get.adjacency(graph.edgelist(as.matrix(dat), directed=FALSE))
png(filename = paste(substring(basename(directory),1,(nchar(basename(directory))-4)),"-net.png",sep = ""), width = 4800, height = 4800, units = "px", pointsize = 48)
plot(graph.adjacency(who.tag.whom, mode="directed", weighted=TRUE))
dev.off()
library(MASS)
write.matrix(who.tag.whom, paste(substring(basename(directory),1,(nchar(basename(directory))-4)),"-adjacency.csv",sep = ""))
#subset and save only instances of tag-in for later consolidation
write.csv(tag.in,paste(substring(basename(directory),1,(nchar(basename(directory))-4)),"-tag.history.csv",sep = ""),row.names = FALSE)
