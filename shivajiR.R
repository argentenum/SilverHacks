#merge nodes with date in all rows
#so that each word count has alongwith it the corresponding node and the date
count.data <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/shivaji-data.csv", header = TRUE)
head(count.data)
unique(count.data$word)
node.date <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/shivaji-date-node.csv", header = TRUE)
head(node.date)
big.mix <- merge(count.data,node.date)
write.csv(big.mix,'~/Documents/wiki-docs/shivaji/shivajiR/big.mix.csv', row.names = F)
#grouping by word
library(dplyr)
big.mix %>% 
  group_by(word) %>% 
  summarise(word.sum=sum(count)) %>% 
  arrange(desc(word.sum)) %>%
  write.csv('~/Documents/wiki-docs/shivaji/shivajiR/data/totcount.arrg.csv', row.names = F)
#use totcount.arrg.csv to select top 60 words
#save results as topwords.csv
topwords <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/topwords.csv", header = TRUE, stringsAsFactors = FALSE)
big.mix <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/big.mix.csv", header = TRUE, stringsAsFactors = FALSE)
top.len <- length(topwords$word)
library(ggplot2)
library(dplyr)
#trying to learn plot on padmini
#looping and plotting using plot
#the file year.axis has to be created manually
#matching the first edit of every year with the node fraction or node.small
year.axis <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/year.axis.csv", header = TRUE)
pdf("shiv-mar18.pdf", paper = "a4r")
for(top.focus in topwords$word) {
  big.mix %>%
    filter(word == top.focus) %>%
    filter(node > 820000000) %>%
    mutate(node.small = node/10000000) %>%
    arrange(node) %>%
    write.csv('~/Documents/wiki-docs/shivaji/shivajiR/data/top.new.csv', row.names = F)
  top.new <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/top.new.csv") 
  plot(top.new$node.small,top.new$count,
       xlab = "", ylab = "Word Count")
  lines(top.new$node.small,top.new$count, type = "l", col = "red")
  axis(1, at = year.axis$node.fraction, labels = year.axis$year, col = "red", line = 2)
  title(top.new$word[1])
  mtext("Node/10^7",1, line = 0, at = -5)
  mtext("Year",1, line = 2, at = -5)
}
dev.off()
#plotting total word count of each version page
#wordcounts done through terminal
#matching total wordcount to date
pad.date <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/shivaji-date-node.csv", header = TRUE)
pad.total <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/total.filecount.csv", header = TRUE)
pad.total <- merge(pad.date,pad.total)
head(pad.total)
#reducing date to smaller decimals
#reducing node size
pad.total %>% 
  mutate(date.small = date.time/10000000000) %>% 
  mutate(node.small = node/10000000) %>%
  write.csv('~/Documents/wiki-docs/shivaji/shivajiR/data/grand.total.csv', row.names = F)
grand.total <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/grand.total.csv", header = TRUE)
year.axis <- read.csv("~/Documents/wiki-docs/shivaji/shivajiR/data/year.axis.csv", header = TRUE)
plot(grand.total$node.small,grand.total$totalcount, xlab = "", ylab = "Wordcount")
lines(grand.total$node.small,grand.total$totalcount, type = "l", col = "green")
axis(1, at = year.axis$node.fraction, labels = year.axis$year, col = "red", line = 2)
title(main = "Total count")
mtext("Node/10^7",1, line = 0, at = -5)
mtext("Year",1, line = 2, at = -5)