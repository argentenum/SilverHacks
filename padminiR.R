#merge nodes with date in all rows
#so that each word count has alongwith it the corresponding node and the date
pad.data <- read.csv("~/Documents/wiki-docs/padminiR/data/padmini-data.csv", header = TRUE)
head(pad.data)
unique(pad.data$word)
pad.date <- read.csv("~/Documents/wiki-docs/padminiR/data/padmini-url-node.csv", header = TRUE)
head(pad.date)
pad.mix <- merge(pad.data,pad.date)
write.csv(pad.mix,'~/Documents/wiki-docs/padminiR/data/pad.mix.csv', row.names = F)
library(dplyr)
pad.mix %>% 
  group_by(word) %>% 
  summarise(word.sum=sum(count)) %>% 
  arrange(desc(word.sum)) %>%
  write.csv('~/Documents/wiki-docs/padminiR/data/pad.arrg.csv', row.names = F)
#use pad.arrg.csv to select top 60 words
#save results as pad.top.csv
pad.topwords <- read.csv("~/Documents/wiki-docs/padminiR/data/pad.topwords.csv", header = TRUE, stringsAsFactors = FALSE)
pad.mix <- read.csv("~/Documents/wiki-docs/padminiR/data/pad.mix.csv", header = TRUE, stringsAsFactors = FALSE)
pad.len <- length(pad.topwords$word)
library(ggplot2)
library(dplyr)
#trying to learn plot on padmini
#looping and plotting using plot
pdf("padgraphs.pdf")
for(pad.focus in pad.topwords$word) {
  pad.mix %>%
    filter(word == pad.focus) %>%
    mutate(node.small = node/10000000) %>%
    arrange(node) %>%
    write.csv('~/Documents/wiki-docs/padminiR/data/pad.new.csv', row.names = F)
  pad.new <- read.csv("~/Documents/wiki-docs/padminiR/data/pad.new.csv") 
  plot(pad.new$node.small,pad.new$count,
       xlab = "Wikipedia Node/10^7", ylab = "Word Count",
       xlim=c(0, 100),
       sub = "in Wikipedia page for Rani Padmini")
  lines(pad.new$node.small,pad.new$count, type = "l", col = "red")
  title(pad.new$word[1])
}
dev.off()