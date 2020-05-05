setwd("~/ownCloud/fb-scraping/ju-presi")
get.files <- list.files('~/ownCloud/fb-scraping/ju-presi', full.names = TRUE)
get.files <- get.files[grep("-tag.history.csv",get.files)]
tag.info <- read.csv(get.files[1], stringsAsFactors = FALSE)
d <- NULL
for (x in 1: length(get.files))
{ d <- read.csv(get.files[x],stringsAsFactors = FALSE)
tag.info <- rbind(tag.info,d)
}
write.csv(tag.info,"tag.info.csv", row.names = FALSE)
deduped.tags <- unique(tag.info)
from.to <- data.frame(deduped.tags$post.by,deduped.tags$tag.friend, row.names = NULL)
names(from.to) <- c("from","to")
all.users <- rownames(get.adjacency(graph.edgelist(as.matrix(from.to), directed=FALSE)))
write.csv(data.frame(all.users, row.names = NULL),"all.users.jnu.csv")
library(igraph)
tag.all <- get.adjacency(graph.edgelist(as.matrix(from.to), directed=FALSE))
tag.graph <- graph_from_adjacency_matrix(tag.all)
mean.tag <- (ceiling(sqrt(mean(degree(tag.graph))))+1)
tag.size <- ifelse(degree(tag.graph)>(mean.tag^2),sqrt(degree(tag.graph))/mean.tag,1)
png(filename = "tag.map.png", width = 13500, height = 12000, units = "px")
par(bg = 'black')
plot(graph.adjacency(tag.all, mode="directed", weighted=TRUE), 
    edge.color = "pink", vertex.size= ifelse(tag.size>1,tag.size,0), edge.arrow.size=0.01, 
     vertex.color = ifelse(tag.size>1,"violetred",adjustcolor("indianred1", alpha.f = 0.1)), 
     vertex.label.color = ifelse(tag.size>1,"white", adjustcolor("white", alpha.f = 0.1)) , vertex.label.size = ifelse(tag.size>5,12,0))
dev.off()
#another attempt without labels - not working
#library(ggplot2)
#library(network)
#library(sna)
#tag.all.adj <- get.adjacency(graph.edgelist(as.matrix(from.to), directed=FALSE))
#ggnet2(tag.all.adj, node.size = 2, node.color = "red", edge.size = 1, edge.color = "pink")
#nw.tags <- network(tag.all.adj)
#network.vertex.names(nw.tags) <- all.users
#ggnet2(nw.tags, node.size = 0, node.color = "red", edge.size = 1, edge.color = "pink",label.nodes=all.users)
