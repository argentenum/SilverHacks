#adding flightpaths with help from https://stackoverflow.com/questions/50333682/different-colors-for-flight-routes-leaflet-with-r
setwd("~/ownCloud/iaclals-scraping")
library(maps)
library(geosphere)
library(magrittr)
library(leaflet)
library(htmltools)
library(devtools)
library(webshot)
coord1 <- data.frame(read.csv(file = "living-output.csv", stringsAsFactors = FALSE)) 
coord1$birthlon <- jitter(coord1$birthlon, factor = 1)
coord1$birthlat <- jitter(coord1$birthlat, factor = 1)
coord1$deathlon <- jitter(coord1$deathlon, factor = 1)
coord1$deathlat <- jitter(coord1$deathlat, factor = 1)

blueMapIcon <- makeIcon(
  iconUrl = "http://www.portlandchronicle.com/wp-content/uploads/leaflet-maps-marker-icons/blue%20good.png",
  iconWidth = 10, iconHeight = 13,
  iconAnchorX = 0, iconAnchorY = 0
)

redMapIcon <- makeIcon(
  iconUrl = "http://gkv.com/wp-content/uploads/leaflet-maps-marker-icons/map_marker-red-small.png",
  iconWidth = 10, iconHeight = 13,
  iconAnchorX = 0, iconAnchorY = 0
)


html_legend <- "Year<br/><img src='http://www.portlandchronicle.com/wp-content/uploads/leaflet-maps-marker-icons/blue%20good.png'>birth<br/>
<img src='http://gkv.com/wp-content/uploads/leaflet-maps-marker-icons/map_marker-red-small.png'>death"

your.map <- gcIntermediate(coord1[,c(4,5)], coord1[,c(6,7)],
               n=100,   
               addStartEnd=TRUE,  
               sp=TRUE) %>%   
  leaflet() %>%   
  addTiles() %>%   
  setView(76.4,10.6, zoom = 8) %>%   
  addPolylines(color="azure3", weight = 1) %>%   
  addMarkers(lng=c(coord1[,4]),lat=c(coord1[,5]), icon= blueMapIcon, 
             label = paste(sep = ", ", coord1$Name, coord1$Birth.Place, coord1$birth.year) )%>%
  addMarkers(lng=c(coord1[,6]),lat=c(coord1[,7]), icon= redMapIcon, 
             label = paste(sep = ", ", coord1$Name, coord1$Death.Place, coord1$death.year))%>%
  addControl(html = html_legend, position = "bottomright")%>%
  addLegend("topright", colors= "#ffa500", labels="Field", title="Life Journey")
your.map

library(htmlwidgets)
saveWidget(your.map, file="m.html")
webshot("m.html", file = "kerala.png", vwidth = 800,
        vheight = 950, cliprect = NULL, selector = NULL, expand = 5,
        delay = 0.2, zoom = 8, eval = NULL, debug = FALSE,
        useragent = NULL)