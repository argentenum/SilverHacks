#check if coordinates are correct in allplacedata.csv
setwd("~/ownCloud/iaclals-scraping")
applacedata <- read.csv("allplacedata.csv", stringsAsFactors = FALSE, header = TRUE)
library(maps)
library(geosphere)
library(magrittr)
library(leaflet)
library(htmltools)



redMapIcon <- makeIcon(
  iconUrl = "http://gkv.com/wp-content/uploads/leaflet-maps-marker-icons/map_marker-red-small.png",
  iconWidth = 10, iconHeight = 13,
  iconAnchorX = 0, iconAnchorY = 0
)

leaflet(data = applacedata) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(applacedata$terra.places), 
            icon = redMapIcon,  label = ~as.character(applacedata$terra.places,
            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)))


  leaflet() %>%   
  addTiles() %>%   
  setView(78.95,21.15, zoom = 4.49) %>%   
  addMarkers(lng = applacedata$long, lat =  applacedata$lat, icon= redMapIcon, label = applacedata$terra.places )%>%
  addControl(html = html_legend, position = "bottomright")%>%
  addLegend("topright", colors= "#ffa500", labels="Field", title="Life Journey")
  