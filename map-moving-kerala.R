#adding flightpaths with help from https://stackoverflow.com/questions/50333682/different-colors-for-flight-routes-leaflet-with-r
setwd("~/ownCloud/iaclals-scraping")
library(maps)
library(geosphere)
library(magrittr)
library(leaflet)
library(htmltools)
library(devtools)
library(webshot)


yeardiv <- data.frame(read.csv(file = "cleanoutput.csv", stringsAsFactors = FALSE)) 
#creating a new columns for residence year
#we will give last 40 years of life as years of residence in the last mentioned city
#one column for the beginning of residence and another for the end
#in the cleanoutput.csv all living persons are given death year as "9999"
res.start <- NULL
res.end <- NULL
currentyear <- as.numeric(substr(Sys.Date(),1,4))
for (yr in 1:nrow(yeardiv))
  {#we will do this with a nested loop
  #first isolate the living cases and then do the dead
  if (yeardiv$death.year[yr] == 9999) {res.end[yr] <- currentyear} else
  {res.end[yr] <- yeardiv$death.year[yr]}
  if (((yeardiv$birth.year[yr]) + 40) > currentyear) {res.start[yr] <- currentyear} else
  {res.start[yr] <-  yeardiv$birth.year[yr] + 40}
}
yeardiv['res.start'] <- res.start
yeardiv['res.end'] <- res.end

#figuring out the first  birth year in the whole set
#last year is the current year
# that's easy min(yeardiv$birth.year)

blueMapIcon <- makeIcon(
  iconUrl = "http://www.portlandchronicle.com/wp-content/uploads/leaflet-maps-marker-icons/blue%20good.png",
  iconWidth = 6, iconHeight = 7,
  iconAnchorX = 0, iconAnchorY = 0
)

redMapIcon <- makeIcon(
  iconUrl = "http://gkv.com/wp-content/uploads/leaflet-maps-marker-icons/map_marker-red-small.png",
  iconWidth = 10, iconHeight = 13,
  iconAnchorX = 0, iconAnchorY = 0
)


# birframe would now be a moving spot from birth till reaching res.start
birspot <- NULL
resspot <- NULL
for (mr in seq(1840,currentyear, by = 0.1))
  {birspot <- which(yeardiv$birth.year <= mr & yeardiv$res.start > mr)
  resspot <- which(yeardiv$res.start <= mr & yeardiv$res.end >= mr)
  # birspot gives the rows in which the mr year falls within birthyear and resstart
  #now we need to get the coordinates for the that year
  # that should be calculated on the basis of the ratio of the span between two years and the mr
  # so let's first calulate the ratio
  movratio <- abs((mr-yeardiv$birth.year[birspot])/(yeardiv$res.start[birspot] - yeardiv$birth.year[birspot]))
  # now calculating lon and lat
  movlon <- yeardiv$birthlon[birspot] + (yeardiv$deathlon[birspot] - yeardiv$birthlon[birspot])*movratio
  movlat <- yeardiv$birthlat[birspot] + (yeardiv$deathlat[birspot] - yeardiv$birthlat[birspot])*movratio
  birframe <- yeardiv[birspot,]
  resframe <- yeardiv[resspot,]
  birframe['birthlon'] <- movlon
  birframe['birthlat'] <- movlat
#now we have 2 frames
#one for birth year matches: only birth coords to be used
#one for residence year matches: only death coord to be used ( same as residence)
#the two are not of equal length

resframe$deathlon <- jitter(resframe$deathlon, factor = 1)
resframe$deathlat <- jitter(resframe$deathlat, factor = 1)



html_legend <- "Year<br/><img src='http://www.portlandchronicle.com/wp-content/uploads/leaflet-maps-marker-icons/blue%20good.png'>Birth<br/>
<img src='http://gkv.com/wp-content/uploads/leaflet-maps-marker-icons/map_marker-red-small.png'>Life"

your.map <- gcIntermediate(yeardiv[,c(4,5)], yeardiv[,c(6,7)],
                           n=100,   
                           addStartEnd=TRUE,  
                           sp=TRUE) %>%   
  leaflet() %>%   
  addTiles() %>%   
  setView(76.4,10.6, zoom = 6.4) %>%   
#  addPolylines(color="azure3", weight = 1) %>%   
  addMarkers(lng=c(birframe[,4]),lat=c(birframe[,5]), icon= blueMapIcon )%>%
  addMarkers(lng=c(resframe[,6]),lat=c(resframe[,7]), icon= redMapIcon)%>%
  addControl(html = html_legend, position = "bottomright")%>%
  addLegend("topright", colors= "#ffa500", labels= as.character(floor(mr)) , title="Year")

outfile <- paste(mr,"-edit.png",sep="")

library(htmlwidgets)
saveWidget(your.map, file="m.html")
webshot("m.html", file = outfile, vwidth = 400,
        vheight = 425, cliprect = NULL, selector = NULL, expand = 5,
        delay = 0.2, zoom = 6.4, eval = NULL, debug = FALSE,
        useragent = NULL)
}
#        cliprect = "viewport")


#  save_html(your.map, file = "temp.html")
