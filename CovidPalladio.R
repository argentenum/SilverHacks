#start with TravelDF from SankeyCovid.R
PalladioTravel <- TravelDF[,c(1,3,4,5,6,7,8,10,11,12,13,19,20)]
for (jix in 1:length(PalladioTravel$detecteddistrict))
{
  if (PalladioTravel$detecteddistrict[jix]==""){PalladioTravel$detecteddistrict[jix]<-PalladioTravel$detectedstate[jix]}
}
for (jix in 1:length(PalladioTravel$detecteddistrict))
{
  if (PalladioTravel$detectedcity[jix]==""){PalladioTravel$detectedcity[jix]<-PalladioTravel$detecteddistrict[jix]}
}
library(maps)
data("world.cities")
#removing duplicate Indian cities from worldcities
world.cities.ind <-world.cities[which(world.cities$country.etc=="India"),]
#if detectedcity not in worldcitiesind replace with detecteddistrict
for (jix in 1:length(PalladioTravel$detectedcity))
{
  if (PalladioTravel$detectedcity[jix] %in% world.cities.ind$name) {} 
    else {PalladioTravel$detectedcity[jix] <- PalladioTravel$detecteddistrict[jix] }
}
#now for the remaining detectedcity not in worldcitiesind replace with detectedstate
for (jix in 1:length(PalladioTravel$detectedcity))
{
  if (PalladioTravel$detectedcity[jix] %in% world.cities.ind$name) {} 
  else {PalladioTravel$detectedcity[jix] <- PalladioTravel$detectedstate[jix] }
}

#Replacing Indian state names with their capitals
StatetoCity <- data.frame(
  state=c("Delhi","Haryana","Ladakh","Tamil Nadu","Kerala","Karnataka",
          "Maharashtra","Andhra Pradesh","Uttarakhand","Puducherry",
          "West Bengal","Uttar Pradesh","Rajasthan","Himachal Pradesh",
          "Gujarat","Punjab","Telangana","Andaman and Nicobar Islands",
          "Jharkhand","Goa","Odisha","Assam","Manipur","Arunachal Pradesh",
          "Jammu and Kashmir","Chandigarh","Chhattisgarh","Madhya Pradesh",
          "Bihar","Mizoram","Dadra and Nagar Haveli","Tripura"),
  capital=c("Delhi","Chandigarh","Srinagar","Chennai","Thiruvananthapuram","Bangalore",
         "Bombay","Visakhapatnam","Haridwar","Pondicherry",
         "Calcutta","Kanpur","Jaipur","Shimla",
         "Gandhinagar","Amritsar","Hyderabad","Port Blair",
         "Dhanbad","Panaji","Bhubaneswar","Guwahati","Imphal","Aizawl","Srinagar",
         "Chandigarh","Raipur","Bhopal","Patna","Aizawl","Valsad","Agartala"),
  stringsAsFactors = FALSE
)

#replace the states with capitals in the Palladio data
for (jix in 1:length(PalladioTravel$detectedcity))
{
  if (PalladioTravel$detectedcity[jix] %in% StatetoCity$state) 
  {PalladioTravel$detectedcity[jix] <- 
      StatetoCity$capital[which(StatetoCity$state==PalladioTravel$detectedcity[jix])]}
}

#Now we will only be left with a list of Indian states that are not there in the world.cities list
#can identify them and replace them with their respective capitals
# the list has to be prepared once
Stateswithoutlatlon <- unique(PalladioTravel$detectedcity[-which(PalladioTravel$detectedcity %in% world.cities.ind$name)])

#replace any state names in Palladio notes with their capitals

for (jix in 1:length(PalladioTravel$notes))
{
  if (PalladioTravel$notes[jix] %in% StatetoCity$state) 
  {PalladioTravel$notes[jix] <- 
    StatetoCity$capital[which(StatetoCity$state==PalladioTravel$notes[jix])]}
}

#now there is a Canadian Delhi which clashes with Indian Delhi - first remove it from worldcities
world.cities.rev <- world.cities[-which(world.cities$name=="Delhi"&world.cities$country.etc=="Canada"),]
world.cities.rev <- world.cities[-which(world.cities$name=="Bombay"&world.cities$country.etc=="New Zealand"),]
world.cities.rev <- world.cities[-which(world.cities$name=="China"&world.cities$country.etc=="Mexico"),]


#create a Places DF
AllPlaces <- unique(c(PalladioTravel$detectedcity,PalladioTravel$notes))
AllLat <- NULL
AllLon <- NULL


for (jix in 1:length(AllPlaces))
{
  #Replace "Middle East" and "West Indies" in AllPlaces
  if (AllPlaces[jix]=="Middle East") {AllPlaces[jix] <- "Dubai"}
  if (AllPlaces[jix]=="West Indies") {AllPlaces[jix] <- "Barbados"}
}

#AllCities <- AllPlaces[which(AllPlaces %in% world.cities.rev$name)]
#AllCountries <- AllPlaces[which(AllPlaces %in% world.cities.rev$country.etc)]
for (jix in 1:length(AllPlaces))
{
  if (AllPlaces[jix] %in% world.cities.rev$name) {
  AllLat[jix] <- world.cities.rev$lat[which(world.cities.rev$name==AllPlaces[jix])]
  AllLon[jix] <- world.cities.rev$lon[which(world.cities.rev$name==AllPlaces[jix])]
  }
  else {
    AllLat[jix] <- world.cities.rev$lat[which(world.cities.rev$country.etc==AllPlaces[jix])]
    AllLon[jix] <- world.cities.rev$lon[which(world.cities.rev$country.etc==AllPlaces[jix])]
  }
}

AllCoords <- paste(AllLat,",",AllLon)

PlacesDF <- data.frame(
  place = AllPlaces,
  location = AllCoords)

write.csv(PalladioTravel,"PalladioMain.csv")
write.csv(PlacesDF, "PalladioPlaces.csv")
