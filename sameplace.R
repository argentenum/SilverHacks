#adding flightpaths with help from https://stackoverflow.com/questions/50333682/different-colors-for-flight-routes-leaflet-with-r
setwd("~/ownCloud/iaclals-scraping")
yeardiv <- data.frame(read.csv(file = "cleanoutput.csv", stringsAsFactors = FALSE)) 
sameplacerows <- subset(yeardiv, yeardiv$Birth.Place == yeardiv$Death.Place)
library(plyr)
sameplacefreq <- count(sameplacerows, 'Birth.Place')
sameplacefreq <- sameplacefreq[order(-sameplacefreq$freq),]
sameplacefreq$Birth.Place[which(sameplacefreq$freq < 3)] <- "Rest"
pie(sameplacefreq$freq, labels = sameplacefreq$Birth.Place)
library(plotly)
library(dplyr)
sameplacemap <- plot_ly(sameplacefreq, labels = ~sameplacefreq$Birth.Place, values = ~sameplacefreq$freq, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
                          marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'Places with SAME birth and death places',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
sameplacemap

birthplaces <- count(yeardiv, yeardiv$Birth.Place)
birthplaces <- birthplaces[order(-birthplaces$n),]
birthplaces$`yeardiv$Birth.Place`[which(birthplaces$n < 13)] <- "Rest"
birthplacemap <- plot_ly(birthplaces, labels = ~birthplaces$`yeardiv$Birth.Place`, values = ~birthplaces$n, type = 'pie',
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = '#FFFFFF'),
                         marker = list(colors = colors,
                                       line = list(color = '#FFFFFF', width = 1)),
                         #The 'pull' attribute can also be used to create space between the sectors
                         showlegend = FALSE) %>%
  layout(title = 'Top birth places',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
birthplacemap


deathplaces <- count(yeardiv, yeardiv$Death.Place)
deathplaces <- deathplaces[order(-deathplaces$n),]
deathplaces$`yeardiv$Death.Place`[which(deathplaces$n < 22)] <- "Rest"
deathplacemap <- plot_ly(deathplaces, labels = ~deathplaces$`yeardiv$Death.Place`, values = ~deathplaces$n, type = 'pie',
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = '#FFFFFF'),
                         marker = list(colors = colors,
                                       line = list(color = '#FFFFFF', width = 1)),
                         #The 'pull' attribute can also be used to create space between the sectors
                         showlegend = FALSE) %>%
  layout(title = 'Top death places',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
deathplacemap
toplaces <- intersect(birthplaces$`yeardiv$Birth.Place`,deathplaces$`yeardiv$Death.Place`)
#topbirthplcframe <- subset(yeardiv, yeardiv$Birth.Place %in% toplaces)
#topdeathplcframe <- subset(yeardiv, yeardiv$Death.Place %in% toplaces)
yearlybirth <- NULL
yearlydeath <- NULL
for (q in 1840:2019)
  {mumbaibirth <- subset(yeardiv, yeardiv$Birth.Place == "Pune")
  mumbaideath <- subset(yeardiv, yeardiv$Death.Place == "Pune")
  yearlybirth[q] <- length(which(mumbaibirth$birth.year == q))
  yearlydeath[q] <- length(which(mumbaideath$death.year == q))
  }
years <- (1:2019)
yearlyframe <- data.frame(years[1840:2019],yearlybirth[1840:2019],yearlydeath[1840:2019])
names(yearlyframe) <- c("year", "birth.no", "death.no")
plot(yearlyframe$birth.no, type = "p")
plot(yearlyframe$death.no, type = "p")
library(plotly)

x <- list(autotick = FALSE,
  dtick = 10,
  tickangle = 270,
  range = c(1840, 2020),
  title = "Year"
)
y <- list(autotick = FALSE,
          ticks = "outside",
          tick0 = 0,
          dtick = 1,
          ticklen = 5,
          tickwidth = 2,
          range = c(0, 9),
          title = "Occurence"
)
plot_ly(data = yearlyframe, x = ~yearlyframe$year, y = ~yearlyframe$death.no, name = 'Death', type = 'scatter', 
            marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 2))) %>%
  add_trace(y = ~yearlyframe$birth.no, name = 'Birth',  
            marker = list(size = 10, color = 'rgba(51, 153, 255, .9)', line = list(color = 'rgba(0, 0, 204, .8)', width = 2))) %>% 
  layout(title = "Pune", xaxis = x, yaxis = y)


