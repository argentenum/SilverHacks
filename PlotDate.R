dateta <- structure(list(time = structure(c(1338361200, 1338390000, 1338445800, 1338476400, 1338532200, 1338562800,
                                            1338618600, 1338647400, 1338791400, 1338822000), 
                                          class = c("POSIXct", "POSIXt"), tzone = ""), 
                         variable = c(168L, 193L, 193L, 201L, 206L, 200L, 218L, 205L, 211L, 230L)),
                    .Names = c("time", "variable"), row.names = c(NA, -10L), class = "data.frame")
dateta
plot(dateta, xaxt="n")
axis.POSIXct(side=1, at=cut(dateta$time, "days"), format="%m/%d") 
NumberDate <- c(1338361200, 1338390000, 1338445800, 1338476400, 1338532200, 1338562800, 1338618600, 1338647400, 1338791400, 1338822000)

WikiDate <- c("10:20, 24 January 2019", "13:40, 23 January 2019", "10:41, 15 December 2018", 
               "10:41, 15 December 2018", "10:36, 15 December 2018", "10:36, 15 December 2018", 
               "10:35, 15 December 2018", "12:47, 27 December 2017", "16:04, 12 December 2017", 
               "22:49, 10 October 2016", "07:30, 12 September 2016", "07:37, 24 August 2016", 
               "14:53, 25 April 2016", "14:33, 25 April 2016", "15:16, 24 April 2016", 
               "09:03, 4 July 2015", "10:59, 3 July 2015", "15:02, 21 July 2014", 
               "16:21, 18 May 2014", "06:54, 2 March 2014")
WikiSize <- c(11336, 11337, 11336, 11443, 11336, 11375, 11361, 11336, 11298, 11247, 11312, 11311, 11298, 
              11282, 11291, 11290, 11232, 11233, 11045, 10979)
WikiDate <- as.POSIXct(strptime(WikiDate, "%H:%M, %d %B %Y", tz = "GMT"))
WikiFrame <- data.frame(WikiDate, WikiSize, stringsAsFactors = FALSE)
names(WikiFrame) <- c("Date", "Size")
plot(WikiFrame, xaxt="n", type = "o")
axis.POSIXct(side=1, at=cut(WikiFrame$Date, "days"), format="%b-%y") 
#This works! Done! Now to increase the beauty of the graph on ggplot
library(ggplot2)
SizePlot <- ggplot(data = WikiFrame, aes(Date, Size))+
    geom_line(color = "#00AFBB", size = 2)
SizePlot + scale_x_datetime(date_labels = "%Y")    

  