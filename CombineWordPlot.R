#combining page date with word data
dt <- data.frame(WikiFrame$Date,WikiFrame$Bytes,WordFrame$Count, stringsAsFactors = FALSE)
names(dt) <- c("Date","Size","Count")
ggplot(data = dt)+
geom_line(mapping=aes(x=Date,y=Size),color = "grey", size = 2)+
  geom_area(mapping=aes(x=Date,y=Size), fill="grey", alpha=1.)+
  geom_line(mapping = aes(x=Date,y=Count*(max(dt$Size)/max(dt$Count))), size = 1, color = "blue") + 
  coord_cartesian(ylim = c(0, (1.1*max(dt$Size)))) +
  scale_x_datetime(date_labels = "%Y")  + ggtitle(input.pagename, subtitle = "word which was counted")+
  scale_y_continuous(name = "Bytes", 
                   sec.axis = sec_axis(~./(max(dt$Size)/max(dt$Count)), name = "Word Count"))
