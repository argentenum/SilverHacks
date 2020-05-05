df2 <- data.frame(Change=rep(c("Removal", "No Change", "Addition"), each=1),
                  freq=rep(c("Freq"),1),
                  len=c(-439, 85, 622))
# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=freq, y=len, fill=Change)) +
  geom_bar(stat="identity", width = 0.3, ) +
  scale_fill_manual(values = c("Blue","Grey","Red")) +
  geom_text(aes(label = len), size = 3.5, color = "White", position = position_stack(vjust = 0.5)) +
  #guides(fill=FALSE) +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle(input.pagename, subtitle = paste("Controversy Factor ",ControFactor)) +
  coord_flip()
  