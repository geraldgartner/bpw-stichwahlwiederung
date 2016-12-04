#Unser Style
library(ggthemes)
theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))  


#Parteifarben festlegen
kandidatenfarben <- c("hofer" = "#7a8fcc","vdb" = "#548750","griss" = "#b398aa","hundstorfer" ="#b34848", "khol" = "#282828", "lugner" = "#bfb58e")
kandidaten <- c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner" = "Lugner")
