library(grid)

theme_fivethirtyeight <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75, 
                                       linetype = "dotted", lineend = "butt"),
      rect =              element_rect(fill = "gray97", colour = "#F0F0F0", 
                                       size = 0.5, linetype = 1),
      text =               element_text(family = base_family, face = "plain",
                                        color = "#656565", size = base_size,
                                        hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,
                                        margin = margin(), debug = FALSE),
      
      
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(1.5), family = '' , 
                                       face = 'bold', hjust = 0, 
                                       vjust = 1, colour = '#3B3B3B'),
      plot.subtitle =     element_text(size= rel(1.2), family = '' , hjust = 0, 
                                       vjust = 0, colour = '#3B3B3B'),
      plot.caption=       element_text(size= rel(0.8), family = '' , hjust = 1, 
                                       vjust = 0, colour = '#3B3B3B'),
      #axis.title.x =      element_blank(),
      #axis.title.y =      element_blank(),
      #axis.text =         element_text(),
      
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(colour = "gray86", linetype = "dotted"),
      #panel.grid.minor =  element_blank(colour = "gray86", linetype = "dotted"),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(fill = "gray97"),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'top',
      legend.title=element_blank(),
      complete = TRUE,
      
      #Facet styling
      strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97")
      
      
    )
}
#theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
               
#   theme(plot.title = element_text(size = 22, face = "bold"), 
#         plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
#   theme(axis.ticks = element_blank(), 
#         axis.line = element_blank(),
#         axis.title = element_text(vjust = 8), 
#         panel.background = element_rect(fill = "grey97", linetype = "solid"), 
#         plot.background = element_rect(colour = "gray97"), 
#         plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
#         plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
#   theme(axis.text=element_text(size=16))  
# 
# 
# #Parteifarben festlegen
kandidatenfarben <- c("hofer" = "#7a8fcc","vdb" = "#548750","griss" = "#b398aa","hundstorfer" ="#b34848", "khol" = "#282828", "lugner" = "#bfb58e")
kandidaten <- c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner" = "Lugner")
# 
