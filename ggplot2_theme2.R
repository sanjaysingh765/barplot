theme_ms <- function(base_size=12, base_family="Arial") {
  library(grid)
  (theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())  +
theme(
axis.text=element_text(size=4, color = "black",family="Arial"),
axis.title=element_text(size=4,face="bold", color = "black"),
plot.title = element_text(color="black", size=5, face="bold.italic",hjust = 0.5,margin=margin(b = 5, unit = "pt")))+
theme(axis.text.x = element_text(angle = 360, hjust = 0.5, vjust = 1.2,color = "black" ))+
theme(  legend.text=element_text(size=4),
        #legend.justification=c(2.5,1),
        legend.key = element_rect(size = 6),
        legend.key.size = unit(2.5, 'lines'),
        legend.position=c(0.5, .9),
        legend.direction = "horizontal",
        legend.title=element_blank())+ 
theme(axis.line = element_line(size = 0.2, color = "black"),axis.ticks = element_line(colour = "black", size = 0.2))+
theme(axis.ticks.length = unit(0.04, "cm"))+
theme(plot.margin=unit(c(0.1,0.1,0.1,0.4),"mm"))+
theme(axis.title.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)))+
theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 2, l = 0)))

)
}
