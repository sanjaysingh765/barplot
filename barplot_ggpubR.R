##############################################################################################################################################
#                       This script calculate annova and plot them
#                     USEs : Rscript script_name data_file plot_title figure_name.png  y_axis_limit y_axis_label

#
#
#  This script load the data and extract the expression of given gene therefore "plot_title" should match with the given gene name. Placement 
#  of significance sign should be given in number except the reference column.  It will calculate the mean, standard error and make a barplot 
#  with the significance stars.
#
#################################################################################################################################################
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) < 4) {
  stop("Give correct parameters\nUses: Rscript script_name data_file plot_title figure_name.png  y_axis_limit y_axis_label\n", call.=FALSE)
} 



#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/#compare-more-than-two-groups
library("reshape2")
library("ggpubr")
#library(Rmisc) 
library(extrafont)
#font_import() # only one time required when first time use the library extrafont
#y
fonts() 
loadfonts()
library(RColorBrewer)

#my.labels <- c("STR-Luc","STR-Luc\n + ORCA5","STR-Luc\n + ORCA6") # first create labels, add \n where appropriate.


rpkm <- read.csv(args[1],header=T, row.names = 1, check.names = FALSE)
rpkm

#aql <- melt(select, id.vars = c("AGI"))
aql <- melt(as.matrix(rpkm))
aql

select <- aql[grep(as.character(args[2]), aql$Var1),]
select

ave <- aggregate( value~Var2, select, FUN = function(x) c(mean = mean(x), sd = sd(x)))
ave


#pdf(file = "tissue.pdf", width = 2, height = 2, family = "Times", pointsize = 16) # defaults to 7 x 7 inches
png(args[3], units="in", family="Times",  width=1.5, height=2, res=300) #pointsize is font size| increase image size to see the key
###  Compare two independent groups (By default method = “wilcox.test” (non-parametric test). You can also specify method = “t.test” for a parametric t-test.)

#Multiple pairwise tests against a reference group:
# Pairwise comparison against reference
# Visualize
ggbarplot(select, x = "Var2", y = "value",
            fill = "Var2",
            position=position_dodge(0.8),
            #palette = c("#00AFBB", "#E7B800"),
            #palette = "rickandmorty" , # "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty". 
            palette = get_palette("Set2", 4),
            color = "Var2",
            size=.1,
            width=.5,
            legend="none",
            add = "mean_se",
            #facet.by="Tissue", 
            add.params = list(color = "black"),
            error.plot = "errorbar",#"pointrange", "linerange", "crossbar", "errorbar", "upper_errorbar", "lower_errorbar", "upper_pointrange","lower_pointrange",  "upper_linerange",  "lower_linerange"
            label = F, #show mean
            lab.col = "red", 
            lab.size = 2,
            lab.nb.digits = 1, # integer indicating the number of decimal places 
            x.text.angle = 90,           # Rotate vertically x axis texts          
            lab.pos= "in", #"out", "in"
            ylab =as.character(args[5]), 
            xlab = "",
            title=args[2]    #plot main title

)+
scale_y_continuous(expand=c(0,0), limits = c(0, as.numeric(args[4])))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank())  +
  theme(
    axis.text=element_text(size=6, color = "black",family="Times"),
    axis.title=element_text(size=7,face="bold", color = "black"),
    plot.title = element_text(color="black", size=5, face="bold.italic",hjust = 0.5,margin=margin(b = 5, unit = "pt")))+
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5, vjust = 1.2,color = "black" ))+
  theme(axis.line = element_line(size = 0.2, color = "black"),axis.ticks = element_line(colour = "black", size = 0.2))+
  theme(axis.ticks.length = unit(0.04, "cm"))+
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.4),"mm"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 2, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 2, l = 0)))
   #scale_x_discrete(labels= my.labels)


dev.off()

