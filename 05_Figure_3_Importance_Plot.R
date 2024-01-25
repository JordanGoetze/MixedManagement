#Make variable importance plot from importance scores derived from FSS analysis

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
library(ggplot2)
library(reshape2)

Data <- read.csv("variable_importance_fpa.csv")

Melt=melt(Data)

#Select reef-ass sharks only
Melt<-subset(Melt, Group %in% c("Reef Associated"))

pos <- position_dodge(width = 0.9)

plot <- ggplot(Melt, aes(x = reorder(variable,value), y = value))+
  stat_summary(geom = 'bar', colour="black", position = pos)+
  theme_bw()+
  theme(strip.text.y = element_text(size = 12,angle = 270),
        strip.background = element_blank(),
        axis.title=element_text(face="bold"), 
        plot.title=element_text(face="bold"),
        strip.text.x=element_text(size = 14,face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        #panel.border = element_rect(colour = "black")
        legend.key = element_blank(),
        axis.line = element_line(colour = 'black'))+
  ylab("Relative Importance")+
  xlab("Variable/Factor")+
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.1))+
  coord_flip()

plot

#Shark pictures and directional plots for important variable (see other script)
#Are added together in powerpoint