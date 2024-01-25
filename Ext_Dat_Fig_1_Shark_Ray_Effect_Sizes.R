library(ggplot2)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
alldata = read.csv("es_results_fpa.csv",header = T,strip.white = T)

# Pre sets for plot
dodge <- position_dodge(0.55)
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 
PaletteFill<- scale_fill_manual(values=c("#000000","#FFFFFF"))
PaletteColour<- scale_colour_manual(values=c("#339900","#FFCC00"))

# Add Sig level to Plots
Sharks = data.frame(es=0.88,Group = "Sharks",text = "n = 66, H***")
Rays = data.frame(es=0.34,Group = "Rays",text = "n = 59,H***")

# Subset and Order
alldata<-subset(alldata, Group%in%c("Sharks","Rays"))
alldata$Significance <- ordered(alldata$Significance,levels=c("Yes","No"))
alldata$Group <- ordered(alldata$Group,levels=c("Sharks","Rays"))

Plot<-ggplot(alldata, aes(y=es, x= Group))+
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub),width = 0.3,position = dodge)+
  geom_errorbar(aes(ymin = cilb75, ymax = ciub75),width = 0,size = 2,position = dodge)+
  geom_point(aes(colour = Significance),size=4,show.legend = T,position = dodge)+ 
  geom_hline(yintercept=0, linetype=2)+
  PaletteColour+
  geom_text(data = Sharks,aes(label = text),size =3,fontface="italic")+
  geom_text(data = Rays,aes(label = text),size =3,fontface="italic")+
  #coord_flip()+
  theme(strip.text.y = element_text(size = 12,face="bold"),
        strip.text.x = element_text(size = 12,face="bold"),
        strip.background = element_blank(),
        axis.title=element_text(face="bold"), 
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),  
        #legend.position = "top",
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = "black"))+
  ylab("Log-ratio Effect Size")

Plot