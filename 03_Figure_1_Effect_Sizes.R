library(ggplot2)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()

alldata = read.csv("es_results_fpa.csv",header = T,strip.white = T)

# Pre sets for plot
dodge <- position_dodge(0.55)
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 
PaletteFill<- scale_fill_manual(values=c("#000000","#FFFFFF"))
PaletteColour<- scale_colour_manual(values=c("#339900","#FFCC00"))
#PaletteShape <-  scale_shape_manual(values=c(15,16,17,18))

# Add Sig level to Plots
W = data.frame(es=0.45,Group = "Wide-ranging",Type = "Sharks",text = "n = 40")
R = data.frame(es=1.06,Group = "Reef-associated",Type = "Sharks",text = "n = 66, H***")
SR = data.frame(es=0.34,Group = "Small Rays",Type = "Rays",text = "n = 28")
LR = data.frame(es=0.38,Group = "Large Rays",Type = "Rays",text = "n = 48")
G = data.frame(es=1.25,Group = "Grey Reef",Type = "Sharks",text = "n = 30,H**")
WT = data.frame(es=1.3,Group = "Whitetip Reef",Type = "Sharks",text = "n = 30, H**")
BL = data.frame(es=0.86,Group = "Blacktip Reef",Type = "Sharks",text = "n = 23, H*")
CA = data.frame(es=1.37,Group = "Caribbean Reef",Type = "Sharks",text = "n = 18, H***")
N = data.frame(es=0.95,Group = "Nurse",Type = "Sharks",text = "n = 37")
labels = rbind(W,R,SR,LR,G,WT,BL,CA,N)
labels$Type <- ordered(labels$Type,levels=c("Sharks","Rays"))

# Subset and Order
alldata<-subset(alldata, Group%in%c("Small Rays","Large Rays","Wide-ranging","Reef-associated","Blacktip Reef","Caribbean Reef","Grey Reef","Nurse","Whitetip Reef"))
#alldata$Group <- ordered(alldata$Group,levels=c("Sharks","Wide Ranging","Wide Ranging","Small Rays","Blacktip","Caribbean","Grey Reef","Nurse","Whitetip"))
alldata$Significance <- ordered(alldata$Significance,levels=c("Yes","No"))
alldata$Type <- ordered(alldata$Type,levels=c("Sharks","Rays"))
alldata$Group <- ordered(alldata$Group,levels=c("Small Rays","Large Rays","Wide-ranging","Reef-associated","Caribbean Reef","Grey Reef","Whitetip Reef","Nurse","Blacktip Reef"))

#Label names (left a large space to trick labelling across facets)
facet_label = c('Sharks' = "Sharks                                                                             Reef-associated Sharks",'Rays'="Rays")

##create forest plot

Plot<-ggplot(alldata, aes(y=es, x= Group))+
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub),width = 0.3,position = dodge)+
  geom_errorbar(aes(ymin = cilb75, ymax = ciub75),width = 0,size = 2,position = dodge)+
  geom_point(aes(colour = Significance),size=4,show.legend = F,position = dodge)+ 
  geom_hline(yintercept=0, linetype=2)+
  #Need to fix the vline
  #geom_vline(alldata = filter(alldata, Type == "Sharks"),aes(xintercept=2.5),linetype=3)+
  PaletteColour+
  geom_text(data = labels,aes(label = text),size =4,fontface="italic")+
  #coord_flip()+
  theme(strip.text.y = element_text(size = 12,face="bold"),
        strip.text.x = element_text(size = 12,face="bold"),
        strip.background = element_blank(),
        axis.title=element_text(face="bold"), 
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),  
        #legend.position = "top",
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = "black"))+
  ylab("Log-ratio Effect Size")

Plot+facet_grid(.~Type,scales = "free_x",space = "free",labeller = as_labeller(facet_label))

#Shark pictures and diagonal lines are added in powerpoint
