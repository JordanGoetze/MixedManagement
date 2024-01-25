library(ggplot2)
library(plyr)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
coef = read.csv("mm_coefs.csv",header= T,strip.white = T)

#Remove unused categories and rename as plotting select effect sizes only
coef = subset(coef,!X %in% c("visibility","Intercept","mixed_managementMR&Ineffective_FM_Open"))

coef$mr = revalue(coef$X,c("mixed_managementEffective_FM" = "Effective Fisheries Management","mixed_managementMR&Effective_FM_Closed" = "Mixed Management","mixed_managementMR&Effective_FM_Open" = "Mixed Management","mixed_managementMR&Ineffective_FM_Closed" = "Fully Protected Areas"))
coef$mr <- ordered(coef$mr,levels=c("Mixed Management","Fully Protected Areas","Effective Fisheries Management"))

coef$Protection_Status = revalue(coef$X,c("mixed_managementEffective_FM" = "Outside","mixed_managementMR&Effective_FM_Closed" = "Inside","mixed_managementMR&Effective_FM_Open" = "Outside","mixed_managementMR&Ineffective_FM_Closed" = "Inside")) 

# Pre sets for plot
dodge <- position_dodge(0.55)
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 
PaletteFill<- scale_fill_manual(values=c("#66CC33","#0000FF"))
PaletteColour<- scale_colour_manual(values=c("#339900","#0000FF"))
PaletteShape <-  scale_shape_manual(values=c(18,16))

#Create labels
mrlabels = c("(a) FPAs & Fisheries Management","(b) Fisheries Management Only")
names(mrlabels) <- c("NTMRs & Fisheries Management","Fisheries Management Only")

#create plot

Plot<-ggplot(coef, aes(y=Estimate, x= mr))+
  geom_errorbar(aes(fill = Protection_Status,ymin = Q2.5, ymax = Q97.5),position = dodge,width = 0.2)+
  geom_point(aes(fill = Protection_Status),position = dodge,size=5,shape = 21)+
  geom_hline(yintercept = 0,linetype = "dashed")+
  PaletteColour+
  PaletteFill+
  theme(strip.text.y = element_text(size = 10),
        strip.background = element_blank(),
        axis.title=element_text(face="bold"), 
        strip.text.x = element_text(face="bold",size =10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),  
        legend.position = "bottom",
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = "black"))+
  ylab("Partial coefficients ± CI")+
  xlab("Management Approach")

Plot

#Combine Figure a and b together in powerpoint and draw in lines for how effects are calculated

