library(ggplot2)
library(plyr)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
coef = read.csv("mm_coefs.csv",header= T,strip.white = T)

#Remove unused categories and rename as plotting select effect sizes only
coef = subset(coef,!X %in% "visibility")

coef$Management_Actions = revalue(coef$X,c("mixed_managementEffective_FM" = "Effective","mixed_managementMR&Effective_FM_Closed" = "Effective","mixed_managementMR&Effective_FM_Open" = "Effective","mixed_managementMR&Ineffective_FM_Closed" = "Ineffective","Intercept" = "Ineffective","mixed_managementMR&Ineffective_FM_Open"= "Ineffective"))
coef$Protection_Status = revalue(coef$X,c("mixed_managementEffective_FM" = "Outside","mixed_managementMR&Effective_FM_Closed" = "Inside","mixed_managementMR&Effective_FM_Open" = "Outside","mixed_managementMR&Ineffective_FM_Closed" = "Inside","Intercept" = "Outside","mixed_managementMR&Ineffective_FM_Open"= "Outside")) 

coef$mr = revalue(coef$X,c("mixed_managementEffective_FM" = "Fisheries Management Only","mixed_managementMR&Effective_FM_Closed" = "FPAs & Fisheries Management","mixed_managementMR&Effective_FM_Open" = "FPAs & Fisheries Management","mixed_managementMR&Ineffective_FM_Closed" = "FPAs & Fisheries Management","Intercept" = "Fisheries Management Only","mixed_managementMR&Ineffective_FM_Open" = "FPAs & Fisheries Management"))
coef$mr <- ordered(coef$mr,levels=c("FPAs & Fisheries Management","Fisheries Management Only"))

# Pre sets for plot
dodge <- position_dodge(0.55)
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 
PaletteFill<- scale_fill_manual(values=c("#339900","#0000FF"))
PaletteColour<- scale_colour_manual(values=c("#339900","#0000FF"))
PaletteShape <-  scale_shape_manual(values=c(18,16))

#om$mr <- ordered(om$mr,levels=c("NTMRs & Fisheries Management","Fisheries Management Only"))
mrlabels = c("(a) FPAs & Fisheries Management","(b) Fisheries Management Only")
names(mrlabels) <- c("FPAs & Fisheries Management","Fisheries Management Only")

#create plot

Plot<-ggplot(coef, aes(y=Estimate, x= Management_Actions))+
  geom_errorbar(aes(fill = Protection_Status,ymin = Q2.5, ymax = Q97.5),position = dodge,width = 0.2)+
  #geom_point(aes(colour = Protection_Status),size=2,position = dodge,alpha = 0.3)+
  geom_point(aes(fill = Protection_Status),position = dodge,size=3,shape = 21)+
  #geom_point(data = outliers, aes(fill = Protection_Status),colour = "red",position = dodge,size=2,alpha = 0.3)+
  #geom_point(data = om, aes(fill = Protection_Status),colour = "red",size=3,position = dodge,shape = 8)+ 
  #geom_text(data = outliers,aes(colour = Protection_Status,label = site_name),size = 3,nudge_x = 0.2,nudge_y = 0.075)+
  #geom_violin(aes(fill = Protection_Status),position = dodge,alpha = 0.2, colour = "black")+ 
  #stat_summary(aes(fill = Protection_Status),fun = mean,geom = "point", shape = 23, size = 4,position = dodge)+
  geom_hline(yintercept = 0,linetype = "dashed")+
  PaletteColour+
  PaletteFill+
  #scale_x_continuous(trans = "log")+
  #coord_flip()+
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
  ylab("Partial effect coefficients ± CI")+
  xlab("Management Actions")

#png(filename = "Management_Coefficients_24-05-2023.png",
  #width = 1750, height = 1750, units = "px",res =300)

Plot + facet_grid(.~mr,labeller = labeller(mr = mrlabels),scales = "free")

#dev.off()


