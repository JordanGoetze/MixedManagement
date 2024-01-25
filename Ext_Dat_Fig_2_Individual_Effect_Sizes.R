library(ggplot2)
library(dplyr)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
#Results extracted from forest plot produced in script 02_Effect_Size_Calculations_FPAs
esdata = read.csv("individual_es_results_clean_names.csv",header = TRUE)
ciandw = read.csv("individual_es_weights_cis_clean_names.csv",header = TRUE)
alldata = left_join(esdata,ciandw,by = "unique_reserve")

# Pre sets for plot
dodge <- position_dodge(0.55)
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 
PaletteFill<- scale_fill_manual(values=c("#FFCC00","#339900"))
PaletteShape <-  scale_shape_manual(values=c(21,23))

#Create joined name
alldata$name = do.call(paste, c(alldata[c("country","unique_reserve")],sep = "-"))
alldata$type = "Individual"

Plot<-ggplot(alldata, aes(y=es, x= reorder(name,es)))+
  geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub),width = 0.3,position = dodge)+
  geom_point(aes(size = weight, fill = significant, shape = type),show.legend = F,position = dodge, alpha = 0.8)+ 
  geom_hline(yintercept=0, linetype=2)+
  PaletteFill+
  PaletteShape+
  #geom_text(data = labels,aes(label = text),size =3,fontface="italic")+
  coord_flip()+
  theme(strip.text.y = element_text(size = 12,face="bold"),
        strip.text.x = element_text(size = 12,face="bold"),
        strip.background = element_blank(),
        axis.title=element_text(face="bold",size = 12), 
        axis.text.y=element_text(size = 10), 
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),  
        #legend.position = "top",
        axis.line = element_line(colour = 'black'),
        #axis.line.y =  element_blank(),
        panel.border = element_blank())+
  ylab("Log-ratio Effect Size")

Plot 
