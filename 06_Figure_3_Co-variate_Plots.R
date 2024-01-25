library(ggplot2)
library(metafor)

# Bring in and format the data----
setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
dat2<-read.csv("es_cov_fpa.csv",header = T,strip.white = T)
dat2$response = dat2$es
reefsharks = subset(dat2,variable %in% c("reef associated"))

#Set up plots
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=22, face = "bold"),
    axis.title.y=element_text(vjust=0.6, angle=90, size=22,face = "bold"),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.line.x = element_line(size =1.2,arrow = grid::arrow(length = unit(0.5, "cm"))),
    axis.line.y = element_line(size =1.2,arrow = grid::arrow(length = unit(0.5, "cm"))),
    strip.background = element_blank())

#####!!!!Load and run script 04_Full_Subsets_Analysis!!!!!#####
# Make the model of interest
#i says which of the loops to use
i=1
use.dat=dat[which(dat$variable==resp.vars[i]),]
Model1=gam(response~s(sqrt_mpa_age,k=3,bs='cr'),family=gaussian(),data=use.dat, weights = 1/var)

plot.formula <- model.set$mod.formula$'log_Grav_Total_Plus_min+log_mpa_area+mpa_isolation'
ramodel <- update(Model1, plot.formula)
plot(ramodel,residuals = T,all.terms = TRUE)

####################################################
# predict - gravity  from MODEL ----
#Making a sequence from the min to max relied with 20 points, others are set as mean
#Add dummy to gravity first
reefsharks$log_Grav_Total_Plus_min = reefsharks$log_Grav_Total_Plus_min+0.0001
testdata <-expand.grid(log_Grav_Total_Plus_min=seq(min(reefsharks$log_Grav_Total_Plus_min),max(reefsharks$log_Grav_Total_Plus_min),length.out = 20),
                       log_mpa_area=mean(ramodel$model$log_mpa_area),mpa_isolation = "continuous") 

fits <- predict(ramodel, newdata=testdata, type='response', se.fit=T)

ra.predicts = testdata%>%data.frame(fits)%>%
  group_by(log_Grav_Total_Plus_min)%>% 
  dplyr::summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

gravityplot<-ggplot(ra.predicts,aes(x=log_Grav_Total_Plus_min,y=response))+
  ylab("Effect Size")+
  xlab("Gravity")+
  xlim(0,8)+
  geom_line(colour = "blue", size = 2)+
  geom_ribbon(aes(ymin = response-se.fit,ymax =response+se.fit),colour = NA,fill = "blue",alpha = 0.3)+
  geom_hline(yintercept=0, linetype='dotted')+
  theme_classic()+Theme1

gravityplot

####################################################
# predict - size  from MODEL ----
#Making a sequence from the min to max relied with 20 points, others are set as mean
testdataarea <-expand.grid(log_mpa_area=seq(min(reefsharks$log_mpa_area),max(reefsharks$log_mpa_area),length.out = 20),
                       log_Grav_Total_Plus_min=mean(ramodel$model$log_Grav_Total_Plus_min),mpa_isolation = "continuous") #sample(ramodel$model$mpa_isolation)

fitsarea <- predict(ramodel, newdata=testdataarea, type='response', se.fit=T)

ra.predicts = testdataarea%>%data.frame(fitsarea)%>%
  group_by(log_mpa_area)%>% #only change here
  dplyr::summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()%>%
  glimpse()

sizeplot<-ggplot(ra.predicts,aes(x=log_mpa_area,y=response))+
  ylab("Effect Size")+
  xlab("Size")+
  xlim(0,8)+
  geom_line(colour = "green4", size = 2)+
  geom_ribbon(aes(ymin = response-se.fit,ymax =response+se.fit),colour = NA,fill = "green4",alpha = 0.3)+
  theme_classic()+Theme1

sizeplot

####################################################
#Now categorical factors
#Set up different theme
Theme2 <-
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), 
    legend.text = element_text(size=15),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=22, face = "bold"),
    axis.title.y=element_text(vjust=0.6, angle=90, size=22,face = "bold"),
    axis.text.x=element_text(colour ="black",size=22),
    axis.line.x = element_line(size =1.2,arrow = grid::arrow(length = unit(0.5, "cm"))),
    axis.line.y = element_line(size =1.2,arrow = grid::arrow(length = unit(0.5, "cm"))),
    strip.background = element_blank())

#Calculate effect sizes for Isolation categories
#CI = 95
metaiso95=rma(reefsharks$es,reefsharks$var,slab=reefsharks$unique_reserve,mods=~reefsharks$mpa_isolation-1,level = 95)
metaiso95

#CI = 75
metaiso75=rma(reefsharks$es,reefsharks$var,slab=reefsharks$unique_reserve,mods=~reefsharks$mpa_isolation-1,level = 75)
metaiso75

#Create database from results above
iso = c(">20 km","<20 km","Continuous")
isoes = c(1.2329,0.6255,0.5491)
iso95lb = c(0.7547,0.2578,0.2184)
iso95ub = c(1.7112,0.9931,0.8798)
iso75lb = c(0.9522,0.4097,0.3550)
iso75ub = c(1.5136,0.8413,0.7432)

isodb = data.frame(iso,isoes,iso95lb,iso95ub,iso75lb,iso75ub)
isodb$iso <- ordered(isodb$iso,levels=c(">20 km","<20 km","Continuous"))

pd <- position_dodge(0.25)
isoplot<-ggplot(isodb,aes(x=iso,y=isoes))+
  ylab("Effect Size")+
  xlab("Distinctness")+
  geom_errorbar(aes(ymin=iso95lb, ymax=iso95ub), width=.25, position=pd, colour = "red4",size = 1)+
  geom_errorbar(aes(ymin=iso75lb, ymax=iso75ub),width = 0,size = 3,position = pd,colour = "red4",alpha = 0.5)+
  geom_point(size = 5)+
  geom_hline(yintercept=0, linetype='dotted')+
  theme_classic()+Theme2

isoplot

#Calculate effect sizes for Shark Sanctuary categories
reefsharks$Shark_Sanctuary <- as.factor(gsub('1', 'Yes', reefsharks$Shark_Sanctuary))
reefsharks$Shark_Sanctuary <- as.factor(gsub('0', 'No', reefsharks$Shark_Sanctuary))

#CI = 95
metass95=rma(reefsharks$es,reefsharks$var,slab=reefsharks$unique_reserve,mods=~reefsharks$Shark_Sanctuary-1,level = 95)
metass95

#CI = 75
metass75=rma(reefsharks$es,reefsharks$var,slab=reefsharks$unique_reserve,mods=~reefsharks$Shark_Sanctuary-1,level = 75)
metass75

#Copy results from above to make database
ss = c("No","Yes")
sses = c(0.7652,0.4497)
ss95lb = c(0.5268,-0.1451)
ss95ub = c(1.0037,1.0445)
ss75lb = c(0.6253,0.1006)
ss75ub = c(0.9052,0.7988)

ssdb = data.frame(ss,sses,ss95lb,ss95ub,ss75lb,ss75ub)

pd <- position_dodge(0.25)
ssplot<-ggplot(ssdb,aes(x=ss,y=sses))+
  ylab("Effect Size")+
  xlab("Shark Sanctuary")+
  geom_errorbar(aes(ymin=ss95lb, ymax=ss95ub), width=.25, position=pd, colour = "darkorange3",size = 1)+
  geom_errorbar(aes(ymin=ss75lb, ymax=ss75ub),width = 0,size = 3,position = pd,colour = "darkorange3",alpha = 0.5)+
  geom_point(size = 5)+
  geom_hline(yintercept=0, linetype='dotted')+
  theme_classic()+Theme2

ssplot

#All plots saved and added to importance plot in powerpoint


