# A simple function for full subsets multiple regression in ecology with R

# Source functions----
devtools::install_github("beckyfisher/FSSgam_package")
library(FSSgam)
library(RCurl)
require(mgcv)
require(MuMIn)
require(doParallel)
require(dplyr)
library(arsenal)
library(mgcViz)

# load data
setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()

#Have include reef associated and all sharks to compare but only uses RA in the manuscript as they had the greatest response on therefore best option for exploring co-variates
ra<-read.csv("reef_associated_es.csv",header = T,strip.white = T)
rs<-read.csv("all_sharks_es.csv",header = T,strip.white = T)
colnames(ra)
names(ra)[names(ra) == "es_ra"] <- "es"
colnames(rs)
dat = rbind(ra,rs) 

#Load in covariate data
covariates1 = read.csv("fpa_metadata.csv",header = T,strip.white = T)
covariates2 = read.csv("location_covariates.csv",header = T,strip.white = T)

allco = left_join(covariates1,covariates2, by = "location_id")

esandco = left_join(dat,allco, by = "unique_reserve")
colnames(esandco)

#Transform data as required
esandco$log_mpa_area = log(esandco$mpa_area)
esandco$log_GDP_per_capita_USD = log(esandco$GDP_per_capita_USD)
esandco$sqrt_mpa_age = sqrt(esandco$mpa_age)
esandco$sqrt_Average.of.depth = sqrt(esandco$Average.of.depth)
esandco$sqrt_live_coral = sqrt(esandco$Live_Coral)
esandco$Average.of.Grav_NC = esandco$Average.of.Grav_NC+1
esandco$log_Average.of.Grav_NC = log(esandco$Average.of.Grav_NC)
esandco$Average.of.Grav_NP = esandco$Average.of.Grav_NP+1
esandco$log_Average.of.Grav_NP = log(esandco$Average.of.Grav_NP)
#Plus minimum value of gravity
esandco$Average.of.Grav_Total = esandco$Average.of.Grav_Total+0.03448276
esandco$log_Grav_Total_Plus_min = log(esandco$Average.of.Grav_Total)
esandco$Reefshark_catch_tonnes = esandco$Reefshark_catch_tonnes+1
esandco$log_Reefshark_catch_tonnes = log(esandco$Reefshark_catch_tonnes)
esandco$log_Average.of.local_population_2015 = log(esandco$Average.of.local_population_2015) 

#Set variable and predictors
colnames(esandco)
# Took out "Shark_Protection_Status" as shark sanctuary and fishing restrictions cover this. 
cat.preds=c("mpa_isolation","mpa_compliance","Shark_Sanctuary","location_name")
#log_reef_shark_tonnes taken out as 1 to 1 correlation with country. Include under 0.7
cont.preds=c("log_Grav_Total_Plus_min","log_mpa_area","sqrt_mpa_age","Average.of.substrate_relief_mean","sqrt_live_coral","sqrt_Average.of.depth")
unique(esandco$variable)
resp.vars = c("reef associated","sharks")
var = "var"

dat<-esandco%>%
  mutate(response=es)

# have a look at the distribution of the continuous predictors
pdf(file="pred_vars_fpa.pdf",onefile=T)
for(p in 1:length(cont.preds)){
  par(mfrow=c(2,1))
  hist(esandco[,cont.preds[p]],main=cont.preds[p])
  plot(jitter(esandco[,cont.preds[p]]))
}
dev.off()

esandco<-esandco%>%
  mutate(response=es)# set generic response for GAMM to be the es
use.dat = esandco

### now fit the models ---------------------------------------------------------
i=1
out.all=list()
var.imp=list()
fss.all=list()
top.all=list()
pdf(file="mod_fits_fpa.pdf",onefile=T,width =10)
for(i in 1:length(resp.vars)){
  print(i)
  print(resp.vars[i])
  use.dat=dat[which(dat$variable==resp.vars[i]),]
  use.dat$region_name = as.factor(use.dat$region_name)
  use.dat$variable = as.factor(use.dat$variable)
  use.dat$mpa_isolation = as.factor(use.dat$mpa_isolation)
  use.dat$Shark_fishing_restrictions_cat = as.factor(use.dat$Shark_fishing_restrictions_cat)
  use.dat$mpa_compliance = as.factor(use.dat$mpa_compliance)
  use.dat$Fisheries_Management = as.factor(use.dat$Fisheries_Management)

  #Example model
  Model1=gam(response~s(sqrt_mpa_age,k=3,bs='cr'),family=gaussian(),data=use.dat, weights = 1/var)

  #Full model
  model.set=generate.model.set(use.dat=use.dat,max.predictors=3,   # limit size here because null model already complex
                               test.fit=Model1,k=3,
                               cov.cutoff = 0.36,
                               smooth.smooth.interactions = FALSE,
                               factor.smooth.interactions = TRUE,
                               pred.vars.cont=cont.preds,
                               pred.vars.fact=cat.preds)
                              
  out.list=fit.model.set(model.set,max.models = 10000, VI.mods="all")
  fss.all=c(fss.all,list(out.list))
  mod.table=out.list$mod.data.out
  mod.table=mod.table[order(mod.table$AICc),]
  out.i=mod.table
  out.all=c(out.all,list(out.i))
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
  all.less.2AICc=mod.table[which(mod.table$delta.AICc<2),]
  top.all=c(top.all,list(all.less.2AICc))
  
  # plot the all best models
  par(oma=c(1,1,4,1))
  for(r in 1:nrow(all.less.2AICc)){
    best.model.name=as.character(all.less.2AICc$modname[r])
    best.model=out.list$success.models[[best.model.name]]
    if(best.model.name!="null"){ 
      plot(best.model,#Change here when switching $gam
      all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=3,text=resp.vars[i],outer=T)}
  }
}

dev.off()
warnings()
names(out.all)=resp.vars
names(var.imp)=resp.vars
names(top.all)=resp.vars
names(fss.all)=resp.vars

all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
top.mod.fits=do.call("rbind",top.all)

# Use to check for spatial autocorrelation using a variogram
#Variogram(Model1$lme,resType="normalized",form=~lat+long)
#plot(Variogram(Model1$lme,resType="normalized",form=~lat+long))


write.csv(all.mod.fits[,-2],"all_model_fits_fpa.csv")
write.csv(top.mod.fits[,-2],"top_model_fits_fpa.csv")
write.csv(model.set$predictor.correlations,"predictor_correlations_fpa.csv")

imptable = as.data.frame(var.imp)
write.csv(imptable,"importance_scores_fpa.csv")

#Save ES results with co-variates for other scripts
write.csv(esandco,"es_cov_fpa.csv",row.names= F)
