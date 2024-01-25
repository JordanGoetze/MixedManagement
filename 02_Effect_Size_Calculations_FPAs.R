#Split sharks into groups and run effect size analysis on fully protected areas
#Load packages
library(plyr)
library(reshape2)
library(metafor)
library(gplots)
library(dplyr)

setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
alldata = read.csv("maxn_data_fpas.csv",header = TRUE, strip.white = TRUE)

#Add zeros by converting to a wide format
widedata=dcast(alldata,unique_id+unique_reserve+protection_status~species_group,value.var="maxn",fun.aggregate=sum)

#Remove the dummy variable
widedata = widedata [,-c(4)]

#melt back to long format
longdata=melt(widedata)

######All_Sharks########
#subset to look at all sharks
all_sharks = subset(longdata, variable == "Shark")

#Summarise data to give Number, Mean, SD and SE
sum_per_sample = ddply(all_sharks, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means <- ddply(sum_per_sample, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Add a constant of lowest density to reserves with one-sides zeros
#For modeled data include two-sided zeros as well
oszeros = c("Sandals Boscobel","Oracabessa","Abrolhos","Apo","Easter","Glover's Reef","Kapoposang 1",
            "Kapoposang 2","Kisite","Maud","North","Pelican",
            "Pigeon Island","Sipadan")

min_dens = means[grepl(paste(oszeros, collapse="|"), means$unique_reserve),]
min_dens$mean = min_dens$mean + 0.01
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.01)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.1)
min_dens2 = means[!grepl(paste(oszeros, collapse="|"), means$unique_reserve),]

means2 = rbind(min_dens,min_dens2)


es = melt(means2)
es = dcast(es,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)


list(colnames(es))

#Calculate effect sizes
es$es=log(es$closed_mean/es$open_mean)

#Calculate error
#Calculating variance of before ratio
es$var=(es$closed_sd^2)/(es$closed_n*es$closed_mean^2)+(es$open_sd^2)/(es$open_n*es$open_mean^2)
es$var = as.numeric(es$var)

es$variable = "sharks"

#Run effect size analyses 
all_sharks_rma=rma(yi = es$es,vi = es$var,slab=es$unique_reserve,level = 95)
all_sharks_rma
forest(all_sharks_rma,main="All Sharks",showweight=T,order = "obs")

write.csv(es,"all_sharks_es.csv",row.names = FALSE)

######All_Rays########
all_rays = subset(longdata, variable == "Ray")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_rays = ddply(all_rays, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_rays <- ddply(sum_per_sample_rays, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Update one sides zeros for this group
oszeros = c("Cloates","Bundegi","Kisite","Mandu","MNP-18-1084","Nasue",
            "North Monte","Osprey","Pelican","Pelsaert","Pigeon Island","Sipadan",
            "South Muiron","Wallabi")
min_dens = means_rays[grepl(paste(oszeros, collapse="|"), means_rays$unique_reserve),]
min_dens$mean = min_dens$mean + 0.015
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.015)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.124)

min_dens2 = means_rays[!grepl(paste(oszeros, collapse="|"), means_rays$unique_reserve),]

means_rays = rbind(min_dens,min_dens2)

es_rays = melt(means_rays)
es_rays = dcast(es_rays,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)
list(colnames(es_rays))

# Calc Effect Size
es_rays$es=log(es_rays$closed_mean/es_rays$open_mean)

#Calculating variance of before ratio
es_rays$var=(es_rays$closed_sd^2)/(es_rays$closed_n*es_rays$closed_mean^2)+(es_rays$open_sd^2)/(es_rays$open_n*es_rays$open_mean^2)
es_rays$var = as.numeric(es_rays$var)

es_rays$variable = "rays"

# Remove assessments with zero rays inside and outside
es_rays <- filter(es_rays,!closed_mean == 0)

#Run effect size analysis
rays_rma=rma(es_rays$es,es_rays$var,slab=es_rays$unique_reserve,level = 95)
rays_rma
forest(rays_rma,main="All Rays",showweight=T,order = "obs")

######Small_Rays########

#Add zeros by converting to a wide format for functional ray group
widedata_rays=dcast(alldata,unique_id+unique_reserve+protection_status~functional_group,value.var="maxn",fun.aggregate=sum)

#Remove the dummy variable
widedata_rays = widedata_rays [,-c(4)]

#melt back to long format
longdata_rays=melt(widedata_rays)

#subset to look at small rays
small_ray = subset(longdata_rays, variable == "small_ray")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_sray = ddply(small_ray, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_sray <- ddply(sum_per_sample_sray, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Update one sided zeros
oszeros = c("Bundegi","Caye Caulker","Gili Matra","iSimangaliso","Kisite","Maud",
            "MNP-18-1084","Namena","North Monte","Pigeon Island","Sipadan","SR-18-2007","Tanikely")
min_dens = means_sray[grepl(paste(oszeros, collapse="|"), means_sray$unique_reserve),]
min_dens$mean = min_dens$mean + 0.009
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.009)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.09)

min_dens2 = means_sray[!grepl(paste(oszeros, collapse="|"), means_sray$unique_reserve),]

means_sray = rbind(min_dens,min_dens2)

es_sray = melt(means_sray)
es_sray = dcast(es_sray,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_sray))

# Calc Effect Size
es_sray$es=log(es_sray$closed_mean/es_sray$open_mean)

# Calc Error

#Calculating variance of before ratio
es_sray$var=(es_sray$closed_sd^2)/(es_sray$closed_n*es_sray$closed_mean^2)+(es_sray$open_sd^2)/(es_sray$open_n*es_sray$open_mean^2)
es_sray$var = as.numeric(es_sray$var)

es_sray$variable = "small_rays"

# Remove assessments with zero rays inside and outside
es_sray <- filter(es_sray,!closed_mean == 0)

#Run effect size analysis
sray_rma=rma(es_sray$es,es_sray$var,slab=es_sray$unique_reserve,level = 95)
sray_rma
forest(sray_rma,main="Small Rays",showweight=T,order = "obs")

######Large_Rays########
#subset to look at large rays
large_ray = subset(longdata_rays, variable == "large_ray")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_lrays = ddply(large_ray, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_lrays <- ddply(sum_per_sample_lrays, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Update one sides zeros for group
oszeros = c("Apo","Bird Cay","Cloates","Kapoposang 2","Lighthouse","Maud","MNP-11-1004",
            "MNP-18-1081","MNP-18-1084","Nasue","North Monte","Osprey","Pelican",
            "Sipadan","South Muiron","Tanikely","Tubbataha")
min_dens = means_lrays[grepl(paste(oszeros, collapse="|"), means_lrays$unique_reserve),]
min_dens$mean = min_dens$mean + 0.008
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.008)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.09)

min_dens2 = means_lrays[!grepl(paste(oszeros, collapse="|"), means_lrays$unique_reserve),]

means_lrays = rbind(min_dens,min_dens2)

es_lrays = melt(means_lrays)
es_lrays = dcast(es_lrays,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)
list(colnames(es_lrays))

# Calc Effect Size
es_lrays$es=log(es_lrays$closed_mean/es_lrays$open_mean)

# Calc Error
es_lrays$var=(es_lrays$closed_sd^2)/(es_lrays$closed_n*es_lrays$closed_mean^2)+(es_lrays$open_sd^2)/(es_lrays$open_n*es_lrays$open_mean^2)
es_lrays$var = as.numeric(es_lrays$var)

es_lrays$variable = "large_rays"

# Remove assessments with zero rays inside and outside
es_lrays <- filter(es_lrays,!closed_mean == 0)

#Run effect size analysis
lrays_rma=rma(es_lrays$es,es_lrays$var,slab=es_lrays$unique_reserve,level = 95)
lrays_rma
forest(lrays_rma,main="Large Rays",showweight=T,order = "obs")

############################################################################
#Bring in movement data to break up sharks into reef associated and wide-ranging

######Reef-associated Sharks########
movement = read.csv("species_movement.csv",header = TRUE)

data_mov = left_join(alldata,movement, by = "latin_name")

#Add zeros by converting to a wide format
widedatamov=dcast(data_mov,unique_id+unique_reserve+protection_status~movement,value.var="maxn",fun.aggregate=sum)

#Remove the dummy variable
widedatamov = widedatamov [,-c(6)]

#melt back to long format
longdatamov=melt(widedatamov)

#subset to look at reef-associated sharks only
reef_ass = subset(longdatamov, variable == "Reef Associated")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species
sum_per_sample_ra = ddply(reef_ass, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_ra <- ddply(sum_per_sample_ra, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Update one sides zeros for this group
oszeros = c("Sandals Boscobel","Oracabessa","Abrolhos","Apo","Bundegi","Corals Rosario and San Bernardo",
            "Easter","Glover's Reef","Kapoposang 1",
            "Kapoposang 2","Kisite","Maud","North","Pigeon Island","Pelican",
            "Petit Terre","Santo Domingo","Sipadan","Techobanine")

min_dens = means_ra[grepl(paste(oszeros, collapse="|"), means$unique_reserve),]
min_dens$mean = min_dens$mean + 0.0169
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.0169)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.13)
min_dens2 = means_ra[!grepl(paste(oszeros, collapse="|"), means_ra$unique_reserve),]
means_ra = rbind(min_dens,min_dens2)

es_ra = melt(means_ra)
es_ra = dcast(es_ra,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_ra))

# Calc Effect Size
es_ra$es_ra=log(es_ra$closed_mean/es_ra$open_mean)

#Calc Error
#Calculating variance of before ratio
es_ra$var=(es_ra$closed_sd^2)/(es_ra$closed_n*es_ra$closed_mean^2)+(es_ra$open_sd^2)/(es_ra$open_n*es_ra$open_mean^2)
es_ra$var = as.numeric(es_ra$var)

es_ra$variable = "reef associated"

#Run effect size analyses 
ra_rma=rma(es_ra$es,es_ra$var,slab=es_ra$unique_reserve,level = 95)
ra_rma
forest(ra_rma,main="Reef-Associated Sharks",showweight=T,order = "obs")

write.csv(es_ra,"reef_associated_es.csv",row.names = FALSE)

######Wide-ranging Sharks########
wide_range = subset(longdatamov, variable == "Wide Ranging")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_wr = ddply(wide_range, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_wr <- ddply(sum_per_sample_wr, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Update one sides zeros for this group
oszeros = c("Ngeruangel","Jardines de la Reina","Ashmore","Bazaruto","Caye Caulker",
            "Curacao","Dutch Reserve","Mandu","Maud","Namena",
            "Osprey","Pelican","Pelsaert","Petit Terre","South Muiron",
            "Saba","SR-18-2007","Sunday Island","Tubbataha","Techobanine",
            "Winderabandi","Sipadan")
min_dens = means_wr[grepl(paste(oszeros, collapse="|"), means_wr$unique_reserve),]
min_dens$mean = min_dens$mean + 0.012
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.012)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.11)
min_dens2 = means_wr[!grepl(paste(oszeros, collapse="|"), means_wr$unique_reserve),]

means_wr = rbind(min_dens,min_dens2)

es_wr = melt(means_wr)
es_wr = dcast(es_wr,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_wr))

# Calc Effect Size
es_wr$es_wr=log(es_wr$closed_mean/es_wr$open_mean)

# Calc Error

#Calculating variance of before ratio
es_wr$var=(es_wr$closed_sd^2)/(es_wr$closed_n*es_wr$closed_mean^2)+(es_wr$open_sd^2)/(es_wr$open_n*es_wr$open_mean^2)

es_wr$var = as.numeric(es_wr$var)

es_wr$variable = "wide ranging"

# Remove assessments with zero sharks inside and outside as these are not modeled for co-variates
es_wr <- filter(es_wr,!closed_mean == 0)

#Run effect size analyses 
wr_rma=rma(es_wr$es,es_wr$var,slab=es_wr$unique_reserve,level = 95)
wr_rma
forest(wr_rma,main="Wide-Ranging Sharks",showweight=T,order = "obs")

################################################################################
#######Individual Species##########
#Add zeros by converting to a wide format for species
widedata_ind=dcast(alldata,unique_id+unique_reserve+protection_status~latin_name,value.var="maxn",fun.aggregate=sum)

#Remove the dummy variable
widedata_ind = widedata_ind [,-c(4)]

#melt back to long format
longdata_ind=melt(widedata_ind)

######Grey_Reef########
#subset to look at sharks only
grey_reef = subset(longdata_ind, variable == "Carcharhinus amblyrhynchos")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_gr = ddply(grey_reef, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_gr <- ddply(sum_per_sample_gr, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Update one sided zeros
oszeros = c("Techobanine","Bundegi","Hawaii","Maud","Pelican","Sipadan","Tetaiuo",
            "Tiahura","Tubbataha")
min_dens = means_gr[grepl(paste(oszeros, collapse="|"), means_gr$unique_reserve),]
min_dens$mean = min_dens$mean + 0.059
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.059)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.24)

min_dens2 = means_gr[!grepl(paste(oszeros, collapse="|"), means_gr$unique_reserve),]

means_gr = rbind(min_dens,min_dens2)

es_gr = melt(means_gr)
es_gr = dcast(es_gr,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_gr))

# Calc Effect Size
es_gr$es=log(es_gr$closed_mean/es_gr$open_mean)

#Calculating variance of before ratio
es_gr$var=(es_gr$closed_sd^2)/(es_gr$closed_n*es_gr$closed_mean^2)+(es_gr$open_sd^2)/(es_gr$open_n*es_gr$open_mean^2)

es_gr$var = as.numeric(es_gr$var)

es_gr$variable = "Carcharhinus amblyrhynchos"

# Remove assessments with zero sharks inside and outside 
es_gr <- filter(es_gr,!closed_mean == 0)

#Run effect size analysis
gr_rma=rma(es_gr$es,es_gr$var,slab=es_gr$unique_reserve,level = 95)
gr_rma
forest(gr_rma,main="Grey Reef",showweight=T,order = "obs")

######Whitetip########

#subset to look at sharks only
whitetip = subset(longdata_ind, variable == "Triaenodon obesus")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species
sum_per_sample_wt = ddply(whitetip, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_wt <- ddply(sum_per_sample_wt, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Add a constant of 0.5 to reserves with one-sides zeros
oszeros = c("Apo","Bazaruto","Cloates","iSimangaliso","Kapoposang 1","Kapoposang 2",
            "Lighthouse","MNP-23-1164","Namuri","Osprey","Pelican","SR-18-2007",
            "Tetaiuo","Tiahura","Sipadan")
min_dens = means_wt[grepl(paste(oszeros, collapse="|"), means_wt$unique_reserve),]
min_dens$mean = min_dens$mean + 0.009
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.009)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.096)

min_dens2 = means_wt[!grepl(paste(oszeros, collapse="|"), means_wt$unique_reserve),]

means_wt = rbind(min_dens,min_dens2)

es_wt = melt(means_wt)
es_wt = dcast(es_wt,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_wt))

# Calc Effect Size
es_wt$es=log(es_wt$closed_mean/es_wt$open_mean)

# Calc Error
es_wt$var=(es_wt$closed_sd^2)/(es_wt$closed_n*es_wt$closed_mean^2)+(es_wt$open_sd^2)/(es_wt$open_n*es_wt$open_mean^2)

es_wt$var = as.numeric(es_wt$var)

es_wt$variable = "Triaenodon obesus"

# Remove assessments with zero sharks inside and outside 
es_wt <- filter(es_wt,!closed_mean == 0)

#Run effect size analysis
wt_rma=rma(es_wt$es,es_wt$var,slab=es_wt$unique_reserve,level = 95)
wt_rma
forest(wt_rma,main="Whitetip",showweight=T,order = "obs")

######Blacktip########
#subset to look at sharks only
blacktip = subset(longdata_ind, variable == "Carcharhinus melanopterus")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_bt = ddply(blacktip, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_bt <- ddply(sum_per_sample_bt, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Apply the lowest mean (and its SE) to the one sided zeros
oszeros = c("Apo","Ashmore","Kapoposang 1","Kapoposang 2","Maud","MNP-23-1164","Namuri","Nasue","North Monte","Pigeon Island","Tubbataha")
min_dens = means_bt[grepl(paste(oszeros, collapse="|"), means_bt$unique_reserve),]
min_dens$mean = min_dens$mean + 0.03
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.03)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.18)

min_dens2 = means_bt[!grepl(paste(oszeros, collapse="|"), means_bt$unique_reserve),]
means_bt = rbind(min_dens,min_dens2)

es_bt = melt(means_bt)
es_bt = dcast(es_bt,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_bt))

# Calc Effect Size
es_bt$es=log(es_bt$closed_mean/es_bt$open_mean)

#Calculating variance of before ratio
es_bt$var=(es_bt$closed_sd^2)/(es_bt$closed_n*es_bt$closed_mean^2)+(es_bt$open_sd^2)/(es_bt$open_n*es_bt$open_mean^2)

es_bt$var = as.numeric(es_bt$var)

es_bt$variable = "Carcharhinus melanopterus"

# Remove assessments with zero sharks inside and outside 
es_bt <- filter(es_bt,!closed_mean == 0)

#Run effect size analysis
########Abun / Target
bt_rma=rma(es_bt$es,es_bt$var,slab=es_bt$unique_reserve,level = 95)
bt_rma
forest(bt_rma,main="Blacktip",showweight=T,order = "obs")

######Caribbean########
caribbean = subset(longdata_ind, variable == "Carcharhinus perezi")

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_cr = ddply(caribbean, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))

means_cr <- ddply(sum_per_sample_cr, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

oszeros = c("Abrolhos","Corals Rosario and San Bernardo","Glover's Reef")
min_dens = means_cr[grepl(paste(oszeros, collapse="|"), means_cr$unique_reserve),]
min_dens$mean = min_dens$mean + 0.01
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.01)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.1)

min_dens2 = means_cr[!grepl(paste(oszeros, collapse="|"), means_cr$unique_reserve),]

means_cr = rbind(min_dens,min_dens2)

es_cr = melt(means_cr)
es_cr = dcast(es_cr,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_cr))

# Calc Effect Size
es_cr$es=log(es_cr$closed_mean/es_cr$open_mean)

#Calculating variance of before ratio
es_cr$var=(es_cr$closed_sd^2)/(es_cr$closed_n*es_cr$closed_mean^2)+(es_cr$open_sd^2)/(es_cr$open_n*es_cr$open_mean^2)

es_cr$var = as.numeric(es_cr$var)

es_cr$variable = "Carcharhinus perezi"

# Remove assessments with zero sharks inside and outside 
es_cr <- filter(es_cr,!closed_mean == 0)

#Run effect size analysis
########Abun / Target
cr_rma=rma(es_cr$es,es_cr$var,slab=es_cr$unique_reserve,level = 95)
cr_rma
forest(cr_rma,main="Caribbean",showweight=T,order = "obs")

######Nurse########

#subset to look at sharks only
subset = subset(longdata_ind, variable %in% c("Ginglymostoma cirratum","Nebrius ferrugineus"))

# Summarises data to give Number, Mean, SD and SE for individual species and all harvested species

sum_per_sample_nur = ddply(subset, .(unique_reserve,protection_status,unique_id), summarise, 
                       maxn  = sum(value))
means_nur <- ddply(sum_per_sample_nur, .(unique_reserve,protection_status), summarise, 
               n      = length(maxn),
               mean = mean(maxn),
               sd     = sd(maxn),
               se     = sd(maxn) / sqrt(length(maxn)) )

#Add a constant (the lowest density) to reserves with one-sides zeros
oszeros = c("Jardines de la Reina","Abrolhos","Ashmore","Cloates","Corals Rosario and San Bernardo","Glover's Reef",
            "Kapoposang 2","Lighthouse","Maud","MNP-11-1004","MNP-18-1081","MNP-23-1164","Pelican",
            "Petit Terre","Santo Domingo","South Muiron","Sunday Island","Techobanine","Tetaiuo","Tiahura","Tubbataha")
min_dens = means_nur[grepl(paste(oszeros, collapse="|"), means_nur$unique_reserve),]
min_dens$mean = min_dens$mean + 0.008
min_dens$se = replace(min_dens$se,min_dens$se == 0,0.008)
min_dens$sd = replace(min_dens$sd,min_dens$sd == 0,0.09)

min_dens2 = means_nur[!grepl(paste(oszeros, collapse="|"), means_nur$unique_reserve),]

means_nur = rbind(min_dens,min_dens2)

es_nur = melt(means_nur)
es_nur = dcast(es_nur,unique_reserve~protection_status+variable,value.var = "value",fun.aggregate = sum)

list(colnames(es_nur))

# Calc Effect Size
es_nur$es=log(es_nur$closed_mean/es_nur$open_mean)

#Calculating variance of before ratio
es_nur$var=(es_nur$closed_sd^2)/(es_nur$closed_n*es_nur$closed_mean^2)+(es_nur$open_sd^2)/(es_nur$open_n*es_nur$open_mean^2)

es_nur$var = as.numeric(es_nur$var)

es_nur$variable = "Ginglymostomatidae"

# Remove assessments with zero sharks inside and outside 
es_nur <- filter(es_nur,!closed_mean == 0)

#Run effect size analysis
########Abun / Target
nur_rma=rma(es_nur$es,es_nur$var,slab=es_nur$unique_reserve,level = 95)
nur_rma
forest(nur_rma,main="Nurse",showweight=T,order = "obs")

#######Finished!!!!!##########