library(ggplot2)
library(plyr)
library(dplyr)
library(EnvStats)
library(reshape2)

# load data
setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()
dat = read.csv("maxn_data_mixed_management.csv",header = T, strip.white = T)

#Add zeros by converting to a wide format
widedata=dcast(dat,unique_id+location_name+site_name+reef_name+mixed_management+depth+visibility+live_coral+substrate_relief_mean+reef_type~species_group,value.var="maxn",fun.aggregate=sum)

#Remove the dummy variable and rays
widedata = widedata [,-c(11,12)]

covariates1 = read.csv("location_covariates.csv",header = T,strip.white = T)

covariates1$location_name = covariates1$FP_location_name

firstjoin = left_join(widedata,covariates1, by = "location_name")

covariates2 = read.csv("set_data_covariates.csv",header = T,strip.white = T)

covariates2$unique_id = do.call(paste, c(covariates2[c("trip_code", "set_code")], sep = "_"))
covariates2 = subset(covariates2,functional_group == "apex") 
covariates2 = covariates2[,c(85,87,88)]

# Collapse as repeated values for each group
datandco = left_join(firstjoin,covariates2, by = "unique_id")
colnames(datandco)

#Calculate means for missing habitat data per location
habmeans <- ddply(datandco, .(site_name), summarise,
                  live_coral = mean(live_coral,na.rm = TRUE),
                  visibility = mean( visibility,na.rm = TRUE),
                  substrate_relief_mean   = mean(substrate_relief_mean,na.rm = TRUE))

#Use mean from nearest site for those missing habitat data
replacementmeans = subset(habmeans, site_name %in% c("Nosy Iranja","Cayo Serranilla","Eastern Qatar","Andros","Penghu","Dongsha","Rurutu","Cagayan Island"))
replacementmeans$site_name = revalue(replacementmeans$site_name,c("Nosy Iranja" = "Barren Islands","Cayo Serranilla" = "Cayos de Albuquerque","Eastern Qatar" = "Central Qatar","Andros" = "Eleuthera","Penghu" = "Green Island","Dongsha" = "Orhcid Island","Rurutu" = "Marutea","Cagayan Island" = "Oslob"))
habmeans = habmeans[complete.cases(habmeans),]
finhabmeans = rbind(habmeans,replacementmeans)

datandco = datandco %>% inner_join(finhabmeans, by= "site_name") %>%
  mutate(visibility = coalesce(visibility.x, visibility.y))  %>%
  mutate(live_coral = coalesce(live_coral.x, live_coral.y))%>%
  mutate(substrate_relief_mean = coalesce(substrate_relief_mean.x, substrate_relief_mean.y))

#Remove Remote Reefs Gravity = 0
gravmeanbyreef <- ddply(datandco, .(reef_name), summarise,
                        mean_grav_total = mean(Grav_Total,na.rm = TRUE))

zerogravreefs = subset(gravmeanbyreef,mean_grav_total == 0)
listzerogravreefs = unique(zerogravreefs$reef_name)

datandco = subset(datandco,!reef_name %in% listzerogravreefs)

#Remove depths greater than 40 meters (not supposed to have been sampled)
datandco = filter(datandco,depth <40.1)

#Change ineffective FM to the intercept for modelling
datandco$mixed_management = revalue(datandco$mixed_management,c("Ineffective_FM" = "AIneffective_FM"))

#Remove categories that are not being assessed (e.g. Shark Sanctuaries)
datandco = subset(datandco,!mixed_management %in% c("Shark_Sanctuary","Remote_NT","Remote","Exclude_NT_Not_Sampled","Exclude_NT_Only_Sampled"))

#Remove Hawaii as the FPA was removed due to having zero gravity
datandco = subset(datandco,!location_name  == "USA-Pacific")

#Calculate the raw means per site for each category
sitemeans <- ddply(datandco, .(mixed_management,site_name), summarise,
                   mean = mean(Shark))

#Rename and create all the various combinations of categories for plotting
sitemeans$category = sitemeans$mixed_management
unique(sitemeans$category)

sitemeans$category = revalue(sitemeans$category,c("AIneffective_FM" = "Ineffective","Effective_FM" = "Effective","MR&Effective_FM_Closed" = "Effective Closed",
                                                  "MR&Effective_FM_Open" = "Effective Open","MR&Ineffective_FM_Closed" = "Ineffective Closed","MR&Ineffective_FM_Open" = "Ineffective Open"))

sitemeans$mr = sitemeans$mixed_management
sitemeans$mr = revalue(sitemeans$mr,c("AIneffective_FM" = "Fisheries Management Only","Effective_FM" = "Fisheries Management Only","MR&Effective_FM_Closed" = "NTMRs & Fisheries Management",
                                      "MR&Effective_FM_Open" = "NTMRs & Fisheries Management","MR&Ineffective_FM_Closed" = "NTMRs & Fisheries Management","MR&Ineffective_FM_Open" = "NTMRs & Fisheries Management"))
unique(sitemeans$mr)

sitemeans$Management_Actions = sitemeans$mixed_management
sitemeans$Management_Actions = revalue(sitemeans$Management_Actions,c("AIneffective_FM" = "Ineffective","Effective_FM" = "Effective","MR&Effective_FM_Closed" = "Effective",
                                                                      "MR&Effective_FM_Open" = "Effective","MR&Ineffective_FM_Closed" = "Ineffective","MR&Ineffective_FM_Open" = "Ineffective"))
unique(sitemeans$Management_Actions)

sitemeans$Protection_Status = sitemeans$mixed_management
sitemeans$Protection_Status = revalue(sitemeans$Protection_Status,c("AIneffective_FM" = "Outside","Effective_FM" = "Outside","MR&Effective_FM_Closed" = "Inside",
                                                                    "MR&Effective_FM_Open" = "Outside","MR&Ineffective_FM_Closed" = "Inside","MR&Ineffective_FM_Open" = "Outside"))
unique(sitemeans$Protection_Status)

#Identify outliers
fmoe = subset(sitemeans,mr == "Fisheries Management Only")
fmoe = subset(fmoe,Management_Actions == "Effective")
test = rosnerTest(fmoe$mean,k=3)
test

ineff = subset(sitemeans,Management_Actions == "Ineffective")
ineffmr = subset(ineff,mr == "NTMRs & Fisheries Management")
ineffmro = subset(ineffmr,Protection_Status == "Outside")
test2 = rosnerTest(ineffmro$mean,k=3)
test2

ineffnmr = subset(ineff,mr == "Fisheries Management Only")
test3 = rosnerTest(ineffnmr$mean,k=3)
test3

effnmr = subset(sitemeans,Management_Actions == "Effective")
effnmr = subset(effnmr,mr == "NTMRs & Fisheries Management")
effmri = subset(effnmr,Protection_Status == "Inside")
test4 = rosnerTest(effmri$mean,k=3)
test4

#Separate outliers from main data and save for modelling
datandcofinal = subset(datandco,!site_name %in% c("Cocos-Keeling","Zaira Area","Pedro Bank"))
write.csv(datandcofinal,"maxn_data_mixed_management_cleaned.csv")

#Separate outliers from mean data
outliers = subset(sitemeans,site_name %in% c("Cocos-Keeling","Zaira Area","Pedro Bank"))
means_no_outliers = subset(sitemeans,!site_name %in% c("Cocos-Keeling","Zaira Area","Pedro Bank"))

#Order for plot
means_no_outliers$mr <- ordered(means_no_outliers$mr,levels=c("NTMRs & Fisheries Management","Fisheries Management Only"))

#Total means with outliers removed
totalmeans <- ddply(means_no_outliers, .(Management_Actions,Protection_Status,mr), summarise,
                   sd =sd(mean),    
                   se = sd(mean)/sqrt(length(mean)),
                   mean = mean(mean))

#Total means with outliers included
totalmeans_with_outliers <- ddply(sitemeans, .(Management_Actions,Protection_Status,mr), summarise,
                    sd =sd(mean),    
                    se = sd(mean)/sqrt(length(mean)),
                    mean = mean(mean))

#Creat a database to plot position of mean if outliers are included
om = data.frame(Management_Actions = c("Effective","Ineffective","Ineffective","Ineffective")
                ,mr = c("Fisheries Management Only","Fisheries Management Only","NTMRs & Fisheries Management","NTMRs & Fisheries Management")
                ,Protection_Status = c("Outside","Outside","Outside","Inside"),mean = c(0.634,0.109,0.173,NA))

om$mr <- ordered(om$mr,levels=c("NTMRs & Fisheries Management","Fisheries Management Only"))
mrlabels = c("(i) FPAs & Fisheries Management","(ii) Fisheries Management Only")
names(mrlabels) <- c("NTMRs & Fisheries Management","Fisheries Management Only")

#rename Zaira and drop Pedro name inside which is not an outlier
outliers$site_name = revalue(outliers$site_name,c("Zaira Area" = "South East Marovo"))
outliers$site_name = replace(outliers$site_name,outliers$Protection_Status == "Inside","")

# Pre sets for plot
dodge <- position_dodge(0.55)
ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() } 
PaletteFill<- scale_fill_manual(values=c("#66CC33","#0000FF"))
PaletteColour<- scale_colour_manual(values=c("#000000","#000000"))
PaletteShape <-  scale_shape_manual(values=c(15,16))

#create plot

Plot<-ggplot(means_no_outliers, aes(y=mean, x= Management_Actions))+
  geom_errorbar(data = totalmeans,aes(fill = Protection_Status,ymin = mean-se, ymax = mean+se),position = dodge,width = 0.2)+
  geom_point(aes(colour = Protection_Status),size=2,position = dodge,alpha = 0.2)+
  geom_point(data = outliers, aes(fill = Protection_Status),colour = "red",position = dodge,size=2,alpha = 0.3)+
  geom_point(data = om, aes(fill = Protection_Status),colour = "red",size=3,position = dodge,shape = 8)+ 
  geom_text(data = outliers,aes(colour = Protection_Status,label = site_name),size = 3,nudge_x = 0.3,nudge_y = 0.09)+
  geom_violin(aes(fill = Protection_Status),position = dodge,alpha = 0.1, colour = "black")+ 
  stat_summary(aes(fill = Protection_Status),fun = mean,geom = "point", shape = 21, size = 5,position = dodge)+
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
  ylab("Mean MaxN ± SE")+
  xlab("Management Actions")

Plot + facet_grid(.~mr,labeller = labeller(mr = mrlabels)) 
