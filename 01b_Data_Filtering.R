#Script used to filter out appropriate data that can be used to assess a fully protected area (FPA)
#Decisions were made by consulting with local partners who completed field work
#and by plotting all replicate on google earth and examining in relation to FPA boundaries

library(plyr)
library(dplyr)


setwd("C:/Users/JordanGoetze/OneDrive - Department of Biodiversity, Conservation and Attractions/Research/Manuscripts/Manuscript_GlobalFinPrint Marine Reserves/Submission/Data")
dir()

data = read.csv("maxn_data_raw.csv", header=T,strip.white = TRUE)
unique(data$reef_name)
dim(data)
names(data)
levels(data$status)
#levels(data$campaignid)
sapply(data,class)

#Fix some mistakes where drops have been called open/restricted but are actually closed
#Includes Palau, Ngeruangel which was incorrectly allocated in metadata and two small FPAs in Jamaica Ocho Rios
toclosed <- scan(text="MM2_003	MM2_004	MM2_005	MM2_006	MM2_025	MM2_026	MM2_027	MM2_029	MM2_030	MM2_031	MM2_045
MM1_029	MM1_014	MM1_015	MM1_016	MM1_017	MM1_018	MM1_019 SL_iSN_013 SL_iSN_014 SL_iSN_015 SL_iSN_016 SL_iSN_017 SL_iSN_018
SL_iSN_019 SL_iSN_019 SL_iSN_019 SL_iSN_020 SL_iSN_021 SL_iSN_022 SL_iSN_022 SL_iSN_023 SL_iSN_024 SM_PPMR_018
SM_PPMR_019 SM_PPMR_020 SM_PPMR_021 SM_PPMR_022 SM_PPMR_023 SM_PPMR_024 SM_PPMR_025 ABPA_041
ABPA_046 ABPA_047 ABPA_051 ABPA_053 ABPA_054 ABPA_055 ABPA_056 ABPA_058 ABPA_059 ABPA_060
ABPA_062 ABPA_063 ABPA_064 ABPA_066 ABPA_067 ABPA_068 ABPA_069 ABPA_070 ABPA_072 ABPA_073
ABPA_075 ABPA_076 ABPA_077 ABPA_078 PBP_002 PBP_003 PBP_006 PBP_007 PBP_008 PBP_016 PBP_017 PBP_019 PBP_020 PBP_024 PBP_025 PBP_026 PBP_027 PBP_037 PBP_038 PBP_039 PBP_040 PBP_042 
NN_036 NN_037 NN_038 NN_039 NN_040 NN_041 NN_042 NN_043 NN_044 NN_045 NN_046 NN_047 NN_048 NN_049 NN_050 NN_051 NN_052 NN_053 NN_054 NN_055 NN_056 NN_057 NN_058 NN_059 NN_060 NN_061 NN_062 NN_063 NN_064 NN_065 NN_066 NN_067 NN_068 NN_069 NN_070 NN_071 NN_072 NN_073 NN_074 NN_075 NN_076 NN_077 NN_078 NN_079 NN_080 NN_081 NN_082 NN_083 NN_084 NN_085 NN_086 NN_087 NN_088 NN_089 NN_090 NN_091 NN_120 NN_121 
JMOR_015 JMOR_029 JMOR_030 JMOR_031 JMOR_032 JMOR_036 JMOR_037 JMOR_038", what="")
data$protection_status = replace(data$protection_status,data$set_code %in% c(toclosed),"closed")

#Opposite problem (some drops in Cuba called closed when open and Saba closed when restricted)
toopen <- scan(text="RR_060 RR_061 RR_062 RR_063 RR_064 RR_065 RR_066 RR_067 RR_068 RR_069", what="")
data$protection_status = replace(data$protection_status,data$set_code %in% c(toopen),"open")
data$protection_status = replace(data$protection_status,data$reef_name == "Saba East","restricted")
data$protection_status = replace(data$protection_status,data$set_code %in% c("SBW_001","SBW_002","SBW_003","SBW_004","SBW_005","SBW_006","SBW_007","SBW_008","SBW_009","SBW_010","SBW_023","SBW_024","SBW_025","SBW_026","SBW_034","SBW_035","SBW_036","SBW_037","SBW_038","SBW_039","SBW_040","SBW_041","SBW_042","SBW_043","SBW_044","SBW_046","SBW_047","SBW_048","SBW_049"),"restricted")

#Simplify to closed and open
data = mutate(data, protection_status = revalue(protection_status, c("restricted" = "open")))
data = mutate(data, protection_status = revalue(protection_status, c("openly fished" = "open")))

#Get a subset of data that has assessed a fully protected area
reserves = subset(data, protection_status%in%c("closed"))
unique(reserves$reef_name)
trips = unique(reserves$trip_code)
trips  = as.character(trips)
trips

#Make sure trips that can be used as controls for more distant reserves remain
trips2 = c("FP_2016_AU-I_02","FP_2018_BS_04","FP_2017_US-P_02","FP_2016_BS_02")
alltrips = c(trips,trips2)

# Now subset
reservedata = subset(data, trip_code%in%alltrips)
unique(reservedata$trip_code)

#create a unique ID as some drop codes are repeated over trips
reservedata$unique_id = do.call(paste, c(reservedata[c("trip_code", "set_code")], sep = "_"))

#Remove trips/reserves with no controls or FPAs that were sampled across multiple years
tripstoremove = c("FP_2015_AU-P_01","FP_2015_AU-P_02","LH_2013_FJ_01","LH_2012_FJ_01","LH_2009_FJ_01","LH_2014_AU-I_05","FP_2018_BS_03","FP_2018_BS_04","FP_2016_BZ_01","FP_2016_BZ_03","FP_2016_CU_03","FP_2016_JM_02","FP_2016_US-P_01","FP_2017_BZ_02",
"FP_2017_CU_03","FP_2017_MY_01","FP_2017_NZ_01","FP_2018_BS_02","FP_2018_BZ_01","LH_2014_US-P_01","LC_2010_BZ_01","LC_2011_BZ_01","LC_2012_BZ_01","LC_2009_BZ_01","LH_2013_AU-I_01","FP_2016_TC_01","FP_2017_TW_01")

#Filter out reefs that lack a controls or have incorrect status
reeftoremove = c("327","402","423","761","771","770","769","768","468","402","343","401","463","465","462","464","198","199","201",
                 "607","187","185","184","637","638","639","643","644","645",
                 "669","182","183","616","736","740","472","469","473","417","332")

#Filter out individual BRUV drops that are not appropriate controls
dropstoremove = c("ASAW_032","ASAW_012","ASAW_011","ASAW_050","ASAW_005","ASAW_004")

#Filter out set IDs with the same reef code but the same FPA was sampled across multiple years (e.g. the GBR) 
setstoremove =  scan(text="1661	1662	1663	1664	1665	1666	1667	1668	1669	1670	1671	1672	1673	1674	1675	1676	1677	1678	1679	1680	1681	1682	1683	1684	1685	1686	1687	1689	1690	1691	1692	1693	1694	1695	1696	1697	1698	1699	1700	1701	1702	1703	1704	1705	1706	1707	1708	1709	1710	1711	1712	1713	1714	1715	1716	1717	1718	1719	1720	1721", what="")

#Complete filtering
reservesandcontrols = reservedata %>% filter(!trip_code %in% tripstoremove)%>%
 filter(!reef_id %in% reeftoremove)%>%
 filter(!set_code %in% dropstoremove)%>%
 filter(!set_id %in% setstoremove)  

unique(reservesandcontrols$trip_code)

#load in data on assigned reserve names
dir()
uniquereservename = read.csv("fpa_names_setids.csv", header=T,strip.white = TRUE)

#Create unique_reserve column to ID reserve and its appropriate controls
uniquereservename$set_id = as.numeric(uniquereservename$set_id)

finaldata<-reservesandcontrols%>%
 left_join(uniquereservename, by="set_id",relationship = "many-to-many")

unique(finaldata$trip_code)
unique(finaldata$unique_reserve)

#Bring in drops to remove based on unbalanced depth and vis
habitatdrop = read.csv("depth_vis_to_remove.csv",header = TRUE)
habitatdrop = unique(habitatdrop$unique_id)
finaldata = subset(finaldata, !(unique_id %in% habitatdrop))

# save data
write.csv(finaldata,"maxn_data_fpas.csv",row.names = F)
