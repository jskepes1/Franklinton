#How to Perform the Crosswalk
#2020 Data pulled from TidyCensus
require(sf)
require(leaflet) #requires WGS 1984/CRS 4326
require(ggplot2)
require(tidycensus)
require(tigris)
require(tidyverse)
require(tidycensus)
require(sp)



##################### Step 1: Read in the Crosswalk and the 2021 Demographic Data
#read in the crosswalk
crosswalk<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/Crosswalk/nhgis_bg2020_tr2010_39.csv")
crosswalk <-crosswalk %>% 
  rename(GEOID=bg2020ge )
#read in the 2020 block group data
DF_2021bg<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/DF_2021.csv")


#####################Step 2: Join the Crosswalk
DF_2021bgcrosswalk<-left_join(DF_2021bg, crosswalk, by='GEOID')

DF_2021bgcrosswalk_weighted<-DF_2021bgcrosswalk

rm(list=setdiff(ls(), "DF_2021bgcrosswalk_weighted"))
#######################Step 3: Multiply by the Weights
DF_2021bgcrosswalk_weighted$asian_2021<-DF_2021bgcrosswalk_weighted$asian_2021*DF_2021bgcrosswalk_weighted$wt_pop
DF_2021bgcrosswalk_weighted$aian_2021<-DF_2021bgcrosswalk_weighted$aian_2021*DF_2021bgcrosswalk_weighted$wt_pop
DF_2021bgcrosswalk_weighted$hhlivingalone_2021<-DF_2021bgcrosswalk_weighted$hhlivingalone_2021*DF_2021bgcrosswalk_weighted$wt_hh
DF_2021bgcrosswalk_weighted$black_2021<-DF_2021bgcrosswalk_weighted$black_2021*DF_2021bgcrosswalk_weighted$wt_pop
DF_2021bgcrosswalk_weighted$familyhouseholds_2021<-DF_2021bgcrosswalk_weighted$familyhouseholds_2021*DF_2021bgcrosswalk_weighted$wt_hh
DF_2021bgcrosswalk_weighted$latino_2021<-DF_2021bgcrosswalk_weighted$latino_2021*DF_2021bgcrosswalk_weighted$latino_2021
DF_2021bgcrosswalk_weighted$multipleraces_2021<-DF_2021bgcrosswalk_weighted$multipleraces_2021*DF_2021bgcrosswalk_weighted$wt_pop
DF_2021bgcrosswalk_weighted$numberofhousingunits_2021<-DF_2021bgcrosswalk_weighted$numberofhousingunits_2021*DF_2021bgcrosswalk_weighted$wt_hu
DF_2021bgcrosswalk_weighted$totalpopulation_2021<-DF_2021bgcrosswalk_weighted$totalpopulation_2021*DF_2021bgcrosswalk_weighted$wt_pop
DF_2021bgcrosswalk_weighted$vacantunits_2021<-DF_2021bgcrosswalk_weighted$vacantunits_2021*DF_2021bgcrosswalk_weighted$wt_hu
DF_2021bgcrosswalk_weighted$white_2021<-DF_2021bgcrosswalk_weighted$white_2021*DF_2021bgcrosswalk_weighted$wt_pop
DF_2021bgcrosswalk_weighted$renters_2021<-DF_2021bgcrosswalk_weighted$renters_2021*DF_2021bgcrosswalk_weighted$wt_hu
DF_2021bgcrosswalk_weighted$occupiedunits_2021<-DF_2021bgcrosswalk_weighted$occupiedunits_2021*DF_2021bgcrosswalk_weighted$wt_hh
DF_2021bgcrosswalk_weighted$nonfamilyhouseholds_2021<-DF_2021bgcrosswalk_weighted$nonfamilyhouseholds_2021*DF_2021bgcrosswalk_weighted$wt_hh


#remove empty rows and medians
DF_2021bgcrosswalk_weighted = subset(DF_2021bgcrosswalk_weighted, select = -c(blackmedianincome_2021,blackpoverty_2021,familyhouseholds_2021, fouryeardegree_2021, 
                                                                              graddegree_2021, hsgrad_2021, latinopoverty_2021, medianage_2021, grossrent_2021, medianyearbuilt_2021, 
                                                                              monthlyhousingcost_2021, over25population_2021, poverty_2021, whitemalebach_2021, whitemalehs_2021, 
                                                                              whitemalelessthanhs_2021, whitemedianincome_2021, whitepoverty_2021, whitefemalebach_2021, whitefemalehs_2021, 
                                                                              whitefemalelessthanhs_2021, vacancyrate_2021, pctwhite_2021, pctblack_2021, pctlatino_2021, pctwhitepoverty_2021, 
                                                                              pctblackpoverty_2021, pctlatinopoverty_2021, pctpoverty_2021, medianagehousing_2021, pctlessthanhs_2021, whitepctlessthanhs_2021, 
                                                                              pcthsgrad_2021, whitepcthsgrad_2021, pctfouryeardegree_2021, whitepctbachabove_2021, pctgraddegree_2021, pctrenters_2021, lessthanhs_2021, white25andolder_2021 ) )
DF_2021bgcrosswalk_weighted_sum = subset(DF_2021bgcrosswalk_weighted, select = -c(bg2020gj, wt_pop, wt_hh, wt_hu, X, parea))
#######################Step 4: Reapportion to CTs
DF_2021bgcrosswalk_weighted_sum<-DF_2021bgcrosswalk_weighted_sum %>% 
  select(-tr2010gj) %>%
  group_by(tr2010ge) %>% 
  summarise_if(is.numeric, sum)
#####################Step 5: Go to Crosswalked CT and BG Data for final percentages (calculate there)
DF_2021bgcrosswalk_weighted_sum <- subset(DF_2021bgcrosswalk_weighted_sum, select = -GEOID)
DF_2021bgcrosswalk_weighted_sum$GEOID<-DF_2021bgcrosswalk_weighted_sum$tr2010ge
DF_2021bgcrosswalk_weighted_sum <- subset(DF_2021bgcrosswalk_weighted_sum, select = -tr2010ge)
write.csv(DF_2021bgcrosswalk_weighted_sum,"C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/DF_2021_BGstoCTs.csv")


#make into Franklinton Dataset
DF_2021bgcrosswalk_weighted_sum<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/DF_2021_BGstoCTs.csv")
#Census Tracts for Franklinton 5000 (WF), 4300(North of Broad), 4200(East Franklinton)
Franklinton_CTs<-subset(DF_2021bgcrosswalk_weighted_sum, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
#State which area is which
Franklinton_CTs$area_name<-0
Franklinton_CTs<-dplyr::mutate(Franklinton_CTs, area_name = recode(GEOID, "39049005000" = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
#move area_name in front of everything
Franklinton_CTs<- Franklinton_CTs %>% select(order(colnames(Franklinton_CTs)))
Franklinton_CTs<-dplyr::select(Franklinton_CTs, area_name, GEOID, everything())
write.csv(Franklinton_CTs,"C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/FranklintonBGstoCTs.csv")
