#Tidy Census 2021 Census Tracts

#Downloading Data for 2021 Geographies from US Census in ct format
#NAs from BG-> Income, White and Black Poverty, education, monthly housing cost, over 25pop, number of poverty, white&black education


require(sf)
require(leaflet) #requires WGS 1984/CRS 4326
require(ggplot2)
require(tidycensus)
require(tigris)
require(tidyverse)
require(tidycensus)
require(sp)
census_api_key("84dba8250c52c9a11ec14575dce9083eea98d7e9", overwrite = TRUE)


#CRS = 4326 is WGS 1984 is best for Lat Long
#CRS = 3735 is Franklin County

#load variables
#v10<-load_variables(2021, "acs5", cache = TRUE)
#variables that will be dropped
#variables_to_drop <- c("moe", "NAME", "variable")


########################POVERTY##########################################
#B06012_002E, Below 100% of poverty line
Poverty<-get_acs(geography = "tract", 
                 variables = "B06012_002E", 
                 state = "OH", 
                 county = "Franklin", 
                 year = 2021)
Poverty <- Poverty %>% 
  rename(poverty_2021= estimate)
Poverty <- subset(Poverty, select = -moe)
Poverty <- subset(Poverty, select = -variable)
Poverty <- subset(Poverty, select = -NAME)

#B17020A_002E = Income in the past 12 months below poverty level, white only	
WhitePoverty<-get_acs(geography = "tract", 
                      variables = "B17020A_002E", 
                      state = "OH", 
                      county = "Franklin", 
                      year = 2021)
WhitePoverty <- WhitePoverty %>% 
  rename(whitepoverty_2021= estimate)
WhitePoverty <- subset(WhitePoverty, select = -moe)
WhitePoverty <- subset(WhitePoverty, select = -variable)
WhitePoverty <- subset(WhitePoverty, select = -NAME)
#BlackPoverty
BlackPoverty<-get_acs(geography = "tract", 
                      variables = "B17020B_002E", 
                      state = "OH", 
                      county = "Franklin", 
                      year = 2021)
BlackPoverty <- BlackPoverty %>% 
  rename(blackpoverty_2021= estimate)
BlackPoverty <- subset(BlackPoverty, select = -moe)
BlackPoverty <- subset(BlackPoverty, select = -variable)
BlackPoverty <- subset(BlackPoverty, select = -NAME)
#Latino
#B17020I_001E
LatinoPoverty<-get_acs(geography = "tract", 
                       variables = "B17020I_001E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2021)
LatinoPoverty <- LatinoPoverty %>% 
  rename(latinopoverty_2021= estimate)
LatinoPoverty <- subset(LatinoPoverty, select = -moe)
LatinoPoverty <- subset(LatinoPoverty, select = -variable)
LatinoPoverty <- subset(LatinoPoverty, select = -NAME)

#########################INCOME############################################
#########################HOUSING###########################################
########################TOTAL EDUCATION#####################################
#B06009_002E, Less than HS
LessthanHS<-get_acs(geography = "tract", 
                    variables = "B06009_002E", 
                    state = "OH", 
                    county = "Franklin", 
                    year = 2021)
LessthanHS <- LessthanHS %>% 
  rename(lessthanhs_2021= estimate)
LessthanHS <- subset(LessthanHS, select = -moe)
LessthanHS <- subset(LessthanHS, select = -variable)
LessthanHS <- subset(LessthanHS, select = -NAME)
#B06009_003E, HS Grad
HSGrad<-get_acs(geography = "tract", 
                variables = "B06009_003E", 
                state = "OH", 
                county = "Franklin", 
                year = 2021)
HSGrad <- HSGrad %>% 
  rename(hsgrad_2021= estimate)
HSGrad <- subset(HSGrad, select = -moe)
HSGrad <- subset(HSGrad, select = -variable)
HSGrad <- subset(HSGrad, select = -NAME)
#B06009_005E, Bachelor's and Above
fouryeardegree<-get_acs(geography = "tract", 
                        variables = "B06009_005E", 
                        state = "OH", 
                        county = "Franklin", 
                        year = 2021)
fouryeardegree <- fouryeardegree %>% 
  rename(fouryeardegree_2021= estimate)
fouryeardegree <- subset(fouryeardegree, select = -moe)
fouryeardegree <- subset(fouryeardegree, select = -variable)
fouryeardegree <- subset(fouryeardegree, select = -NAME)
#B06009_006E, Graduate or Professional Degree
graddegree<-get_acs(geography = "tract", 
                    variables = "B06009_006E", 
                    state = "OH", 
                    county = "Franklin", 
                    year = 2021)
graddegree <- graddegree %>% 
  rename(graddegree_2021= estimate)
graddegree <- subset(graddegree, select = -moe)
graddegree <- subset(graddegree, select = -variable)
graddegree <- subset(graddegree, select = -NAME)

########################WHITE EDUCATION##########################################
#C15002A_001E, White Education 25 and Older, Total
White25andOlder<-get_acs(geography = "tract", 
                         variables = "C15002A_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2021)
White25andOlder <- White25andOlder %>% 
  rename(white25andolder_2021= estimate)
White25andOlder <- subset(White25andOlder, select = -moe)
White25andOlder <- subset(White25andOlder, select = -variable)
White25andOlder <- subset(White25andOlder, select = -NAME)


########################################AGE##############################
over25population<-get_acs(geography = "tract", 
                          variables = "B06009_001E", 
                          state = "OH", 
                          county = "Franklin", 
                          year = 2021)
over25population <- over25population %>% 
  rename(over25population_2021= estimate)
over25population <- subset(over25population, select = -moe)
over25population <- subset(over25population, select = -variable)
over25population <- subset(over25population, select = -NAME)


######Combine them all into one df
#make a list 
DF_2021ct<-purrr::reduce(list(BlackPoverty, fouryeardegree, graddegree, HSGrad, LatinoPoverty, LessthanHS, over25population, Poverty, White25andOlder, WhitePoverty), dplyr::left_join, by = 'GEOID')
#Remove all those except DF_2021ct
rm(list=setdiff(ls(), "DF_2021ct"))
#######################################Create Variables########################
#Vacancy Rate
#Percent Poverty
DF_2021ct$pctwhitepoverty_2021<-DF_2021ct$whitepoverty_2021/DF_2021ct$white_2021*100
DF_2021ct$pctblackpoverty_2021<-DF_2021ct$blackpoverty_2021/DF_2021ct$black_2021*100
DF_2021ct$pctlatinopoverty_2021<-DF_2021ct$latinopoverty_2021/DF_2021ct$latino_2021*100
DF_2021ct$pctpoverty_2021<-DF_2021ct$poverty_2021/DF_2021ct$totalpopulation_2021*100

#Percent Less than HS HERE
DF_2021ct$pctlessthanhs_2021<- DF_2021ct$lessthanhs_2021/DF_2021ct$over25population_2021*100
#Percent HS
DF_2021ct$pcthsgrad_2021<- DF_2021ct$hsgrad_2021/DF_2021ct$over25population_2021*100
#Percent Bachelor's or Above
DF_2021ct$pctfouryeardegree_2021<- DF_2021ct$fouryeardegree_2021/DF_2021ct$over25population_2021*100
#Grad and Professional Degree
DF_2021ct$pctgraddegree_2021<- DF_2021ct$graddegree_2021/DF_2021ct$over25population_2021*100

#Character to double
DF_2021ct$GEOID<-as.double(DF_2021ct$GEOID)


#write csv
write.csv(DF_2021ct, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/DF_2021ct.csv")


###################Edit THESE######################
#Census tracts for Franklinton 5000 (WF), 4300(North of Broad), 4200(East Franklinton)
Franklinton_CTs_2021<-subset(DF_2021ct, GEOID== "39049005001" |  GEOID== "39049005002"| GEOID== "39049004301" | GEOID== "39049004302" |GEOID== "39049004200"  )
#State which area is which
#Franklinton_CTs$area_name<-0
#Franklinton_CTs<-dplyr::mutate(Franklinton_CTs, area_name = recode(GEOID, "39049005002"  = "West Franklinton", "39049004301" = "North of Broad", "39049004200" = "East Franklinton"))
#move area_name in front of everything
#Franklinton_CTs<- Franklinton_CTs %>% select(order(colnames(Franklinton_CTs)))
#Franklinton_CTs<-dplyr::select(Franklinton_CTs, area_name, GEOID, everything())
#Franklinton_CTs <- subset(Franklinton_CTs, select = -X.1)
#Franklinton_CTs <- subset(Franklinton_CTs, select = -X)
write.csv(Franklinton_CTs_2021, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/Franklinton_CTs2021.csv")

#Franklinton Totals
#require(janitor)
#Franklinton_CTs_Totals<-Franklinton_CTs %>%
#adorn_totals("row"))