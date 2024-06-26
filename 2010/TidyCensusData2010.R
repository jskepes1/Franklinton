#Downloading Data for 2010 Geographies from US Census
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
#v10<-load_variables(2010, "acs5", cache = TRUE)
#variables that will be dropped
#variables_to_drop <- c("moe", "NAME", "variable")


#Total Population
TotalPop <- get_acs(
  geography = "tract", 
  variables = "B01003_001E", 
  state = "OH", 
  county = "Franklin", 
  year = 2010)

TotalPop <- TotalPop %>% 
  rename(totalpopulation_2010= estimate)
TotalPop <- subset(TotalPop, select = -moe)
TotalPop <- subset(TotalPop, select = -variable)
TotalPop <- subset(TotalPop, select = -NAME)


#Race######################################################################
#Latino
Latino<-get_acs(geography = "tract", 
               variables = "B03003_003E", 
               state = "OH", 
               county = "Franklin", 
               year = 2010)

Latino <- Latino %>% 
  rename(latino_2010= estimate)
Latino <- subset(Latino, select = -moe)
Latino <- subset(Latino, select = -variable)
Latino <- subset(Latino, select = -NAME)
#white
White<-get_acs(geography = "tract", 
                variables = "B02001_002E", 
                state = "OH", 
                county = "Franklin", 
                year = 2010)
White <- White %>% 
  rename(white_2010= estimate)
White <- subset(White, select = -moe)
White <- subset(White, select = -variable)
White <- subset(White, select = -NAME)
Black<-get_acs(geography = "tract", 
               variables = "B02001_003E", 
               state = "OH", 
               county = "Franklin", 
               year = 2010)
Black <- Black %>% 
  rename(black_2010= estimate)
Black <- subset(Black, select = -moe)
Black <- subset(Black, select = -variable)
Black <- subset(Black, select = -NAME)
#Asian
Asian<-get_acs(geography = "tract", 
               variables = "B02001_005E", 
               state = "OH", 
               county = "Franklin", 
               year = 2010)
Asian <- Asian %>% 
  rename(asian_2010= estimate)
Asian <- subset(Asian, select = -moe)
Asian <- subset(Asian, select = -variable)
Asian <- subset(Asian, select = -NAME)

#American Indian and Alaska Native
AIAN<-get_acs(geography = "tract", 
                       variables = "B02001_006E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2010)
AIAN <- AIAN %>% 
  rename(aian_2010= estimate)
AIAN <- subset(AIAN, select = -moe)
AIAN <- subset(AIAN, select = -variable)
AIAN <- subset(AIAN, select = -NAME)
#Multiple Including Other
MultipleRaces<-get_acs(geography = "tract", 
               variables = "B02001_009E", 
               state = "OH", 
               county = "Franklin", 
               year = 2010)
MultipleRaces <- MultipleRaces %>% 
  rename(multipleraces_2010= estimate)
MultipleRaces <- subset(MultipleRaces, select = -moe)
MultipleRaces <- subset(MultipleRaces, select = -variable)
MultipleRaces <- subset(MultipleRaces, select = -NAME)


#########################POVERTY##########################################
#B06012_002E, Below 100% of poverty line
Poverty<-get_acs(geography = "tract", 
                      variables = "B06012_002E", 
                      state = "OH", 
                      county = "Franklin", 
                      year = 2010)
Poverty <- Poverty %>% 
  rename(poverty_2010= estimate)
Poverty <- subset(Poverty, select = -moe)
Poverty <- subset(Poverty, select = -variable)
Poverty <- subset(Poverty, select = -NAME)

#B17020A_002E = Income in the past 12 months below poverty level, white only	
WhitePoverty<-get_acs(geography = "tract", 
                  variables = "B17020A_002E", 
                  state = "OH", 
                  county = "Franklin", 
                  year = 2010)
WhitePoverty <- WhitePoverty %>% 
  rename(whitepoverty_2010= estimate)
WhitePoverty <- subset(WhitePoverty, select = -moe)
WhitePoverty <- subset(WhitePoverty, select = -variable)
WhitePoverty <- subset(WhitePoverty, select = -NAME)
#BlackPoverty
BlackPoverty<-get_acs(geography = "tract", 
                      variables = "B17020B_002E", 
                      state = "OH", 
                      county = "Franklin", 
                      year = 2010)
BlackPoverty <- BlackPoverty %>% 
  rename(blackpoverty_2010= estimate)
BlackPoverty <- subset(BlackPoverty, select = -moe)
BlackPoverty <- subset(BlackPoverty, select = -variable)
BlackPoverty <- subset(BlackPoverty, select = -NAME)
#Latino
#B17020I_001E
LatinoPoverty<-get_acs(geography = "tract", 
                      variables = "B17020I_001E", 
                      state = "OH", 
                      county = "Franklin", 
                      year = 2010)
LatinoPoverty <- LatinoPoverty %>% 
  rename(latinopoverty_2010= estimate)
LatinoPoverty <- subset(LatinoPoverty, select = -moe)
LatinoPoverty <- subset(LatinoPoverty, select = -variable)
LatinoPoverty <- subset(LatinoPoverty, select = -NAME)

#########################INCOME############################################
MedianIncome<-get_acs(geography = "tract", 
                       variables  = "B19013_001E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2010)
MedianIncome <- MedianIncome %>%  #############HERE 
  rename(medianincome_2010= estimate)
MedianIncome <- subset(MedianIncome, select = -moe)
MedianIncome <- subset(MedianIncome, select = -variable)
MedianIncome <- subset(MedianIncome, select = -NAME)
#WhiteIncome
WhiteMedianIncome<-get_acs(geography = "tract", 
                     variables = "B19013A_001E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2010)
WhiteMedianIncome <- WhiteMedianIncome %>% 
  rename(whitemedianincome_2010= estimate)
WhiteMedianIncome <- subset(WhiteMedianIncome, select = -moe)
WhiteMedianIncome <- subset(WhiteMedianIncome, select = -variable)
WhiteMedianIncome <- subset(WhiteMedianIncome, select = -NAME)
#BlackIncome
BlackIncome<-get_acs(geography = "tract", 
                     variables = "B19013B_001E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2010)
BlackIncome <- BlackIncome %>% 
  rename(blackmedianincome_2010= estimate)
BlackIncome <- subset(BlackIncome, select = -moe)
BlackIncome <- subset(BlackIncome, select = -variable)
BlackIncome <- subset(BlackIncome, select = -NAME)
#########################HOUSING###########################################
#Total Housing Units
NumberofHousingUnits<-get_acs(geography = "tract", 
                         variables = "B25002_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2010)
NumberofHousingUnits <- NumberofHousingUnits %>% 
  rename(numberofhousingunits_2010= estimate)
NumberofHousingUnits <- subset(NumberofHousingUnits, select = -moe)
NumberofHousingUnits <- subset(NumberofHousingUnits, select = -variable)
NumberofHousingUnits <- subset(NumberofHousingUnits, select = -NAME)
#Vacant Housing Units
VacantHousingUnits<-get_acs(geography = "tract", 
                              variables = "B25002_003E", 
                              state = "OH", 
                              county = "Franklin", 
                              year = 2010)
VacantHousingUnits <- VacantHousingUnits %>% 
  rename(vacantunits_2010= estimate)
VacantHousingUnits <- subset(VacantHousingUnits, select = -moe)
VacantHousingUnits <- subset(VacantHousingUnits, select = -variable)
VacantHousingUnits <- subset(VacantHousingUnits, select = -NAME)
#Median Home Price (owner-occupied if it was for sale),https://www.census.gov/quickfacts/fact/note/US/HSG495221
MedianHomeValue<-get_acs(geography = "tract", 
                         variables = "B25077_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2010)
MedianHomeValue <- MedianHomeValue %>% 
  rename(medianhomevalue_2010= estimate)
MedianHomeValue <- subset(MedianHomeValue, select = -moe)
MedianHomeValue <- subset(MedianHomeValue, select = -variable)
MedianHomeValue <- subset(MedianHomeValue, select = -NAME)
#Median HH Income 
MedianHHIncome<-get_acs(geography = "tract", 
                         variables = "B19013_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2010)
MedianHHIncome <- MedianHHIncome %>% 
  rename(medianhhincome_2010= estimate)
MedianHHIncome <- subset(MedianHHIncome, select = -moe)
MedianHHIncome <- subset(MedianHHIncome, select = -variable)
MedianHHIncome <- subset(MedianHHIncome, select = -NAME)
#Rent Price
MedianGrossRent<-get_acs(geography = "tract", 
                            variables = "B25064_001E", 
                            state = "OH", 
                            county = "Franklin", 
                            year = 2010)
MedianGrossRent <- MedianGrossRent %>% 
  rename(grossrent_2010= estimate)
MedianGrossRent <- subset(MedianGrossRent, select = -moe)
MedianGrossRent <- subset(MedianGrossRent, select = -variable)
MedianGrossRent <- subset(MedianGrossRent, select = -NAME)
#Monthly Housing Cost (includes mortgage)
MonthlyHousingCost<-get_acs(geography = "tract", 
                     variables = "B25105_001E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2010)
MonthlyHousingCost <- MonthlyHousingCost %>% 
  rename(monthlyhousingcost_2010= estimate)
MonthlyHousingCost <- subset(MonthlyHousingCost, select = -moe)
MonthlyHousingCost <- subset(MonthlyHousingCost, select = -variable)
MonthlyHousingCost <- subset(MonthlyHousingCost, select = -NAME)

########################TOTAL EDUCATION#####################################
#B06009_002E, Less than HS
LessthanHS<-get_acs(geography = "tract", 
                         variables = "B06009_002E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2010)
LessthanHS <- LessthanHS %>% 
  rename(lessthanhs_2010= estimate)
LessthanHS <- subset(LessthanHS, select = -moe)
LessthanHS <- subset(LessthanHS, select = -variable)
LessthanHS <- subset(LessthanHS, select = -NAME)
#B06009_003E, HS Grad
HSGrad<-get_acs(geography = "tract", 
                    variables = "B06009_003E", 
                    state = "OH", 
                    county = "Franklin", 
                    year = 2010)
HSGrad <- HSGrad %>% 
  rename(hsgrad_2010= estimate)
HSGrad <- subset(HSGrad, select = -moe)
HSGrad <- subset(HSGrad, select = -variable)
HSGrad <- subset(HSGrad, select = -NAME)
#B06009_005E, Bachelor's and Above
fouryeardegree<-get_acs(geography = "tract", 
                variables = "B06009_005E", 
                state = "OH", 
                county = "Franklin", 
                year = 2010)
fouryeardegree <- fouryeardegree %>% 
  rename(fouryeardegree_2010= estimate)
fouryeardegree <- subset(fouryeardegree, select = -moe)
fouryeardegree <- subset(fouryeardegree, select = -variable)
fouryeardegree <- subset(fouryeardegree, select = -NAME)
#B06009_006E, Graduate or Professional Degree
graddegree<-get_acs(geography = "tract", 
                        variables = "B06009_006E", 
                        state = "OH", 
                        county = "Franklin", 
                        year = 2010)
graddegree <- graddegree %>% 
  rename(graddegree_2010= estimate)
graddegree <- subset(graddegree, select = -moe)
graddegree <- subset(graddegree, select = -variable)
graddegree <- subset(graddegree, select = -NAME)

########################WHITE EDUCATION##########################################
#C15002A_001E, White Education 25 and Older, Total
White25andOlder<-get_acs(geography = "tract", 
                            variables = "C15002A_001E", 
                            state = "OH", 
                            county = "Franklin", 
                            year = 2010)
White25andOlder <- White25andOlder %>% 
  rename(white25andolder_2010= estimate)
White25andOlder <- subset(White25andOlder, select = -moe)
White25andOlder <- subset(White25andOlder, select = -variable)
White25andOlder <- subset(White25andOlder, select = -NAME)

########################HOUSEHOLDS TYPE####################################
#B11001_002E = Family Households
FamilyHH<-get_acs(geography = "tract", 
                       variables = "B11001_002E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2010)
FamilyHH <- FamilyHH %>% 
  rename(familyhouseholds_2010= estimate)
FamilyHH <- subset(FamilyHH, select = -moe)
FamilyHH <- subset(FamilyHH, select = -variable)
FamilyHH <- subset(FamilyHH, select = -NAME)
#B11001_007E = Non-family households
NonFamilyHH<-get_acs(geography = "tract", 
                  variables = "B11001_007E", 
                  state = "OH", 
                  county = "Franklin", 
                  year = 2010)
NonFamilyHH <- NonFamilyHH %>% 
  rename(nonfamilyhouseholds_2010= estimate)
NonFamilyHH <- subset(NonFamilyHH, select = -moe)
NonFamilyHH <- subset(NonFamilyHH, select = -variable)
NonFamilyHH <- subset(NonFamilyHH, select = -NAME)
#B11001_008E = Householder Living Alone
AloneHH<-get_acs(geography = "tract", 
                     variables = "B11001_008E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2010)
AloneHH <- AloneHH %>% 
  rename(hhlivingalone_2010= estimate)
AloneHH <- subset(AloneHH, select = -moe)
AloneHH <- subset(AloneHH, select = -variable)
AloneHH <- subset(AloneHH, select = -NAME)

########################################AGE##############################
#Median Age, B01002_001E
MedianAge<-get_acs(geography = "tract", 
                 variables = "B01002_001E", 
                 state = "OH", 
                 county = "Franklin", 
                 year = 2010)
MedianAge <- MedianAge %>% 
  rename(medianage_2010= estimate)
MedianAge <- subset(MedianAge, select = -moe)
MedianAge <- subset(MedianAge, select = -variable)
MedianAge <- subset(MedianAge, select = -NAME)

over25population<-get_acs(geography = "tract", 
                          variables = "B06009_001E", 
                          state = "OH", 
                          county = "Franklin", 
                          year = 2010)
over25population <- over25population %>% 
  rename(over25population_2010= estimate)
over25population <- subset(over25population, select = -moe)
over25population <- subset(over25population, select = -variable)
over25population <- subset(over25population, select = -NAME)


########################################RENTERS############################################################################
Renters<-get_acs(geography = "tract", 
                          variables = "B25003_003E", 
                          state = "OH", 
                          county = "Franklin", 
                          year = 2010)
Renters <- Renters %>% 
  rename(renters_2010= estimate)
Renters <- subset(Renters, select = -moe)
Renters <- subset(Renters, select = -variable)
Renters <- subset(Renters, select = -NAME)

occupiedunits<-get_acs(geography = "tract", 
                 variables = "B25008_001E", 
                 state = "OH", 
                 county = "Franklin", 
                 year = 2010)
occupiedunits <- occupiedunits %>% 
  rename(occupiedunits_2010= estimate)
occupiedunits <- subset(occupiedunits, select = -moe)
occupiedunits <- subset(occupiedunits, select = -variable)
occupiedunits <- subset(occupiedunits, select = -NAME)






######Combine them all into one df
#make a list 
DF_2010<-purrr::reduce(list(MedianHHIncome, AIAN, Asian, AloneHH, Black, BlackIncome, BlackPoverty, FamilyHH, fouryeardegree, graddegree, HSGrad, Latino, LatinoPoverty, LessthanHS, MedianAge, MedianGrossRent, MonthlyHousingCost, MultipleRaces, NonFamilyHH, NumberofHousingUnits, over25population, Poverty,TotalPop, VacantHousingUnits, White, White25andOlder, WhiteMedianIncome, WhitePoverty, Renters, occupiedunits), dplyr::left_join, by = 'GEOID')
#Remove all those except DF_2010
rm(list=setdiff(ls(), "DF_2010"))
#######################################Create Variables########################
#Vacancy Rate
DF_2010$vacancyrate_2010<- DF_2010$vacantunits_2010/DF_2010$numberofhousingunits_2010*100
#Percent White
DF_2010$pctwhite_2010<-DF_2010$white_2010/DF_2010$totalpopulation_2010*100
#Percent Black
DF_2010$pctblack_2010<-DF_2010$black_2010/DF_2010$totalpopulation_2010*100
#Percent Latino
DF_2010$pctlatino_2010<-DF_2010$latino_2010/DF_2010$totalpopulation_2010*100
#Percent Poverty
DF_2010$pctwhitepoverty_2010<-DF_2010$whitepoverty_2010/DF_2010$white_2010*100
DF_2010$pctblackpoverty_2010<-DF_2010$blackpoverty_2010/DF_2010$black_2010*100
DF_2010$pctlatinopoverty_2010<-DF_2010$latinopoverty_2010/DF_2010$latino_2010*100
DF_2010$pctpoverty_2010<-DF_2010$poverty_2010/DF_2010$totalpopulation_2010*100

#Percent Less than HS HERE
DF_2010$pctlessthanhs_2010<- DF_2010$lessthanhs_2010/DF_2010$over25population_2010*100
#Percent HS
DF_2010$pcthsgrad_2010<- DF_2010$hsgrad_2010/DF_2010$over25population_2010*100
#Percent Bachelor's or Above
DF_2010$pctfouryeardegree_2010<- DF_2010$fouryeardegree_2010/DF_2010$over25population_2010*100
#Grad and Professional Degree
DF_2010$pctgraddegree_2010<- DF_2010$graddegree_2010/DF_2010$over25population_2010*100

#Percent Renters
DF_2010$pctrenters_2010<-DF_2010$renters_2010/DF_2010$occupiedunits_2010*100

#Character to double
DF_2010$GEOID<-as.double(DF_2010$GEOID)


#write csv
write.csv(DF_2010, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2010/DF_2010.csv")


#Combine with DF_2000
DF_2000<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/DF_2000.csv")
DF_2000to2010<-left_join(DF_2010, DF_2000, by = 'GEOID')
write.csv(DF_2000to2010, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2010/DF_2000to2010.csv")

#Census Tracts for Franklinton 5000 (WF), 4300(North of Broad), 4200(East Franklinton)
Franklinton_CTs<-subset(DF_2000to2010, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
#State which area is which
Franklinton_CTs$area_name<-0
Franklinton_CTs<-dplyr::mutate(Franklinton_CTs, area_name = recode(GEOID, "39049005000" = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
#move area_name in front of everything
Franklinton_CTs<- Franklinton_CTs %>% select(order(colnames(Franklinton_CTs)))
Franklinton_CTs<-dplyr::select(Franklinton_CTs, area_name, GEOID, everything())

#write.csv(Franklinton_CTs, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2010/Franklinton_CTs.csv")
