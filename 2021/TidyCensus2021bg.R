#Tidy Census 2021

#Downloading Data for 2021 Geographies from US Census in block group format
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


#Total Population
TotalPop <- get_acs(
  geography = "cbg", 
  variables = "B01003_001E", 
  state = "OH", 
  county = "Franklin", 
  year = 2021)

TotalPop <- TotalPop %>% 
  rename(totalpopulation_2021= estimate)
TotalPop <- subset(TotalPop, select = -moe)
TotalPop <- subset(TotalPop, select = -variable)
TotalPop <- subset(TotalPop, select = -NAME)


#Race######################################################################
#Latino
Latino<-get_acs(geography = "cbg", 
                variables = "B03003_003E", 
                state = "OH", 
                county = "Franklin", 
                year = 2021)

Latino <- Latino %>% 
  rename(latino_2021= estimate)
Latino <- subset(Latino, select = -moe)
Latino <- subset(Latino, select = -variable)
Latino <- subset(Latino, select = -NAME)
#white
White<-get_acs(geography = "cbg", 
               variables = "B02001_002E", 
               state = "OH", 
               county = "Franklin", 
               year = 2021)
White <- White %>% 
  rename(white_2021= estimate)
White <- subset(White, select = -moe)
White <- subset(White, select = -variable)
White <- subset(White, select = -NAME)
Black<-get_acs(geography = "cbg", 
               variables = "B02001_003E", 
               state = "OH", 
               county = "Franklin", 
               year = 2021)
Black <- Black %>% 
  rename(black_2021= estimate)
Black <- subset(Black, select = -moe)
Black <- subset(Black, select = -variable)
Black <- subset(Black, select = -NAME)
#Asian
Asian<-get_acs(geography = "cbg", 
               variables = "B02001_005E", 
               state = "OH", 
               county = "Franklin", 
               year = 2021)
Asian <- Asian %>% 
  rename(asian_2021= estimate)
Asian <- subset(Asian, select = -moe)
Asian <- subset(Asian, select = -variable)
Asian <- subset(Asian, select = -NAME)

#American Indian and Alaska Native
AIAN<-get_acs(geography = "cbg", 
              variables = "B02001_006E", 
              state = "OH", 
              county = "Franklin", 
              year = 2021)
AIAN <- AIAN %>% 
  rename(aian_2021= estimate)
AIAN <- subset(AIAN, select = -moe)
AIAN <- subset(AIAN, select = -variable)
AIAN <- subset(AIAN, select = -NAME)
#Multiple Including Other
MultipleRaces<-get_acs(geography = "cbg", 
                       variables = "B02001_009E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2021)
MultipleRaces <- MultipleRaces %>% 
  rename(multipleraces_2021= estimate)
MultipleRaces <- subset(MultipleRaces, select = -moe)
MultipleRaces <- subset(MultipleRaces, select = -variable)
MultipleRaces <- subset(MultipleRaces, select = -NAME)


#########################POVERTY##########################################
#B06012_002E, Below 100% of poverty line
Poverty<-get_acs(geography = "cbg", 
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
WhitePoverty<-get_acs(geography = "cbg", 
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
BlackPoverty<-get_acs(geography = "cbg", 
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
LatinoPoverty<-get_acs(geography = "cbg", 
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
MedianIncome<-get_acs(geography = "cbg", 
                      variables  = "B19013_001E", 
                      state = "OH", 
                      county = "Franklin", 
                      year = 2021)
MedianIncome <- MedianIncome %>%  #############HERE 
  rename(medianincome_2021= estimate)
MedianIncome <- subset(MedianIncome, select = -moe)
MedianIncome <- subset(MedianIncome, select = -variable)
MedianIncome <- subset(MedianIncome, select = -NAME)
#WhiteIncome
WhiteMedianIncome<-get_acs(geography = "cbg", 
                           variables = "B19013A_001E", 
                           state = "OH", 
                           county = "Franklin", 
                           year = 2021)
WhiteMedianIncome <- WhiteMedianIncome %>% 
  rename(whitemedianincome_2021= estimate)
WhiteMedianIncome <- subset(WhiteMedianIncome, select = -moe)
WhiteMedianIncome <- subset(WhiteMedianIncome, select = -variable)
WhiteMedianIncome <- subset(WhiteMedianIncome, select = -NAME)
#BlackIncome
BlackIncome<-get_acs(geography = "cbg", 
                     variables = "B19013B_001E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2021)
BlackIncome <- BlackIncome %>% 
  rename(blackmedianincome_2021= estimate)
BlackIncome <- subset(BlackIncome, select = -moe)
BlackIncome <- subset(BlackIncome, select = -variable)
BlackIncome <- subset(BlackIncome, select = -NAME)
#########################HOUSING###########################################
#Total Housing Units
NumberofHousingUnits<-get_acs(geography = "cbg", 
                              variables = "B25002_001E", 
                              state = "OH", 
                              county = "Franklin", 
                              year = 2021)
NumberofHousingUnits <- NumberofHousingUnits %>% 
  rename(numberofhousingunits_2021= estimate)
NumberofHousingUnits <- subset(NumberofHousingUnits, select = -moe)
NumberofHousingUnits <- subset(NumberofHousingUnits, select = -variable)
NumberofHousingUnits <- subset(NumberofHousingUnits, select = -NAME)
#Vacant Housing Units
VacantHousingUnits<-get_acs(geography = "cbg", 
                            variables = "B25002_003E", 
                            state = "OH", 
                            county = "Franklin", 
                            year = 2021)
VacantHousingUnits <- VacantHousingUnits %>% 
  rename(vacantunits_2021= estimate)
VacantHousingUnits <- subset(VacantHousingUnits, select = -moe)
VacantHousingUnits <- subset(VacantHousingUnits, select = -variable)
VacantHousingUnits <- subset(VacantHousingUnits, select = -NAME)
#Housing Age
MedianYearBuilt<-get_acs(geography = "cbg", 
                         variables = "B25035_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2021)
MedianYearBuilt <- MedianYearBuilt %>% 
  rename(medianyearbuilt_2021= estimate)
MedianYearBuilt <- subset(MedianYearBuilt, select = -moe)
MedianYearBuilt <- subset(MedianYearBuilt, select = -variable)
MedianYearBuilt <- subset(MedianYearBuilt, select = -NAME)
#Median Home Price (owner-occupied if it was for sale),https://www.census.gov/quickfacts/fact/note/US/HSG495221
MedianHomeValue<-get_acs(geography = "cbg", 
                         variables = "B25077_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2021)
MedianHomeValue <- MedianHomeValue %>% 
  rename(medianhomevalue_2021= estimate)
MedianHomeValue <- subset(MedianHomeValue, select = -moe)
MedianHomeValue <- subset(MedianHomeValue, select = -variable)
MedianHomeValue <- subset(MedianHomeValue, select = -NAME)
#Rent Price
MedianGrossRent<-get_acs(geography = "cbg", 
                         variables = "B25064_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2021)
MedianGrossRent <- MedianGrossRent %>% 
  rename(grossrent_2021= estimate)
MedianGrossRent <- subset(MedianGrossRent, select = -moe)
MedianGrossRent <- subset(MedianGrossRent, select = -variable)
MedianGrossRent <- subset(MedianGrossRent, select = -NAME)
#Monthly Housing Cost (includes mortgage)
MonthlyHousingCost<-get_acs(geography = "cbg", 
                            variables = "B25105_001E", 
                            state = "OH", 
                            county = "Franklin", 
                            year = 2021)
MonthlyHousingCost <- MonthlyHousingCost %>% 
  rename(monthlyhousingcost_2021= estimate)
MonthlyHousingCost <- subset(MonthlyHousingCost, select = -moe)
MonthlyHousingCost <- subset(MonthlyHousingCost, select = -variable)
MonthlyHousingCost <- subset(MonthlyHousingCost, select = -NAME)

########################TOTAL EDUCATION#####################################
#B06009_002E, Less than HS
LessthanHS<-get_acs(geography = "cbg", 
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
HSGrad<-get_acs(geography = "cbg", 
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
fouryeardegree<-get_acs(geography = "cbg", 
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
graddegree<-get_acs(geography = "cbg", 
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
White25andOlder<-get_acs(geography = "cbg", 
                         variables = "C15002A_001E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2021)
White25andOlder <- White25andOlder %>% 
  rename(white25andolder_2021= estimate)
White25andOlder <- subset(White25andOlder, select = -moe)
White25andOlder <- subset(White25andOlder, select = -variable)
White25andOlder <- subset(White25andOlder, select = -NAME)
#C15002A_003E, Male Less than HS
WhiteMaleLessthanHS<-get_acs(geography = "cbg", 
                             variables = "C15002A_003E", 
                             state = "OH", 
                             county = "Franklin", 
                             year = 2021)
WhiteMaleLessthanHS <-WhiteMaleLessthanHS %>% 
  rename(whitemalelessthanhs_2021= estimate)
WhiteMaleLessthanHS <- subset(WhiteMaleLessthanHS, select = -moe)
WhiteMaleLessthanHS <- subset(WhiteMaleLessthanHS, select = -variable)
WhiteMaleLessthanHS <- subset(WhiteMaleLessthanHS, select = -NAME)
#C15002A_004E, Male HS
WhiteMaleHS<-get_acs(geography = "cbg", 
                     variables = "C15002A_004E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2021)
WhiteMaleHS <-WhiteMaleHS %>% 
  rename(whitemalehs_2021= estimate)
WhiteMaleHS <- subset(WhiteMaleHS, select = -moe)
WhiteMaleHS <- subset(WhiteMaleHS, select = -variable)
WhiteMaleHS <- subset(WhiteMaleHS, select = -NAME)
#C15002A_006E, Male Bachelor's Degree or Higher
WhiteMaleBach<-get_acs(geography = "cbg", 
                       variables = "C15002A_006E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2021)
WhiteMaleBach <-WhiteMaleBach %>% 
  rename(whitemalebach_2021= estimate)
WhiteMaleBach <- subset(WhiteMaleBach, select = -moe)
WhiteMaleBach <- subset(WhiteMaleBach, select = -variable)
WhiteMaleBach <- subset(WhiteMaleBach, select = -NAME)
#C15002A_008E, Female Less than HS
WhiteFemaleLessthanHS<-get_acs(geography = "cbg", 
                               variables = "C15002A_008E", 
                               state = "OH", 
                               county = "Franklin", 
                               year = 2021)
WhiteFemaleLessthanHS <-WhiteFemaleLessthanHS %>% 
  rename(whitefemalelessthanhs_2021= estimate)
WhiteFemaleLessthanHS <- subset(WhiteFemaleLessthanHS, select = -moe)
WhiteFemaleLessthanHS <- subset(WhiteFemaleLessthanHS, select = -variable)
WhiteFemaleLessthanHS <- subset(WhiteFemaleLessthanHS, select = -NAME)
#C15002A_009E, Female HS
WhiteFemaleHS<-get_acs(geography = "cbg", 
                       variables = "C15002A_009E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2021)
WhiteFemaleHS <-WhiteFemaleHS %>% 
  rename(whitefemalehs_2021= estimate)
WhiteFemaleHS <- subset(WhiteFemaleHS, select = -moe)
WhiteFemaleHS <- subset(WhiteFemaleHS, select = -variable)
WhiteFemaleHS <- subset(WhiteFemaleHS, select = -NAME)
#C15002A_011E, Female Bachelor's Degree or Higher
WhiteFemaleBach<-get_acs(geography = "cbg", 
                         variables = "C15002A_011E", 
                         state = "OH", 
                         county = "Franklin", 
                         year = 2021)
WhiteFemaleBach <-WhiteFemaleBach %>% 
  rename(whitefemalebach_2021= estimate)
WhiteFemaleBach <- subset(WhiteFemaleBach, select = -moe)
WhiteFemaleBach <- subset(WhiteFemaleBach, select = -variable)
WhiteFemaleBach <- subset(WhiteFemaleBach, select = -NAME)
########################HOUSEHOLDS TYPE####################################
#B11001_002E = Family Households
FamilyHH<-get_acs(geography = "cbg", 
                  variables = "B11001_002E", 
                  state = "OH", 
                  county = "Franklin", 
                  year = 2021)
FamilyHH <- FamilyHH %>% 
  rename(familyhouseholds_2021= estimate)
FamilyHH <- subset(FamilyHH, select = -moe)
FamilyHH <- subset(FamilyHH, select = -variable)
FamilyHH <- subset(FamilyHH, select = -NAME)
#B11001_007E = Non-family households
NonFamilyHH<-get_acs(geography = "cbg", 
                     variables = "B11001_007E", 
                     state = "OH", 
                     county = "Franklin", 
                     year = 2021)
NonFamilyHH <- NonFamilyHH %>% 
  rename(nonfamilyhouseholds_2021= estimate)
NonFamilyHH <- subset(NonFamilyHH, select = -moe)
NonFamilyHH <- subset(NonFamilyHH, select = -variable)
NonFamilyHH <- subset(NonFamilyHH, select = -NAME)
#B11001_008E = Householder Living Alone
AloneHH<-get_acs(geography = "cbg", 
                 variables = "B11001_008E", 
                 state = "OH", 
                 county = "Franklin", 
                 year = 2021)
AloneHH <- AloneHH %>% 
  rename(hhlivingalone_2021= estimate)
AloneHH <- subset(AloneHH, select = -moe)
AloneHH <- subset(AloneHH, select = -variable)
AloneHH <- subset(AloneHH, select = -NAME)

########################################AGE##############################
#Median Age, B01002_001E
MedianAge<-get_acs(geography = "cbg", 
                   variables = "B01002_001E", 
                   state = "OH", 
                   county = "Franklin", 
                   year = 2021)
MedianAge <- MedianAge %>% 
  rename(medianage_2021= estimate)
MedianAge <- subset(MedianAge, select = -moe)
MedianAge <- subset(MedianAge, select = -variable)
MedianAge <- subset(MedianAge, select = -NAME)

over25population<-get_acs(geography = "cbg", 
                          variables = "B06009_001E", 
                          state = "OH", 
                          county = "Franklin", 
                          year = 2021)
over25population <- over25population %>% 
  rename(over25population_2021= estimate)
over25population <- subset(over25population, select = -moe)
over25population <- subset(over25population, select = -variable)
over25population <- subset(over25population, select = -NAME)

Renters<-get_acs(geography = "cbg", 
                 variables = "B25008_003E", 
                 state = "OH", 
                 county = "Franklin", 
                 year = 2021)
Renters <- Renters %>% 
  rename(renters_2021= estimate)
Renters <- subset(Renters, select = -moe)
Renters <- subset(Renters, select = -variable)
Renters <- subset(Renters, select = -NAME)

occupiedunits<-get_acs(geography = "cbg", 
                       variables = "B25008_001E", 
                       state = "OH", 
                       county = "Franklin", 
                       year = 2021)
occupiedunits <- occupiedunits %>% 
  rename(occupiedunits_2021= estimate)
occupiedunits <- subset(occupiedunits, select = -moe)
occupiedunits <- subset(occupiedunits, select = -variable)
occupiedunits <- subset(occupiedunits, select = -NAME)


######Combine them all into one df
#make a list 
DF_2021<-purrr::reduce(list(AIAN, Asian, AloneHH, Black, BlackIncome, BlackPoverty, FamilyHH, fouryeardegree, graddegree, HSGrad, Latino, LatinoPoverty, LessthanHS, MedianAge, MedianGrossRent, MedianYearBuilt, MonthlyHousingCost, MultipleRaces, NonFamilyHH, NumberofHousingUnits, over25population, Poverty,TotalPop, VacantHousingUnits, WhiteMaleBach, WhiteMaleHS, WhiteMaleLessthanHS, White, White25andOlder, WhiteMedianIncome, WhitePoverty, WhiteFemaleBach, WhiteFemaleHS, WhiteFemaleLessthanHS, Renters, occupiedunits), dplyr::left_join, by = 'GEOID')
#Remove all those except DF_2021
rm(list=setdiff(ls(), "DF_2021"))
#######################################Create Variables########################
#Vacancy Rate
DF_2021$vacancyrate_2021<- DF_2021$vacantunits_2021/DF_2021$numberofhousingunits_2021
#Percent White
DF_2021$pctwhite_2021<-DF_2021$white_2021/DF_2021$totalpopulation_2021*100
#Percent Black
DF_2021$pctblack_2021<-DF_2021$black_2021/DF_2021$totalpopulation_2021*100
#Percent Latino
DF_2021$pctlatino_2021<-DF_2021$latino_2021/DF_2021$totalpopulation_2021*100
#Percent Poverty
DF_2021$pctwhitepoverty_2021<-DF_2021$whitepoverty_2021/DF_2021$white_2021*100
DF_2021$pctblackpoverty_2021<-DF_2021$blackpoverty_2021/DF_2021$black_2021*100
DF_2021$pctlatinopoverty_2021<-DF_2021$latinopoverty_2021/DF_2021$latino_2021*100
DF_2021$pctpoverty_2021<-DF_2021$poverty_2021/DF_2021$totalpopulation_2021*100

#Median Age of Housing (2021-Median Year Structure Built)
DF_2021$medianagehousing_2021<- (2021) - DF_2021$medianyearbuilt_2021
#Percent Less than HS HERE
DF_2021$pctlessthanhs_2021<- DF_2021$lessthanhs_2021/DF_2021$over25population_2021*100
DF_2021$whitepctlessthanhs_2021<-(DF_2021$whitemalelessthanhs_2021+DF_2021$whitefemalelessthanhs_2021)/DF_2021$white25andolder_2021*100
#Percent HS
DF_2021$pcthsgrad_2021<- DF_2021$hsgrad_2021/DF_2021$over25population_2021*100
DF_2021$whitepcthsgrad_2021<-(DF_2021$whitemalehs_2021+DF_2021$whitefemalehs_2021)/DF_2021$white25andolder_2021*100
#Percent Bachelor's or Above
DF_2021$pctfouryeardegree_2021<- DF_2021$fouryeardegree_2021/DF_2021$over25population_2021*100
DF_2021$whitepctbachabove_2021<-(DF_2021$whitemalebach_2021+DF_2021$whitefemalebach_2021)/DF_2021$white25andolder_2021*100
#Grad and Professional Degree
DF_2021$pctgraddegree_2021<- DF_2021$graddegree_2021/DF_2021$over25population_2021*100

DF_2021$pctrenters_2021<-DF_2021$renters_2021/DF_2021$occupiedunits_2021*100


#Character to double
DF_2021$GEOID<-as.double(DF_2021$GEOID)
DF_2021bg<-DF_2021

#write csv
write.csv(DF_2021bg, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/DF_2021bg.csv")