#TidyCensusData2000
#Pulled from Social Explorer for 2000 data on 2010 Boundaries
require(sf)
require(leaflet) #requires WGS 1984/CRS 4326
require(ggplot2)
require(tidycensus)
require(tigris)
require(tidyverse)
require(tidycensus)
require(sp)

#read in social explorer data files
age<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/Age2000.csv")
basic<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/Basic2000.csv")
education<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/Education2000.csv")
income<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/hhincome2000.csv")
housing<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/Housing2000.csv")
latino<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/Latino2000.csv")
other<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/Other HH Income and Poverty Data.csv")


#write csv
DF_2000<-purrr::reduce(list(age, basic, education, housing, income, latino, other), dplyr::left_join, by = 'GEOID')
DF_2000 <- DF_2000 %>% 
  rename(renters_2000= renterunits_2000)
write.csv(DF_2000, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/DF_2000.csv")
Franklinton_CTs2000<-subset(DF_2000, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs2000$area_name<-0
Franklinton_CTs2000<-dplyr::mutate(Franklinton_CTs2000, area_name = recode(GEOID, "39049005000" = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
#move area_name in front of everything
Franklinton_CTs2000<- Franklinton_CTs2000 %>% select(order(colnames(Franklinton_CTs2000)))
Franklinton_CTs2000<-dplyr::select(Franklinton_CTs2000, area_name, GEOID, everything())
