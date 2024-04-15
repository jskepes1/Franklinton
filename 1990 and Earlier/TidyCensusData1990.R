#Reading in Social Explorer Data from 1970-1990 Geographies from US Census
require(sf)
require(leaflet) #requires WGS 1984/CRS 4326
require(ggplot2)
require(tidycensus)
require(tigris)
require(tidyverse)
require(tidycensus)
require(sp)

#read in social explorer data files
DF_1970<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/1990 and Earlier/1970 Data (inflated).csv")
DF_1980<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/1990 and Earlier/1980 Data (inflated).csv")
DF_1990<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/1990 and Earlier/1990 Data (inflated).csv")

DF_1970to1990<-purrr::reduce(list(DF_1970, DF_1980, DF_1990), dplyr::left_join, by = 'GEOID')

write.csv(DF_1970to1990, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/1990 and Earlier/DF_1970to1990.csv")
Franklinton_CTs1970to1990<-subset(DF_1970to1990, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs1970to1990$area_name<-0
Franklinton_CTs1970to1990<-dplyr::mutate(Franklinton_CTs1970to1990, area_name = recode(GEOID, "39049005000" = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
#move area_name in front of everything
Franklinton_CTs1970to1990<- Franklinton_CTs1970to1990 %>% select(order(colnames(Franklinton_CTs1970to1990)))
Franklinton_CTs1970to1990<-dplyr::select(Franklinton_CTs1970to1990, area_name, GEOID, everything())
write.csv(Franklinton_CTs1970to1990, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/1990 and Earlier/Franklinton_CTs1970to1990.csv")
