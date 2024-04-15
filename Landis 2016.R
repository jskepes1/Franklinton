#read in CSV
require(sf)
require(leaflet) #requires WGS 1984/CRS 4326
require(ggplot2)
require(tidycensus)
require(tidyr)
require(tigris)
require(tidyverse)
require(tidycensus)
require(sp)
require(dplyr)
census_api_key("84dba8250c52c9a11ec14575dce9083eea98d7e9", overwrite = TRUE)
DF_1990<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Others/Franklin County MHH 1990.csv")


#Create Income Deciles
DF_1990$decile <- ntile(DF_1990$medianhhincome_1990, 10)
#Rank them
DF_1990$decile
#view where 1990 Franklinton CTs are
Franklinton_CTs_1990<-subset(DF_1990, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs_1990$area_name<-0
Franklinton_CTs_1990<-dplyr::mutate(Franklinton_CTs_1990, area_name = recode(GEOID, "39049005000"  = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
View(Franklinton_CTs_1990)
#Interpret
#If The data value falls between the percentile 20% and 30%, thus it falls in the third decile.

#2010 Decile
DF_2000to2021$decile2010<-ntile(DF_2000to2021$medianhhincome_2010, 10)
Franklinton_CTs_2010<-subset(DF_2000to2021, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs_2010$area_name<-0
Franklinton_CTs_2010<-dplyr::mutate(Franklinton_CTs_2010, area_name = recode(GEOID, "39049005000"  = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
Franklinton_CTs_2010<-Franklinton_CTs_2010 %>% select(area_name, medianhhincome_2010, decile2010)
View(Franklinton_CTs_2010)


#2000 Decile
DF_2000MHH<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Others/Franklin County MHH 2000.csv")
DF_2000MHH$decile2000<-ntile(DF_2000MHH$medianhhincome_2000, 10)
DF_2000MHH<-subset(DF_2000MHH, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
DF_2000MHH$area_name<-0
DF_2000MHH<-dplyr::mutate(DF_2000MHH, area_name = recode(GEOID, "39049005000"  = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
DF_2000MHH<-DF_2000MHH %>% select(area_name, medianhhincome_2000, decile2000)
View(DF_2000MHH)


#2019 Decile
DF_2000to2021$decile2019<-ntile(DF_2000to2021$medianhhincome_2019, 10)
Franklinton_CTs_2019<-subset(DF_2000to2021, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs_2019$area_name<-0
Franklinton_CTs_2019<-dplyr::mutate(Franklinton_CTs_2019, area_name = recode(GEOID, "39049005000"  = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
Franklinton_CTs_2019<-Franklinton_CTs_2019 %>% select(area_name, medianhhincome_2019, decile2019)
View(Franklinton_CTs_2019)

#2021 Decile
DF_2000to2021$decile2021<-ntile(DF_2000to2021$medianhhincome_2021, 10)
Franklinton_CTs_2021<-subset(DF_2000to2021, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs_2021$area_name<-0
Franklinton_CTs_2021<-dplyr::mutate(Franklinton_CTs_2021, area_name = recode(GEOID, "39049005000"  = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
Franklinton_CTs_2021<-Franklinton_CTs_2021 %>% select(area_name, medianhhincome_2021, decile2021)
View(Franklinton_CTs_2021)
#EF still in first decile
