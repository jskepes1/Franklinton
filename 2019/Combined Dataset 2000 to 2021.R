#Combining 2000-2019
require(sf)
require(leaflet) #requires WGS 1984/CRS 4326
require(ggplot2)
require(tidycensus)
require(tidyr)
require(tigris)
require(tidyverse)
require(tidycensus)
require(sp)
census_api_key("84dba8250c52c9a11ec14575dce9083eea98d7e9", overwrite = TRUE)

DF_2000<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2000/DF_2000.csv")
DF_2010<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2010/DF_2010.csv")
DF_2019<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2019/DF_2019.csv")
DF_2021<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/2021/DF_2021_manual.csv")

DF_2000to2021<-purrr::reduce(list(DF_2019, DF_2010, DF_2000, DF_2021), dplyr::left_join, by = 'GEOID')
DF_2000to2021 <- subset(DF_2000to2021, select = -X.x)
DF_2000to2021 <- subset(DF_2000to2021, select = -X)
DF_2000to2021 <- subset(DF_2000to2021, select = -X.y)

#inflate the dollar amounts
DF_2000to2021$medianhhincome_2010<-DF_2000to2021$medianhhincome_2010*1.2975
DF_2000to2021$medianhhincome_2019<-DF_2000to2021$medianhhincome_2019*1.1169
DF_2000to2021$grossrent_2010<-DF_2000to2021$grossrent_2010*1.2975
DF_2000to2021$grossrent_2019<-DF_2000to2021$grossrent_2019*1.1169
DF_2000to2021$whitemedianincome_2010<-DF_2000to2021$whitemedianincome_2010*1.2975
DF_2000to2021$whitemedianincome_2019<-DF_2000to2021$whitemedianincome_2019*1.1169
DF_2000to2021$blackmedianincome_2010<-DF_2000to2021$whitemedianincome_2010*1.2975
DF_2000to2021$whitemedianincome_2019<-DF_2000to2021$whitemedianincome_2019*1.1169

#add crime
crime<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Crime/CrimeRates.csv")
DF_2000to2021<-purrr::reduce(list(DF_2000to2021, crime), dplyr::left_join, by = 'GEOID')


#add median home value
medianhomevalue2000<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Others/Median Home Value 2000.csv")
medianhomevalue2010<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Others/Median Home Value 2006-2010.csv")
medianhomevalue2019<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Others/Median Home Value 2015-2019.csv")
medianhomevalue2021<-read.csv("C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Others/Median Home Value 2017-2021.csv")
medianhomevalue2000 <- medianhomevalue2000[-1, ]
medianhomevalue2010 <- medianhomevalue2010[-1, ]
medianhomevalue2019 <- medianhomevalue2019[-1, ]
medianhomevalue2021 <- medianhomevalue2021[-1, ]
medianhomevalue2000 <- subset(medianhomevalue2000, select = c("FIPS", "Owner.Occupied.Housing.Units..Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2021."))
medianhomevalue2010 <- subset(medianhomevalue2010, select = c("FIPS", "Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2021."))
medianhomevalue2019 <- subset(medianhomevalue2019, select = c("FIPS", "Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2021."))
medianhomevalue2021 <- subset(medianhomevalue2021, select = c("FIPS", "Median.Value"))
medianhomevalue2000 <- rename(medianhomevalue2000, GEOID = FIPS)
medianhomevalue2010 <- rename(medianhomevalue2010, GEOID = FIPS)
medianhomevalue2019 <- rename(medianhomevalue2019, GEOID = FIPS)
medianhomevalue2021 <- rename(medianhomevalue2021, GEOID = FIPS)
medianhomevalue2000 <- rename(medianhomevalue2000, medianhomevalue2000 = Owner.Occupied.Housing.Units..Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2021.)
medianhomevalue2010 <- rename(medianhomevalue2010, medianhomevalue2010 = Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2021.)
medianhomevalue2019 <- rename(medianhomevalue2019, medianhomevalue2019 = Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2021.)
medianhomevalue2021 <- rename(medianhomevalue2021, medianhomevalue2021 = Median.Value)
medianhomevalue<-purrr::reduce(list(medianhomevalue2000,medianhomevalue2010, medianhomevalue2019, medianhomevalue2021), dplyr::left_join, by = 'GEOID')
medianhomevalue$GEOID<-as.double(medianhomevalue$GEOID)
#rejoin to median home values
DF_2000to2021<-purrr::reduce(list(DF_2000to2021, medianhomevalue), dplyr::left_join, by = 'GEOID')


#Franklinton Area
Franklinton_CTs<-subset(DF_2000to2021, GEOID== "39049005000" | GEOID== "39049004300" | GEOID== "39049004200")
Franklinton_CTs$area_name<-0
Franklinton_CTs<-dplyr::mutate(Franklinton_CTs, area_name = recode(GEOID, "39049005000"  = "West Franklinton", "39049004300" = "North of Broad", "39049004200" = "East Franklinton"))
#add gross rent 2000
grossrent_2000<- data.frame(GEOID = c("39049005000", "39049004300", "39049004200"),
                            grossrent_2000 = c(749.52, 1190.904, 266.496))
grossrent_2000$GEOID<-as.double(grossrent_2000$GEOID)
Franklinton_CTs<-purrr::reduce(list(Franklinton_CTs, grossrent_2000), dplyr::left_join, by = 'GEOID')
#move area_name in front of everything
Franklinton_CTs<- Franklinton_CTs %>% select(order(colnames(Franklinton_CTs)))
Franklinton_CTs<-dplyr::select(Franklinton_CTs, area_name, GEOID, everything())

F_CTs_Home<-Franklinton_CTs %>% select(area_name, medianhomevalue2000, medianhomevalue2010, medianhomevalue2019, medianhomevalue2021)
view(F_CTs_Home)
F_CTs_VCrime<-Franklinton_CTs %>% select(area_name, vc_rate_2011, vc_rate_2012, vc_rate_2013, vc_rate_2014, vc_rate_2015,  vc_rate_2016, vc_rate_2017, vc_rate_2018, vc_rate_2019)
view(F_CTs_VCrime)
F_CTs_PCrime<-Franklinton_CTs %>% select(area_name, pc_rate_2011, pc_rate_2012, pc_rate_2013, pc_rate_2014, pc_rate_2015,  pc_rate_2016, pc_rate_2017, pc_rate_2018, pc_rate_2019)
view(F_CTs_PCrime)

#write the csv values
write.csv(Franklinton_CTs, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Combined Datasets/Franklinton_CTs2000to2021.csv")
write.csv(DF_2000to2021, "C:/Users/aeroa/OneDrive - The Ohio State University/Franklinton/Analysis/Combined Datasets/DF_2000to2021.csv")
