library(tidyverse)
library(suncalc)
library(lubridate)
library(data.table)
library(adehabitatLT)

options(scipen = 999)

#THINK ABOUT USING THAT CLUSTERING ANALYSIS FOR WINTERING & BREEDING CLASSIFICATION

#1. Read in data----
dat <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/CONIMCP_CleanDataAll.csv") %>% 
  mutate(Date = as.Date(str_sub(DateTime, 1, 10)),
         DateTime = ymd_hms(paste0(Date, Time))) %>% 
  dplyr::select(PinpointID, Population, Mass, Wing, Sex, Type, DateTime, Date, Time, Year, Lat, Long, BandDist, WintDist, GCD, Season, Season2, Winter) %>% 
  unique() %>% 
  dplyr::filter(Winter==1)
  
#2. Calculate sun times----
dat.sun <- getSunlightTimes(data=dat%>% 
                               rename(lat = Lat, lon = Long, date=Date) %>% 
                               dplyr::select(date, lat, lon)) %>% 
  dplyr::select(sunrise, sunset) %>% 
  cbind(dat) %>% 
  mutate(sun = ifelse(DateTime > sunrise & DateTime < sunset, 1, 0))

#3. Calculate NSD----
traj <- as.ltraj(xy=dat.sun[,c("Long", "Lat")],
                 id=dat.sun$PinpointID,
                 date=dat.sun$DateTime,
                 proj4string = CRS("+proj=longlat +datum=WGS84")) 

dat.traj <- rbindlist(traj) %>% 
  dplyr::select(dist, R2n, abs.angle, rel.angle) %>% 
  cbind(dat.sun) %>% 
  mutate(doy=yday(DateTime)) %>% 
  group_by(PinpointID) %>% 
  mutate(R2n2 = max(R2n) - R2n) %>% 
  ungroup()

#4. Recalculate seasons----
dat.season <- dat.traj %>% 
  mutate(R2nRound = round(R2n, -1)) %>% 
  mutate(SeasonR2n = case_when(R2n2 < 10 ~ "Winter",
                               R2n < 0.01 & BandDist < 100 & year(DateTime)==Year ~ "Breed1",
                               R2n < 0.01 & BandDist < 100 & year(DateTime)>Year ~ "Breed2",
                               R2n < 0.1 & PinpointID==826 & year(DateTime)>Year ~ "Breed2",
                               PinpointID==81 & R2nRound %in% c(7230, 7240) ~ "Winter2",
                               PinpointID==439 & R2nRound==7720 ~ "Winter2",
                               PinpointID==443 & R2nRound==3460 ~ "Winter2",
                               PinpointID==490 & R2nRound==4150 ~ "Winter2",
                               PinpointID==825 & R2nRound==7770 ~ "Winter2",
                               PinpointID==826 & R2nRound==6590 ~ "Winter2")) %>% 
  mutate(Breed = ifelse(SeasonR2n=="Breed" & year(DateTime)==Year, "Breed1", "Breed2"))
table(dat.season$SeasonR2n, dat.season$PinpointID)

#5. Filter----
dat.hab <- dat.season %>% 
  dplyr::filter(!is.na(SeasonR2n))
table(dat.hab$PinpointID, dat.hab$SeasonR2n)

write.csv(dat.hab, "CONIMCP_CleanDataAll_Habitat.csv", row.names = FALSE)
