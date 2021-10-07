library(tidyverse)
library(sf)
library(geosphere)

options(scipen = 999)

#1. Read in data----
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat_Roosting.csv") %>% 
  mutate(PtYear = year(ymd(Date)))

#2. Calculate winter centroid for all birds----
cent.wint <- dat.hab %>% 
  dplyr::filter(Season=="Winter") %>% 
  group_by(Population, PinpointID, Season, Winter, Sex, Mass, Wing, Year) %>% 
  summarize(Lat = mean(Lat),
            Long = mean(Long),
            count=n()) %>% 
  ungroup()

table(cent.wint$PinpointID)

#2. Calculate breeding centroid for all birds (banding location will just get included in this)----
cent.breed <- dat.hab %>% 
  dplyr::filter(Season=="Breed") %>% 
  group_by(Population, PinpointID, Season, Winter, Sex, Mass, Wing, Year, PtYear) %>% 
  summarize(Lat = mean(Lat),
            Long = mean(Long),
            count=n()) %>% 
  ungroup()

table(cent.breed$PinpointID)

#3. Select year with more data points for birds with data from both seasons----
#Wrangle
ids.trans.1 <- data.frame(table(cent.breed$PinpointID)) %>% 
  rename(PinpointID=Var1) %>% 
  mutate(PinpointID=as.numeric(as.character(PinpointID))) %>% 
  dplyr::filter(Freq==1) %>% 
  left_join(cent.breed)

ids.trans.2 <- data.frame(table(cent.breed$PinpointID)) %>% 
  rename(PinpointID=Var1) %>% 
  mutate(PinpointID=as.numeric(as.character(PinpointID))) %>% 
  dplyr::filter(Freq==2) %>% 
  left_join(cent.breed) %>% 
  group_by(PinpointID) %>% 
  mutate(max = max(count)) %>% 
  ungroup() %>% 
  dplyr::filter(max==count,
                !(PinpointID==61 & PtYear==2015 |
                    PinpointID==79 & PtYear==2015 |
                    PinpointID==487 & PtYear==2017)) %>% 
  dplyr::select(-max)

ids.exclude <- data.frame(table(cent.breed$PinpointID)) %>% 
  rename(PinpointID=Var1) %>% 
  mutate(PinpointID=as.numeric(as.character(PinpointID))) %>% 
  dplyr::filter(Freq==2) %>% 
  left_join(cent.breed) %>% 
  group_by(PinpointID) %>% 
  mutate(max = max(count)) %>% 
  ungroup() %>% 
  anti_join(ids.trans.2) %>% 
  dplyr::select(PinpointID, Season, PtYear)
  

write.csv(ids.exclude, "ExclusionYearForBirdsWith2BreedingYears.csv", row.names = FALSE)

cent.trans <- rbind(ids.trans.1, ids.trans.2) %>% 
  dplyr::select(Population, PinpointID, Season, Winter, Sex, Mass, Wing, Year, Lat, Long, count)

#4. Put together----
dat.cent <- rbind(cent.wint, cent.trans)

write.csv(dat.cent, "BreedingWinterCentroids.csv", row.names=FALSE)