library(tidyverse)
library(readxl)
library(lubridate)
library(suncalc)

#1. Read in other datasets----
dat <- read.csv("CONIMCP_CleanDataAll_Habitat_3857.csv")

pop <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_population_abundance.csv")

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population, Sex) %>% 
  dplyr::filter(PinpointID != -99) %>% 
  left_join(pop %>% 
              dplyr::select(Population, Abbreviation))

#2. Wrangle human modification data from GEE----
hm5 <- read.csv("Covariates/HumanModification_500.csv") %>% 
  mutate(buffer="500m")
hm200 <- read.csv("Covariates/HumanModification_20000.csv") %>% 
  mutate(buffer="20km")
hmpt <- read.csv("Covariates/HumanModification_point.csv") %>% 
  mutate(buffer="point")

hm <- hm5 %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean, buffer)

hm <- rbind(hm5, hm200, hmpt) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, Lat, Long, mean, buffer) %>% 
  rename(hm = mean) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number(),
         hm=100*hm)

#4. Wrangle Copernicus landcover from GEE----
lc200 <- read.csv("Covariates/Landcover_200.csv") %>% 
  mutate(buffer="200m")
lc2000 <- read.csv("Covariates/Landcover_2000.csv") %>% 
  mutate(buffer="2km")
lc20000 <- read.csv("Covariates/Landcover_20000.csv") %>% 
  mutate(buffer="20km")

lc <- rbind(lc200, lc2000, lc20000) %>% 
  rename_with(~gsub(pattern=".coverfraction", replacement = "", .x, fixed=TRUE), ends_with(".coverfraction")) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, bare, crops, grass, moss, shrub, snow, tree, urban, water.permanent, water.seasonal, buffer) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number()) 

#5. Put all together and add other metadata for individuals----
covs <- full_join(forest, hm) %>% 
  full_join(lc) %>% 
  left_join(band) %>% 
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  left_join(dat %>% 
              dplyr::select(PinpointID, Season2, DateTime, Lat, Long, Winter, Time) %>% 
              unique() %>% 
              mutate(DateTime=ymd_hms(DateTime))) %>% 
  separate(DateTime, into=c("Date", "Time2"), sep=" ") %>% 
  dplyr::select(-Time2) %>% 
  mutate(DateTime=ymd_hms(paste0(Date, Time)))
  
write.csv(covs, "Covariates_Breed&Winter.csv", row.names=FALSE)

