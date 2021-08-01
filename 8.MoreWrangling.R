library(tidyverse)
library(sf)

options(scipen = 999)

#1. Read in metadata----
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat.csv") %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(read.csv("CONIMCP_CleanDataAll_Habitat.csv")) %>% 
  dplyr::filter(Winter==1) %>% 
  mutate(GPS = ifelse(Type=="Band", "Band", "GPS")) %>% 
  dplyr::select(PinpointID, DateTime, SeasonR2n, X, Y, GPS, Population, Mass, Wing, Sex, Year, doy, sun)

#2. ID birds that need the banding point for used----
dat.hab.breed <- dat.hab %>% 
  dplyr::filter(SeasonR2n %in% c("Breed1", "Breed2"))

dat.hab.band <- data.frame(table(dat.hab.breed$GPS, dat.hab.breed$PinpointID)) %>% 
  rename(GPS = Var1, PinpointID=Var2) %>% 
  dplyr::filter(Freq==0, GPS=="GPS") %>% 
  dplyr::select(PinpointID) %>% 
  mutate(BreedPt=1,
         SeasonR2n="Breed1",
         PinpointID=as.integer(as.character(PinpointID)))

#3. ID birds that do and don't have a 2nd wintering location----
dat.hab.wint <- dat.hab %>% 
  dplyr::filter(SeasonR2n=="Winter2") %>% 
  dplyr::select(PinpointID) %>% 
  unique() %>% 
  mutate(Winter2Pt=1)

#4. Join & filter----
#filter out non-roosting points here
dat.hab.use <- dat.hab %>% 
  left_join(dat.hab.band) %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  dplyr::filter(BreedPt==1 | sun==1) %>% 
  rename(Season=SeasonR2n) %>% 
  arrange(PinpointID, DateTime, X, Y)

table(dat.hab.use$GPS, dat.hab.use$PinpointID, dat.hab.use$Season)
table(dat.hab.use$sun)

#5. Read in covariates & wrangle----
#Remove NAs for modelling (all available pts)
#Add population & individual metadata

pop <- dat.hab.use %>% 
  dplyr::select(PinpointID, Population) %>% 
  unique() %>% 
  rename(Population2 = Population)

sex <- dat.hab %>% 
  dplyr::select(PinpointID, Sex, Wing, Mass) %>% 
  unique() %>% 
  rename(Sex2=Sex, Wing2=Wing, Mass2=Mass)

covs <- read.csv("Covariates_Breed&Winter_VIF.csv") %>%   
  dplyr::filter(!is.na(drought.pt), !is.na(hm.pt)) %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  left_join(dat.hab.use %>% 
              dplyr::select(-Season)) %>% 
  arrange(PinpointID, DateTime, X, Y) %>% 
  dplyr::filter(!(Type=="Used" & is.na(Population))) %>% 
  left_join(pop) %>% 
  left_join(sex) %>% 
  mutate(Population = ifelse(is.na(Population), Population2, Population),
         Sex = ifelse(is.na(Sex), Sex2, Sex),
         Mass = ifelse(is.na(Mass), Mass2, Mass),
         Wing = ifelse(is.na(Wing), Wing2, Wing)) %>% 
  dplyr::select(-Population2, Sex2, -Mass2, -Wing2) %>% 
  left_join(dat.hab.wint) %>% 
  mutate(Winter2Pt = ifelse(is.na(Winter2Pt) & Radius %in% c("1km", "10km", "100km", NA), 0, Winter2Pt)) %>% 
  dplyr::filter(PinpointID != 480)
#Take out Female 480; she doesn't have enough data

write.csv(covs, "Covariates_Breed&Winter_Metadata.csv", row.names = FALSE)
