library(tidyverse)
library(lme4)
library(MuMIn)

options(scipen = 999)

#TO DO: DECIDE WHETHER TO SPLIT 2ND WINTERING GROUNDS INTO SEPARATE ANALYSIS####
#TO DO: ADD ONE MORE LARGER (I.E., RANGE) SCALE TO 3RD ORDER SELECTION####
#TO DO: DECIDE WHETHER TO ADD ALL BREEDING PTS OR JUST ONE SEASON####

#1. Read in metadata----
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat.csv") %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(read.csv("CONIMCP_CleanDataAll_Habitat.csv")) %>% 
  dplyr::filter(Winter==1) %>% 
  mutate(GPS = ifelse(Type=="Band", "Band", "GPS")) %>% 
  dplyr::select(PinpointID, DateTime, SeasonR2n, X, Y, GPS, Population, Mass, Wing, Sex, Year, doy, sun)

#2. ID birds that need the banding point for used
dat.hab.breed <- dat.hab %>% 
  dplyr::filter(SeasonR2n %in% c("Breed1", "Breed2"))

dat.hab.band <- data.frame(table(dat.hab.breed$GPS, dat.hab.breed$PinpointID)) %>% 
  rename(GPS = Var1, PinpointID=Var2) %>% 
  dplyr::filter(Freq==0, GPS=="GPS") %>% 
  dplyr::select(PinpointID) %>% 
  mutate(BreedPt=1,
         SeasonR2n="Breed1",
         PinpointID=as.integer(as.character(PinpointID)))

#filter out non-roosting points here
dat.hab.use <- dat.hab %>% 
  left_join(dat.hab.band) %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  dplyr::filter(BreedPt==1 | sun==1) %>% 
  rename(Season=SeasonR2n) %>% 
  arrange(PinpointID, DateTime, X, Y)

table(dat.hab.use$GPS, dat.hab.use$PinpointID, dat.hab.use$Season)
table(dat.hab.use$sun)

#3. Read in covariates & wrangle----
#Remove NAs for modelling (all available pts)
#Remove banding points for most birds
#Filter by roosting pts
covs.pt <- read.csv("Covariates_Breed&Winter_Point.csv") %>%   
  dplyr::filter(!is.na(drought.pt), !is.na(hm.pt)) %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  left_join(dat.hab.use %>% 
              dplyr::select(-Season)) %>% 
  arrange(PinpointID, DateTime, X, Y) %>% 
  dplyr::filter(!(Type=="Used" & is.na(Population)))

covs.5 <- read.csv("Covariates_Breed&Winter_500.csv")  %>% 
  dplyr::filter(!is.na(drought.5), !is.na(bare.5)) %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  left_join(dat.hab.use %>% 
              dplyr::select(-Season)) %>% 
  arrange(PinpointID, DateTime, X, Y) %>% 
  dplyr::filter(!(Type=="Used" & is.na(Population)))

covs.200 <- read.csv("Covariates_Breed&Winter_20000.csv") %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  left_join(dat.hab.use %>% 
              dplyr::select(-Season)) %>% 
  arrange(PinpointID, DateTime, X, Y) %>% 
  dplyr::filter(!(Type=="Used" & is.na(Population)))

#A. SECOND ORDER####
#include 500 m radius and 20 km radius
#Compare centroids to range (don't need random effects)

#2. Wrangle----
#2a. 500m
covs.5.3rd <- covs.5 %>% 
  dplyr::filter(Type %in% c("Centroid", "Available"),
                Radius %in% c("Breed", "Winter", NA)) %>% 
  mutate(Response = case_when(Type=="Centroid" ~ 1,
                              Type=="Available" ~ 0))

covs.5.3rd.breed <- covs.5.3rd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", NA))

covs.5.3rd.winter1 <- covs.5.3rd %>% 
  dplyr::filter(Season %in% c("Winter1", NA))

#2b. 20km 
covs.200.3rd <- covs.200 %>% 
  dplyr::filter(Type %in% c("Centroid", "Available"),
                Radius %in% c("Breed", "Winter", NA)) %>% 
  mutate(Response = case_when(Type=="Centroid" ~ 1,
                              Type=="Available" ~ 0))

covs.200.3rd.breed <- covs.200.3rd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", NA))

covs.200.3rd.winter1 <- covs.200.3rd %>% 
  dplyr::filter(Season %in% c("Winter1", NA))

#3. Winter selection----
#3a. 500m
mod.5.3rd.winter1.glob <- lm(Response ~ bare.5 + crops.5 + grass.5 + shrub.5 + water.permanent.5 + water.seasonal.5 + stable.lights.5 + Length.5 + pest.5 + drought.5, data=covs.5.3rd.winter1, na.action="na.fail")
summary(mod.5.3rd.winter1.glob)
mod.5.3rd.winter1.d <- dredge(mod.5.3rd.winter1.glob)
mod.5.3rd.winter1.d
#shrub, drought, grass, crop

mod.5.3rd.winter1 <- lm(Response ~ crops.5 + grass.5 + shrub.5 + drought.5, data=covs.5.3rd.winter1, na.action="na.fail")
summary(mod.5.3rd.winter1)

#3b. 20km
mod.200.3rd.winter1.glob <- lm(Response ~ bare.200 + crops.200 + grass.200 + shrub.200 + water.permanent.200 + water.seasonal.200 + stable.lights.200 + Length.200 + pest.200 + drought.200, data=covs.200.3rd.winter1, na.action="na.fail")
summary(mod.200.3rd.winter1.glob)
mod.200.3rd.winter1.d <- dredge(mod.200.3rd.winter1.glob)
mod.200.3rd.winter1.d
#drought, grass, shrub, permanent water

mod.200.3rd.winter1 <- lm(Response ~ water.permanent.200 + grass.200 + shrub.200 + drought.200, data=covs.200.3rd.winter1, na.action="na.fail")
summary(mod.200.3rd.winter1)

#4. Breeding selection----
#4a. 500m
mod.5.3rd.breed.glob <- lm(Response ~ bare.5 + crops.5 + grass.5 + shrub.5 + water.permanent.5 + water.seasonal.5 + stable.lights.5 + Length.5 + pest.5 + drought.5, data=covs.5.3rd.breed, na.action="na.fail")
summary(mod.5.3rd.breed.glob)
mod.5.3rd.breed.d <- dredge(mod.5.3rd.breed.glob)
mod.5.3rd.breed.d
#crop, drought, length, shrub, light

mod.5.3rd.breed <- lm(Response ~ crops.5 + shrub.5 + drought.5 + Length.5 + stable.lights.5, data=covs.5.3rd.breed, na.action="na.fail")
summary(mod.5.3rd.breed)

#4b. 20km
mod.200.3rd.breed.glob <- lm(Response ~ bare.200 + crops.200 + grass.200 + shrub.200 + water.permanent.200 + water.seasonal.200 + stable.lights.200 + Length.200 + pest.200 + drought.200, data=covs.200.3rd.breed, na.action="na.fail")
summary(mod.200.3rd.breed.glob)
mod.200.3rd.breed.d <- dredge(mod.200.3rd.breed.glob)
mod.200.3rd.breed.d
#drought, shrub, light

mod.200.3rd.breed <- lm(Response ~ shrub.200 + drought.200 + stable.lights.200, data=covs.200.3rd.breed, na.action="na.fail")
summary(mod.200.3rd.breed)

#Ok, but what does 2nd order selection really tell us in regards to our actual question about grouping of individuals in populations & habitat use? Nothing. But it's fun to know.

#B. THIRD ORDER####

#5. Wrangle----
#5a. 500m availability: use point level data
covs.pt.2nd <- covs.pt %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("500m", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.pt.2nd.breed <- covs.pt.2nd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

table(covs.pt.2nd.breed$GPS, covs.pt.2nd.breed$PinpointID)

covs.pt.2nd.winter1 <- covs.pt.2nd %>% 
  dplyr::filter(Season %in% c("Winter"))

#5b. 20km availability: use 500m buffer data
covs.5.2nd <- covs.5 %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("20km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.5.2nd.breed <- covs.5.2nd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

table(covs.5.2nd.breed$GPS, covs.5.2nd.breed$PinpointID)

covs.5.2nd.winter1 <- covs.5.2nd %>% 
  dplyr::filter(Season %in% c("Winter"))

#6. Winter selection----
#6a. 500m availability: use point value data
mod.pt.2nd.winter1.glob <- lmer(Response ~ lc + hm.pt + stable.lights.pt + drought.pt + pest.pt + Population +(1|PinpointID), data=covs.pt.2nd.winter1, na.action="na.fail")
summary(mod.pt.2nd.winter1.glob)
mod.pt.2nd.winter1.d <- dredge(mod.pt.2nd.winter1.glob)
mod.pt.2nd.winter1.d
#shrub, drought, grass, crop

mod.pt.2nd.winter1 <- lm(Response ~ crops.pt + grass.pt + shrub.pt + drought.pt, data=covs.pt.2nd.winter1, na.action="na.fail")
summary(mod.pt.2nd.winter1)

#6b. 20km extent availability: use 500m radius data