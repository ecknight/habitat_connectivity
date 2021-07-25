library(tidyverse)
library(lme4)
library(MuMIn)
library(sf)

options(scipen = 999)

#TO DO: DECIDE WHETHER TO SPLIT 2ND WINTERING GROUNDS INTO SEPARATE ANALYSIS####
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
#Add population

pop <- dat.hab.use %>% 
  dplyr::select(PinpointID, Population) %>% 
  unique() %>% 
  rename(Population2 = Population)

covs <- read.csv("Covariates_Breed&Winter_VIF.csv") %>%   
  dplyr::filter(!is.na(drought.pt), !is.na(hm.pt)) %>% 
  mutate(X=round(X), Y=round(Y)) %>% 
  left_join(dat.hab.use %>% 
              dplyr::select(-Season)) %>% 
  arrange(PinpointID, DateTime, X, Y) %>% 
  dplyr::filter(!(Type=="Used" & is.na(Population))) %>% 
  left_join(pop) %>% 
  mutate(Population = ifelse(is.na(Population), Population2, Population)) %>% 
  dplyr::select(-Population2)

#A. SECOND ORDER####
#include 500 m radius and 20 km radius
#Compare centroids to range (don't need random effects)

#2. Wrangle----
#2a. 1km
covs.3rd <- covs %>% 
  dplyr::filter(Type %in% c("Centroid", "Available"),
                Radius %in% c("Breed", "Winter", NA)) %>% 
  mutate(Response = case_when(Type=="Centroid" ~ 1,
                              Type=="Available" ~ 0))

covs.3rd.breed <- covs.3rd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", NA))

covs.3rd.winter1 <- covs.3rd %>% 
  dplyr::filter(Season %in% c("Winter1", NA))

#3. Winter selection----
#3a. 1km
mod.3rd.winter1.glob <- lm(Response ~ bare.1 + crops.1 + grass.1 + shrub.1 + water.permanent.1 + water.seasonal.1 + light.1 + Length.1 + pest.1 + drought.1, data=covs.3rd.winter1, na.action="na.fail")
summary(mod.3rd.winter1.glob)

mod.3rd.winter1.d <- dredge(mod.3rd.winter1.glob)
mod.3rd.winter1.d
#shrub, drought, grass, crop

mod.3rd.winter1 <- lm(Response ~ crops.1 + grass.1 + shrub.1 + drought.1, data=covs.3rd.winter1, na.action="na.fail")
summary(mod.3rd.winter1)

#3b. 10km
mod.3rd.winter1.glob <- lm(Response ~ bare.10 + crops.10 + grass.10 + shrub.10 + water.permanent.10 + water.seasonal.10 + light.10 + Length.10 + pest.10 + drought.10, data=covs.3rd.winter1, na.action="na.fail")
summary(mod.3rd.winter1.glob)
mod.3rd.winter1.d <- dredge(mod.3rd.winter1.glob)
mod.3rd.winter1.d
#drought, grass, shrub, permanent water, road length

mod.3rd.winter1 <- lm(Response ~ water.permanent.10 + grass.10 + shrub.10 + drought.10 + Length.10, data=covs.3rd.winter1, na.action="na.fail")
summary(mod.3rd.winter1)

#4. Breeding selection----
#4a. 1km
mod.3rd.breed.glob <- lm(Response ~ bare.1 + crops.1 + grass.1 + shrub.1 + water.permanent.1 + water.seasonal.1 + light.1 + Length.1 + pest.1 + drought.1, data=covs.3rd.breed, na.action="na.fail")
summary(mod.3rd.breed.glob)
mod.3rd.breed.d <- dredge(mod.3rd.breed.glob)
mod.3rd.breed.d
#crop, drought, length, shrub, light

mod.3rd.breed <- lm(Response ~ crops.1 + shrub.1 + drought.1 + Length.1 + light.1, data=covs.3rd.breed, na.action="na.fail")
summary(mod.3rd.breed)

#4b. 10km
mod.3rd.breed.glob <- lm(Response ~ bare.10 + crops.10 + grass.10 + shrub.10 + water.permanent.10 + water.seasonal.10 + light.10 + Length.10 + pest.10 + drought.10, data=covs.3rd.breed, na.action="na.fail")
summary(mod.3rd.breed.glob)
mod.3rd.breed.d <- dredge(mod.3rd.breed.glob)
mod.3rd.breed.d
#drought, shrub, light

mod.10.3rd.breed <- lm(Response ~ shrub.10 + drought.10 + light.10 + Length.10, data=covs.10.3rd.breed, na.action="na.fail")
summary(mod.10.3rd.breed)

#Ok, but what does 2nd order selection really tell us in regards to our actual question about grouping of individuals in populations & habitat use? Nothing. But it's fun to know.

#B. THIRD ORDER####

#5. Wrangle----
#5a. 1km availability: use point level data
covs.pt.2nd <- covs.pt %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("1km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.pt.2nd.breed <- covs.pt.2nd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.pt.2nd.winter1 <- covs.pt.2nd %>% 
  dplyr::filter(Season %in% c("Winter"))

#5b. 10km availability: use 1km buffer data
covs.1.2nd <- covs.1 %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("100km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.1.2nd.breed <- covs.1.2nd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.1.2nd.winter1 <- covs.1.2nd %>% 
  dplyr::filter(Season %in% c("Winter"))

#5c. 100km availability: use 10km buffer data
covs.10.2nd <- covs.10 %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("100km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.10.2nd.breed <- covs.10.2nd %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.10.2nd.winter1 <- covs.10.2nd %>% 
  dplyr::filter(Season %in% c("Winter"))

#6. Winter selection----

#TO DO: THINK ABOUT ROLE OF POPULATION IN THESE MODELS: DO I NEED AN INTERACTION?####
#TO DO: THINK ABOUT BUFFER EXTENT FOR EACH AVAILABILITY EXTENT A BIT MORE####

#6a. 1km availability: use point value data
mod.pt.2nd.winter1.glob <- lmer(Response ~ factor(lc) + hm.pt + light.pt + drought.pt + pest.pt + factor(Population) +(1|PinpointID), data=covs.pt.2nd.winter1, na.action="na.fail")

mod.pt.2nd.winter1.d <- dredge(mod.pt.2nd.winter1.glob, trace=2)
mod.pt.2nd.winter1.d
#null

#6b. 10km extent availability: use 1km radius data
mod.1.2nd.winter1.glob <- lmer(Response ~ bare.1 + crops.1 + grass.1 + shrub.1 + water.permanent.1 + water.seasonal.1 + light.1 + Length.1 + pest.1 + drought.1 + factor(Population) +(1|PinpointID), data=covs.1.2nd.winter1, na.action="na.fail")

mod.1.2nd.winter1.d <- dredge(mod.1.2nd.winter1.glob, trace=2)
mod.1.2nd.winter1.d
#shrub, road, grass

mod.1.2nd.winter1.glob <- lmer(Response ~ grass.1 + shrub.1 + Length.1 + (1|PinpointID), data=covs.1.2nd.winter1, na.action="na.fail")
summary(mod.1.2nd.winter1.glob)

#6c. 100km extent availability: use 10km radius data
mod.10.2nd.winter1.glob <- lmer(Response ~ bare.10 + crops.10 + grass.10 + shrub.10 + water.permanent.10 + water.seasonal.10 + light.10 + Length.10 + pest.10 + drought.10 + factor(Population) +(1|PinpointID), data=covs.10.2nd.winter1, na.action="na.fail")

mod.10.2nd.winter1.d <- dredge(mod.10.2nd.winter1.glob, trace=2)
mod.10.2nd.winter1.d
#grass, drought

mod.10.2nd.winter1.glob <- lmer(Response ~ grass.10 + drought.10 + (1|PinpointID), data=covs.10.2nd.winter1, na.action="na.fail")
summary(mod.10.2nd.winter1.glob)

#7. Breeding selection----

#7a. 1km availability: use point value data
mod.pt.2nd.breed.glob <- lmer(Response ~ factor(lc) + hm.pt + light.pt + drought.pt + pest.pt + factor(Population) +(1|PinpointID), data=covs.pt.2nd.breed, na.action="na.fail")

mod.pt.2nd.breed.d <- dredge(mod.pt.2nd.breed.glob, trace=2)
mod.pt.2nd.breed.d
#null

#7b. 10km extent availability: use 1km radius data
mod.1.2nd.breed.glob <- lmer(Response ~ bare.1 + crops.1 + grass.1 + shrub.1 + water.permanent.1 + water.seasonal.1 + light.1 + Length.1 + pest.1 + drought.1 + factor(Population) +(1|PinpointID), data=covs.1.2nd.breed, na.action="na.fail")

mod.1.2nd.breed.d <- dredge(mod.1.2nd.breed.glob, trace=2)
mod.1.2nd.breed.d
#crop, light, permanent water

mod.1.2nd.breed <- lmer(Response ~ crops.1 + light.1 + water.permanent.1 + (1|PinpointID), data=covs.1.2nd.breed, na.action="na.fail")
summary(mod.1.2nd.breed)

#7c. 100km extent availability: use 10km radius data
mod.10.2nd.breed.glob <- lmer(Response ~ bare.10 + crops.10 + grass.10 + shrub.10 + water.permanent.10 + water.seasonal.10 + light.10 + Length.10 + pest.10 + drought.10 + factor(Population) +(1|PinpointID), data=covs.10.2nd.breed, na.action="na.fail")

mod.10.2nd.breed.d <- dredge(mod.10.2nd.breed.glob, trace=2)
mod.10.2nd.breed.d
#pesticides, shrubs

mod.10.2nd.breed <- lmer(Response ~ shrub.10 + pest.10 + (1|PinpointID), data=covs.10.2nd.breed, na.action="na.fail")
summary(mod.10.2nd.breed)

#OK SO WHY DOES POPULATION NOT AFFECT HABITAT SELECTION, BUT IT SHOWS IN THE MANTEL???

#8. Focus on 100km extent: visualize for shape----
ggplot(covs.1.2nd.winter1, aes(x=drought.1, y=Response)) +
  geom_jitter() +
  geom_smooth()

ggplot(covs.1.2nd.winter1, aes(x=drought.1, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Population), scales="free_x")

ggplot(covs.1.2nd.winter1) +
  geom_boxplot(aes(colour=factor(Response), y=drought.1, x=factor(Population)))
