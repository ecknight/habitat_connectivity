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

#4. Wrangle for selection----
covs.sel <- covs %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("100km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.breed <- covs.sel %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.winter1 <- covs.sel %>% 
  dplyr::filter(Season %in% c("Winter"))

#5. Visualize----
ggplot(covs.breed, aes(x=light.10, y=Response)) +
  geom_jitter() +
  geom_smooth()

ggplot(covs.breed, aes(x=light.10, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Population), scales="free_x")

ggplot(covs.breed) +
  geom_boxplot(aes(colour=factor(Response), y=light.10, x=factor(Population)))

ggplot(covs.winter1, aes(x=light.10, y=Response)) +
  geom_jitter() +
  geom_smooth()

ggplot(covs.winter1, aes(x=light.10, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Population), scales="free_x")

ggplot(covs.winter1) +
  geom_jitter(aes(colour=factor(Response), y=crops.10, x=factor(Population)))

#6. Breeding selection----
mod.breed.global <- lmer(Response ~ pest.10*factor(Population) +
                           change.10*factor(Population) +
                           Length.10*factor(Population) +
                           poly(crops.10, 2)*factor(Population) +
                           poly(drought.10, 2)*factor(Population) +
                           light.10*factor(Population) +
                           (1|PinpointID),
                         data=covs.breed,
                         na.action="na.fail")

mod.breed.dredge <- dredge(mod.breed.global, trace=2)
mod.breed.dredge

#7. Wintering selection----
mod.winter1.global <- lmer(Response ~ pest.10*factor(Population) +
                           change.10*factor(Population) +
                           Length.10*factor(Population) +
                           poly(crops.10, 2)*factor(Population) +
                           poly(drought.10, 2)*factor(Population) +
                           light.10*factor(Population) +
                           (1|PinpointID),
                         data=covs.winter1,
                         na.action="na.fail")

mod.winter1.dredge <- dredge(mod.winter1.global, trace=2)
mod.winter1.dredge

#8. Thinking----
nbirds <- covs.breed %>% 
  dplyr::select(Population, PinpointID) %>% 
  unique()
table(nbirds$Population)

mod.wint.test <- lmer(Response ~ poly(drought.10)*factor(Population) +
                        (1|PinpointID),
                      data=covs.winter1,
                      na.action="na.fail")
dredge(mod.wint.test)
