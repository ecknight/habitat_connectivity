library(tidyverse)
library(lme4)
library(MuMIn)
library(sf)

options(scipen = 999)

#TO DO: DECIDE WHETHER TO SPLIT 2ND WINTERING GROUNDS INTO SEPARATE ANALYSIS####
#TO DO: DECIDE WHETHER TO ADD ALL BREEDING PTS OR JUST ONE SEASON####

#1. Wrangle for selection----
covs.sel <- covs %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("100km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0))

covs.breed <- covs.sel %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.winter1 <- covs.sel %>% 
  dplyr::filter(Season %in% c("Winter"))

#2. Visualize----
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

#3. Breeding selection----
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

#4. Wintering selection----
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

#5. Thinking----
nbirds <- covs.breed %>% 
  dplyr::select(Population, PinpointID) %>% 
  unique()
table(nbirds$Population)

mod.wint.test <- lmer(Response ~ poly(drought.10)*factor(Population) +
                        (1|PinpointID),
                      data=covs.winter1,
                      na.action="na.fail")
dredge(mod.wint.test)
