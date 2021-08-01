library(tidyverse)
library(lme4)
library(MuMIn)
library(sf)

options(scipen = 999)

#TO DO: DECIDE WHETHER TO SPLIT 2ND WINTERING GROUNDS INTO SEPARATE ANALYSIS####
#TO DO: DECIDE WHETHER TO ADD ALL BREEDING PTS OR JUST ONE SEASON####

#1. Wrangle for selection----
covs <- read.csv("Covariates_Breed&Winter_Metadata.csv")

covs.sel <- covs %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("10km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0)) %>% 
  mutate(pest.scale = scale(pest.1),
         change.scale = scale(change.1),
         length.scale = scale(Length.1),
         crops.scale = scale(crops.1),
         drought.scale = scale(drought.1),
         light.scale = scale(light.1),
         Population = as.factor(Population)) %>% 
  mutate(Season2 = ifelse(Season %in% c("Breed1", "Breed2", "Breed"), "Breed", "Winter"))

covs.breed <- covs.sel %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.winter <- covs.sel %>% 
  dplyr::filter(Season %in% c("Winter", "Winter2"))

#2. Visualize----
#Linear: light, pest, length, crops,
#Unsure: change
#quad: drought

ggplot(covs.breed, aes(x=drought.scale, y=Response)) +
  geom_jitter() +
  geom_smooth()

ggplot(covs.sel, aes(x=drought.scale, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Season2), scales="free_x")

ggplot(covs.breed) +
  geom_boxplot(aes(colour=factor(Response), y=light.scale, x=factor(Population)))

ggplot(covs.winter1, aes(x=light.scale, y=Response)) +
  geom_jitter() +
  geom_smooth()

ggplot(covs.winter1, aes(x=light.scale, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Population), scales="free_x")

ggplot(covs.winter1) +
  geom_jitter(aes(colour=factor(Response), y=crops.scale, x=factor(Population)))

#3. Seasonal model----
mod.season.global <- lmer(Response ~ pest.scale*factor(Season2) +
                            change.scale*factor(Season2) +
                            length.scale*factor(Season2) +
                            crops.scale*factor(Season2) +
                            poly(drought.scale, 2)*factor(Season2) +
                            light.scale*factor(Season2) +
                            (1|PinpointID),
                          data=covs.sel,
                          na.action="na.fail")
mod.season.dredge <- dredge(mod.season.global, trace=2)
mod.season.dredge

mod.season <- lmer(Response ~ change.scale*factor(Season2) +
                     length.scale*factor(Season2)+
                     poly(drought.scale, 2)*factor(Season2) +
                     (1|PinpointID),
                   data=covs.sel,
                   na.action="na.fail")
summary(mod.season)
dredge(mod.season) #check

#4. Predictions----
newdat.change <- data.frame(expand.grid(change.scale = seq(min(covs.sel$change.scale), max(covs.sel$change.scale), 0.1),
                                        length.scale = mean(covs.sel$length.scale),
                                        drought.scale = mean(covs.sel$drought.scale),
                                        PinpointID = unique(covs.sel$PinpointID),
                                        Season2=unique(covs.sel$Season2)))

newdat.length <- data.frame(expand.grid(change.scale = mean(covs.sel$change.scale),
                                        length.scale = seq(min(covs.sel$length.scale), max(covs.sel$length.scale), 0.1),
                                        drought.scale = mean(covs.sel$drought.scale),
                                        PinpointID = unique(covs.sel$PinpointID),
                                        Season2=unique(covs.sel$Season2)))

newdat.drought <- data.frame(expand.grid(change.scale = mean(covs.sel$change.scale),
                                         length.scale = mean(covs.sel$length.scale),
                                         drought.scale = seq(min(covs.sel$drought.scale), max(covs.sel$drought.scale), 0.1),
                                         PinpointID = unique(covs.sel$PinpointID),
                                         Season2=unique(covs.sel$Season2)))

pred.change <- data.frame(prediction = predict(mod.season, newdat.change)) %>% 
  cbind(newdat.change) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population) %>% 
              unique()) %>% 
  dplyr::filter((Season2=="Breed" & change.scale > min(covs.breed$change.scale) & change.scale < max(covs.breed$change.scale))|
                  (Season2=="Winter" & change.scale > min(covs.winter$change.scale) & change.scale < max(covs.winter$change.scale))) %>% 
  rename(cov = change.scale) %>% 
  mutate(model="change") %>% 
  dplyr::select(model, cov, prediction, Season2, PinpointID)

pred.length <- data.frame(prediction = predict(mod.season, newdat.length)) %>% 
  cbind(newdat.length) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population) %>% 
              unique()) %>% 
  dplyr::filter((Season2=="Breed" & length.scale > min(covs.breed$length.scale) & length.scale < max(covs.breed$length.scale))|
                  (Season2=="Winter" & length.scale > min(covs.winter$length.scale) & length.scale < max(covs.winter$length.scale))) %>% 
  rename(cov = length.scale) %>% 
  mutate(model="length") %>% 
  dplyr::select(model, cov, prediction, Season2, PinpointID)

pred.drought <- data.frame(prediction = predict(mod.season, newdat.drought)) %>% 
  cbind(newdat.drought) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population) %>% 
              unique()) %>% 
  dplyr::filter((Season2=="Breed" & drought.scale > min(covs.breed$drought.scale) & drought.scale < max(covs.breed$drought.scale))|
                  (Season2=="Winter" & drought.scale > min(covs.winter$drought.scale) & drought.scale < max(covs.winter$drought.scale))) %>% 
  rename(cov = drought.scale) %>% 
  mutate(model="drought") %>% 
  dplyr::select(model, cov, prediction, Season2, PinpointID)

pred <- rbind(pred.change, pred.length, pred.drought)

write.csv(pred, "RSFPredictions.csv", row.names = FALSE)

#5. Population differences----
ids <- covs.sel %>% 
  dplyr::select(Population, PinpointID, Sex) %>% 
  unique()
table(ids$Population)
table(ids$Sex)

pops <- c(1,3,7,8,9,13)
#pops <- c(1,3,8,9)

covs.pop <- covs.sel %>% 
  dplyr::filter(Population %in% pops)

covs.pop.breed <- covs.pop %>% 
  dplyr::filter(Season2=="Breed")

covs.pop.winter <- covs.pop %>% 
  dplyr::filter(Season2=="Winter")

#5a. Breeding selection----
mod.breed.global <- lmer(Response ~ change.scale*factor(Population) +
                           length.scale*factor(Population) +
                           poly(drought.scale, 2)*factor(Population) +
                           (1|PinpointID),
                         data=covs.pop.breed,
                         na.action="na.fail")

mod.breed.dredge <- dredge(mod.breed.global, trace=2)
mod.breed.dredge

mod.breed <- lmer(Response ~ change.scale*factor(Population) +
                    length.scale*factor(Population) +
                    poly(drought.scale, 2)*factor(Population) +
                    (1|PinpointID),
                  data=covs.pop.breed,
                  na.action="na.fail")
dredge(mod.breed)

#5b. Wintering selection----
mod.winter.global <- lmer(Response ~ change.scale*factor(Population) +
                            length.scale*factor(Population) +
                            poly(drought.scale, 2)*factor(Population) +
                            (1|PinpointID),
                           data=covs.pop.winter,
                           na.action="na.fail")

mod.winter.dredge <- dredge(mod.winter.global, trace=2)
mod.winter.dredge

mod.winter <- lmer(Response ~ change.scale*factor(Population) +
                     poly(drought.scale, 2)*factor(Population) +
                     change.scale +
                    (1|PinpointID),
                  data=covs.pop.winter,
                  na.action="na.fail")
dredge(mod.winter)

#6. Sex differences----
#6a. Breeding----
mod.breed.global <- lmer(Response ~ change.scale*factor(Sex) +
                           length.scale*factor(Sex) +
                           poly(drought.scale, 2)*factor(Sex) +
                           (1|PinpointID),
                         data=covs.breed,
                         na.action="na.fail")

mod.breed.dredge <- dredge(mod.breed.global, trace=2)
mod.breed.dredge

#6b. Wintering----
mod.winter.global <- lmer(Response ~ change.scale*factor(Sex) +
                           length.scale*factor(Sex) +
                           poly(drought.scale, 2)*factor(Sex) +
                           (1|PinpointID),
                         data=covs.winter,
                         na.action="na.fail")

mod.winter.dredge <- dredge(mod.winter.global, trace=2)
mod.winter.dredge
