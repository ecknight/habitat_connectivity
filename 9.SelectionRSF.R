library(tidyverse)
library(lme4)
library(MuMIn)
library(sf)
library(merTools)

options(scipen = 999)

#TO DO: DECIDE WHETHER TO SPLIT 2ND WINTERING GROUNDS INTO SEPARATE ANALYSIS####
#TO DO: DECIDE WHETHER TO ADD ALL BREEDING PTS OR JUST ONE SEASON####

#1. Wrangle for selection----
covs <- read.csv("Covariates_Breed&Winter_Roosting.csv") %>% 
  rename_all(~gsub(., pattern=".coverfraction", replacement="")) %>% 
  rename_all(~gsub(., pattern="stable.lights", replacement="light")) %>% 
  rename_all(~gsub(., pattern="change.confidence", replacement="change")) %>% 
  rename(length.1 = Length.1, length.10 = Length.10) %>% 
  mutate(Population = as.factor(Population))

covs.sel <- covs %>% 
  dplyr::filter(Type %in% c("Used", "Available"),
                Radius %in% c("100km", NA)) %>% 
  mutate(Response = case_when(Type=="Used" ~ 1,
                              Type=="Available" ~ 0)) %>% 
  mutate(pest.scale = scale(pest.10),
         loss.scale = scale(loss.10),
         length.scale = scale(length.10),
         crops.scale = scale(crops.10),
         drought.scale = scale(drought.10),
         light.scale = scale(light.10)) %>% 
  dplyr::filter(!is.na(crops.scale))

covs.breed <- covs.sel %>% 
  dplyr::filter(Season %in% c("Breed"))

covs.winter <- covs.sel %>% 
  dplyr::filter(Season %in% c("Winter"))

pops <- c(1,3,7,8,9,13)
#pops <- c(1,3,8,9)

covs.pop <- covs.sel %>% 
  dplyr::filter(Population %in% pops)

#For backtransforming
att.loss <- attributes(scale(covs$loss.10))
att.length <- attributes(scale(covs$length.10))
att.light <- attributes(scale(covs$light.10))
att.drought <- attributes(scale(covs$drought.10))

#2. Visualize----
#Linear: light, pest, length, crops,
#Unsure: loss
#quad: drought

ggplot(covs.breed, aes(x=drought.scale, y=Response, colour=Population)) +
  geom_jitter() +
  geom_smooth()

ggplot(covs.sel, aes(x=drought.scale, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Season), scales="free_x")

ggplot(covs.sel, aes(x=loss.scale, y=Response)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~factor(Season), scales="free_x")

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
mod.season.global <- lmer(Response ~ loss.scale*factor(Season)*Population +
                            length.scale*factor(Season)*Population +
                            drought.scale*factor(Season)*Population +
                            light.scale*factor(Season)*Population +
                            (1|PinpointID),
                          data=covs.pop,
                          na.action="na.fail")
mod.season.dredge <- dredge(mod.season.global, trace=2)
mod.season.dredge

mod.season <- lmer(Response ~ loss.scale*factor(Season2) +
                     length.scale*factor(Season2)+
                     poly(drought.scale, 2)*factor(Season2) +
                     (1|PinpointID),
                   data=covs.sel,
                   na.action="na.fail")
summary(mod.season)
dredge(mod.season) #check

#4. Seasonal predictions----
#New data
newdat.loss <- data.frame(expand.grid(loss.scale = seq(min(covs.sel$loss.scale), max(covs.sel$loss.scale), 0.1),
                                        length.scale = mean(covs.sel$length.scale),
                                        drought.scale = mean(covs.sel$drought.scale),
                                        PinpointID = unique(covs.sel$PinpointID),
                                        Season2=unique(covs.sel$Season2)))

newdat.length <- data.frame(expand.grid(loss.scale = mean(covs.sel$loss.scale),
                                        length.scale = seq(min(covs.sel$length.scale), max(covs.sel$length.scale), 0.1),
                                        drought.scale = mean(covs.sel$drought.scale),
                                        PinpointID = unique(covs.sel$PinpointID),
                                        Season2=unique(covs.sel$Season2)))

newdat.drought <- data.frame(expand.grid(loss.scale = mean(covs.sel$loss.scale),
                                         length.scale = mean(covs.sel$length.scale),
                                         drought.scale = seq(min(covs.sel$drought.scale), max(covs.sel$drought.scale), 0.1),
                                         PinpointID = unique(covs.sel$PinpointID),
                                         Season2=unique(covs.sel$Season2)))

#Predict
pred.loss <- predictInterval(mod.season, newdata = newdat.loss, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.loss) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population) %>% 
              unique()) %>% 
  dplyr::filter((Season2=="Breed" & loss.scale > min(covs.breed$loss.scale) & loss.scale < max(covs.breed$loss.scale))|
                  (Season2=="Winter" & loss.scale > min(covs.winter$loss.scale) & loss.scale < max(covs.winter$loss.scale))) %>% 
  mutate(loss=loss.scale*att.loss$`scaled:scale` + att.loss$`scaled:center`) %>% 
  rename(cov = loss) %>% 
  mutate(model="loss") %>% 
  dplyr::select(model, cov, Season2, PinpointID, fit, upr, lwr)

pred.length <- predictInterval(mod.season, newdata = newdat.length, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.length) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population) %>% 
              unique()) %>% 
  dplyr::filter((Season2=="Breed" & length.scale > min(covs.breed$length.scale) & length.scale < max(covs.breed$length.scale))|
                  (Season2=="Winter" & length.scale > min(covs.winter$length.scale) & length.scale < max(covs.winter$length.scale))) %>% 
  mutate(length=length.scale*att.length$`scaled:scale` + att.length$`scaled:center`) %>% 
  rename(cov = length) %>% 
  mutate(model="length") %>% 
  dplyr::select(model, cov, Season2, PinpointID, fit, upr, lwr)

pred.drought <- predictInterval(mod.season, newdata = newdat.drought, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.drought) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population) %>% 
              unique()) %>% 
  dplyr::filter((Season2=="Breed" & drought.scale > min(covs.breed$drought.scale) & drought.scale < max(covs.breed$drought.scale))|
                  (Season2=="Winter" & drought.scale > min(covs.winter$drought.scale) & drought.scale < max(covs.winter$drought.scale))) %>% 
  mutate(drought=drought.scale*att.drought$`scaled:scale` + att.drought$`scaled:center`) %>% 
  rename(cov = drought) %>% 
  mutate(model="drought") %>% 
  dplyr::select(model, cov, Season2, PinpointID, fit, upr, lwr)

#Put together
pred <- rbind(pred.loss, pred.length, pred.drought)

write.csv(pred, "RSFPredictions_Season.csv", row.names = FALSE)

#Visualize
ggplot(pred) +
  geom_line(aes(x=cov, y=fit, colour=factor(Season2))) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(Season2)), alpha=0.3) +
  facet_wrap(~model, scales="free_x")

#5. Population model----
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
  dplyr::filter(Season=="Breed")

covs.pop.winter <- covs.pop %>% 
  dplyr::filter(Season=="Winter")

#5a. Breeding----
mod.breed.global <- lmer(Response ~ loss.scale*factor(Population) +
                           length.scale*factor(Population) +
                           poly(drought.scale, 2)*factor(Population) +
                           light.scale*factor(Population) +
                           (1|PinpointID),
                         data=covs.pop.breed,
                         na.action="na.fail")

mod.breed.dredge <- dredge(mod.breed.global, trace=2)
mod.breed.dredge

mod.breed <- lmer(Response ~ loss.scale*factor(Population) +
                    length.scale*factor(Population) +
                    poly(drought.scale, 2)*factor(Population) +
                    (1|PinpointID),
                  data=covs.pop.breed,
                  na.action="na.fail")
dredge(mod.breed)

#5b. Wintering selection----
mod.winter.global <- lmer(Response ~ loss.scale*factor(Population) +
                            length.scale*factor(Population) +
                            poly(drought.scale, 2)*factor(Population) +
                            light.scale*factor(Population) +
                            (1|PinpointID),
                           data=covs.pop.winter,
                           na.action="na.fail")

mod.winter.dredge <- dredge(mod.winter.global, trace=2)
mod.winter.dredge

mod.winter <- lmer(Response ~ loss.scale +
                     poly(drought.scale, 2)*factor(Population) +
                     light.scale*factor(Population) +
                    (1|PinpointID),
                  data=covs.pop.winter,
                  na.action="na.fail")
dredge(mod.winter)

#6. Population predictions----
#6a. Breeding----
#New data
newdat.loss <- data.frame()
newdat.length <- data.frame()
newdat.drought <- data.frame()
for(i in 1:length(pops)){
  covs.pop.breed.i <- covs.pop.breed %>% 
    dplyr::filter(Population==pops[i])
  
  newdat.loss.i <- data.frame(expand.grid(loss.scale = seq(min(covs.pop.breed.i$loss.scale),
                                                               max(covs.pop.breed.i$loss.scale), 0.1),
                                          length.scale = mean(covs.pop.breed.i$length.scale),
                                          drought.scale = mean(covs.pop.breed.i$drought.scale),
                                          PinpointID = unique(covs.pop.breed.i$PinpointID),
                                          Population=pops[i]))
  
  newdat.length.i <- data.frame(expand.grid(loss.scale = mean(covs.pop.breed.i$loss.scale),
                                          length.scale = seq(min(covs.pop.breed.i$length.scale),
                                                             max(covs.pop.breed.i$length.scale), 0.1),
                                          drought.scale = mean(covs.pop.breed.i$drought.scale),
                                          PinpointID = unique(covs.pop.breed.i$PinpointID),
                                          Population=pops[i]))
  
  newdat.drought.i <- data.frame(expand.grid(loss.scale = mean(covs.pop.breed.i$loss.scale),
                                           length.scale = mean(covs.pop.breed.i$length.scale),
                                           drought.scale = seq(min(covs.pop.breed.i$drought.scale),
                                                               max(covs.pop.breed.i$drought.scale), 0.1),
                                           PinpointID = unique(covs.pop.breed.i$PinpointID),
                                           Population=pops[i]))
  
  newdat.loss <- rbind(newdat.loss, newdat.loss.i)
  newdat.length <- rbind(newdat.length, newdat.length.i)
  newdat.drought <- rbind(newdat.drought, newdat.drought.i)
  
}


#Predict
pred.loss.breed <- predictInterval(mod.breed, newdata = newdat.loss, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.loss) %>% 
  mutate(loss=loss.scale*att.loss$`scaled:scale` + att.loss$`scaled:center`) %>% 
  rename(cov = loss) %>% 
  mutate(model="loss",
         season="breed") %>% 
  dplyr::select(model, season, cov, Population, PinpointID, fit, upr, lwr)

pred.length.breed <- predictInterval(mod.breed, newdata = newdat.length, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.length) %>% 
  mutate(length=length.scale*att.length$`scaled:scale` + att.length$`scaled:center`) %>% 
  rename(cov = length) %>% 
  mutate(model="length",
         season="breed") %>% 
  dplyr::select(model, season, cov, Population, PinpointID, fit, upr, lwr)

pred.drought.breed <- predictInterval(mod.breed, newdata = newdat.drought, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.drought) %>% 
  mutate(drought=drought.scale*att.drought$`scaled:scale` + att.drought$`scaled:center`) %>% 
  rename(cov = drought) %>% 
  mutate(model="drought",
         season="breed") %>% 
  dplyr::select(model, season, cov, Population, PinpointID, fit, upr, lwr)

#6b. Wintering----
#New data
newdat.loss <- data.frame()
newdat.drought <- data.frame()
newdat.light <- data.frame()
for(i in 1:length(pops)){
  covs.pop.winter.i <- covs.pop.winter %>% 
    dplyr::filter(Population==pops[i])
  
  newdat.loss.i <- data.frame(expand.grid(loss.scale = seq(min(covs.pop.winter.i$loss.scale),
                                                               max(covs.pop.winter.i$loss.scale), 0.1),
                                            length.scale = mean(covs.pop.winter.i$length.scale),
                                            light.scale = mean(covs.pop.winter.i$light.scale),
                                            drought.scale = mean(covs.pop.winter.i$drought.scale),
                                            PinpointID = unique(covs.pop.winter.i$PinpointID),
                                            Population=pops[i]))
  
  newdat.drought.i <- data.frame(expand.grid(loss.scale = mean(covs.pop.winter.i$loss.scale),
                                             length.scale = mean(covs.pop.winter.i$length.scale),
                                             light.scale = mean(covs.pop.winter.i$light.scale),
                                             drought.scale = seq(min(covs.pop.winter.i$drought.scale),
                                                                 max(covs.pop.winter.i$drought.scale), 0.1),
                                             PinpointID = unique(covs.pop.winter.i$PinpointID),
                                             Population=pops[i]))
  
  newdat.light.i <- data.frame(expand.grid(loss.scale = mean(covs.pop.winter.i$loss.scale),
                                             length.scale = mean(covs.pop.winter.i$length.scale),
                                             light.scale = seq(min(covs.pop.winter.i$light.scale),
                                                                 max(covs.pop.winter.i$light.scale), 0.1),
                                           drought.scale = mean(covs.pop.winter.i$drought.scale),
                                             PinpointID = unique(covs.pop.winter.i$PinpointID),
                                             Population=pops[i]))
  
  newdat.loss <- rbind(newdat.loss, newdat.loss.i)
  newdat.drought <- rbind(newdat.drought, newdat.drought.i)
  newdat.light <- rbind(newdat.light, newdat.light.i)
  
}

#Predict
pred.loss.winter <- predictInterval(mod.winter, newdata = newdat.loss, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.loss) %>% 
  mutate(loss=loss.scale*att.loss$`scaled:scale` + att.loss$`scaled:center`) %>% 
  rename(cov = loss) %>% 
  mutate(model="loss",
         season="winter") %>% 
  dplyr::select(model, season, cov, Population, PinpointID, fit, upr, lwr)

pred.drought.winter <- predictInterval(mod.winter, newdata = newdat.drought, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.drought) %>% 
  mutate(drought=drought.scale*att.drought$`scaled:scale` + att.drought$`scaled:center`) %>% 
  rename(cov = drought) %>% 
  mutate(model="drought",
         season="winter") %>% 
  dplyr::select(model, season, cov, Population, PinpointID, fit, upr, lwr)

pred.light.winter <- predictInterval(mod.winter, newdata = newdat.light, which="fixed", n.sims = 1000, level=0.95) %>% 
  cbind(newdat.light) %>% 
  mutate(light=light.scale*att.light$`scaled:scale` + att.light$`scaled:center`) %>% 
  rename(cov = light) %>% 
  mutate(model="light",
         season="winter") %>% 
  dplyr::select(model, season, cov, Population, PinpointID, fit, upr, lwr)

#6c. Put together----
pred <- rbind(pred.loss.breed, pred.length.breed, pred.drought.breed,
              pred.loss.winter, pred.drought.winter)

write.csv(pred, "RSFPredictions_Population.csv", row.names = FALSE)

#6d. Visualize----
ggplot(pred.light.winter) +
  geom_line(aes(x=cov, y=fit, colour=factor(Population))) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(Population)), alpha=0.3) +
  facet_wrap(season~model, scales="free")
