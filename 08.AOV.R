library(tidyverse)
library(lme4)
library(AICcmodavg)
library(data.table)

#1. Wrangling----
covs <- read.csv("Covariates_Breed&Winter_Roosting.csv") %>% 
  rename_all(~gsub(., pattern=".coverfraction", replacement="")) %>% 
  rename_all(~gsub(., pattern="stable.lights", replacement="light")) %>% 
  rename_all(~gsub(., pattern="change.confidence", replacement="change")) %>% 
  rename(length.1 = Length.1, length.10 = Length.10) %>% 
  mutate(Population = as.factor(Population)) %>% 
  dplyr::filter(Type %in% c("Used"))

covs.breed <- covs %>% 
  dplyr::filter(Season %in% c("Breed")) %>% 
  mutate(crops.scale = scale(crops.10+ 0.00001, center=FALSE),
         loss.scale = scale(loss.10+ 0.00001, center=FALSE),
         drought.scale = scale(drought.10+ 0.00001, center=FALSE),
         length.scale = scale(length.10+ 0.00001, center=FALSE),
         pest.scale = scale(pest.10+ 0.00001, center=FALSE),
         hm.scale = scale(hm.10+ 0.00001, center=FALSE),
         light.scale = scale(light.10+ 0.00001, center=FALSE)) %>% 
  dplyr::select(crops.scale, loss.scale, drought.scale, length.scale, pest.scale, hm.scale, light.scale,
#                crops.10, loss.10, drought.10, length.10, pest.10, hm.10, light.10,
                Population, PinpointID, Season, Type, Sex, Wing, Mass) %>% 
  rename_all(~gsub(.,pattern=".10", replacement=""))

covs.winter1 <- covs %>% 
  dplyr::filter(Season %in% c("Winter") & Winter==1) %>% 
  mutate(crops.scale = scale(crops.1+ 0.00001, center=FALSE),
         loss.scale = scale(loss.1+ 0.00001, center=FALSE),
         drought.scale = scale(drought.1+ 0.00001, center=FALSE),
         length.scale = scale(length.1+ 0.00001, center=FALSE),
         pest.scale = scale(pest.1+ 0.00001, center=FALSE),
         hm.scale = scale(hm.1+ 0.00001, center=FALSE),
         light.scale = scale(light.1+ 0.00001, center=FALSE)) %>% 
  dplyr::select(crops.scale, loss.scale, drought.scale, length.scale, pest.scale, hm.scale, light.scale,
#                crops.1, loss.1, drought.1, length.1, pest.1, hm.1, light.1,
                Population, PinpointID, Season, Type, Sex, Wing, Mass) %>% 
  rename_all(~gsub(.,pattern=".1", replacement="")) 

covs.winter2 <- covs %>% 
  dplyr::filter(Season %in% c("Winter") & Winter==2) %>% 
  mutate(crops.scale = scale(crops.1+ 0.00001, center=FALSE),
         loss.scale = scale(loss.1+ 0.00001, center=FALSE),
         drought.scale = scale(drought.1+ 0.00001, center=FALSE),
         length.scale = scale(length.1+ 0.00001, center=FALSE),
         pest.scale = scale(pest.1+ 0.00001, center=FALSE),
         hm.scale = scale(hm.1+ 0.00001, center=FALSE),
         light.scale = scale(light.1+ 0.00001, center=FALSE)) %>% 
  dplyr::select(crops.scale, loss.scale, drought.scale, length.scale, pest.scale, hm.scale, light.scale,
#                crops.1, loss.1, drought.1, length.1, pest.1, hm.1, light.1,
                Population, PinpointID, Season, Type, Sex, Wing, Mass) %>% 
  rename_all(~gsub(.,pattern=".1", replacement="")) %>% 
  rbind(covs.winter1 %>% 
          dplyr::filter(!PinpointID %in% covs.id))

covs.all.scale <- data.frame(rbind(covs.breed, covs.winter1)) %>% 
  pivot_longer(cols=crops.scale:light.scale, names_to="variable", values_to="value")

#2. Visualize----
ggplot(covs.all.scale) +
  geom_boxplot(aes(x=Population, y=value, colour=variable)) +
  facet_wrap(~Season, scales="free")

#2. Test for differences----
combos <- expand.grid(variable = unique(covs.all.scale$variable), season = unique(covs.all.scale$Season))

aic.list <- data.frame()
for(i in 1:nrow(combos)){
  
  covs.i <- covs.all.scale %>% 
    dplyr::filter(variable==combos$variable[i],
                  Season==combos$season[i])
  
  mod.pop <- lmer(value ~ factor(Population) + (1|PinpointID), data=covs.i, REML=FALSE)
  mod.null <- lmer(value ~ 1 + (1|PinpointID), data=covs.i, REML=FALSE)
  aic.list <- data.frame(aictab(list(population=mod.pop, null=mod.null))) %>% 
    mutate(variable=combos$variable[i],
           Season=combos$season[i]) %>% 
    rbind(aic.list)
  
}

aic.select <- aic.list %>% 
  group_by(variable, Season) %>% 
  mutate(max = max(AICcWt),
         select = ifelse(max==AICcWt, 1, 0)) %>% 
  dplyr::select(variable, Season, Delta_AICc, Modnames, select)

aic.table <- aic.list %>% 
  dplyr::select(Season, variable, Modnames, K, AICc, Delta_AICc, AICcWt)

write.csv(aic.table, "ThreatVariableAICResults.csv", row.names = FALSE)
