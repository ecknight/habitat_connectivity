library(tidyverse)
library(usdm)
library(corrplot)

options(scipen = 999)

#1. Wrangling----
covs <- read.csv("Covariates_Breed&Winter_Roosting.csv") %>% 
  rename_all(~gsub(., pattern=".coverfraction", replacement="")) %>% 
  rename_all(~gsub(., pattern="stable.lights", replacement="light")) %>% 
  rename_all(~gsub(., pattern="change.confidence", replacement="change"))
str(covs)

#2. All variables---
covs.vif <- covs %>% 
  dplyr::select(-PinpointID, -Season, -DateTime, -Type, -Radius, -X, -Y, -lc)

M <- cor(covs.vif, use="complete.obs")
corrplot(M)
#Ok let's split out into scale

#3. Point level----
covs.pt <- covs %>% 
  dplyr::select(hm.pt, light.pt, drought.pt, pest.pt)

M.pt <- cor(covs.pt, use="complete.obs")
M.pt
corrplot(M.pt)
#Lights & hm are at .70 correlation

vif(covs.pt)
#But everything ok

#4. 1km radius----
covs.1 <- covs %>% 
  dplyr::select(hm.1, bare.1, crops.1, grass.1, moss.1, shrub.1, tree.1, urban.1, water.permanent.1, water.seasonal.1, light.1, drought.1, Length.1, pest.1, change.1)

M.1 <- cor(covs.1, use="complete.obs")
M.1
corrplot(M.1)
#Grass & tree -.80
#Hm & lights 0.71

vif(covs.1)
#Should probably take out tree & hm
#Also take out moss & urban, not really relevant

covs.1 <- covs %>% 
  dplyr::select(bare.1, crops.1, grass.1, shrub.1, water.permanent.1, water.seasonal.1, light.1, drought.1, Length.1, pest.1, change.1)

M.1 <- cor(covs.1, use="complete.obs")
M.1
corrplot(M.1)
#Hm & stable lights 0.72

vif(covs.1)

#5. 10km radius----
covs.10 <- covs %>% 
  dplyr::select(hm.10, bare.10, crops.10, grass.10, moss.10, shrub.10, tree.10, urban.10, water.permanent.10, water.seasonal.10, light.10, drought.10, Length.10, pest.10, change.10)

M.10 <- cor(covs.10, use="complete.obs")
M.10
corrplot(M.10)
#Grass & tree -.81
#Hm & lights 0.71

vif(covs.10)
#Should probably take out tree & hm
#Also take out moss & urban, not really relevant

covs.10 <- covs %>% 
  dplyr::select(bare.10, crops.10, grass.10, shrub.10, water.permanent.10, water.seasonal.10, light.10, drought.10, Length.10, pest.10, change.10)

M.10 <- cor(covs.10, use="complete.obs")
M.10
corrplot(M.10)

vif(covs.10)
#good

#6. Save out to one dataframe----
covs.vif <- covs %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y,
                lc, hm.pt, light.pt, drought.pt, pest.pt, 
                bare.1, crops.1, grass.1, shrub.1, water.permanent.1, water.seasonal.1, light.1, drought.1, Length.1, pest.1, change.1, urban.1, moss.1, tree.1, hm.1,
                bare.10, crops.10, grass.10, shrub.10, water.permanent.10, water.seasonal.10, light.10, drought.10, Length.10, pest.10, change.10, urban.10, moss.10, tree.10, hm.10)

write.csv(covs.vif, "Covariates_Breed&Winter_VIF.csv", row.names=FALSE)
