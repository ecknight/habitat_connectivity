library(tidyverse)
library(usdm)
library(corrplot)

options(scipen = 999)

#1. Wrangling----
covs <- read.csv("Covariates_Breed&Winter.csv") %>% 
  rename_all(~gsub(., pattern=".coverfraction", replacement=""))
str(covs)

#2. All variables---
covs.vif <- covs %>% 
  dplyr::select(-PinpointID, -Season, -DateTime, -Type, -Radius, -X, -Y, -lc)

M <- cor(covs.vif, use="complete.obs")
corrplot(M)
#Ok let's split out into scale

#3. Point level----
covs.pt <- covs %>% 
  dplyr::select(hm.pt, stable.lights.pt, drought.pt, pest.pt)

M.pt <- cor(covs.pt, use="complete.obs")
M.pt
corrplot(M.pt)
#Lights & hm are at .71 correlation

vif(covs.pt)
#But everything ok

covs.pt <- covs %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, lc, hm.pt, stable.lights.pt, drought.pt, pest.pt)

write.csv(covs.pt, "Covariates_Breed&Winter_Point.csv", row.names=FALSE)

#4. 500m radius----
covs.5 <- covs %>% 
  dplyr::select(hm.5, bare.5, crops.5, grass.5, moss.5, shrub.5, tree.5, urban.5, water.permanent.5, water.seasonal.5, stable.lights.5, drought.5, Length.5, pest.5)

M.5 <- cor(covs.5, use="complete.obs")
M.5
corrplot(M.5)
#Grass & tree -.79
#Hm & stable lights 0.72
#Hm & urban 0.77

vif(covs.5)
#Should probably take out tree & urban...
#Also take out moss, not really relevant

covs.5 <- covs %>% 
  dplyr::select(hm.5, bare.5, crops.5, grass.5, shrub.5, water.permanent.5, water.seasonal.5, stable.lights.5, drought.5, Length.5, pest.5)

M.5 <- cor(covs.5, use="complete.obs")
M.5
corrplot(M.5)
#Hm & stable lights 0.72

vif(covs.5)
#Let's take out hm anyway

covs.5 <- covs %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, bare.5, crops.5, grass.5, shrub.5, water.permanent.5, water.seasonal.5, stable.lights.5, drought.5, Length.5, pest.5)

write.csv(covs.5, "Covariates_Breed&Winter_500.csv", row.names=FALSE)

#5. 20km radius----
covs.200 <- covs %>% 
  dplyr::select(hm.200, bare.200, crops.200, grass.200, moss.200, shrub.200, tree.200, urban.200, water.permanent.200, water.seasonal.200, stable.lights.200, drought.200, Length.200, pest.200)

M.200 <- cor(covs.200, use="complete.obs")
M.200
corrplot(M.200)
#Grass & tree -.81
#Hm & stable lights 0.71
#Hm and roads 0.77

vif(covs.200)
#Should probably take out tree & urban...
#Also take out moss, not really relevant

covs.200 <- covs %>% 
  dplyr::select(hm.200, bare.200, crops.200, grass.200, shrub.200, water.permanent.200, water.seasonal.200, stable.lights.200, drought.200, Length.200, pest.200)

M.200 <- cor(covs.200, use="complete.obs")
M.200
corrplot(M.200)

vif(covs.200)
#Also HM

covs.200 <- covs %>% 
  dplyr::select(bare.200, crops.200, grass.200, shrub.200, water.permanent.200, water.seasonal.200, stable.lights.200, drought.200, Length.200, pest.200)

M.200 <- cor(covs.200, use="complete.obs")
M.200
corrplot(M.200)

vif(covs.200)
#good

covs.200 <- covs %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, bare.200, crops.200, grass.200, moss.200, shrub.200, urban.200, water.permanent.200, water.seasonal.200, stable.lights.200, drought.200, Length.200, pest.200)

write.csv(covs.200, "Covariates_Breed&Winter_20000.csv", row.names=FALSE)
