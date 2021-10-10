library(tidyverse)
library(readxl)
library(lubridate)
library(suncalc)

#1. Read in other datasets----
dat <- read.csv("CONIMCP_CleanDataAll_Habitat_Roosting_3857.csv")

pop <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_population_abundance.csv")

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population, Sex) %>% 
  dplyr::filter(PinpointID != -99) %>% 
  left_join(pop %>% 
              dplyr::select(Population, Abbreviation))

#2. Wrangle human modification data from GEE----
hm1 <- read.csv("Covariates/HumanModification_1km.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(hm.1 = mean)
hm10 <- read.csv("Covariates/HumanModification_10km.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(hm.10 = mean)
hmpt <- read.csv("Covariates/HumanModification_point.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(hm.pt = mean)

hm <- hm1 %>% 
  left_join(hm10) %>% 
  left_join(hmpt)

#4. Wrangle Copernicus landcover from GEE----
names <- read.csv("Covariates/Landcover_ClassNames.csv")

lc1 <- read.csv("Covariates/Landcover_1km.csv") %>% 
  dplyr::select(-system.index, -ID, -.geo) %>% 
  rename_with(.cols = c("bare.coverfraction":"water.seasonal.coverfraction"),
              ~ paste0(., ".1"))
lc10 <- read.csv("Covariates/Landcover_10km.csv") %>% 
  dplyr::select(-system.index, -ID, -.geo) %>% 
  rename_with(.cols = c("bare.coverfraction":"water.seasonal.coverfraction"),
              ~ paste0(., ".10"))
lcpt <- read.csv("Covariates/Landcover_point.csv") %>% 
  dplyr::select(-system.index, -ID, -.geo) %>% 
  rename(lc = first)

lc <- lc1 %>% 
  left_join(lc10) %>% 
  left_join(lcpt)

#5. Wrangle light pollution from GEE----
light1 <- read.csv("Covariates/LightPollution_1km.csv") %>% 
  rename(stable.lights.1 = F182013_stable_lights) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, stable.lights.1)
light10 <- read.csv("Covariates/LightPollution_10km.csv") %>% 
  rename(stable.lights.10 = F182013_stable_lights) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, stable.lights.10)
lightpt <- read.csv("Covariates/LightPollution_point.csv") %>% 
  rename(stable.lights.pt = F182013_stable_lights) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, stable.lights.pt)

light <- light1 %>% 
  left_join(light10) %>% 
  left_join(lightpt)

#6. Wrangle drought data from GEE----
drought1 <- read.csv("Covariates/Drought_1km.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(drought.1 = mean)
drought10 <- read.csv("Covariates/Drought_10km.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(drought.10 = mean)
droughtpt <- read.csv("Covariates/Drought_point.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(drought.pt = mean)

drought <- drought1 %>% 
  left_join(drought10) %>% 
  left_join(droughtpt)

#7. Wrangle forest loss from GEE----
forest1 <- read.csv("Covariates/ForestLoss_1km.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, X0_loss) %>% 
  rename(loss.1 = X0_loss)
forest10 <- read.csv("Covariates/ForestLoss_10km.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, X0_loss) %>% 
  rename(loss.10 = X0_loss)
forestpt <- read.csv("Covariates/ForestLoss_point.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, X0_loss) %>% 
  rename(loss.pt = X0_loss)

forest <- forest1 %>% 
  left_join(forest10) %>% 
  left_join(forestpt)

#8. Read in data from other sources----
pts.covs <- read.csv("ExtractedCovariates.csv") %>% 
  mutate_all(~ifelse(.=="", NA, .)) %>% 
  arrange(X, Y, PinpointID, Season, DateTime, Type, Radius) %>% 
  mutate(ID = row_number()) %>% 
  dplyr::select(-PinpointID, -Season, -DateTime, -Type, -Radius, -X, -Y)

#9. Put all together----
covs <- hm %>% 
  left_join(lc) %>% 
  left_join(light) %>% 
  left_join(drought) %>% 
  left_join(forest) %>% 
  mutate_all(~ifelse(.=="", NA, .)) %>% 
  arrange(X, Y, PinpointID, Season, DateTime, Type, Radius) %>% 
  mutate(ID = row_number()) %>% 
  left_join(pts.covs) %>% 
  dplyr::select(-discrete_classification.1, -discrete_classification.proba.1, -data.density.indicator.1, -forest_type.1, -snow.coverfraction.1, -discrete_classification.10, -discrete_classification.proba.10, -data.density.indicator.10, -forest_type.10, -snow.coverfraction.10, -ID)
summary(covs)

#10. Add metadata----
meta <- read.csv("CONIMCP_CleanDataAll_Habitat_Roosting.csv") %>% 
  dplyr::select(PinpointID, Population, Mass, Wing, Sex) %>% 
  unique()

winter <- read.csv("CONIMCP_CleanDataAll_Habitat_Roosting.csv") %>% 
  dplyr::select(Winter, PinpointID, DateTime, Lat, Long) %>% 
  unique()

covs.final <- covs %>% 
  left_join(meta) %>% 
  left_join(winter)
  
write.csv(covs.final, "Covariates_Breed&Winter_Roosting.csv", row.names=FALSE)

