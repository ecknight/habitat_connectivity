library(tidyverse)
library(readxl)
library(lubridate)
library(suncalc)

#1. Read in other datasets----
dat <- read.csv("CONIMCP_CleanDataAll_Habitat_3857.csv")

pop <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_population_abundance.csv")

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population, Sex) %>% 
  dplyr::filter(PinpointID != -99) %>% 
  left_join(pop %>% 
              dplyr::select(Population, Abbreviation))

#2. Wrangle human modification data from GEE----
hm5 <- read.csv("Covariates/HumanModification_500.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(hm.5 = mean)
hm200 <- read.csv("Covariates/HumanModification_20000.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(hm.200 = mean)
hmpt <- read.csv("Covariates/HumanModification_point.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(hm.pt = mean)

hm <- hm5 %>% 
  left_join(hm200) %>% 
  left_join(hmpt)

#4. Wrangle Copernicus landcover from GEE----
names <- read.csv("Covariates/Landcover_ClassNames.csv")

lc5 <- read.csv("Covariates/Landcover_500.csv") %>% 
  dplyr::select(-system.index, -ID, -.geo) %>% 
  rename_with(.cols = c("bare.coverfraction":"water.seasonal.coverfraction"),
              ~ paste0(., ".5"))
lc200 <- read.csv("Covariates/Landcover_20000.csv") %>% 
  dplyr::select(-system.index, -ID, -.geo) %>% 
  rename_with(.cols = c("bare.coverfraction":"water.seasonal.coverfraction"),
              ~ paste0(., ".200"))
lcpt <- read.csv("Covariates/Landcover_point.csv") %>% 
  dplyr::select(-system.index, -ID, -.geo) %>% 
  rename(lc = first)

lc <- lc5 %>% 
  left_join(lc200) %>% 
  left_join(lcpt)

#5. Wrangle light pollution from GEE----
light5 <- read.csv("Covariates/LightPollution_500.csv") %>% 
  rename(stable.lights.5 = F182013_stable_lights) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, stable.lights.5)
light200 <- read.csv("Covariates/LightPollution_20000.csv") %>% 
  rename(stable.lights.200 = F182013_stable_lights) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, stable.lights.200)
lightpt <- read.csv("Covariates/LightPollution_point.csv") %>% 
  rename(stable.lights.pt = F182013_stable_lights) %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, stable.lights.pt)

light <- light5 %>% 
  left_join(light200) %>% 
  left_join(lightpt)

#6. Wrangle drought data from GEE----
drought5 <- read.csv("Covariates/Drought_500.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(drought.5 = mean)
drought200 <- read.csv("Covariates/Drought_20000.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(drought.200 = mean)
droughtpt <- read.csv("Covariates/Drought_point.csv") %>% 
  dplyr::select(PinpointID, Season, DateTime, Type, Radius, X, Y, mean) %>% 
  rename(drought.pt = mean)

drought <- drought5 %>% 
  left_join(drought200) %>% 
  left_join(droughtpt)

#7. Read in data from other sources----
pts.covs <- read.csv("ExtractedCovariates.csv") %>% 
  mutate_all(~ifelse(.=="", NA, .)) %>% 
  arrange(X, Y, PinpointID, Season, DateTime, Type, Radius) %>% 
  mutate(ID = row_number()) %>% 
  dplyr::select(-PinpointID, -Season, -DateTime, -Type, -Radius, -X, -Y)

#8. Put all together and add other metadata for individuals----
covs <- hm %>% 
  left_join(lc) %>% 
  left_join(light) %>% 
  left_join(drought) %>% 
  mutate_all(~ifelse(.=="", NA, .)) %>% 
  arrange(X, Y, PinpointID, Season, DateTime, Type, Radius) %>% 
  mutate(ID = row_number()) %>% 
  left_join(pts.covs)
summary(covs)
  
write.csv(covs, "Covariates_Breed&Winter.csv", row.names=FALSE)

