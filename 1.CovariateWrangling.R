library(tidyverse)
library(readxl)

#OTHER LAYERS THAT I COULD ADD:
#LIGHT POLLUTION
#ROADS
#EVI
#PESTICIDES?

#1. Read in other datasets----
dat <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/CONIMCP_CleanDataAll.csv")

pop <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_population_abundance.csv")

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population, Sex) %>% 
  dplyr::filter(PinpointID != -99) %>% 
  left_join(pop %>% 
              dplyr::select(Population, Abbreviation))

#2. Wrangle forest loss data from GEE----
forest200 <- read.csv("Covariates/ForestLoss_200.csv") %>% 
  mutate(buffer="200m")
forest2000 <- read.csv("Covariates/ForestLoss_2000.csv") %>% 
  mutate(buffer="2km")
forest20000 <- read.csv("Covariates/ForestLoss_20000.csv") %>% 
  mutate(buffer="20km")

forest <- rbind(forest200, forest2000, forest20000) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, loss, period, treecover2000, buffer) %>% 
  mutate(treecover2020 = treecover2000*(1-loss)) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number(),
         loss=100*loss,
         treecover=100*treecover2000,
         treecover2020=110*treecover2020) 

#3. Wrangle human modification data from GEE----
hm200 <- read.csv("Covariates/HumanModification_200.csv") %>% 
  mutate(buffer="200m")
hm2000 <- read.csv("Covariates/HumanModification_2000.csv") %>% 
  mutate(buffer="2km")
hm20000 <- read.csv("Covariates/HumanModification_20000.csv") %>% 
  mutate(buffer="20km")

hm <- rbind(hm200, hm2000, hm20000) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, mean, buffer) %>% 
  rename(hm = mean) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number(),
         hm=100*hm)

#4. Wrangle Copernicus landcover from GEE----
lc200 <- read.csv("Covariates/Landcover_200.csv") %>% 
  mutate(buffer="200m")
lc2000 <- read.csv("Covariates/Landcover_2000.csv") %>% 
  mutate(buffer="2km")
lc20000 <- read.csv("Covariates/Landcover_20000.csv") %>% 
  mutate(buffer="20km")

lc <- rbind(lc200, lc2000, lc20000) %>% 
  rename_with(~gsub(pattern=".coverfraction", replacement = "", .x, fixed=TRUE), ends_with(".coverfraction")) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, bare, crops, grass, moss, shrub, snow, tree, urban, water.permanent, water.seasonal, buffer) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number()) 

#5. Put all together and add other metadata for individuals----
covs <- full_join(forest, hm) %>% 
  full_join(lc) %>% 
  left_join(band) %>% 
  left_join(dat %>% 
              dplyr::select(PinpointID, Winter) %>% 
              unique())
  
write.csv(covs, "Covariates_Breed&Winter.csv", row.names=FALSE)

