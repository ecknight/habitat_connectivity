library(tidyverse)
library(readxl)

#OTHER LAYERS THAT I COULD ADD:
#LIGHT POLLUTION
#ROADS
#EVI
#PESTICIDES?

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population) %>% 
  dplyr::filter(PinpointID != -99)

forest200 <- read.csv("Covariates/ForestLoss_200.csv") %>% 
  mutate(buffer="200m")
forest2000 <- read.csv("Covariates/ForestLoss_2000.csv") %>% 
  mutate(buffer="2km")
forest20000 <- read.csv("Covariates/ForestLoss_20000.csv") %>% 
  mutate(buffer="20km")

forest <- rbind(forest200, forest2000, forest20000) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, loss, period, treecover2000, buffer) %>% 
  mutate(treecover2020 = treecover2000*(1-loss)) %>% 
  left_join(band) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number())

hm200 <- read.csv("Covariates/HumanModification_200.csv") %>% 
  mutate(buffer="200m")
hm2000 <- read.csv("Covariates/HumanModification_2000.csv") %>% 
  mutate(buffer="2km")
hm20000 <- read.csv("Covariates/HumanModification_20000.csv") %>% 
  mutate(buffer="20km")

hm <- rbind(hm200, hm2000, hm20000) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, mean, buffer) %>% 
  rename(hm = mean) %>% 
  left_join(band) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number())

ggplot(hm, aes(x=Season, y=hm, colour=factor(PinpointID))) +
  geom_point() +
  facet_grid(buffer~Population)

lc200 <- read.csv("Covariates/Landcover_200.csv") %>% 
  mutate(buffer="200m")
lc2000 <- read.csv("Covariates/Landcover_2000.csv") %>% 
  mutate(buffer="2km")
lc20000 <- read.csv("Covariates/Landcover_20000.csv") %>% 
  mutate(buffer="20km")

lc <- rbind(lc200, lc2000, lc20000) %>% 
  rename_with(~gsub(pattern=".coverfraction", replacement = "", .x, fixed=TRUE), ends_with(".coverfraction")) %>% 
  dplyr::select(PinpointID, Season2, DateTime, Lat, Long, bare, crops, grass, moss, shrub, snow, tree, urban, water.permanent, water.seasonal, buffer) %>% 
  left_join(band) %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2")) %>% 
  arrange(PinpointID, DateTime) %>% 
  mutate(Season = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", "Winter"),
         ID = row_number())

ggplot(lc, aes(x=Season, y=water.seasonal, colour=factor(PinpointID))) +
  geom_point() +
  facet_grid(buffer~Population)

covs <- full_join(forest, hm) %>% 
  full_join(lc)

write.csv(covs, "Covariates_Breed&Winter.csv", row.names=FALSE)
