library(tidyverse)
library(vegan)

#Wrangle----
scores.breed <- read.csv("NMDSScores_Breed.csv") %>% 
  rename_all(~paste0("breed_", .)) %>% 
  rename(PinpointID = breed_PinpointID)

scores.winter <- read.csv("NMDSScores_Winter.csv") %>% 
  rename_all(~paste0("winter_", .)) %>% 
  rename(PinpointID = winter_PinpointID)

scores.1 <- inner_join(scores.breed, scores.winter) %>% 
  dplyr::filter(winter_Season=="Winter")
table(scores.1$PinpointID)

#Calculate distance matrices----
dist.breed <- scores.1 %>% 
  dplyr::select(breed_NMDS1, breed_NMDS2) %>% 
  dist()

dist.winter <- scores.1 %>% 
  dplyr::select(winter_NMDS1, winter_NMDS2) %>% 
  dist()

#Run Mantel----
mantel.1 <- mantel(dist.breed, dist.winter)
mantel.1
