library(tidyverse)
library(sf)
library(data.table)

#1. Read in used data----
dat.cent <- read.csv("BreedingWinterPoints.csv")
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat.csv") %>% 
  dplyr::filter(Winter==1)

#2. recall radius size for each season----
area <- read.csv("KDEArea.csv") %>% 
  dplyr::select(season, radius.mean) %>% 
  rename(Radius = radius.mean, Season = season) %>% 
  mutate(Radius = round(Radius, 1))

dat.area <- dat.cent %>% 
  left_join(area)

#3. Random points within 0.5 km----
buff.small <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=500) %>% 
  arrange(PinpointID, Season)
  
ids <- data.frame(PinpointID = rep(buff.small$PinpointID, 20),
                  Season = rep(buff.small$Season, 20)) %>% 
  arrange(PinpointID, Season)

set.seed(1234)
pts.small <- st_sample(buff.small, size=rep(20, nrow(buff.small))) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  cbind(ids) %>% 
  mutate(DateTime = NA,
         Type="Available",
         Radius="500m")

#4. Random points within 20 km----
buff.large <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=20000) %>% 
  arrange(PinpointID, Season)

set.seed(1234)
pts.large <- st_sample(buff.large, size=rep(20, nrow(buff.large))) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  cbind(ids) %>% 
  mutate(DateTime = NA,
         Type="Available",
         Radius="20km")

#5. Random points within the breeding range----
#Use extended range from BBS paper
#THINK ABOUT WHETHER THIS IS PROBLEMATIC GIVEN THAT THESE ARE HAND SELECTED

range.breed <- read_sf("Shapefiles/CONI_edited.shp") %>% 
  st_make_valid()

#of birds
birds.breed <- dat.cent %>% 
  dplyr::filter(Season=="Breed") %>% 
  dplyr::select(PinpointID, SeasonID) %>% 
  unique() %>% 
  nrow()

set.seed(1234)
pts.breed <- st_sample(range.breed, 20*birds.breed) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(PinpointID = NA,
         Season = NA,
         DateTime = NA,
         Type="Available",
         Radius="Breed")

#6. Random points within the wintering range----

kd.wint <- read_sf("Shapefiles/WinterRange.shp") %>% 
  dplyr::filter(iso==95)
plot(kd.wint)

#of birds
birds.wint <- dat.cent %>% 
  dplyr::filter(Season=="Winter") %>% 
  dplyr::select(PinpointID, SeasonID) %>% 
  unique() %>% 
  nrow()

set.seed(1234)
pts.wint <- st_sample(kd.wint, 20*birds.wint) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(PinpointID = NA,
         Season = NA,
         DateTime = NA,
         Type="Available",
         Radius="Winter")

#6. Put together----
pts.available <- rbind(pts.small, pts.large, pts.breed, pts.wint)

pts.cent <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(dat.cent) %>% 
  mutate(Season=paste0(Season,SeasonID)) %>% 
  dplyr::select(X, Y, PinpointID, Season) %>% 
  mutate(DateTime = NA,
         Type="Centroid",
         Radius = NA)

pts.used <- dat.hab %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(dat.hab) %>% 
  dplyr::select(X, Y, PinpointID, SeasonR2n, DateTime) %>% 
  mutate(Type="Used",
         Radius = NA) %>% 
  rename(Season=SeasonR2n)

pts.all <- rbind(pts.available, pts.cent, pts.used)

#7. Export for GEE----
pts.all.sf <- pts.all %>% 
  st_as_sf(coords=c("X", "Y"), crs=3857)

write_sf(pts.all.sf, "CONIMCP_CleanDataAll_Habitat_3857.shp", row.names = FALSE)
