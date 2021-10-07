library(tidyverse)
library(sf)
library(data.table)

#1. Read in used data----
dat.cent <- read.csv("BreedingWinterCentroids.csv")
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat_Roosting.csv") %>% 
  dplyr::filter(Winter==1)

#2. recall radius size for each season----
area <- read.csv("KDEAreaMean.csv") %>% 
  dplyr::select(season, radius.mean) %>% 
  rename(Radius = radius.mean, Season = season) %>% 
  mutate(Radius = round(Radius, 1))

dat.area <- dat.cent %>% 
  left_join(area)

#3. Random points within 1 km----
buff.small <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=1000) %>% 
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
         Radius="1km")

#4. Random points within 20 km----
buff.medium <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=10000) %>% 
  arrange(PinpointID, Season)

set.seed(1234)
pts.medium <- st_sample(buff.medium, size=rep(20, nrow(buff.medium))) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  cbind(ids) %>% 
  mutate(DateTime = NA,
         Type="Available",
         Radius="10km")

#5. Random points within 100 km----
buff.large <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=100000) %>% 
  arrange(PinpointID, Season)

set.seed(1234)
pts.large <- st_sample(buff.large, size=rep(20, nrow(buff.large))) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  cbind(ids) %>% 
  mutate(DateTime = NA,
         Type="Available",
         Radius="100km")

#6. Random points within the breeding range----
#Use extended range from BBS paper

range.breed <- read_sf("Shapefiles/CONI_edited.shp") %>% 
  st_make_valid()

#of birds
birds.breed <- dat.cent %>% 
  dplyr::filter(Season=="Breed") %>% 
  dplyr::select(PinpointID, Season) %>% 
  unique() %>% 
  nrow()

set.seed(1234)
pts.breed <- st_sample(range.breed, 20*birds.breed) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(PinpointID = NA,
         Season = NA,
         DateTime = NA,
         Type="Available",
         Radius="Breed")

#7. Random points within the wintering range----

kd.wint <- read_sf("Shapefiles/WinterRange.shp") %>% 
  dplyr::filter(iso==95)
plot(kd.wint)

#of birds
birds.wint <- dat.cent %>% 
  dplyr::filter(Season=="Winter") %>% 
  dplyr::select(PinpointID, Season) %>% 
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

#8. Put together----
pts.available <- rbind(pts.small, pts.medium, pts.large, pts.breed, pts.wint)

pts.cent <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(dat.cent) %>% 
  dplyr::select(X, Y, PinpointID, Season) %>% 
  mutate(DateTime = NA,
         Type="Centroid",
         Radius = NA)

pts.used <- dat.hab %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  cbind(dat.hab) %>% 
  dplyr::select(X, Y, PinpointID, Season, DateTime) %>% 
  mutate(Type="Used",
         Radius = NA)

pts.all <- rbind(pts.available, pts.cent, pts.used)

write.csv(pts.all, "CONIMCP_CleanDataAll_Habitat_Roosting_3857.csv", row.names = FALSE)

#9. Export for GEE----
pts.all.sf <- pts.all %>% 
  st_as_sf(coords=c("X", "Y"), crs=3857) %>% 
  cbind(pts.all %>% 
          dplyr::select(X, Y))

plot(pts.all.sf)

write_sf(pts.all.sf, "Shapefiles/CONIMCP_CleanDataAll_Habitat_Roosting_3857.shp", append=FALSE)
