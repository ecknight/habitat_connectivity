#7. recall radius size for each season----
area <- read.csv("KDEArea.csv") %>% 
  dplyr::select(season, radius.mean) %>% 
  rename(Radius = radius.mean, Season = season) %>% 
  mutate(Radius = round(Radius, 1))

dat.area <- dat.cent %>% 
  left_join(area)

#8. Random points within 0.5 km----
buff.small <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=500)

set.seed(1234)
pts.small <- st_sample(buff.small, 20)

#9. Random points within 20 km----
buff.large <- dat.cent %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_buffer(dist=20000)

set.seed(1234)
pts.large <- st_sample(buff.large, 20)

#10. Random points within the breeding range----
#Use extended range from BBS paper
#THINK ABOUT WHETHER THIS IS PROBLEMATIC GIVEN THAT THESE ARE HAND SELECTED

#10. Random points within the wintering range----


#Export to CRS 3857 for GEE----
dat.hab.sf <- dat.hab %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857)

write_sf(dat.sun.sf, "CONIMCP_CleanDataAll_Habitat_3857.shp", row.names = FALSE)