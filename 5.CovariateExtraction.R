library(tidyverse)
library(sf)

options(scipen=999)

#1. Read in data----
pts.all.sf <- read_sf("Shapefiles/CONIMCP_CleanDataAll_Habitat_3857.shp") %>% 
  mutate(ID = row_number())

#2. Buffer----
pts.5 <- st_buffer(pts.all.sf, 500)
pts.200 <- st_buffer(pts.all.sf, 20000)

#A. ROADS####
#3. Read in roads data----
rds <- read_sf("Shapefiles/groads-v1-americas-shp/gROADS-v1-americas.shp") %>% 
  st_transform(crs=3857) %>% 
  dplyr::select(ROADID, LENGTH_KM, Shape_Leng, geometry)

#4. Clip by buffers----
rds.5 <- rds %>% 
  st_intersection(pts.5)

rds.200 <- rds %>% 
  st_intersection(pts.200)

#5. Calculate length per buffer----
length.5 <- data.frame(Length5 = st_length(rds.5)) %>% 
  cbind(rds.5) %>% 
  data.frame() %>% 
  group_by(ID) %>% 
  summarize(Length5 = sum(Length5))

length.200 <- data.frame(Length200 = st_length(rds.200)) %>% 
  cbind(rds.200) %>% 
  data.frame() %>% 
  group_by(ID) %>% 
  summarize(Length200 = sum(Length200))
  
#6. Put back together----
pts.rds <- pts.all.sf %>% 
  left_join(length.5) %>% 
  left_join(length.200) %>% 
  mutate(Length5 =ifelse(is.na(Length5), 0, Length5),
         Length200 = ifelse(is.na(Length200), 0, Length200))

#B. PESTICIDES####
#7. Read in Chemgrids----
