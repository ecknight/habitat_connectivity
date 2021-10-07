library(tidyverse)
library(sf)
library(raster)

options(scipen=999)

#1. Read in data----
pts.all.sf <- read_sf("Shapefiles/CONIMCP_CleanDataAll_Habitat_Roosting_3857.shp") %>% 
  mutate(ID = row_number())

#2. Buffer----
pts.1 <- st_buffer(pts.all.sf, 1000)
pts.10 <- st_buffer(pts.all.sf, 10000)

#A. ROADS####
#3. Read in roads data----
rds <- read_sf("Shapefiles/groads-v1-americas-shp/gROADS-v1-americas.shp") %>% 
  st_transform(crs=3857) %>% 
  dplyr::select(ROADID, LENGTH_KM, Shape_Leng, geometry)

#4. Clip by buffers----
rds.1 <- rds %>% 
  st_intersection(pts.1)

rds.10 <- rds %>% 
  st_intersection(pts.10)

#5. Calculate length per buffer----
length.1 <- data.frame(Length.1 = st_length(rds.1)) %>% 
  cbind(rds.1) %>% 
  data.frame() %>% 
  group_by(ID) %>% 
  summarize(Length.1 = sum(Length.1))

length.10 <- data.frame(Length.10 = st_length(rds.10)) %>% 
  cbind(rds.10) %>% 
  data.frame() %>% 
  group_by(ID) %>% 
  summarize(Length.10 = sum(Length.10))
  
#6. Put back together----
pts.rds <- pts.all.sf %>% 
  left_join(length.1) %>% 
  left_join(length.10) %>% 
  mutate(Length.1 =ifelse(is.na(Length.1), 0, Length.1),
         Length.10 = ifelse(is.na(Length.10), 0, Length.10))

#B. PESTICIDES####
#7. Get list of tifs----
tifs <- data.frame(file=list.files("/Users/ellyknight/Downloads/ferman-v1-pest-chemgrids-v1-01-geotiff/ApplicationRate/GEOTIFF")) %>% 
  separate(file, into=c("APR", "Crop", "Pesticide", "Year", "Level"), sep="_", remove=FALSE) %>% 
  mutate(Level = str_sub(Level, 1, 1),
         filepath = paste0("/Users/ellyknight/Downloads/ferman-v1-pest-chemgrids-v1-01-geotiff/ApplicationRate/GEOTIFF/", file))

#8. Select which tifs to use----
tifs.use <- tifs %>% 
  dplyr::filter(Year==2015, Level=="H")
table(tifs.use$Pesticide, tifs.use$Crop)

tifs.use.list <- as.list(tifs.use)

#9. Stack all tifs & calculate mean----
#stack.all <- raster::stack(tifs.use.list[["filepath"]])
#stack.all.scale <- scale(stack.all)
#pesticides.mean <- calc(stack.all.scale, fun=mean)

#writeRaster(pesticides.mean, "Shapefiles/PesticidesMean.tif", overwrite=TRUE)
pesticides.mean <- raster("Shapefiles/PesticidesMean.tif") %>% 
  projectRaster(crs=3857)
plot(pesticides.mean)

#10. Extract raster values----
pest.10 <- raster::extract(pesticides.mean, pts.10, fun=mean, df=TRUE) %>% 
  rename(pest.10 = PesticidesMean) 
pest.1 <- raster::extract(pesticides.mean, pts.1, fun=mean, df=TRUE) %>% 
  rename(pest.1 = PesticidesMean)
pest.pt <- raster::extract(pesticides.mean, pts.all.sf, df=TRUE) %>% 
  rename(pest.pt = PesticidesMean)

#11. Put back together----
pts.pest <- cbind(pts.all.sf, pest.10, pest.1, pest.pt) %>% 
  data.frame() %>% 
  dplyr::select(-geometry, -ID.1, -ID.2, -ID.3)

#12. Put all together and save----
pts.covs <- pts.rds %>% 
  left_join(pts.pest) %>% 
  data.frame() %>% 
  dplyr::select(-ID, -geometry)

write.csv(pts.covs, "ExtractedCovariates.csv", row.names = FALSE)
