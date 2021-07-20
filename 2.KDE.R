library(tidyverse)
library(sf)
library(sp)
library(adehabitatHR)
library(data.table)

options(scipen = 999)

#STILL NEED TO DEAL WITH MUTLIPLE WINTERING RANGE BIRDS

#1. Read in data----
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat.csv") %>% 
  mutate(Season3 = ifelse(Season2 %in% c("Breed1", "Breed2"), "Breed", Season2))

#A. HOME RANGES####

#2. Identify individuals that only have 1 point for breeding season----
ids <- data.frame(table(dat.hab$PinpointID, dat.hab$Season3)) %>% 
  dplyr::filter(Freq>5) %>% 
  rename(PinpointID=Var1, Season3=Var2) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID)))

#3. Separate data into KDE and not----
#remove banding points for these
dat.kde <- dat.hab %>% 
  inner_join(ids) %>% 
  dplyr::filter(Type!="Band") %>% 
  mutate(ID=paste0(PinpointID, "-", Season3))
table(dat.kde$PinpointID, dat.kde$Season3)

#not removing banding points - THINK ABOUT THIS
dat.buff <- dat.hab %>% 
  anti_join(ids) %>% 
  group_by(PinpointID, Season3) %>% 
  summarize(Lat = mean(Lat),
            Long = mean(Long))
table(dat.buff$PinpointID, dat.buff$Season3)

#4. Reproject to UTM
dat.kde.m <- dat.kde %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates()

#Visualize
dat.kde.m.id <- dat.kde.m %>% 
  data.frame() %>% 
  cbind(dat.kde %>% 
          dplyr::select(ID))
ggplot(dat.kde.m.id) +
  geom_point(aes(x=X, y=Y)) +
  facet_wrap(~ID)

#5. Calculate KDE for individuals with > 3 points----
dat.kde.sp <- SpatialPointsDataFrame(coords=dat.kde.m, 
                                   data=data.frame(dat.kde$ID),
                                   proj4string = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))

kd <- kernelUD(dat.kde.sp, grid = 1000, extent=2, h="href", same4all=FALSE)

#6. Calculate area of 95% isopleth----
#Calculate area----
kd.area<- kernel.area(kd, percent=95,
                           unin="m", unout="km2",
                           standardize = FALSE) %>% 
  data.frame() %>% 
  transpose(keep.names="ID") %>% 
  mutate(ID=str_sub(ID, 2, 20)) %>% 
  rename(HRarea=V1) %>% 
    separate(ID, into=c("PinpointID", "Season3"))

ggplot(kd.area) +
  geom_histogram(aes(x=HRarea)) +
  facet_wrap(~Season3, scales="free")
#STILL WAY TOO BIG FIGURE THIS OUT
