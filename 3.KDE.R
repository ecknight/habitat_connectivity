library(tidyverse)
library(sf)
library(sp)
library(adehabitatHR)
library(data.table)
library(MuMIn)

options(scipen = 999)

#1. Read in data----
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat.csv")

#A. HOME RANGES####

#2. Identify individuals that don't have enough points for KDE ----
dat.use <- dat.hab %>% 
  dplyr::filter(SeasonR2n!="Breed1",
                sun==1)

ids <- data.frame(table(dat.use$PinpointID, dat.use$SeasonR2n)) %>% 
  dplyr::filter(Freq>5) %>% 
  rename(PinpointID=Var1, SeasonR2n=Var2) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID)))

#3. Separate data into KDE and not----
#remove banding points for these
dat.kde <- dat.hab %>% 
  inner_join(ids) %>% 
  dplyr::filter(Type!="Band",
                sun==1,
                SeasonR2n!="Breed1") %>% 
  mutate(ID=paste0(PinpointID, "-", SeasonR2n))
table(dat.kde$PinpointID, dat.kde$SeasonR2n)

#not removing banding points - THINK ABOUT THIS
dat.buff <- dat.hab %>% 
  anti_join(ids) %>% 
  group_by(PinpointID, SeasonR2n) %>% 
  summarize(Lat = mean(Lat),
            Long = mean(Long))
table(dat.buff$PinpointID, dat.buff$SeasonR2n)

#4. Reproject to UTM
dat.kde.m <- dat.kde %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates()

#5. Calculate KDE for individuals with >= 5 points (minimum required)----
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
    separate(ID, into=c("PinpointID", "SeasonR2n")) %>% 
  mutate(PinpointID = as.integer(PinpointID))

ggplot(kd.area) +
  geom_histogram(aes(x=HRarea)) +
  facet_wrap(~SeasonR2n, scales="free")
#NEED TO SORT OUT BANDING ONES STILL
#Use only one year of data
#443 - Exclude 1st point

#7. Join to data----
dat.area <- kd.area %>%
  right_join(dat.kde) %>% 
  group_by(PinpointID) %>% 
  mutate(count=n()) %>% 
  ungroup() %>% 
  mutate(SeasonKDE = ifelse(SeasonR2n=="Breed2", "Breed", "Winter"))

#Visualize
ggplot(dat.area) +
  geom_point(aes(x=Long, y=Lat, colour=HRarea), size=1.5) +
  facet_wrap(~ID, scales="free") +
  scale_colour_viridis_c()

ggsave("KDETroubleshooting.jpeg", height=16, width=20, units="in")

#8. Sample size----
ggplot(dat.area) +
  geom_point(aes(x=count, y=HRarea, colour=factor(Sex))) +
  facet_wrap(~SeasonKDE, scales="free") #Seems like maybe a relationship for breeding but not for winter

dat.area.breed <- dat.area %>% 
  dplyr::filter(SeasonKDE=="Breed") %>% 
  dplyr::select(PinpointID, Population, Sex, count, HRarea) %>% 
  unique() %>% 
  mutate(season="breed")
dat.area.winter <- dat.area %>% 
  dplyr::filter(SeasonKDE=="Winter") %>% 
  dplyr::select(PinpointID, Population, Sex, count, HRarea) %>% 
  unique() %>% 
  mutate(season="winter")

dat.area.lm <- rbind(dat.area.breed, dat.area.winter)

#Test for a relationship with sample size
lm.area <- lm(HRarea~poly(count,1)*Sex*season, data=dat.area.lm, na.action="na.fail")
dredge(lm.area)

lm.breed <- lm(HRarea~poly(count,1)*Sex, data=dat.area.breed, na.action="na.fail")
dredge(lm.breed)

lm.winter <- lm(HRarea~poly(count,1)*Sex, data=dat.area.winter, na.action="na.fail")
dredge(lm.winter)

#9. Mean HR area
area.breed <- dat.area.breed %>% 
  summarize(area.mean=mean(HRarea),
            area.sd=sd(HRarea),
            area.max=max(HRarea)) %>% 
  mutate(season="Breed")

area.winter <- dat.area.winter %>% 
  summarize(area.mean=mean(HRarea),
            area.sd=sd(HRarea),
            area.max=max(HRarea)) %>% 
  mutate(season="Winter")

area <- rbind(area.winter, area.breed) %>% 
  mutate(radius.mean = area.mean/(2*3.1416))
area

write.csv(area, "KDEArea.csv", row.names = FALSE)

#B. WINTERING GROUNDS####
dat.cent <- read.csv("BreedingWinterPoints.csv")
dat.wint <- dat.cent %>% 
  dplyr::filter(Season=="Winter") %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates()

#5. Calculate KDE for individuals with >= 5 points (minimum required)----
dat.wint.sp <- SpatialPointsDataFrame(coords=dat.wint, 
                                     data=data.frame(ID=rep(1, nrow(dat.wint))),
                                     proj4string = CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))

kd.wint <- kernelUD(dat.wint.sp, grid = 1000, extent=2, h="href")

kd.shp.95 <- getverticeshr(kd.wint, 95) %>% 
  st_as_sf() %>% 
  mutate(iso=95)
kd.shp.75 <- getverticeshr(kd.wint, 75) %>% 
  st_as_sf() %>% 
  mutate(iso=75)
kd.shp.50 <- getverticeshr(kd.wint, 50) %>% 
  st_as_sf() %>% 
  mutate(iso=50)
kd.shp.25 <- getverticeshr(kd.wint, 25) %>% 
  st_as_sf() %>% 
  mutate(iso=25)
kd.shp.05 <- getverticeshr(kd.wint, 5) %>% 
  st_as_sf() %>% 
  mutate(iso=5)

kd.shp <- rbind(kd.shp.95, kd.shp.75, kd.shp.50, kd.shp.25, kd.shp.05)

ggplot() +
  geom_sf(data=kd.shp, aes(fill=iso)) +
  geom_point(aes(x=X, y=Y), data=data.frame(dat.wint))

write_sf(kd.shp, "Shapefiles/WinterRange.shp")
