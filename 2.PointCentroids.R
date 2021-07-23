library(tidyverse)
library(sf)
library(geosphere)

options(scipen = 999)

#1. Read in data----
dat.hab <- read.csv("CONIMCP_CleanDataAll_Habitat.csv") %>% 
  dplyr::filter(Winter==1)

#2. Identify birds that should use tag season for breeding----
dat.sun <- dat.hab %>% 
  dplyr::filter(sun==1)

ids.breed1 <- data.frame(table(dat.sun$PinpointID, dat.sun$SeasonR2n)) %>% 
  rename(PinpointID=Var1, SeasonR2n=Var2) %>% 
  dplyr::filter(Freq==0, SeasonR2n=="Breed2") %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID)),
         SeasonR2n = "Breed1") %>% 
  left_join(dat.hab)

#3. Identify birds that should use banding location for breeding----
dat.band <- data.frame(table(ids.breed1$PinpointID, ids.breed1$Type)) %>% 
  rename(PinpointID=Var1, Type=Var2) %>% 
  dplyr::filter(Freq==0, Type=="Transmission") %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID)),
         Type="Band") %>% 
  left_join(dat.hab) %>% 
  dplyr::select(Population, PinpointID, SeasonR2n, Sex, Mass, Wing, Year, Lat, Long) %>% 
  mutate(count=NA,
         Type="Band")

#4. Calculate centroid for the rest of the birds----
dat.trans <- dat.sun %>% 
  dplyr::filter(Type!="Band") %>% 
  group_by(Population, PinpointID, SeasonR2n, Sex, Mass, Wing, Year) %>% 
  summarize(Lat = mean(Lat),
            Long = mean(Long),
            count=n()) %>% 
  mutate(Type="Transmission")

#5. Compare between tag year & return year for birds with points for both----
#Wrangle
ids.trans <- data.frame(table(dat.trans$PinpointID, dat.trans$SeasonR2n)) %>% 
  rename(PinpointID=Var1, SeasonR2n=Var2) %>% 
  pivot_wider(names_from="SeasonR2n", values_from="Freq") %>% 
  dplyr::filter(Breed1==1, Breed2==1) %>% 
  dplyr::select(PinpointID) %>% 
  mutate(PinpointID = as.integer(as.character(PinpointID))) %>% 
  left_join(dat.trans) %>% 
  dplyr::filter(SeasonR2n %in% c("Breed1", "Breed2"))

#Visualize
ggplot(ids.trans) +
  geom_point(aes(x=Long, y=Lat, colour=count), size=2) +
  facet_wrap(~PinpointID, scales="free") +
  scale_colour_viridis_c()

#Calculate distance
ids.trans.1 <- ids.trans %>% 
  dplyr::filter(SeasonR2n=="Breed1") %>% 
  arrange(PinpointID) %>% 
  dplyr::select(Long, Lat) %>% 
  rename(Long1 = Long, Lat1 = Lat)

ids.trans.2 <- ids.trans %>% 
  dplyr::filter(SeasonR2n=="Breed2") %>% 
  arrange(PinpointID) %>% 
  dplyr::select(Long, Lat) %>% 
  rename(Long2 = Long, Lat2 = Lat)

ids.trans.dist <- data.frame(Distance = distGeo(ids.trans.1, ids.trans.2)) %>% 
  cbind(ids.trans.1, ids.trans.2) %>% 
  left_join(ids.trans %>% 
              dplyr::select(PinpointID, Lat, Long) %>% 
              rename(Lat1 = Lat, Long1 = Long))

#COME BACK TO THIS AFTER BETTER SEASON SEPARATION. GO WITH COUNT FOR NOW
#USE RETURN YEAR FOR BIRDS WITH SAME # IN BOTH YEARS (DONE BY HAND)
dat.breed.both <- ids.trans %>% 
  group_by(PinpointID) %>% 
  summarize(count=max(count)) %>% 
  left_join(ids.trans) %>% 
  dplyr::filter(!(PinpointID %in% c(79,487) & SeasonR2n=="Breed1"))
  
#6. Put together & check----
#All other return year breeding locations
dat.breed2 <- dat.trans %>% 
  dplyr::filter(SeasonR2n=="Breed2",
                !PinpointID %in% dat.breed.both$PinpointID,
                !PinpointID %in% dat.band$PinpointID) 

#all other band year breeding locations
dat.breed1 <- dat.trans %>% 
  dplyr::filter(SeasonR2n=="Breed1",
                !PinpointID %in% dat.breed.both$PinpointID,
                !PinpointID %in% dat.band$PinpointID,
                !PinpointID %in% dat.breed2$PinpointID) 

#Wintering locations
dat.wint <- dat.trans %>% 
  dplyr::filter(SeasonR2n %in% c("Winter", "Winter2"))

#Put together
dat.cent <- rbind(dat.band, dat.breed.both, dat.breed2, dat.breed1, dat.wint) %>% 
  rename(Season = SeasonR2n) %>% 
  mutate(Breed = case_when(Season=="Breed1" ~ 1,
                           Season=="Breed2" ~ 2),
         Winter = case_when(Season=="Winter" ~ 1,
                            Season=="Winter2" ~ 2),
         Season = ifelse(Season %in% c("Breed1", "Breed2"), "Breed", "Winter"))
 
#Check         
table(dat.cent$PinpointID, dat.cent$Season)

#Nbirds
nrow(dat.cent %>% 
  dplyr::select(PinpointID) %>% 
  unique())
#same as connectivity profile paper. good

write.csv(dat.cent, "BreedingWinterPoints.csv", row.names=FALSE)
