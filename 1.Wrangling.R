library(tidyverse)

#1. Read in data----
dat <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/CONIMCP_CleanDataAll.csv") %>% 
  mutate(Date = as.Date(str_sub(DateTime, 1, 10)),
         DateTime = ymd_hms(paste0(Date, Time)))

#2. Calculate sun times----
dat.sun <- getSunlightTimes(data=dat%>% 
                               rename(lat = Lat, lon = Long, date=Date) %>% 
                               dplyr::select(date, lat, lon)) %>% 
  dplyr::select(sunrise, sunset) %>% 
  cbind(dat) %>% 
  mutate(sun = ifelse(DateTime > sunrise & DateTime < sunset, 1, 0))
table(dat.sun$Season2, dat.sun$sun)

#3. Filter----
#breeding & wintering locations, roost only, birds with wintering HR
dat.hab <- dat.sun %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2", "Winter", "Winter2"),
                sun==1 | Type=="Band",
                Winter==1) %>% 
  mutate(Season=ifelse(Season2%in%c("Breed1", "Breed2"), "Breed", "Winter"))
table(dat.hab$PinpointID, dat.hab$Season)

write.csv(dat.hab, "CONIMCP_CleanDataAll_Habitat.csv", row.names = FALSE)