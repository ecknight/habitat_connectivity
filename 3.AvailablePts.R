library(tidyverse)
library(sf)


#Export to CRS 3857 for GEE----
dat.hab.sf <- dat.hab %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857)

write_sf(dat.sun.sf, "CONIMCP_CleanDataAll_Habitat_3857.shp", row.names = FALSE)