library(tidyverse)
library(ggpubr)

#TO DO: TEST ASSUMPTIONS AND DECIDE IF SHOULD BE NONPARAMETRIC####
#TO DO: EXPLORE PERMANOVA####

#1. Read in data----
dat <- read.csv("Covariates_Breed&Winter_Metadata.csv")

#2. Scale values of interest----
dat.scale <- dat %>% 
  mutate(pest.scale = scale(pest.10),
         change.scale = scale(change.10),
         length.scale = scale(Length.10),
         crops.scale = scale(crops.10),
         drought.scale = scale(drought.10),
         light.scale = scale(light.10),
         Population = as.factor(Population))

#A. BREEDING####
#3. Wrangle----
dat.breed <- dat.scale %>% 
  dplyr::filter(Type=="Centroid",
                Season %in% c("Breed1", "Breed2"))

#3. Visualize----
ggboxplot(dat.breed, x = "Population", y = c("pest.scale", "change.scale", "length.scale", "crops.scale", "drought.scale", "light.scale"), 
  merge = TRUE, palette = "jco")
  
#4. AOVs----
aov.pest <- aov(pest.scale ~ Population, data=dat.breed)
summary(aov.pest)

aov.change <- aov(change.scale ~ Population, data=dat.breed)
summary(aov.change)

aov.length <- aov(length.scale ~ Population, data=dat.breed)
summary(aov.length)

aov.crops <- aov(crops.scale ~ Population, data=dat.breed)
summary(aov.crops)

aov.drought <- aov(drought.scale ~ Population, data=dat.breed)
summary(aov.drought)

aov.light <- aov(light.scale ~ Population, data=dat.breed)
summary(aov.light)

#B. WINTERING####
#3. Wrangle----
dat.winter <- dat.scale %>% 
  dplyr::filter(Type=="Centroid",
                Season %in% c("Winter1", "Winter2"))

#3. Visualize----
ggboxplot(dat.winter, x = "Population", y = c("pest.scale", "change.scale", "length.scale", "crops.scale", "drought.scale", "light.scale"), 
          merge = TRUE, palette = "jco")

#4. AOVs----
aov.pest <- aov(pest.scale ~ Population, data=dat.winter)
summary(aov.pest)

aov.change <- aov(change.scale ~ Population, data=dat.winter)
summary(aov.change)

aov.length <- aov(length.scale ~ Population, data=dat.winter)
summary(aov.length)

aov.crops <- aov(crops.scale ~ Population, data=dat.winter)
summary(aov.crops)

aov.drought <- aov(drought.scale ~ Population, data=dat.winter)
summary(aov.drought)

aov.light <- aov(light.scale ~ Population, data=dat.winter)
summary(aov.light)

