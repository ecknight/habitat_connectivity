library(tidyverse)
library(vegan)

covs <- read.csv("Covariates_Breed&Winter.csv") %>% 
  dplyr::filter(buffer=="2km")


set.seed(1234)

#Breeding----
#Wrangle
covs.breed <- covs %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2")) %>% 
  mutate(ID=paste0(PinpointID, "-", "Breed")) %>% 
  dplyr::select(Population, ID, treecover2020, hm, bare, crops, grass, shrub, tree, water.permanent, water.seasonal) %>% 
  group_by(Population, ID) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  data.frame()

#Matrix
mat.breed <- covs.breed %>% 
  dplyr::select(-Population, -ID) %>% 
  data.matrix()
rownames(mat.breed) <- covs.breed$ID

#Run
nmds.breed <- metaMDS(mat.breed, k=2)

#Plotting
stressplot(nmds.breed) #Excellent

ordiplot(nmds.breed,type="n")
orditorp(nmds.breed,display="species",col="red",air=0.01)

ordiplot(nmds.breed,type="n")
orditorp(nmds.breed,display="sites",cex=1.25,air=0.01, col=covs.breed$Population)
ordihull(nmds.breed, groups=covs.breed$Population, draw="polygon", col="grey90", label=FALSE)

#Save out coords
scores.breed <- scores(nmds.breed) %>% 
  data.frame() %>% 
  mutate(ID=rownames(scores(nmds.breed))) %>% 
  separate(ID, into=c("PinpointID", "Season"))
write.csv(scores.breed, "NMDSScores_Breed.csv", row.names = FALSE)

#Winter----
covs.winter <- covs %>% 
  dplyr::filter(Season2 %in% c("Winter", "Winter2")) %>% 
  mutate(ID=paste0(PinpointID, "-", Season2)) %>% 
  dplyr::select(Population, ID, treecover2020, hm, bare, crops, grass, shrub, tree, water.permanent, water.seasonal) %>% 
  group_by(Population, ID) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  data.frame()

#Matrix
mat.winter <- covs.winter %>% 
  dplyr::select(-Population, -ID) %>% 
  data.matrix()
rownames(mat.winter) <- covs.winter$ID

#Run
nmds.winter <- metaMDS(mat.winter, k=2)

#Plotting
stressplot(nmds.winter) #Excellent

ordiplot(nmds.winter,type="n")
orditorp(nmds.winter,display="species",col="red",air=0.01)

ordiplot(nmds.winter,type="n")
orditorp(nmds.winter,display="sites",cex=1.25,air=0.01, col=covs.winter$Population)
ordihull(nmds.winter, groups=covs.winter$Population, draw="polygon", col="grey90", label=FALSE)

#Save out coords
scores.winter <- scores(nmds.winter) %>% 
  data.frame() %>% 
  mutate(ID=rownames(scores(nmds.winter))) %>% 
  separate(ID, into=c("PinpointID", "Season"))
write.csv(scores.winter, "NMDSScores_Winter.csv", row.names = FALSE)
  
#SHOULD TRY LOOKING AT SEX DIFFERENCES TOO
