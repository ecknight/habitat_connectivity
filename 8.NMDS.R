library(tidyverse)
library(vegan)
library(data.table)

#SHOULD TRY LOOKING AT SEX DIFFERENCES TOO
#SHOULD COMPARE 1st & 2nd WINTERING LOCATIONS
#NEED TO SELECT # OF AXES
#NEED TO SELECT BEST EXTENT
#NEED TO CHOOSE DISTANCE MEASURE
#NEED TO SAVE OUT COVS SCORES TOO

#1. Wrangle----
covs <- read.csv("Covariates_Breed&Winter.csv") %>% 
  dplyr::filter(Winter==1,
                buffer=="2km")

covs.breed <- covs %>% 
  dplyr::filter(Season2 %in% c("Breed1", "Breed2")) %>% 
  dplyr::select(Population, Sex, PinpointID, loss, hm, bare, crops, grass, shrub, water.permanent, water.seasonal)

covs.winter1 <- covs %>% 
  dplyr::filter(Season2=="Winter") %>% 
  dplyr::select(Population, Sex, PinpointID, loss, hm, bare, crops, grass, shrub, water.permanent, water.seasonal)

ids.winter <- covs %>% 
  dplyr::select(PinpointID, Season2) %>% 
  dplyr::filter(Season2=="Winter2") %>% 
  unique() %>% 
  mutate(Winter2=1) %>% 
  dplyr::select(-Season2)

covs.winter2 <- covs %>% 
  left_join(ids.winter) %>% 
  mutate(Winter2=ifelse(is.na(Winter2), 0, Winter2)) %>% 
  dplyr::filter((Winter2==1 & Season2=="Winter2") | (Winter2==0 & Season2=="Winter")) %>% 
  dplyr::select(Population, Sex, PinpointID, loss, hm, bare, crops, grass, shrub, water.permanent, water.seasonal)

#2. Set up bootstrap----
boot <- 100
set.seed(1234)
scores.list <- list()
covscores.list <- list()

for(i in 1:boot){
  
  #3. Select one location per individual
  covs.breed.i <- covs.breed %>% 
    group_by(PinpointID) %>% 
    sample_n(1) %>% 
    ungroup()
  
  covs.winter1.i <- covs.winter1 %>% 
    group_by(PinpointID) %>% 
    sample_n(1) %>% 
    ungroup()
  
  covs.winter2.i <- covs.winter2 %>% 
    group_by(PinpointID) %>% 
    sample_n(1) %>% 
    ungroup()
  
  #4. Convert to matrix----
  mat.breed <- covs.breed.i %>% 
    dplyr::select(-Population, -PinpointID, -Sex) %>% 
    data.matrix()
  rownames(mat.breed) <- covs.breed.i$PinpointID
  
  mat.winter1 <- covs.winter1.i %>% 
    dplyr::select(-Population, -PinpointID, -Sex) %>% 
    data.matrix()
  rownames(mat.winter1) <- covs.winter1.i$PinpointID
  
  mat.winter2 <- covs.winter2.i %>% 
    dplyr::select(-Population, -PinpointID, -Sex) %>% 
    data.matrix()
  rownames(mat.winter2) <- covs.winter2.i$PinpointID
  
  #5. Calculate distance matrix----
  
  #5. Run NMDS----
  nmds.breed <- metaMDS(mat.breed, k=2, trace=0)
  nmds.winter1 <- metaMDS(mat.winter1, k=2, trace=0)
  nmds.winter2 <- metaMDS(mat.winter2, k=2, trace=0)
  
  #Plotting
#  stressplot(nmds.breed) #Excellent
  
#  ordiplot(nmds.breed,type="n")
#  orditorp(nmds.breed,display="sites",cex=1.25,air=0.01, col=covs.breed$Population)
#  ordihull(nmds.breed, groups=covs.breed$Population, draw="polygon", col="grey90", label=FALSE)
#  orditorp(nmds.breed,display="species",col="red",air=0.01)
  
  #6. Save out coords for each individual----
  scores.breed <- scores(nmds.breed, "sites") %>% 
    data.frame() %>% 
    mutate(PinpointID=rownames(scores(nmds.breed)),
           Season="breed")
  
  scores.winter1 <- scores(nmds.winter1, "sites") %>% 
    data.frame() %>% 
    mutate(PinpointID=rownames(scores(nmds.winter1)),
           Season="winter1")
  
  scores.winter2 <- scores(nmds.winter2, "sites") %>% 
    data.frame() %>% 
    mutate(PinpointID=rownames(scores(nmds.winter2)),
           Season="winter2")
  
  scores.list[[i]] <- rbind(scores.breed, scores.winter1, scores.winter2) %>% 
    mutate(boot=i)
  
  #7. Save out coords for each covariate----
  covscores.breed <- scores(nmds.breed, "species") %>% 
    data.frame() %>% 
    mutate(cov=rownames(scores(nmds.breed, "species")),
           Season="breed")
  
  covscores.winter1 <- scores(nmds.winter1, "species") %>% 
    data.frame() %>% 
    mutate(cov=rownames(scores(nmds.winter1, "species")),
           Season="winter1")
  
  covscores.winter2 <- scores(nmds.winter2, "species") %>% 
    data.frame() %>% 
    mutate(cov=rownames(scores(nmds.winter2, "species")),
           Season="winter2")

  
  covscores.list[[i]] <- rbind(covscores.breed, covscores.winter1, covscores.winter2) %>% 
    mutate(boot=i)
  
  print(paste0("Finished boostrap ", i))

}

#8. Convert to datatable and write out----
scores <- rbindlist(scores.list)
covscores <- rbindlist(covscores.list)

write.csv(scores, "NMDSScores_Bootstap.csv", row.names = FALSE)
write.csv(covscores, "NMDSCovScores_Bootstap.csv", row.names = FALSE)

#9. Look at variance----
#Individual scores
ggplot(scores) +
  geom_point(aes(x=PinpointID, y=NMDS1, colour=factor(PinpointID))) +
  facet_wrap(~Season)

ggplot(scores) +
  geom_point(aes(x=PinpointID, y=NMDS2, colour=factor(PinpointID))) +
  facet_wrap(~Season)
#More variance in breeding, but looks like makes sense overall

ggplot(scores %>% 
         dplyr::filter(Season!="winter2")) +
  geom_point(aes(x=NMDS1, y=NMDS2, colour=factor(PinpointID))) +
  facet_wrap(~Season)

#Habitat scores
ggplot(covscores) +
  geom_point(aes(x=cov, y=NMDS1, colour=factor(cov))) +
  facet_wrap(~Season)

ggplot(covscores) +
  geom_point(aes(x=cov, y=NMDS2, colour=factor(cov))) +
  facet_wrap(~Season)

ggplot(covscores %>% 
         dplyr::filter(Season!="winter2")) +
  geom_point(aes(x=NMDS1, y=NMDS2, colour=factor(cov))) +
  facet_wrap(~Season)

#10. Interrogate a bit----
covscores.summary <- covscores %>% 
  mutate(length = sqrt(NMDS1^2 + NMDS2^2)) %>% 
  group_by(cov, Season) %>% 
  summarize(length.mn = mean(length),
            length.sd = sd(length)) %>% 
  ungroup() %>% 
  arrange(Season, -length.mn)
View(covscores.summary)

#SOMETHING WEIRD IS HAPPENING IN WINTER2 WITH INDIVIDUAL 443 AND ALSO WITH THE COVS
#SOMETHING WEIRD IS HAPPENING WITH BREEDING - WEIRD MIRROR IMAGE ISSUE
