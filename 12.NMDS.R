library(tidyverse)
library(vegan)
library(data.table)

options(scipen=999)

#TO DO: UPDATE TO 3 AXES
#TO DO: ANOSIM BETWEEN SEXES

#1. Wrangle----
covs <- read.csv("Covariates_Breed&Winter_Metadata.csv")

covs.sel <- covs %>% 
  dplyr::filter(Type %in% c("Used")) %>% 
  mutate(drought.10 = drought.10 + abs(min(drought.10)),
         pest.10 = ifelse(pest.10 <0, 0, pest.10)) %>% 
  mutate(pest.scale = scale(pest.10),
         change.scale = scale(change.10),
         length.scale = scale(Length.10),
         crops.scale = scale(crops.10),
         drought.scale = scale(drought.10),
         light.scale = scale(light.10),
         Population = as.factor(Population)) %>% 
  rename(length.10 = Length.10) %>% 
  dplyr::select(pest.10, change.10, length.10, crops.10, drought.10, light.10, Population, PinpointID, Season, Type, Sex, Wing, Mass, Winter2Pt)

covs.breed <- covs.sel %>% 
  dplyr::filter(Season %in% c("Breed1", "Breed2", "Breed"))

covs.winter1 <- covs.sel %>% 
  dplyr::filter(Season %in% c("Winter"))

covs.winter2 <- covs.sel %>% 
  dplyr::filter(Season=="Winter2" |
                  (Season=="Winter" & Winter2Pt==0))

#TO DO: ADD IN 2ND WINTERING GROUND ANALYSIS LATER####

#2. Set up bootstrap----
boot <- 100
set.seed(1234)
scores.list <- list()
covscores.list <- list()
ano.list <- list()

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
    dplyr::select(-Population, -PinpointID, -Sex, -Season, -Wing, -Mass, -Winter2Pt, -Type) %>% 
    data.matrix()
  rownames(mat.breed) <- covs.breed.i$PinpointID
  
  mat.winter1 <- covs.winter1.i %>% 
    dplyr::select(-Population, -PinpointID, -Sex, -Season, -Wing, -Mass, -Winter2Pt, -Type) %>% 
    data.matrix()
  rownames(mat.winter1) <- covs.winter1.i$PinpointID
  
  mat.winter2 <- covs.winter2.i %>% 
    dplyr::select(-Population, -PinpointID, -Sex, -Season, -Wing, -Mass, -Winter2Pt, -Type) %>% 
    data.matrix()
  rownames(mat.winter2) <- covs.winter2.i$PinpointID
  
  #5. Calculate distance matrix----
  
  #5. Run NMDS----
  nmds.breed <- metaMDS(mat.breed, k=2, trace=0, distance = "mahalanobis")
  nmds.winter1 <- metaMDS(mat.winter1, k=2, trace=0, distance = "mahalanobis")
  nmds.winter2 <- metaMDS(mat.winter2, k=2, trace=0, distance = "mahalanobis")
  
  #Plotting
  stressplot(nmds.breed) #Excellent
  
  ordiplot(nmds.breed,type="n")
  orditorp(nmds.breed,display="sites",cex=1.25,air=0.01, col=as.numeric(as.character(covs.winter1.i$Population)))
  ordihull(nmds.breed, groups=as.numeric(as.character(covs.winter1.i$Population)), draw="polygon", col="grey90", label=FALSE)
  orditorp(nmds.breed,display="species",col="red",air=0.01)
  
  ordiplot(nmds.winter1,type="n")
  orditorp(nmds.winter1,display="sites",cex=1.25,air=0.01, col=as.numeric(as.character(covs.winter1.i$Population)))
  ordihull(nmds.winter1, groups=as.numeric(as.character(covs.winter1.i$Population)), draw="polygon", col="grey90", label=FALSE)
  orditorp(nmds.winter1,display="species",col="red",air=0.01)
  
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
  
  #8. Test for group differences----
  dist.breed <- vegdist(mat.breed, method = "mahalanobis")
  ano.breed <- anosim(dist.breed, covs.breed.i$Population)
  ano.results.breed <- data.frame(significance = ano.breed$signif,
                                  statistic = ano.breed$statistic,
                                  Season="breed")
  
  dist.winter1 <- vegdist(mat.winter1, method = "mahalanobis")
  ano.winter1 <- anosim(dist.winter1, covs.winter1.i$Population)
  ano.results.winter1 <- data.frame(significance = ano.winter1$signif,
                                  statistic = ano.winter1$statistic,
                                  Season="winter1")
  
  dist.winter2 <- vegdist(mat.winter2, method = "mahalanobis")
  ano.winter2 <- anosim(dist.winter2, covs.winter2.i$Population)
  ano.results.winter2 <- data.frame(significance = ano.winter2$signif,
                                    statistic = ano.winter2$statistic,
                                    Season="winter2")
  
  ano.list[[i]] <- rbind(ano.results.breed, ano.results.winter1, ano.results.winter2)
  
  print(paste0("Finished boostrap ", i))
  
}

#8. Convert to datatable and write out----
scores <- rbindlist(scores.list) %>% 
  mutate(PinpointID = as.numeric(PinpointID)) %>% 
  left_join(covs.sel %>% 
              dplyr::select(PinpointID, Population, Sex, Wing, Mass) %>% 
              unique())
covscores <- rbindlist(covscores.list)

ano <- rbindlist(ano.list)

write.csv(scores, "NMDSScores_Bootstap.csv", row.names = FALSE)
write.csv(covscores, "NMDSCovScores_Bootstap.csv", row.names = FALSE)
write.csv(ano, "ANOSIMResults_Bootstrap.csv", row.names = FALSE)

#9. Look at variance----
#Individual scores
ggplot(scores) +
  geom_point(aes(x=factor(PinpointID), y=NMDS1, colour=factor(Population))) +
  facet_wrap(~Season)

ggplot(scores) +
  geom_point(aes(x=factor(PinpointID), y=NMDS2, colour=factor(Population))) +
  facet_wrap(~Season)
#More variance in breeding, but looks like makes sense overall

ggplot(scores %>% 
         dplyr::filter(Season!="winter2")) +
  geom_point(aes(x=NMDS1, y=NMDS2, colour=factor(Population))) +
  facet_wrap(~Season) +
  scale_colour_viridis_d()

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

#10. Covariate effects----
covscores.summary <- covscores %>% 
  mutate(length = sqrt(NMDS1^2 + NMDS2^2)) %>% 
  group_by(cov, Season) %>% 
  summarize(length.mn = mean(length),
            length.sd = sd(length)) %>% 
  ungroup() %>% 
  arrange(Season, -length.mn)
View(covscores.summary)

#11. Anosim results----
ano.summary <- ano %>%
  group_by(Season) %>%
  summarize(sig = mean(significance),
            stat = mean(statistic),
            sig.sd = sd(significance),
            stat.sd = sd(statistic)) %>% 
  ungroup()
ano.summary
