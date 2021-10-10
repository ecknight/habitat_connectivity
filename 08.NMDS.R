library(tidyverse)
library(vegan)
library(data.table)
library(usdm)
library(corrplot)

options(scipen=999)

#TO DO: ANOSIM BETWEEN SEXES

#1. Wrangle----
covs <- read.csv("Covariates_Breed&Winter_Roosting.csv") %>% 
  rename_all(~gsub(., pattern=".coverfraction", replacement="")) %>% 
  rename_all(~gsub(., pattern="stable.lights", replacement="light")) %>% 
  rename_all(~gsub(., pattern="change.confidence", replacement="change")) %>% 
  mutate(Population = as.factor(Population)) %>% 
  dplyr::filter(Type %in% c("Used"))

covs.id <- covs %>% 
  dplyr::filter(Winter==2) %>% 
  dplyr::select(PinpointID) %>% 
  unique()

covs.breed <- covs %>% 
  dplyr::filter(Season %in% c("Breed")) %>% 
  mutate(crops.scale = scale(crops.10+ 0.00001, center=FALSE),
         bare.scale = scale(bare.10+ 0.00001, center=FALSE),
         grass.scale = scale(grass.10+ 0.00001, center=FALSE),
         shrub.scale = scale(shrub.10+ 0.00001, center=FALSE),
         urban.scale = scale(urban.10+ 0.00001, center=FALSE),
         moss.scale = scale(moss.10+ 0.00001, center=FALSE),
         tree.scale = scale(tree.10+ 0.00001, center=FALSE),
         water.p.scale = scale(water.permanent.10+ 0.00001, center=FALSE),
         water.s.scale = scale(water.seasonal.10+ 0.00001, center=FALSE)) %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, moss.scale, tree.scale, water.p.scale, water.s.scale, Population, PinpointID, Season, Type, Sex, Wing, Mass)

covs.winter1 <- covs %>% 
  dplyr::filter(Season %in% c("Winter") & Winter==1) %>% 
  mutate(crops.scale = scale(crops.1+ 0.00001, center=FALSE),
         bare.scale = scale(bare.1+ 0.00001, center=FALSE),
         grass.scale = scale(grass.1+ 0.00001, center=FALSE),
         shrub.scale = scale(shrub.1+ 0.00001, center=FALSE),
         urban.scale = scale(urban.1+ 0.00001, center=FALSE),
         moss.scale = scale(moss.1+ 0.00001, center=FALSE),
         tree.scale = scale(tree.1+ 0.00001, center=FALSE),
         water.p.scale = scale(water.permanent.1+ 0.00001, center=FALSE),
         water.s.scale = scale(water.seasonal.1+ 0.00001, center=FALSE)) %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, moss.scale, tree.scale, water.p.scale, water.s.scale, Population, PinpointID, Season, Type, Sex, Wing, Mass)

covs.winter2 <- covs %>% 
  dplyr::filter(Season %in% c("Winter") & Winter==2) %>% 
  mutate(crops.scale = scale(crops.1+ 0.00001, center=FALSE),
         bare.scale = scale(bare.1+ 0.00001, center=FALSE),
         grass.scale = scale(grass.1+ 0.00001, center=FALSE),
         shrub.scale = scale(shrub.1+ 0.00001, center=FALSE),
         urban.scale = scale(urban.1+ 0.00001, center=FALSE),
         moss.scale = scale(moss.1+ 0.00001, center=FALSE),
         tree.scale = scale(tree.1+ 0.00001, center=FALSE),
         water.p.scale = scale(water.permanent.1+ 0.00001, center=FALSE),
         water.s.scale = scale(water.seasonal.1+ 0.00001, center=FALSE)) %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, moss.scale, tree.scale, water.p.scale, water.s.scale, Population, PinpointID, Season, Type, Sex, Wing, Mass) %>% 
  rbind(covs.winter1 %>% 
          dplyr::filter(!PinpointID %in% covs.id))

#2. VIF----

#Breeding
covs.breed.vif <- covs.breed %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, tree.scale, water.p.scale, water.s.scale)

M <- cor(covs.breed.vif, use="complete.obs")
M
corrplot(M)
#Urban & seasonal water are highly correlated?

vif(covs.breed.vif)
#Take out seasonal water

covs.breed.vif <- covs.breed %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, tree.scale, water.p.scale)

M <- cor(covs.breed.vif, use="complete.obs")
M
corrplot(M)

vif(covs.breed.vif)
#Take out tree cover

covs.breed.vif <- covs.breed %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, water.p.scale)

M <- cor(covs.breed.vif, use="complete.obs")
M
corrplot(M)

vif(covs.breed.vif)

#Wintering
covs.winter1.vif <- covs.winter1 %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, tree.scale, water.p.scale, water.s.scale)

M <- cor(covs.winter1.vif, use="complete.obs")
M
corrplot(M)
#permanent and seasonal water are highly correlated?

vif(covs.winter1.vif)
#Take out seasonal water

covs.winter1.vif <- covs.winter1 %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, tree.scale, water.p.scale)

M <- cor(covs.winter1.vif, use="complete.obs")
M
corrplot(M)

vif(covs.winter1.vif)
#Take out tree cover

covs.winter1.vif <- covs.winter1 %>% 
  dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, water.p.scale)

M <- cor(covs.winter1.vif, use="complete.obs")
M
corrplot(M)

vif(covs.winter1.vif)


#3. Set up bootstrap----
boot <- 100
set.seed(1234)
stress.list <- list()
scores.list <- list()
covscores.list <- list()
ano.list <- list()
pro.list <- list()

for(i in 1:boot){
  
  #4. Select one location per individual
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
  
  #5. Convert to matrix----
  mat.breed <- covs.breed.i %>% 
    dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, water.p.scale) %>% 
    data.matrix()
  rownames(mat.breed) <- covs.breed.i$PinpointID
  
  mat.winter1 <- covs.winter1.i %>% 
    dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, water.p.scale) %>% 
    data.matrix()
  rownames(mat.winter1) <- covs.winter1.i$PinpointID
  
  mat.winter2 <- covs.winter2.i %>% 
    dplyr::select(crops.scale, bare.scale, grass.scale, shrub.scale, urban.scale, water.p.scale) %>% 
    data.matrix()
  rownames(mat.winter2) <- covs.winter2.i$PinpointID
  
  #6. Run NMDS----
  nmds.breed <- metaMDS(mat.breed, k=3, trace=0, distance = "mahalanobis", trymax=100, maxit=1000)
  nmds.winter1 <- metaMDS(mat.winter1, k=3, trace=0, distance = "mahalanobis", trymax=100, maxit=1000)
  nmds.winter2 <- metaMDS(mat.winter2, k=3, trace=0, distance = "mahalanobis", trymax=100, maxit=1000)
  
  #save out diagnostics
  stress.breed <- data.frame(stress = nmds.breed$stress,
                             converge = nmds.breed$converged, 
                             Season="breed")
  stress.winter1 <- data.frame(stress = nmds.winter1$stress,
                             converge = nmds.winter1$converged,
                             Season="winter1")
  stress.winter2 <- data.frame(stress = nmds.winter2$stress,
                               converge = nmds.winter2$converged,
                               Season="winter2")
  
  stress.list[[i]] <- rbind(stress.breed, stress.winter1, stress.winter2) %>% 
    mutate(boot=i)
  
  #Plotting
#  stressplot(nmds.breed) #Excellent
  
#  ordiplot(nmds.breed,type="n")
#  orditorp(nmds.breed,display="sites",cex=1.25,air=0.01, col=as.numeric(as.character(covs.winter1.i$Population)))
#  ordihull(nmds.breed, groups=as.numeric(as.character(covs.winter1.i$Population)), draw="polygon", col="grey90", label=FALSE)
#  orditorp(nmds.breed,display="species",col="red",air=0.01)
  
#  ordiplot(nmds.winter1,type="n")
#  orditorp(nmds.winter1,display="sites",cex=1.25,air=0.01, col=as.numeric(as.character(covs.winter1.i$Population)))
#  ordihull(nmds.winter1, groups=as.numeric(as.character(covs.winter1.i$Population)), draw="polygon", col="grey90", label=FALSE)
#  orditorp(nmds.winter1,display="species",col="red",air=0.01)
  
  #7. Save out coords for each individual----
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
  
  #8. Save out coords for each covariate----
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
  
  #9. Test for group differences----
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
  
  #10. Procrustes test----
  pro1 <- protest(nmds.breed, nmds.winter1, "sites")
  pro2 <- protest(nmds.breed, nmds.winter2, "sites")
  
  pro.results1 <- data.frame(significance=pro1$signif,
                             statistic=pro1$t0,
                             Season="winter1")
  
  pro.results2 <- data.frame(significance=pro2$signif,
                             statistic=pro2$t0,
                             Season="winter2")
  
  pro.list[[i]] <- rbind(pro.results1, pro.results2)
  
  
  print(paste0("Finished boostrap ", i))
  
}

#11. Convert to datatable and write out----
stress <- rbindlist(stress.list)

scores <- rbindlist(scores.list) %>% 
  mutate(PinpointID = as.numeric(PinpointID)) %>% 
  left_join(covs %>% 
              dplyr::select(PinpointID, Population, Sex, Wing, Mass) %>% 
              unique())

covscores <- rbindlist(covscores.list)

ano <- rbindlist(ano.list)
pro <- rbindlist(pro.list)

write.csv(stress, "NMDSStress_Bootstrap.csv", row.names = FALSE)
write.csv(scores, "NMDSScores_Bootstap.csv", row.names = FALSE)
write.csv(covscores, "NMDSCovScores_Bootstap.csv", row.names = FALSE)
write.csv(ano, "ANOSIMResults_Bootstrap.csv", row.names = FALSE)
write.csv(pro, "ProcrustesResults_Bootstrap.csv", row.names = FALSE)

#12. Look at variance----
#Individual scores
ggplot(scores) +
  geom_point(aes(x=factor(PinpointID), y=NMDS1, colour=factor(Population))) +
  facet_wrap(~Season)

ggplot(scores) +
  geom_point(aes(x=factor(PinpointID), y=NMDS2, colour=factor(Population))) +
  facet_wrap(~Season)

ggplot(scores) +
  geom_point(aes(x=factor(PinpointID), y=NMDS3, colour=factor(Population))) +
  facet_wrap(~Season)
#More variance in wintering, but looks like makes sense overall

ggplot(scores) +
  geom_point(aes(x=NMDS1, y=NMDS2, colour=factor(Population))) +
  facet_wrap(~Season) +
  scale_colour_viridis_d()

ggplot(scores) +
  geom_point(aes(x=NMDS2, y=NMDS3, colour=factor(Population))) +
  facet_wrap(~Season) +
  scale_colour_viridis_d()

ggplot(scores) +
  geom_point(aes(x=NMDS1, y=NMDS3, colour=factor(Population))) +
  facet_wrap(~Season) +
  scale_colour_viridis_d()

#Habitat scores
ggplot(covscores) +
  geom_point(aes(x=cov, y=NMDS1, colour=factor(cov))) +
  facet_wrap(~Season)

ggplot(covscores) +
  geom_point(aes(x=cov, y=NMDS2, colour=factor(cov))) +
  facet_wrap(~Season)

ggplot(covscores) +
  geom_point(aes(x=cov, y=NMDS3, colour=factor(cov))) +
  facet_wrap(~Season)

ggplot(covscores) +
  geom_point(aes(x=NMDS1, y=NMDS2, colour=factor(cov))) +
  facet_wrap(~Season)

#13. Covariate effects----
#NEED TO DECIDE IF I WANT THIS SECTION?
covscores.summary <- covscores %>% 
  mutate(length = sqrt(NMDS1^2 + NMDS2^2)) %>% 
  group_by(cov, Season) %>% 
  summarize(length.mn = mean(length),
            length.sd = sd(length),
            NMDS1.mn = mean(NMDS1),
            NMDS2.mn = mean(NMDS2)) %>% 
  ungroup() %>% 
  arrange(Season, -length.mn)
View(covscores.summary)

#14. Anosim results----
ano.summary <- ano %>%
  group_by(Season) %>%
  summarize(sig = mean(significance),
            stat = mean(statistic),
            sig.sd = sd(significance),
            stat.sd = sd(statistic)) %>% 
  ungroup()
ano.summary

#15. Procrustes results----
pro.summary <- pro %>% 
  group_by(Season) %>%
  summarize(sig = mean(significance),
            stat = mean(statistic),
            sig.sd = sd(significance),
            stat.sd = sd(statistic)) %>% 
  ungroup()
pro.summary