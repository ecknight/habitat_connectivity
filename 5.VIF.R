library(tidyverse)
library(usdm)
library(corrplot)
library(Rnalytica)

options(scipen = 999)

#1. Wrangling----
covs <- read.csv("Covariates_Breed&Winter.csv") %>% 
  dplyr::filter(Winter==1)

#2. All variables---
covs.vif <- covs %>% 
  dplyr::select(buffer, loss, treecover2000, treecover2020, hm, bare, crops, grass, moss, shrub, snow, tree, urban, water.permanent, water.seasonal) %>% 
  group_by(buffer) %>% 
  mutate(ID=row_number()) %>% 
  pivot_wider(id_cols=ID, names_from=buffer, values_from = c(loss:water.seasonal)) %>% 
  dplyr::select(-ID) %>% 
  data.frame()

M <- cor(covs.vif)
corrplot(M)

#3. Take out some stuff----
covs.vif <- covs %>% 
  dplyr::filter(buffer!="2km") %>% 
  dplyr::select(buffer, loss, hm, bare, crops, grass, shrub, tree, water.permanent, water.seasonal) %>% 
  group_by(buffer) %>% 
  mutate(ID=row_number()) %>% 
  pivot_wider(id_cols=ID, names_from=buffer, values_from = c(loss:water.seasonal)) %>% 
  dplyr::select(-ID) %>% 
  data.frame()

M <- cor(covs.vif)
corrplot(M)

vif(covs.vif)

#4. Try taking out tree----
covs.vif <- covs %>% 
  dplyr::filter(buffer!="2km") %>% 
  dplyr::select(buffer, loss, hm, bare, crops, grass, shrub, water.permanent, water.seasonal) %>% 
  group_by(buffer) %>% 
  mutate(ID=row_number()) %>% 
  pivot_wider(id_cols=ID, names_from=buffer, values_from = c(loss:water.seasonal)) %>% 
  dplyr::select(-ID) %>% 
  data.frame()

M <- cor(covs.vif)
M
corrplot(M)

vif(covs.vif)

#5. Try using only one extent but keeping tree----
covs.vif <- covs %>% 
  dplyr::filter(buffer=="2km") %>% 
  dplyr::select(buffer, loss, hm, tree, bare, crops, grass, shrub, water.permanent, water.seasonal) %>% 
  group_by(buffer) %>% 
  mutate(ID=row_number()) %>% 
  pivot_wider(id_cols=ID, names_from=buffer, values_from = c(loss:water.seasonal)) %>% 
  dplyr::select(-ID) %>% 
  data.frame()

M <- cor(covs.vif)
M
corrplot(M)

vif(covs.vif) #Nope too correlated with grass

#6. Final selection----
covs.vif <- covs %>% 
  dplyr::filter(buffer=="2km") %>% 
  dplyr::select(buffer, loss, hm, bare, crops, grass, shrub, water.permanent, water.seasonal) %>% 
  group_by(buffer) %>% 
  mutate(ID=row_number()) %>% 
  pivot_wider(id_cols=ID, names_from=buffer, values_from = c(loss:water.seasonal)) %>% 
  dplyr::select(-ID) %>% 
  data.frame()

M <- cor(covs.vif)
M
corrplot(M) #Interesting that shrub & loss are correlated!

vif(covs.vif)
#Pick extent in next step of nmds via lowest stress
