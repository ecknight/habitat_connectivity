library(tidyverse)
library(readxl)
library(vegan)
library(data.table)

#SHOULD COMPARE 1st & 2nd WINTERING LOCATIONS

#1. Read in & wrangle----
pop <- read.csv("/Users/ellyknight/Documents/UoA/Projects/Projects/MCP2/Analysis/Data/tbl_population_abundance.csv")

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population, Sex) %>% 
  dplyr::filter(PinpointID != -99) %>% 
  left_join(pop %>% 
              dplyr::select(Population, Abbreviation))

scores <- read.csv("NMDSScores_Bootstap.csv") %>% 
  left_join(band) %>% 
  pivot_wider(names_from="Season", values_from=c("NMDS1", "NMDS2"), names_sep = "_")

#2. Set up bootstrap----
boot <- max(scores$boot)
mantel.results.list <- list()

for(i in 1:boot){
  
  #3. Subset data----
  scores.i <- scores %>% 
    dplyr::filter(boot==i)
  
  #4. Calculate distance matrices----
  dist.breed <- scores.i %>% 
    dplyr::select(NMDS1_breed, NMDS2_breed) %>% 
    dist()
  
  dist.winter1 <- scores.i %>% 
    dplyr::select(NMDS1_winter1, NMDS2_winter1) %>% 
    dist()
  
  dist.winter2 <- scores.i %>% 
    dplyr::select(NMDS1_winter2, NMDS2_winter2) %>% 
    dist()
  
  #5. Run Mantel----
  mantel1.i <- mantel(dist.breed, dist.winter1)
  mantel2.i <- mantel(dist.breed, dist.winter2)
  mantelwinter.i <- mantel(dist.winter1, dist.winter2)
  
  #6. Save out results----
  mantel.results1 <- data.frame(statistic = mantel1.i[["statistic"]],
                                signif = mantel1.i[["signif"]],
                                boot=i,
                                comparison = "Breed_Winter1")
  
  mantel.results2 <- data.frame(statistic = mantel2.i[["statistic"]],
                                signif = mantel2.i[["signif"]],
                                boot=i,
                                comparison = "Breed_Winter2")
  
  mantel.resultswinter <- data.frame(statistic = mantelwinter.i[["statistic"]],
                                signif = mantelwinter.i[["signif"]],
                                boot=i,
                                comparison = "Winter1_Winter2")
  
  
  mantel.results.list[[i]] <- rbind(mantel.results1, mantel.results2, mantel.resultswinter)
  
  print(paste0("Finished boostrap ", i))
  
}

#7. Convert to datatable and write out----
mantel.results <- rbindlist(mantel.results.list)

write.csv(mantel.results, "MantelResults_Bootstrap.csv", row.names=FALSE)

#8. Look at variance----
ggplot(mantel.results) +
  geom_point(aes(x=signif, y=statistic)) +
  geom_vline(aes(xintercept = 0.05))

#9. Summarize----
mantel.summary <- mantel.results %>% 
  group_by(comparison) %>% 
  summarize(sig.mn = mean(signif),
            sig.sd = sd(signif),
            stat.mm = mean(statistic),
            stat.sd = sd(statistic)) %>% 
  ungroup()
mantel.summary
