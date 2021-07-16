library(tidyverse)
library(readxl)
library(vegan)
library(data.table)

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
mantel.permutations.list <- list()

for(i in 1:boot){
  
  #3. Subset data----
  scores.i <- scores %>% 
    dplyr::filter(boot==i)
  
  #4. Calculate distance matrices----
  dist.breed <- scores.i %>% 
    dplyr::select(NMDS1_breed, NMDS2_breed) %>% 
    dist()
  
  dist.winter <- scores.i %>% 
    dplyr::select(NMDS1_winter1, NMDS2_winter1) %>% 
    dist()
  
  #5. Run Mantel----
  mantel.i <- mantel(dist.breed, dist.winter)
  
  #6. Save out results----
  mantel.results.list[[i]] <- data.frame(statistic = mantel.i[["statistic"]],
                            signif = mantel.i[["signif"]],
                            boot=i)
  mantel.permutations.list[[i]] <- data.frame(mantel = mantel.i[["perm"]],
                                              boot=i)
  
  print(paste0("Finished boostrap ", i))
  
}

#7. Convert to datatable and write out----
mantel.results <- rbindlist(mantel.results.list)
mantel.permutations <- rbindlist(mantel.permutations.list)

write.csv(mantel.results, "MantelResults_Bootstrap.csv", row.names=FALSE)
write.csv(mantel.permutations, "MantelPermutations_Bootstrap.csv", row.names = FALSE)

#8. Look at variance----
ggplot(mantel.results) +
  geom_point(aes(x=signif, y=statistic)) +
  geom_vline(aes(xintercept = 0.05))

ggplot(mantel.permutations) +
  geom_point(aes(x=boot, y=mantel))
