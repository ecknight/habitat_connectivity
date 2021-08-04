library(tidyverse)
library(readxl)
library(vegan)
library(data.table)
library(gridExtra)
library(cowplot)
library(sf)
library(ggsn)
library(ggmap)

my.theme <- theme_classic() +
  theme(text=element_text(size=12, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        plot.title=element_text(size=12, hjust = 0.5))

#1. Individual birds ordination----

#1a. Read in & wrangle
pop <- read.csv("tbl_population_abundance.csv") %>% 
  dplyr::filter(Region != "Florida") %>% 
  dplyr::mutate(Region = case_when(Region=="BC coast" ~ "Coastal BC",
                                   Region=="BC Okanagan" ~ "Southcentral BC",
                                   !is.na(Region) ~ as.character(Region))) %>% 
  arrange(-Lat) %>% 
  mutate(order=row_number()) 

pop$Region <- factor(pop$Region, levels=c("Yukon", "Northwest Territories", "Alberta", "Saskatchewan", "Southcentral BC", "Coastal BC", "New Brunswick", "Ontario", "South Dakota", "Oregon", "Arizona", "Texas"))

band <- read_excel("tbl_band.xlsx") %>% 
  dplyr::select(PinpointID, Population, Sex) %>% 
  dplyr::filter(PinpointID != -99) %>% 
  left_join(pop %>% 
              dplyr::select(Population, Abbreviation))

scores <- read.csv("NMDSScores_Bootstap.csv") %>% 
  left_join(band)

#1b. Calculate mean & sd
scores.mn <- scores %>% 
  group_by(Season, Population, PinpointID, Sex) %>% 
  summarize(NMDS1.mn = mean(NMDS1),
            NMDS1.sd = sd(NMDS1),
            NMDS2.mn = mean(NMDS2),
            NMDS2.sd = sd(NMDS2)) %>% 
  ungroup() %>% 
  left_join(pop)

scores.mn$Season <- factor(scores.mn$Season, labels=c("Breeding grounds", "First wintering grounds", "Second wintering grounds"))

#1c. Summarize cov scores
covscores <- read.csv("NMDSCovScores_Bootstap.csv")
covscores.mn <- covscores %>% 
  group_by(Season, cov) %>% 
  summarize(NMDS1.mn = mean(NMDS1),
            NMDS2.mn = mean(NMDS2),
            NMDS1.sd = sd(NMDS1),
            NMDS2.sd = sd(NMDS2)) %>% 
  ungroup()

covscores.mn$Season <- factor(covscores.mn$Season, labels=c("Breeding grounds", "First wintering grounds", "Second wintering grounds"))

#1c. Plot
plot.breed <- ggplot(scores.mn %>% 
                            dplyr::filter(Season=="Breeding grounds")) +
  geom_point(aes(x=NMDS1.mn, y=NMDS2.mn, colour=factor(Region), shape=factor(Region)), size=4) +
  geom_segment(aes(x=NMDS1.mn-NMDS1.sd, xend=NMDS1.mn+NMDS1.sd, y=NMDS2.mn, yend=NMDS2.mn, colour=factor(Region))) +
  geom_segment(aes(y=NMDS2.mn-NMDS2.sd, yend=NMDS2.mn+NMDS2.sd, x=NMDS1.mn, xend=NMDS1.mn, colour=factor(Region))) +
  scale_shape_manual(values=c(0,1,2,3,4,5,7,15,16,17,18,25), name="") +
  scale_colour_viridis_d(name="") +
  xlab("Grass, human modification, seasonal water, tree") +
  ylab("Crop, permanent water, shrub") +
  my.theme +
  theme(legend.position = "none",
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) +
  guides(colour=guide_legend(nrow=2),
         shape=guide_legend(nrow=2)) +
  ggtitle("Breeding grounds")

plot.winter1 <- ggplot(scores.mn %>% 
                            dplyr::filter(Season=="First wintering grounds")) +
  geom_point(aes(x=NMDS1.mn, y=NMDS2.mn, colour=factor(Region), shape=factor(Region)), size=4) +
  geom_segment(aes(x=NMDS1.mn-NMDS1.sd, xend=NMDS1.mn+NMDS1.sd, y=NMDS2.mn, yend=NMDS2.mn, colour=factor(Region))) +
  geom_segment(aes(y=NMDS2.mn-NMDS2.sd, yend=NMDS2.mn+NMDS2.sd, x=NMDS1.mn, xend=NMDS1.mn, colour=factor(Region))) +
  scale_shape_manual(values=c(0,1,2,3,4,5,7,15,16,17,18,25), name="") +
  scale_colour_viridis_d(name="") +
  xlab("Crop") +
  ylab("Grass, human modification, permanent water,\nseasonal water, shrub, tree") +
  my.theme +
  theme(legend.position = "none",
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) +
  guides(colour=guide_legend(nrow=2),
         shape=guide_legend(nrow=2)) + 
  ggtitle("First wintering grounds")

plot.winter2 <- ggplot(scores.mn %>% 
                              dplyr::filter(Season=="Second wintering grounds")) +
  geom_point(aes(x=NMDS1.mn, y=NMDS2.mn, colour=factor(Region), shape=factor(Region)), size=4) +
  geom_segment(aes(x=NMDS1.mn-NMDS1.sd, xend=NMDS1.mn+NMDS1.sd, y=NMDS2.mn, yend=NMDS2.mn, colour=factor(Region))) +
  geom_segment(aes(y=NMDS2.mn-NMDS2.sd, yend=NMDS2.mn+NMDS2.sd, x=NMDS1.mn, xend=NMDS1.mn, colour=factor(Region))) +
  scale_shape_manual(values=c(0,1,2,3,4,5,7,15,16,17,18,25), name="") +
  scale_colour_viridis_d(name="") +
  xlab("Human modification, permanent water, shrub") +
  ylab("Crop, grass, seasonal water, tree") +
  my.theme +
  theme(legend.position = "none",
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) +
  guides(colour=guide_legend(nrow=2),
         shape=guide_legend(nrow=2)) + 
  ggtitle("Second wintering grounds")

#1d. Make legend
plot.legend <- plot.nmds.breed <- ggplot(scores.mn) +
  geom_point(aes(x=NMDS1.mn, y=NMDS2.mn, colour=factor(Region), shape=factor(Region)), size=4) +
  geom_segment(aes(x=NMDS1.mn-NMDS1.sd, xend=NMDS1.mn+NMDS1.sd, y=NMDS2.mn, yend=NMDS2.mn, colour=factor(Region))) +
  geom_segment(aes(y=NMDS2.mn-NMDS2.sd, yend=NMDS2.mn+NMDS2.sd, x=NMDS1.mn, xend=NMDS1.mn, colour=factor(Region))) +
  scale_shape_manual(values=c(0,1,2,3,4,5,7,15,16,17,18,25), name="") +
  scale_colour_viridis_d(name="") +
  my.theme +
  theme(legend.position = "bottom") +
  facet_wrap(~Season, scales="free") +
  guides(colour=guide_legend(nrow=2),
         shape=guide_legend(nrow=2))
legend <- get_legend(plot.legend)

#1e. Put together
ggsave(plot=grid.arrange(plot.breed, plot.winter1, plot.winter2, legend,
                         widths=c(4,4,4),
                         heights = c(4,1),
                         layout_matrix = rbind(c(1,2,3),
                                               c(4,4,4))),
       "Figures/NMDS.jpeg", width=12, height=5, units="in", device="jpeg")

#2. NMDS covariates----

#2a. Wrangle
covscores <- read.csv("NMDSCovScores_Bootstap.csv")
covscores.mn <- covscores %>% 
  group_by(Season, cov) %>% 
  summarize(NMDS1.mn = mean(NMDS1),
            NMDS2.mn = mean(NMDS2),
            NMDS1.sd = sd(NMDS1),
            NMDS2.sd = sd(NMDS2)) %>% 
  ungroup()

#2b. Plot
plot.nmds.covs <- ggplot(covscores.mn) +
  geom_segment(aes(x=NMDS1.mn-NMDS1.sd, xend=NMDS1.mn+NMDS1.sd, y=NMDS2.mn, yend=NMDS2.mn, colour=factor(cov))) +
  geom_segment(aes(y=NMDS2.mn-NMDS2.sd, yend=NMDS2.mn+NMDS2.sd, x=NMDS1.mn, xend=NMDS1.mn, colour=factor(cov))) +
  geom_segment(aes(x=0, xend=NMDS1.mn, y=0, yend=NMDS2.mn, colour=factor(cov)), size=1.5) +
  geom_point(aes(x=NMDS1.mn, y=NMDS2.mn, colour=factor(cov)), size=3) +
  facet_wrap(~Season, scales="free") +
#  scale_colour_viridis_d(name="Covariate", labels=c("Land cover change", "% crop", "Drought index", "Road density", "Light pollution", "Pesticide application"), option="B") +
  xlab("NMDS1") +
  ylab("NMDS2") +
  my.theme +
  theme(legend.position="bottom")
plot.nmds.covs

ggsave(plot=plot.nmds.covs, "Figures/NMDS_covs.jpeg", width=15, height=6, units="in", device="jpeg")

#3. Example ordination plot----
plot.nmds.example <- ggplot(covscores.mn %>% 
                              dplyr::filter(Season=="breed")) +
  geom_point(data=scores.mn, aes(x=NMDS1.mn, y=NMDS2.mn),
             show.legend=FALSE) +
  geom_segment(aes(x=0, xend=NMDS1.mn, y=0, yend=NMDS2.mn, colour=factor(cov)), size=1.5, 
               arrow = arrow(length = unit(0.5, "cm"))) +
  scale_colour_viridis_d(name="Covariate", labels=c("% crop", "% grass", "human modification", "% shrub", "% tree", "% permanent water", "% seasonal water"), option="E") +
  xlab("NMDS1") +
  ylab("NMDS2") +
  my.theme +
  theme(legend.position="right") +
  guides(colour=guide_legend(ncol=1))
plot.nmds.example

ggsave(plot=plot.nmds.example, "Figures/NMDS_example.jpeg", width=7, height=5, units="in", device="jpeg")

#4. Mean covariates----
#4a. Wrangle
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
  dplyr::select(pest.scale, change.scale, length.scale, crops.scale, drought.scale, light.scale, Population, PinpointID, Season, Type, Sex, Wing, Mass, Winter2Pt) %>% 
  pivot_longer(cols=c(pest.scale:light.scale), names_to="cov", values_to="val") %>% 
  mutate(Season = ifelse(Season %in% c("Breed1", "Breed2"), "Breed", "Winter"))

covs.sel$cov <- factor(covs.sel$cov, labels=c("Land cover change", "$ crop", "Drought index", "Road density", "Light pollution", "Pesticide application"))

plot.covs <- ggplot(covs.sel) +
  geom_boxplot(aes(x=cov, y=val, colour=factor(Population))) + 
  facet_wrap(~Season) +
  my.theme +
  ylab("Scaled & centered value") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_viridis_d(name="Population")

ggsave(plot=plot.covs, "Figures/Covs.jpeg", width=10, height=6, units="in", device="jpeg")

#5. Seasonal RSF----

#5a. Read in data
pred <- read.csv("RSFPredictions_Season.csv") %>% 
  mutate(cov = ifelse(model=="length", cov/1000, cov))
pred$Season2 <- factor(pred$Season2, labels=c("Breeding\ngrounds", "Wintering\ngrounds"))
pred$model <- factor(pred$model, labels=c("Land cover change", "Drought index", "Road density (1000 km)"))

#5b. Plot
plot.rsf <- ggplot(pred) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(Season2)), alpha=0.3) +
  geom_line(aes(x=cov, y=fit, colour=factor(Season2))) +
#  geom_rug(data=(covs.sel %>% 
#               dplyr::filter(Response==1)), aes(x=change.scale, y=Response, colour=as.factor(PinpointID)), sides="t") +
#  geom_rug(data=(covs.sel %>% 
#               dplyr::filter(Response==0)), aes(x=change.scale, y=Response, colour=as.factor(PinpointID)), sides="b") +
  facet_wrap(~model, scales="free_x") +
  my.theme + 
  xlab("Covariate value") +
  ylab("Relative selection probability") +
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season")

ggsave(plot=plot.rsf, "Figures/RSFPredictions_Season.jpeg", width=10, height=4, units="in", device="jpeg")


#6. Population RSF----
pop <- read.csv("tbl_population_abundance.csv") %>% 
  arrange(desc(Lat)) %>% 
  mutate(order=row_number(),
         poporder=ifelse(nchar(order)==1, paste0("0", order), order)) %>% 
  dplyr::select(Population, Region, poporder, order) %>% 
  dplyr::mutate(Region = case_when(Region=="BC coast" ~ "Coastal BC",
                                   Region=="BC Okanagan" ~ "Southcentral BC",
                                   !is.na(Region) ~ as.character(Region))) %>% 
  mutate(label = paste0(order, " - ", Region)) 

pop$Region <- factor(pop$Region, levels=c("Yukon", "Northwest Territories", "Alberta", "Saskatchewan", "Southcentral BC", "Coastal BC", "New Brunswick", "Ontario", "South Dakota", "Oregon", "Arizona", "Texas"),
                     labels=c("Yukon", "NWT (n=3)", "Alberta (n=11)", "Saskatchewan (n=5)", "BC (n=3)", "Coastal BC", "New Brunswick", "Ontario (n=7)", "South Dakota (n=4)", "Oregon", "Arizona", "Texas"))

pred <- read.csv("RSFPredictions_Population.csv") %>% 
  left_join(pop)
pred$season <- factor(pred$season, labels=c("Breeding\ngrounds", "Wintering\ngrounds"))
pred$model <- factor(pred$model, labels=c("Land cover change", "Drought index", "Road density (1000 km)"))

pred.length <- pred %>% 
  dplyr::filter(model=="Road density (1000 km)") %>% 
  mutate(cov = cov/1000)
pred.change <- pred %>% 
  dplyr::filter(model=="Land cover change")
pred.drought <- pred %>% 
  dplyr::filter(model=="Drought index")

#6a. Breeding season only----
plot.rsf.change.1 <- ggplot(pred.change %>% 
                            dplyr::filter(season=="Breeding\ngrounds")) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(season)), alpha=0.3) +
  geom_line(aes(x=cov, y=fit, colour=factor(season))) +
  facet_grid(~Region, scales="free_x") +
  my.theme + 
  xlab("Land cover change") +
  ylab("") +
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")
plot.rsf.change.1

plot.rsf.drought.1 <- ggplot(pred.drought %>% 
                               dplyr::filter(season=="Breeding\ngrounds")) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(season)), alpha=0.3) +
  geom_line(aes(x=cov, y=fit, colour=factor(season))) +
  facet_grid(~Region, scales="free_x") +
  my.theme + 
  xlab("Drought index") +
  ylab("Relative selection probability") +
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(colour="white"),
        strip.text.x = element_blank(),
        legend.position = "none")
plot.rsf.drought.1

plot.rsf.length.1 <- ggplot(pred.length %>% 
                              dplyr::filter(season=="Breeding\ngrounds")) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(season)), alpha=0.3) +
  geom_line(aes(x=cov, y=fit, colour=factor(season))) +
  facet_grid(~Region, scales="free_x") +
  my.theme + 
  xlab("Road density (1000 km)") +
  ylab("") +
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(colour="white"),
        strip.text.x = element_blank(),
        legend.position = "none")
plot.rsf.length.1

plot.legend <- ggplot(pred.drought) +
  geom_line(aes(x=cov, y=fit, colour=factor(season))) +
  my.theme + 
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season") +
  theme(legend.position = "right")
legend <- get_legend(plot.legend)

plot.rsf <- grid.arrange(plot.rsf.change.1, plot.rsf.drought.1, plot.rsf.length.1, legend,
                         widths=c(10,2),
                         heights=c(5,4,4),
                         layout_matrix=rbind(c(1,NA),
                                             c(2,4),
                                             c(3,NA)))

ggsave(plot=plot.rsf, "Figures/RSFPredictions_Population_Breeding.jpeg", width=13, height=6, units="in", device="jpeg")

#6b. Both seasons----
plot.rsf.change.2 <- ggplot(pred.change) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(season)), alpha=0.3) +
  geom_line(aes(x=cov, y=fit, colour=factor(season))) +
  facet_grid(~Region, scales="free_x") +
  my.theme + 
  xlab("Land cover change") +
  ylab("") +
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="none")

plot.rsf.drought.2 <- ggplot(pred.drought) +
  geom_ribbon(aes(x=cov, ymin=lwr, ymax=upr, group=factor(season)), alpha=0.3) +
  geom_line(aes(x=cov, y=fit, colour=factor(season))) +
  facet_grid(~Region, scales="free_x") +
  my.theme + 
  xlab("Drought index") +
  ylab("Relative selection probability") +
  scale_colour_manual(values=c("goldenrod3", "cyan4"), name="Season") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(colour="white"),
        strip.text.x = element_blank(),
        legend.position = "none")

plot.rsf2 <- grid.arrange(plot.rsf.change.2, plot.rsf.drought.2, plot.rsf.length.1, legend,
                         widths=c(10,2),
                         heights=c(5,4,4),
                         layout_matrix=rbind(c(1,NA),
                                             c(2,4),
                                             c(3,NA)))

ggsave(plot=plot.rsf2, "Figures/RSFPredictions_Population_Both.jpeg", width=13, height=6, units="in", device="jpeg")


#7. Data availability dot plot----
pop <- read.csv("tbl_population_abundance.csv") %>% 
  arrange(desc(Lat)) %>% 
  mutate(order=row_number(),
         poporder=ifelse(nchar(order)==1, paste0("0", order), order)) %>% 
  dplyr::select(Population, Region, poporder, order) %>% 
  dplyr::mutate(Region = case_when(Region=="BC coast" ~ "Coastal BC",
                                   Region=="BC Okanagan" ~ "Southcentral BC",
                                   !is.na(Region) ~ as.character(Region))) %>% 
  mutate(label = paste0(order, " - ", Region)) 

dat <- read.csv("CONIMCP_CleanDataAll.csv") %>% 
  mutate(doy.order = ifelse(doy >=222, doy-222, doy+143)) %>% 
  dplyr::select(Winter, Population, PinpointID, doy, doy.order, Season2) %>% 
  rename(Season=Season2) %>% 
  mutate(Type = "Transmission",
         sun = NA) %>% 
  dplyr::filter(Season %in% c("FallMig", "SpringMig", "WinterMig"))

covs <- read.csv("CONIMCP_CleanDataAll_Habitat.csv") %>% 
  mutate(doy.order = ifelse(doy >=222, doy-222, doy+143),
         Winter=1) %>% 
  dplyr::select(Winter, Population, PinpointID, doy, doy.order, SeasonR2n, Type, sun) %>% 
  rename(Season = SeasonR2n) %>% 
  rbind(dat) %>% 
  left_join(pop) %>% 
  dplyr::mutate(ID=paste0(poporder, Region, PinpointID)) %>% 
  arrange(desc(ID)) %>% 
  group_by(ID) %>% 
  mutate(rowID=row_number(),
         max=max(rowID)) %>% 
  ungroup() %>% 
  mutate(ID = as.factor(ID),
         ID = factor(ID, levels = rev(levels(ID))),
         Season = ifelse(Season %in% c("Breed1", "Breed2"), "Breeding", Season),
         use = case_when(sun!=1 ~ 0,
                         Type == "Band" ~ 0,
                         Season %in% c("FallMig", "WinterMig", "SpringMig") ~ 0,
                         Winter!=1 ~0,
                         PinpointID==480 ~ 0),
         use = ifelse(is.na(use), 1, 0))

covs$Season <- factor(covs$Season, levels=c("Breeding", "FallMig", "Winter", "WinterMig", "Winter2", "SpringMig"),
                     labels=c("Breeding", "Fall migration", "1st Wintering", "Winter migration", "2nd Wintering", "Spring migration"))
labels <- covs %>% 
  arrange(ID) %>% 
  dplyr::select(label, ID) %>% 
  unique() %>% 
  group_by(label) %>% 
  mutate(order=row_number(),
         max=max(order)) %>% 
  ungroup() %>% 
  mutate(label=ifelse(order==max, as.character(label), ""),
         roworder=row_number())

plot1 <- ggplot(covs) +
  geom_point(aes(x=doy.order, y=factor(ID), colour=Season), size=2) +
  ylab("") +
  xlab("") +
  my.theme +
  scale_colour_viridis_d(name="", option="D") +
  scale_y_discrete(labels=labels$label) +
  scale_x_continuous(breaks=c(22, 113, 203, 295),
                     labels=c("Sep", "Dec", "Mar", "June")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(angle=0)) +
  theme(plot.margin = unit(c(0,0,-0.6,-0.6), "cm"),
        legend.position = "none") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

plot2 <- ggplot(covs) +
  geom_point(aes(x=doy.order, y=factor(ID), colour=Season, alpha = factor(use)), size=2) +
  ylab("") +
  xlab("") +
  my.theme +
  scale_colour_viridis_d(name="", option="D") +
  scale_alpha_manual(values=c(0,100)) +
  scale_y_discrete(labels=labels$label) +
  scale_x_continuous(breaks=c(22, 113, 203, 295),
                     labels=c("Sep", "Dec", "Mar", "June")) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(angle=0)) +
  theme(plot.margin = unit(c(0,0,-0.6,-0.6), "cm"),
        legend.position = "none") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

plot1
plot2

legend <- get_legend(plot1 + theme(legend.position="bottom",
                                   legend.text=element_text(size=14)))
ggsave(plot = grid.arrange(plot1, legend, 
                           widths = c(1),
                           heights = c(1, 0.1),
                           layout_matrix = rbind(c(1),
                                                 c(2))),
       "Figures/AllDataPoints.jpeg",
       width=8, height=9, units="in", device="jpeg")

legend <- get_legend(plot1 + theme(legend.position="bottom",
                                   legend.text=element_text(size=14)))
ggsave(plot = grid.arrange(plot2, legend, 
                           widths = c(1),
                           heights = c(1, 0.1),
                           layout_matrix = rbind(c(1),
                                                 c(2))),
       "Figures/HabitatDataPoints.jpeg",
       width=8, height=9, units="in", device="jpeg")

#8. KDE----

#8a. Example----
#Read in
kd.shp <- read_sf("Shapefiles/ExampleKDE.shp")
dat.kde.i <- read_sf("Shapefiles/ExampleKDEData.csv")

#Wrangle
kd.shp.breed <- kd.shp %>% 
  dplyr::filter(Season=="Breed2")
dat.kde.i.breed <- dat.kde.i %>% 
  dplyr::filter(SeasonR2n=="Breed2") %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  cbind(dat.kde.i %>% 
          dplyr::filter(SeasonR2n=="Breed2"))

kd.shp.winter <- kd.shp %>% 
  dplyr::filter(Season=="Winter2")
dat.kde.i.winter <- dat.kde.i %>% 
  dplyr::filter(SeasonR2n=="Winter2") %>% 
  st_as_sf(coords=c("Long", "Lat"), crs=4326) %>% 
  st_transform(crs=3857) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  cbind(dat.kde.i %>% 
          dplyr::filter(SeasonR2n=="Winter2"))

#plot
breed <- ggplot() +
  geom_sf(data=kd.shp.breed, aes(fill=iso), show.legend=FALSE) +
  geom_point(data=dat.kde.i.breed, aes(x=X, y=Y), size=2) +
  scale_fill_viridis_c(name="Isopleth") +
  ggsn::scalebar(data=kd.shp.breed,
                 transform=FALSE, model="WGS84",
                 dist=5, dist_unit="km",
                 box.fill=c("grey80", "grey20"),
                 box.color="grey20",
                 st.dist = 0.04,
                 height = 0.05,
                 location="bottomleft") +
  theme_void() +
  theme(plot.title = element_text(size=20, hjust=0.5)) +
  ggtitle("Individual 825\n(male, Alberta)")
breed

winter <- ggplot() +
  geom_sf(data=kd.shp.winter, aes(fill=iso), show.legend = TRUE) +
  geom_point(data=dat.kde.i.winter, aes(x=X, y=Y), size=2) +
  scale_fill_viridis_c(name="Isopleth") +
  ggsn::scalebar(data=kd.shp.winter,
                 transform=FALSE, model="WGS84",
                 dist=50, dist_unit="m",
                 box.fill=c("grey80", "grey20"),
                 box.color="grey20",
                 st.dist = 0.04,
                 height = 0.08,
                 location="bottomleft") +
  theme_void() +
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        plot.title = element_text(size=20, hjust=0.5))
winter

kde <- grid.arrange(breed, winter,
                         widths = c(6),
                         heights = c(6, 4),
                         layout_matrix = rbind(c(1),
                                               c(2)))

#8b. Area histogram----
dat.area <- read.csv("KDEArea.csv") %>% 
  rename(n=count)
hist <- ggplot(dat.area) +
  geom_histogram(aes(x=radius, fill=SeasonKDE), show.legend=FALSE) +
  facet_grid(SeasonKDE~.) +
  my.theme +
  xlab("Roosting home range radius from 95% isopleth (km)") +
  theme(axis.title.y = element_blank()) +
  scale_fill_manual(values=c("goldenrod3", "cyan4"), name="Season")

#8c. Put together----
ggsave(plot=grid.arrange(kde, hist,
                         widths = c(6, 6),
                         layout_matrix = rbind(c(1, 2))), "Figures/KDEExample.jpeg", height = 8, width = 8, units="in", device="jpeg")

#9. Example RSF points----
#Read in
covs <- read.csv("Covariates_Breed&Winter_Metadata.csv") %>% 
  dplyr::filter(PinpointID==455,
                doy==162) %>% 
  st_as_sf(coords=c("X", "Y"), crs=3857) %>% 
  dplyr::select(PinpointID) %>% 
  mutate(Type="Used")

covs.coords <- covs %>% 
  st_transform(crs=4326) %>% 
  st_coordinates()

#Create 100 km buffer 
covs.100 <- covs %>% 
  st_buffer(dist=100000)

write_sf(covs.100, "Figures/ExampleRSFExtent.shp")

#Create 20 random points
covs.rand <- st_sample(covs.100, size=20) %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(PinpointID=455,
         Type="Available") %>% 
  st_as_sf(coords=c("X", "Y"), crs=3857)

#Bind together and buffer
covs.all <- rbind(covs, covs.rand)

write_sf(covs.all, "Figures/ExampleRSFPoints.shp")

#Buffer by 10 km
covs.10 <- covs.all %>% 
  st_buffer(dist=10000)

write_sf(covs.10, "Figures/ExampleRSFBufferedPoints.shp")

#Move everything over to 4326
covs.10.wgs <- covs.10 %>% 
  st_transform(crs=4326)

covs.100.wgs <- covs.100 %>% 
  st_transform(crs=4326)

covs.all.wgs <- covs.all %>% 
  st_transform(crs=4326) %>% 
  st_coordinates() %>% 
  cbind(covs.all %>% 
          data.frame() %>% 
          dplyr::select(PinpointID, Type))
  
#Get background data
register_google(key="AIzaSyCta9P4x7jGNELznpwlx07VZkkLVk3FP4M")

map <- get_map(covs.coords, zoom=8, force=TRUE, maptype="satellite", color="color")

map_attributes <- attributes(map)

map_transparent <- matrix(adjustcolor(map, 
                                      alpha.f = 0.8), 
                          nrow = nrow(map))
attributes(map_transparent) <- map_attributes

#Map
map.site <- ggmap(map_transparent) +
  geom_point(aes(x = X, y = Y, colour=factor(Type)),
                     data = covs.all.wgs, 
                     alpha = 0.7,
                     show.legend = FALSE) +
  geom_sf(data=covs.100.wgs, fill=NA) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_orienteering(fill = c("grey80", "grey20"), line_col = "grey20")) +
  ggsn::scalebar(x.min = -111.75, x.max = -111.55, 
                 y.min = 57.38, y.max = 57.7, 
                 transform=TRUE, model="WGS84",
                 dist=5, dist_unit="km",
                 box.fill=c("grey80", "grey20"),
                 box.color="grey20") +
  coord_sf(crs=4326) +
  my.theme +
  xlab("") +
  ylab("") +
  theme(plot.margin = unit(c(0.5,0,0,0), "cm"),
        legend.position = "right")
#map.site

ggsave(plot=map.site, "Figures/RSFExample.jpeg", width=8, height=8, units="in", device="jpeg")
