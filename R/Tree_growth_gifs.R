# Script to make cross sectional images for the tree ring data:
library(dplR)
library(tidyverse)
all.trees.info <- read.csv( "subset.musical.trees.info.csv")

PIPO.q.rwl.wide <- read.csv( paste0("outputs/random_100_sorted/PIPO_AZ_rwl_wide.csv"))
PIED.q.rwl.wide <- read.csv( paste0("outputs/random_100_sorted/PIED_AZ_rwl_wide.csv"))
PSME.q.rwl.wide<- read.csv( paste0("outputs/random_100_sorted/PSME_AZ_rwl_wide.csv"))

PIPO.q.rwl.wide <- PIPO.q.rwl.wide %>% filter(series %in% all.trees.info$TREE.ID.)
colnames(PIPO.q.rwl.wide)[2:length(PIPO.q.rwl.wide)] <- 1719:1997
PIPO.q.rwl.m <- reshape::melt(PIPO.q.rwl.wide, id.vars = "series")
colnames(PIPO.q.rwl.m) <- c("series","year", "rwl")

PIED.q.rwl.wide <- PIED.q.rwl.wide %>% filter(series %in% all.trees.info$TREE.ID.)
colnames(PIED.q.rwl.wide)[2:length(PIED.q.rwl.wide)] <- 1895:1996
PIED.q.rwl.m <- reshape::melt(PIED.q.rwl.wide, id.vars = "series")
colnames(PIED.q.rwl.m) <- c("series","year", "rwl")

PSME.q.rwl.wide <- PSME.q.rwl.wide %>% filter(series %in% paste0(0,all.trees.info$TREE.ID.))
colnames(PSME.q.rwl.wide)[2:length(PSME.q.rwl.wide)] <- 1772:1997
PSME.q.rwl.m <- reshape::melt(PSME.q.rwl.wide, id.vars = "series")
colnames(PSME.q.rwl.m) <- c("series","year", "rwl")

PIED.1 <- PIED.q.rwl.m %>% filter(series %in% "1058501")
PIED.2 <- PIED.q.rwl.m %>% filter(series %in% "1060201")
PIED.3 <- PIED.q.rwl.m %>% filter(series %in% "1060204"& !is.na(rwl))
PIED.4 <- PIED.q.rwl.m %>% filter(series %in% "5177904")
PIED.5 <- PIED.q.rwl.m %>% filter(series %in% "11001304")
PIED.6 <- PIED.q.rwl.m %>% filter(series %in% "17095001")


PIPO.1 <- PIPO.q.rwl.m %>% filter(series %in% "X61" & !is.na(rwl))
PIPO.2 <- PIPO.q.rwl.m %>% filter(series %in% "X154" & !is.na(rwl))
PIPO.3 <- PIPO.q.rwl.m %>% filter(series %in% "X190" & !is.na(rwl))



PSME.1 <- PSME.q.rwl.m %>% filter(series %in% "01112001" & !is.na(rwl))
PSME.2 <- PSME.q.rwl.m %>% filter(series %in% "03333040" & !is.na(rwl))
PSME.3 <- PSME.q.rwl.m %>% filter(series %in% "07015504" & !is.na(rwl))

source("R/plotRings2.R")
# Six PIED trees:
plotRings2(year = as.integer(as.character(PIED.1$year)), trwN= PIED.1$rwl,  saveGIF = TRUE, species = "Pinus edulis", treeid = "1058501", fname = "PIED_1058501_growth")
plotRings2(year = as.integer(as.character(PIED.2$year)), trwN= PIED.2$rwl,  saveGIF = TRUE, species = "Pinus edulis", treeid = "1060201", fname = "PIED_1060201_growth")
plotRings2(year = as.integer(as.character(PIED.3$year)), trwN= PIED.3$rwl,  saveGIF = TRUE, species = "Pinus edulis", treeid = "1060204",fname = "PIED_1060204_growth")
plotRings2(year = as.integer(as.character(PIED.4$year)), trwN= PIED.4$rwl,  saveGIF = TRUE, species = "Pinus edulis", treeid = "5177904", fname = "PIED_5177904_growth")
plotRings2(year = as.integer(as.character(PIED.5$year)), trwN= PIED.5$rwl,  saveGIF = TRUE, species = "Pinus edulis", treeid = "11001304", fname = "PIED_11001304_growth")
plotRings2(year = as.integer(as.character(PIED.6$year)), trwN= PIED.6$rwl,  saveGIF = TRUE, species = "Pinus edulis", treeid = "17095001",fname = "PIED_17095001_growth")

#

# three PIPO trees:
plotRings2(year = as.integer(as.character(PIPO.1$year)), trwN= PIPO.1$rwl,  saveGIF = TRUE, species = "Pinus ponderosa", treeid = "X61", fname = "PIPO_X61_growth")
plotRings2(year = as.integer(as.character(PIPO.2$year)), trwN= PIPO.2$rwl,  saveGIF = TRUE, species = "Pinus ponderosa", treeid = "X154", fname = "PIPO_X154_growth")
plotRings2(year = as.integer(as.character(PIPO.3$year)), trwN= PIPO.3$rwl,  saveGIF = TRUE, species = "Pinus ponderosa", treeid = "X190", fpath = "growth_gifs",fname = "PIPO_X190_growth")
#plotRings2(year = PIED.1$year, trwN= PIPO.4$rwl,  saveGIF = TRUE, species = "Pinus ponderosa", treeid = "1058501", fname = "PIED_1058501_growth.gif")


# Three PSME trees:
plotRings2(year = as.integer(as.character(PSME.1$year)), trwN = PSME.1$rwl,  saveGIF = TRUE, species = "Pseudotsuga menziesii", treeid = "1112001", fname = "PSME_1112001_growth")
plotRings2(year = as.integer(as.character(PSME.2$year)), trwN = PSME.2$rwl,  saveGIF = TRUE, species = "Pseudotsuga menziesii", treeid = "3333040", fname = "PSME_3333040_growth")
plotRings2(year = as.integer(as.character(PSME.3$year)), trwN = PSME.3$rwl,  saveGIF = TRUE, species = "Pseudotsuga menziesii", treeid = "7015504", fpath = "growth_gifs",fname = "PSME_70155040_growth")

#---------------------------------------------------------------
# now make timeseries plots of all the tree rings:
#---------------------------------------------------------------
kellystheme <- theme_bw(base_size = 14)+theme(panel.grid = element_blank())

PIPO.1.plt <- ggplot(PIPO.1, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus ponderosa",unique(PIPO.1$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIPO.2.plt <- ggplot(PIPO.2, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus ponderosa",unique(PIPO.2$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIPO.3.plt <- ggplot(PIPO.3, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus ponderosa",unique(PIPO.3$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme

PIED.1.plt <- ggplot(PIED.1, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus edulis",unique(PIED.1$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIED.2.plt <- ggplot(PIED.2, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus edulis",unique(PIED.2$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIED.3.plt <- ggplot(PIED.3, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus edulis",unique(PIED.3$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIED.4.plt <- ggplot(PIED.4, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus edulis",unique(PIED.4$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIED.5.plt <- ggplot(PIED.5, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus edulis",unique(PIED.5$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PIED.6.plt <- ggplot(PIED.6, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pinus eduils",unique(PIED.6$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme

PSME.1.plt <- ggplot(PSME.1, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pseudotsuga menziesii",unique(PSME.1$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PSME.2.plt <- ggplot(PSME.2, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pseudotsuga menziesii",unique(PSME.2$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme
PSME.3.plt <- ggplot(PSME.3, aes(x=as.integer(as.character(year)), y = rwl))+geom_line()+ggtitle(paste("Pseudotsuga menziesii",unique(PSME.3$series)))+ylab("Tree Growth Incrment (mm)")+xlab("Year")+kellystheme

ggsave("Growth_timeseries_plots/PIPO.X61.png", PIPO.1.plt, device = "png")
ggsave("Growth_timeseries_plots/PIPO.X154.png", PIPO.2.plt, device = "png")
ggsave("Growth_timeseries_plots/PIPO.X190.png", PIPO.3.plt, device = "png")


ggsave("Growth_timeseries_plots/PIED.1058501.png", PIED.1.plt, device = "png")
ggsave("Growth_timeseries_plots/PIED.1060201.png", PIED.2.plt, device = "png")
ggsave("Growth_timeseries_plots/PIED.1060204.png", PIED.3.plt, device = "png")
ggsave("Growth_timeseries_plots/PIED.5177904.png", PIED.4.plt, device = "png")
ggsave("Growth_timeseries_plots/PIED.11001304.png", PIED.5.plt, device = "png")
ggsave("Growth_timeseries_plots/PIED.17095001.png", PIED.6.plt, device = "png")


ggsave("Growth_timeseries_plots/PSME.1112001.png", PSME.1.plt, device = "png")
ggsave("Growth_timeseries_plots/PSME.3333040.png", PSME.2.plt, device = "png")
ggsave("Growth_timeseries_plots/PSME.7015504.png", PSME.3.plt, device = "png")
