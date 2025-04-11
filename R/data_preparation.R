##############################################################
###                     DATA PREPARATION                   ###
##############################################################

library(stringr)
library(dplyr)
library(tibble)
library(vegan)
library(maps)
library(ggplot2)
library(terra)
library(sf)
library(gridExtra)

## load raw data
plots_all <- tibble(read.csv("data/plots.csv", sep = "\t"))
trees_all <- tibble(read.csv("data/trees.csv", sep = "\t"))

## remove PCQ sampling & plot SPO & plot PSE5B-10B
trees_all <- trees_all %>% 
  filter(subpl_type != "point" & subpl_type != "quarter") %>%
  filter(plot_label != "SPO") %>% 
  filter(plot_label != "PSE5B-10B")

plots_all <- plots_all %>% 
  filter(is.na(area) != T) %>%
  filter(plot_label != "SPO") %>% 
  filter(plot_label != "PSE5B-10B")

## remove plots under 0.2 ha
small_plots <- plots_all$plot_label[which(plots_all$area < 0.2)]

trees_all <- trees_all %>% 
  filter(!(plot_label %in% small_plots))

plots_all <- plots_all %>% 
  filter(!(plot_label %in% small_plots))

## remove trees without species id & trees < dbh 10cm
trees_all <- trees_all %>%
  filter(dbh1 >= 10 | dbh2 >= 10 | (is.na(dbh1)&is.na(dbh2))) %>%
  filter(species != "")

## community dataframe
community1 <- tibble(plot = trees$plot_label, species = trees$species)
species_vect <- unique(community1$species)
plot_vect <- unique(plots$plot_label)

#nb_sp_per_ha <- as.numeric()
community_round <- matrix(0, ncol = length(species_vect), nrow = length(plot_vect))
for (i in 1:length(plot_vect)){
  temp <- community1 %>%
    filter(plot == plot_vect[i])
  area <- plots[plots$plot_label == plot_vect[i],]$area
  for (j in 1:length(species_vect)){
    community_round[i,j] <- sum(temp$species == species_vect[j])
  }
  community_round[i,] <- round(community_round[i,]/area, digits = 0)
  #nb_sp_per_ha[i] <- length(which(community_round[i,]>0))
}
community_round[is.na(community_round)] <- 0

## Reineke index computation
trees <- trees_all[,c(1:2,17,23,27)]
reineke_index_vect <- as.numeric()
for (i in 1:length(plot_vect)){
  area <- plots[plots$plot_label == plot_vect[i],]$area
  temp <- trees %>%
    filter(plot_label == plot_vect[i]) %>%
    filter(!(is.na(dbh1)&is.na(dbh2)))
 
  ## when no dbh1 replace by dbh2
  temp$dbh1[which(is.na(temp$dbh1))] <- temp$dbh2[which(is.na(temp$dbh1))]
  
  reineke_index_vect[i] <- reineke_index(diameters = temp$dbh1, area = area)
}

## table for figures
df_figures <- cbind(plots_all[,c(1,4:5,10:11,17:18,21:22)], 
                    nb_tree_per_ha = plots$dbh10_inv1/plots$area,
                    nb_sp_per_ha = specnumber(community_round), 
                    fisher_alpha = fisher.alpha(community_round),
                    shannon_index = diversity(community_round, index = "shannon"),
                    reineke_index = reineke_index_vect)

saveRDS(df_figures, "data/table_for_figures")
saveRDS(community_round, "data/community_table")



