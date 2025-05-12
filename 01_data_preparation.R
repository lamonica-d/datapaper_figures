##############################################################
###                     DATA PREPARATION                   ###
##############################################################

library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(vegan)
library(maps)
library(ggplot2)
library(terra)
library(sf)
library(gridExtra)
source("R/reineke_index_computation.R")

## load raw data
plots_all <- tibble(read.csv("data_raw/plots.csv", sep = "\t"))
trees_all <- tibble(read.csv("data_raw/trees.csv", sep = "\t"))

## remove PCQ sampling & plot SP* & plot PSE5B-10B & plot TRIE5
trees <- trees_all %>% 
  filter(subpl_type != "point" & subpl_type != "quarter") %>%
  filter(plot_label != "SPO") %>% 
  filter(plot_label != "PSE5B-10B") %>%
  filter(plot_label != "SPM") %>%
  filter(plot_label != "SPN") %>%
  filter(plot_label != "SPP") %>%
  filter(plot_label != "TRIE5")

plots <- plots_all %>% 
  filter(is.na(area) != T) %>%
  filter(plot_label != "SPO") %>% 
  filter(plot_label != "PSE5B-10B") %>%
  filter(plot_label != "SPM") %>%
  filter(plot_label != "SPN") %>%
  filter(plot_label != "SPP") %>%
  filter(plot_label != "TRIE5")

## remove trees without species id & trees < dbh 10cm
trees <- trees %>%
  filter(dbh1 >= 10 | dbh2 >= 10 | (is.na(dbh1)&is.na(dbh2))) %>%
  filter(species != "")

## remove plots under 0.2 ha
small_plots <- plots$plot_label[which(plots$area < 0.2)]

trees <- trees %>% 
  filter(!(plot_label %in% small_plots))

plots <- plots %>% 
  filter(!(plot_label %in% small_plots))

## community dataframe
community1 <- tibble(plot = trees$plot_label, species = trees$species)
species_vect <- unique(community1$species)
plot_vect <- unique(plots$plot_label)
 
community <- matrix(0, ncol = length(species_vect), nrow = length(plot_vect))
for (i in 1:length(plot_vect)){
  temp <- community1 %>%
    filter(plot == plot_vect[i])
  area <- plots[plots$plot_label == plot_vect[i],]$area
  
  for (j in 1:length(species_vect)){
    community[i,j] <- sum(temp$species == species_vect[j])
  }
  community[i,] <- ceiling(community[i,]/area)
}
community[is.na(community)] <- 0
colnames(community) <- species_vect
rownames(community) <- plot_vect

## 15 species that are the most represented across plots
species_in_plots <- apply(X = community, MARGIN = 2, FUN = function(x) sum(x>0))
most_rep_sp <- sort(species_in_plots, decreasing = T)[1:15]
df_most_rep_sp_abondance <- as.data.frame(community) %>%
  select(names(most_rep_sp))
df_most_rep_sp_presence <- ifelse(df_most_rep_sp_abondance == 0, 0, 1)
df_most_rep_sp_presence_count <- tibble(species = names(most_rep_sp), 
                                        nb_plot = colSums(df_most_rep_sp_presence),
                                        percent = colSums(df_most_rep_sp_presence)/length(plot_vect)*100)

## Reineke index computation
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
df_interpol <- cbind(plots[,c(1,4:5,10)], 
                    nb_tree_per_ha = plots$dbh10_inv1/plots$area,
                    nb_sp_per_ha = round(specnumber(community)/plots$area,digits =0), 
                    fisher_alpha = fisher.alpha(community),
                    shannon_index = diversity(community, index = "shannon"),
                    reineke_index = reineke_index_vect)

df_figures <- df_interpol %>% 
  pivot_longer(c(nb_tree_per_ha, nb_sp_per_ha, fisher_alpha, shannon_index, reineke_index),
               names_to = "index", values_to = "values")

saveRDS(df_interpol, "outputs/table_for_interpolation.RDS")
saveRDS(df_figures, "outputs/table_for_figures.RDS")
saveRDS(community, "outputs/community_table.RDS")
saveRDS(df_most_rep_sp_presence_count, "outputs/table_for_most_rep_sp.RDS")





