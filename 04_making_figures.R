##############################################################
###                     PLOT GENERATION                    ###
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

## load data
world <- map_data("world")
french_guyana <- world %>%
  filter(region == "French Guiana")

community_table <- readRDS("outputs/community_table.RDS")
df_figures <- readRDS("outputs/table_for_figures.RDS")
df_maps_list <- readRDS("outputs/list_table_for_maps.RDS")

## species acc computation & plot
sac <- specaccum(community_table)
pdf(file = "figures/species_acc_v3.pdf", height = 8, width = 8)
par(mfrow = c(1,1))
plot(sac, ci.type="polygon", ci.col="lightblue", ylab = "Cumulative number of species",
     xlab = "Sites", main = "Species accumulation curve")
dev.off()

## hist + maps of indexes
hist(df_figures$nb_tree_per_ha, main = "Number of trees per ha", xlab = "")
hist(df_figures$nb_sp_per_ha, main = "Number of species per ha",  xlab = "")
hist(df_figures$fisher_alpha, main = "Fisher's alpha index", xlab = "")
hist(df_figures$shannon_index, main = "Shannon index",  xlab = "")
hist(df_figures$reineke_index, main = "Reineke index",  xlab = "")


## maps of interpolated variables & plots

variable_names <- c("Nb of trees per ha", "Nb of species per ha", 
                    "Fisher's alpha index", "Shannon index", "Reineke index")

maps_plot <- list()
for (i in 1:length(variable_names)){
maps_plot[[i]] <- ggplot() +
  coord_fixed(1) +
  geom_raster(data = df_maps_list[[i]], aes(x=long_dd, y=lat_dd, 
                                                     fill = variable.fit)) +
  scale_fill_viridis_c() +
  geom_point(data = df_figures, aes(x=long_dd, y=lat_dd)) +
  theme_minimal()+
  labs(fill = variable_names[i])+
  geom_polygon(data = french_guyana, aes(x=long, y = lat), fill=NA, colour="red")
}

pdf(file = "figures/all_maps_v2.pdf", height = 8, width = 8)
grid.arrange(maps_plot[[1]], maps_plot[[2]],
             maps_plot[[3]], maps_plot[[4]], maps_plot[[5]], ncol = 3)
dev.off()





