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


## load & prepare data
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

## remove trees without species id & trees < dbh 10cm
trees_all <- trees_all %>%
  filter(dbh1 >= 10 | dbh2 >= 10 | (is.na(dbh1)&is.na(dbh2))) %>%
  filter(species != "")

## REMOVE PLOTS WITH AREA < 0.2 !!!!!!
small_plots <- plots_all$plot_label[which(plots_all$area < 0.2)]

trees_all <- trees_all %>% 
  filter(!(plot_label %in% small_plots))

plots_all <- plots_all %>% 
      filter(!(plot_label %in% small_plots))

## tibbles to keep/save
trees <- trees_all[,c(1:2,17,23,27)]
plots <- plots_all[,c(1,4:5,10:11,17:18,21:22)]

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

# calcul Reineke index

## table for figures
df_figures <- cbind(plots, nb_tree_per_ha = plots$dbh10_inv1/plots$area,
                    nb_sp_per_ha = specnumber(community_round), 
                    fisher_alpha = fisher.alpha(community_round),
                    shannon_index = diversity(community_round, index = "shannon"))

saveRDS(df_figures, "data/table_for_figures")

## build grid for interpolation
world <- map_data("world")
french_guyana <- world %>%
  filter(region == "French Guiana")

AmazonForestGrid <- read.table("data/AmazonLowLandForestRaisg.csv", header = T,
                               sep = ",", row.names = 1)[,1:2]
colnames(AmazonForestGrid) <- c("long_dd", "lat_dd")

## add les carres vides 
# truc <-  AmazonForestGrid %>%
#    filter(long_dd < -52 & long_dd > -53.5) %>%
#    filter(lat_dd < 5 & lat_dd > 4.3) 
# truc <- rast(cbind(truc, value = rep(1, nrow(truc))))
# plot(truc)

region_grid <- rbind(AmazonForestGrid, c(-53.15,4.45), c(-53.15,4.95), 
                          c(-53.05,4.95), c(-52.75,4.65), c(-52.55,4.65),c(-52.45,4.65)
)
region_grid <- cbind(region_grid, value = rep(1, nrow(region_grid)))
temp <- rast(region_grid)
polygon <- french_guyana[,1:2] %>%
  st_as_sf(coords = c("long", "lat")) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
temp1 <- mask(temp, polygon)
temp2 <- disagg(temp1, fact = 16)
region_grid_finer <- crds(temp2, df=T)
colnames(region_grid_finer) <- c("long_dd", "lat_dd")

saveRDS(region_grid_finer, "data/grid_for_interpolation")

