##Figures

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

world <- map_data("world")
french_guyana <- world %>%
  filter(region == "French Guiana")
  
plots_all <- tibble(read.csv("data/plots.csv", sep = "\t"))
trees_all <- tibble(read.csv("data/trees.csv", sep = "\t"))
AmazonForestGrid <- read.table("data/AmazonLowLandForestRaisg.csv", header = T,
                               sep = ",", row.names = 1)[,1:2]
colnames(AmazonForestGrid) <- c("long_dd", "lat_dd")

## remove PCQ sampling & plot SPO
trees_all <- trees_all %>% 
  filter(subpl_type != "point" & subpl_type != "quarter") %>%
  filter(plot_label != "SPO")

plots_all <- plots_all %>% 
  filter(is.na(area) != T) %>%
  filter(plot_label != "SPO")

trees <- trees_all[,c(1:2,17,23,27)]
plots <- plots_all[,c(1,4:5,10:11,17:18,21:22)]

## remove trees without species id & trees < dbh 10cm
trees <- trees %>%
  filter(dbh1 >= 10 | dbh2 >= 10 | (is.na(dbh1)&is.na(dbh2))) %>%
  filter(species != "")

## REMOVE PLOTS WITH AREA < 0.2 !!!!!!
trees_all <- trees_all %>% 
  filter(plot_label != "SPO")

plots_all <- plots_all %>% 
  filter(is.na(area) != T) %>%
  filter(plot_label != "SPO")

## a faire

## build grid for interpolation
region_grid <- AmazonForestGrid %>%
  filter(long_dd < max(french_guyana$long + 0.1) & long_dd > min(french_guyana$long - 0.1)) %>%
  filter(lat_dd < max(french_guyana$lat + 0.1) & lat_dd > min(french_guyana$lat - 0.1)) 

region_grid <- cbind(region_grid, value = rep(1, nrow(region_grid)))

# region_grid_finer <- tibble_row()
# 
# long_vect <- unique(region_grid$long_dd)
# for (i in 1:length(long_vect)){
#   temp <- region_grid %>%
#     filter(long_dd == long_vect[i])
#  
#   region_grid_finer <- rbind(region_grid_finer,  
#    tibble(rep(long_vect[i], length(seq(min(temp$lat_dd), max(temp$lat_dd), by = .05))), 
#           seq(min(temp$lat_dd), max(temp$lat_dd), by = .05))
#    )
# }
# colnames(region_grid_finer) <- c("long_dd", "lat_dd")
# 
# new_long_vect <- seq(min(region_grid_finer$long_dd), max(region_grid_finer$long_dd), by = .05)

##OU
temp <- rast(region_grid)

polygon <- french_guyana[,1:2] %>%
  st_as_sf(coords = c("long", "lat")) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
temp1 <- mask(temp, polygon)
temp2 <- disagg(temp1, fact = 16)
region_grid_finer <- crds(temp2, df=T)
colnames(region_grid_finer) <- c("long_dd", "lat_dd")

## community dataframe
community1 <- tibble(plot = trees$plot_label, species = trees$species)
species_vect <- unique(community1$species)
plot_vect <- unique(plots$plot_label)

nb_sp_per_ha <- as.numeric()
community_round <- matrix(0, ncol = length(species_vect), nrow = length(plot_vect))
for (i in 1:length(plot_vect)){
  temp <- community1 %>%
    filter(plot == plot_vect[i])
  area <- plots[plots$plot_label == plot_vect[i],]$area
  for (j in 1:length(species_vect)){
  community_round[i,j] <- sum(temp$species == species_vect[j])
  }
  nb_sp_per_ha[i] <- length(which(community_round[i,]>0))/area
}
community_round[is.na(community_round)] <- 0

## species acc computation and plot
sac <- specaccum(community_round)
plot(sac, ci.type="polygon", ci.col="lightgreen")

## table
plots <- cbind(plots, nb_sp_per_ha = nb_sp_per_ha, 
               nb_tree_per_ha = plots$dbh10_inv1/plots$area,
               fisher_alpha = fisher.alpha(community_round),
               shannon_index = diversity(community_round, index = "shannon"))

## diversity & fisher's alpha & shannon index per plot
par(mfrow = c(2,2))
hist(df_figures$nb_tree_per_ha, main = "Number of trees per ha", xlab = "")
hist(df_figures$nb_sp_per_ha, main = "Number of species per ha", ylab="", xlab = "")
hist(df_figures$fisher_alpha, main = "Fisher's alpha index", xlab = "")
hist(df_figures$shannon_index, main = "Shannon index", ylab="", xlab = "")


## loess models

df_plot_variable_list <- list()
for (i in 1:4){
  index_in_plots <- 9 + i
  table_temp <- plots[,c(1:3,index_in_plots)]
  colnames(table_temp)[4] <- "variable"
  
z.loess <- loess(variable ~ long_dd * lat_dd, data = table_temp,
                span = 0.75, degree = 2, se = T,
                normalize = TRUE, family = "gaussian",
                surface = "direct")

grid.z.predict <- predict(z.loess, region_grid_finer, se = T)
min_variable <- min(table_temp$variable)
max_variable <- max(table_temp$variable)

grid.z.predict$fit[grid.z.predict$fit < min_variable] <- min_variable
grid.z.predict$fit[grid.z.predict$fit > max_variable] <- max_variable

df_plot_variable_list[[i]] <- cbind(region_grid_finer, variable = grid.z.predict)

# grid.min = min(grid.z.predict$fit, na.rm = T)
# grid.max = max(grid.z.predict$fit, na.rm = T)
# 
# n.colors = 256
# grid.pal = heat.colors(n.colors, alpha = 1)
# 
# grid.col   = vector(length = length(region_grid$long_dd))
# grid.range = grid.max - grid.min
# grid.col   = 1 -(grid.z.predict$fit - grid.min)/grid.range
# grid.col   = grid.pal[1+round((n.colors-1)*(grid.z.predict$fit - grid.min)/grid.range)]

}

variable_names <- c("Nb of trees per ha", "Nb of species per ha", 
                    "Fisher's alpha index", "Shannon index")
for (i in 1:4){
maps_plot[[i]] <- ggplot() +
  coord_fixed(1) +
  geom_raster(data = df_plot_variable_list[[i]], aes(x=long_dd, y=lat_dd, 
                                                     fill = variable.fit)) +
  scale_fill_viridis_c() +
  geom_point(data = plots, aes(x=long_dd, y=lat_dd)) +
  theme_minimal()+
  labs(fill = variable_names[i])+
  geom_polygon(data = french_guyana, aes(x=long, y = lat), fill=NA, colour="red")
}

pdf(file = "figures/all_maps.pdf", height = 8, width = 8)
grid.arrange(maps_plot[[1]], maps_plot[[2]],
             maps_plot[[3]], maps_plot[[4]], ncol = 2)
dev.off()





