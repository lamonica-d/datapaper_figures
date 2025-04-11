##############################################################
###               build grid for interpolation             ###
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
