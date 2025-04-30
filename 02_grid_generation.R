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

## load data
world <- map_data("world")
french_guiana <- world %>%
  filter(region == "French Guiana")

AmazonForestGrid <- read.table("data_raw/AmazonLowLandForestRaisg.csv", header = T,
                               sep = ",", row.names = 1)[,1:2]
colnames(AmazonForestGrid) <- c("long_dd", "lat_dd")

## add 6 missing pixels
region_grid <- rbind(AmazonForestGrid, c(-53.15,4.45), c(-53.15,4.95), 
                     c(-53.05,4.95), c(-52.75,4.65), c(-52.55,4.65),c(-52.45,4.65)
)

## only french guiana
region_grid <- cbind(region_grid, value = rep(1, nrow(region_grid)))
temp <- rast(region_grid)
polygon <- french_guiana[,1:2] %>%
  st_as_sf(coords = c("long", "lat")) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
temp1 <- mask(temp, polygon)

## finer resolution
temp2 <- disagg(temp1, fact = 4)
region_grid_finer <- crds(temp2, df=T)
colnames(region_grid_finer) <- c("long_dd", "lat_dd")

saveRDS(region_grid_finer, "outputs/grid_for_interpolation.RDS")
