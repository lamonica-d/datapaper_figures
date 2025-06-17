##############################################################
###             SMALL PLOTS & NUMBER OF SPECIES            ###
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
plots_all <- tibble(read.csv("data_raw/plots.csv", sep = ","))
trees_all <- tibble(read.csv("data_raw/trees.csv", sep = ","))

plots <- plots_all %>% 
  filter(type == "exhaustive") %>%
  filter(!(plot_label %in% c("SPC", "SPD", "SPM", "SPN", "SPO", "SPP"))) %>% 
  filter(plot_label != "TRIES5") %>%
  filter(plot_label != "PSE5B-10B")

trees <- trees_all %>%
  filter(plot_label %in% unique(plots$plot_label))

## remove trees without species id & trees < dbh 10cm
trees <- trees %>%
  filter(dbh_before_2000 >= 10 | dbh_after_2000 >= 10) %>%
  filter(!is.na(species)) %>%
  filter(!is.na(genus))

trees <- cbind(trees, genus_species =  str_c(trees$genus, trees$species, sep = "_"))

## community dataframe
community1 <- tibble(plot = trees$plot_label,
                     species = trees$genus_species)
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

## figure
data <- tibble(plot_label = plot_vect,
               area = plots$area,
               nb_sp_per_ha = round(specnumber(community)/plots$area,digits =0)
)

pdf(file = "figures/small_plots.pdf", height = 8, width = 8)
plot(data$area, data$nb_sp_per_ha, xlab = "Plot area (ha)", ylim = c(0,705),
     ylab = "Number of species per ha", pch = 16, las = 1)
abline(v = 0.2, lty = 2, col = "red")
abline(lm(data$nb_sp_per_ha ~ data$area), col = "DarkGrey")
dev.off()

