#######################################################
###                SIMPSON INDEX                    ###
#######################################################

## all plots

library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(vegan)
library(maps)
library(ggplot2)
library(gridExtra)
library(cowplot)

#1) DATA PREPARATION

## load raw data
world <- map_data("world")
french_guiana <- world %>%
  filter(region == "French Guiana")

plots_all <- tibble(read.csv("data_raw/plots.csv", sep = ","))
trees_all <- tibble(read.csv("data_raw/trees.csv", sep = ","))

plots <- plots_all 

## remove trees without species id & trees < dbh 10cm
trees <- trees_all %>%
  filter(dbh_before_2000 >= 10 | dbh_after_2000 >= 10) %>%
  filter(!is.na(species)) 

trees <- cbind(trees, fam_gen_sp =  str_c(trees$family, trees$genus, trees$species, sep = "_"))

## community dataframe
community1 <- tibble(plot = trees$plot_label,
                     species = trees$fam_gen_sp)
species_vect <- unique(community1$species)
plot_vect <- unique(plots$plot_label)

community <- matrix(0, ncol = length(species_vect), nrow = length(plot_vect))
for (i in 1:length(plot_vect)){
  temp <- community1 %>%
    filter(plot == plot_vect[i])
  #area <- plots[plots$plot_label == plot_vect[i],]$area
  
  for (j in 1:length(species_vect)){
    community[i,j] <- sum(match(temp$species, species_vect[j]), na.rm = T)
  }
  #community[i,] <- ceiling(community[i,]/area)
}
community[is.na(community)] <- 0

colnames(community) <- species_vect
rownames(community) <- plot_vect

## table for figures
df_temp <- cbind(plots[,c(1,4:5,10)], 
                 simpson_index = diversity(community, index = "simpson")
)

#2) HISTOGRAM OF SIMPSON INDEX PER PLOT

hist_simpson <- ggplot(df_temp, aes(x=simpson_index)) + 
  geom_histogram(color="black", fill="grey", binwidth = 0.01)+
  geom_vline(aes(xintercept=mean(simpson_index)), color="red", 
             linetype="dashed", lwd = 1.3) +
  theme_minimal()+
  ylab("Count")+
  xlab("")+
  ggtitle("Simpson index")

#3) MAP OF PLOTS WITH SIMPSON INDEXES

maps_simpson <- ggplot() +
  coord_fixed(1) +
  scale_fill_viridis_c() +
  geom_point(data = df_temp, aes(x=long_dd, y=lat_dd, size = simpson_index), 
             colour = "darkgrey", shape=1) +
  theme_minimal()+
  labs(size = "Simpson index")+
  geom_polygon(data = french_guiana, aes(x=long, y = lat), fill=NA, colour="black")+
  theme(legend.position = "right")+
  xlab("Longitude")+
  ylab("Latitude")

## print & save
pdf(file = paste0("figures/hist_maps_simpson.pdf", sep = ""), height = 6, width = 12)
print(plot_grid(hist_simpson, maps_simpson, align = "h", nrow = 1,
                  rel_widths = c(1/2, 1/2)))
dev.off()



