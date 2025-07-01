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

plots <- plots_all %>%
  filter(plot_label != "PSE5B-10B")

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
  for (j in 1:length(species_vect)){
    community[i,j] <- sum(match(temp$species, species_vect[j]), na.rm = T)
  }
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
  geom_histogram(color="black", fill="yellow", binwidth = 0.01, alpha = 0.2)+
  geom_vline(aes(xintercept=mean(simpson_index)), color="red", 
             linetype="dashed", lwd = 1.3) +
  theme_get()+
  ylab("Count")+
  xlab("")

#3) MAP OF PLOTS WITH SIMPSON INDEXES
#replace missing latitude for one of PCQ sublot IT01
df_temp$lat_dd[df_temp$plot_label == "ITO1"] <- 3.02

map_simpson_size <- ggplot() +
  coord_fixed(1) +
  geom_point(data = df_temp, aes(x=long_dd, y=lat_dd, size = simpson_index,
                                 color = simpson_index), 
             , shape=2) +
  scale_color_gradient()+
  theme_get()+
  labs(color = "Simpson index", size = "")+
  geom_polygon(data = french_guiana, aes(x=long, y = lat), fill=NA, colour="black")+
  theme(legend.position = "right")+
  xlab("Longitude")+
  ylab("Latitude")


## print & save
pdf(file = paste0("figures/hist_map_simpson_size.pdf", sep = ""), height = 6, width = 12)
print(plot_grid(hist_simpson, map_simpson_size, align = "h", nrow = 1,
                  rel_widths = c(1/2, 1/2)))
dev.off()

# ## second option
# map_simpson_col <- ggplot() +
#   coord_fixed(1) +
#   geom_point(data = df_temp, aes(x=long_dd, y=lat_dd, colour = simpson_index), 
#              size = 3, shape = 8) +
#   scale_colour_viridis_b()+
#   theme_minimal()+
#   labs(colour = "Simpson index")+
#   geom_polygon(data = french_guiana, aes(x=long, y = lat), fill=NA, colour="black")+
#   theme(legend.position = "right")+
#   xlab("Longitude")+
#   ylab("Latitude")
# 
# ## print & save
# pdf(file = paste0("figures/hist_map_simpson_color.pdf", sep = ""), height = 6, width = 12)
# print(plot_grid(hist_simpson, map_simpson_col, align = "h", nrow = 1,
#                 rel_widths = c(1/2, 1/2)))
# dev.off()


