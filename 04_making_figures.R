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
library(cowplot)

## load data
world <- map_data("world")
french_guiana <- world %>%
  filter(region == "French Guiana")

community_table <- readRDS("outputs/community_table.RDS")
df_figures <- readRDS("outputs/table_for_figures.RDS")

## species acc computation & plot
sac <- specaccum(community_table)
pdf(file = "figures/species_acc_v3.pdf", height = 8, width = 8)
par(mfrow = c(1,1))
plot(sac, ci.type="polygon", ci.col="lightblue", ylab = "Cumulative number of species",
     xlab = "Sites", main = "Species accumulation curve")
dev.off()


## histograms of observed variables
variable_names <- c("Number of trees per hectare", "Number of species per hectare", 
                    "Simpson index")
binwidth_vect <- c(50,10,0.01)

hist_plot <- list()
for (i in 1:length(variable_names)){
hist_plot[[i]] <- ggplot(df_figures[df_figures$index == unique(df_figures$index)[i],], 
                         aes(x=values)) + 
    geom_histogram(color="black", fill="grey", binwidth = binwidth_vect[i])+
    geom_vline(aes(xintercept=mean(values)), color="red", 
               linetype="dashed", lwd = 1.3) +
  theme_minimal()+
  xlab(variable_names[i])+
  ylab("Count")+
  xlab("")+
  ggtitle(variable_names[i])
}
  
## map Simpson index
  maps_plot <- ggplot() +
    coord_fixed(1) +
    scale_fill_viridis_c() +
    geom_point(data = df_figures[df_figures$index == unique(df_figures$index)[3],], 
               aes(x=long_dd, y=lat_dd, size = values), colour = "darkgrey", shape=1) +
  theme_minimal()+
  labs(fill = "Simpson index")+
  geom_polygon(data = french_guiana, aes(x=long, y = lat), fill=NA, colour="black")+
   theme(legend.position = "right")+
   xlab("Longitude")+
  ylab("Latitude")


## print & save all plots
for (i in 1:length(variable_names)){
pdf(file = paste0("figures/hist_maps_var",i,".pdf", sep = ""), height = 6, width = 12)
print(plot_grid(hist_plot[[i]], maps_plot[[i]], align = "h", nrow = 1,
                rel_widths = c(0.8/2, 1.2/2)))
dev.off()
}





