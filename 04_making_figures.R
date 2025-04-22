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
df_maps_list <- readRDS("outputs/list_table_for_maps.RDS")
df_most_rep_sp <- readRDS("outputs/table_for_most_rep_sp.RDS")

## species acc computation & plot
sac <- specaccum(community_table)
pdf(file = "figures/species_acc_v3.pdf", height = 8, width = 8)
par(mfrow = c(1,1))
plot(sac, ci.type="polygon", ci.col="lightblue", ylab = "Cumulative number of species",
     xlab = "Sites", main = "Species accumulation curve")
dev.off()

## histograms of the most represented species
plot_most_rep_sp <- ggplot(df_most_rep_sp, aes(x=species, y = nb_plot)) + 
  geom_bar(color="grey", fill="grey", stat = "identity", width = 0.5)+
  theme_minimal()+
  ylab("Number of plots")+
  xlab("")+
  ggtitle("Number of sampled plots that contain the 15 most represented species")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.05, hjust=1.05))

pdf(file = "figures/most_rep_sp.pdf", height = 6, width = 10)
plot_most_rep_sp
dev.off()

plot_most_rep_sp_percent <- ggplot(df_most_rep_sp, aes(x=species, y = percent)) + 
  geom_bar(color="grey", fill="grey", stat = "identity", width = 0.5)+
  theme_minimal()+
  ylab("Percentage of plots")+
  xlab("")+
  ggtitle("Number of sampled plots that contain the 15 most represented species")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.05, hjust=1.05))+
  ylim(c(0,100))

pdf(file = "figures/most_rep_sp_percent.pdf", height = 6, width = 10)
plot_most_rep_sp_percent
dev.off()

## histograms of observed variables
variable_names <- c("Number of trees per hectare", "Number of species per hectare", 
                    "Fisher's alpha index", "Shannon index", "Reineke index")
binwidth_vect <- c(25,10,5,0.09,25)

hist_plot <- list()
for (i in 1:length(variable_names)){
hist_plot[[i]] <- ggplot(df_figures[df_figures$index == unique(df_figures$index)[i],], 
                         aes(x=values)) + 
    geom_histogram(color="black", fill="white", binwidth = binwidth_vect[i])+
    geom_vline(aes(xintercept=mean(values)), color="red", 
               linetype="dashed", lwd = 1.3) +
  theme_minimal()+
  xlab(variable_names[i])+
  ylab("Count")+
  xlab("")+
  ggtitle(variable_names[i])
}
  
## maps of interpolated variables
legend_names <- c("Trees/ha", "Species/ha", 
                    "Fisher's alpha index", "Shannon index", "Reineke index")
maps_plot <- list()
for (i in 1:length(variable_names)){
maps_plot[[i]] <- ggplot() +
  coord_fixed(1) +
  geom_raster(data = df_maps_list[[i]], aes(x=long_dd, y=lat_dd, 
                                                     fill = variable.fit)) +
  scale_fill_viridis_c() +
  geom_point(data = df_figures, aes(x=long_dd, y=lat_dd), colour = "darkgrey", shape=3) +
  theme_minimal()+
  labs(fill =legend_names[i])+
  geom_polygon(data = french_guiana, aes(x=long, y = lat), fill=NA, colour="black")+
  theme(legend.position = "right")+
  xlab("Longitude")+
  ylab("Latitude")

}

## print & save all plots
for (i in 1:length(variable_names)){
pdf(file = paste0("figures/hist_maps_var",i,".pdf", sep = ""), height = 6, width = 12)
print(plot_grid(hist_plot[[i]], maps_plot[[i]], align = "h", nrow = 1,
                rel_widths = c(0.8/2, 1.2/2)))
dev.off()
}





