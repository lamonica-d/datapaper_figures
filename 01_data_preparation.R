##############################################################
###                     AREA PLOTS ONLY                    ###
##############################################################

## histogram of the number of species per hectare 
## species accumulation curve

library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(vegan)
library(maps)
library(ggplot2)
library(terra)
library(sf)
library(gridExtra)
library(EntropyEstimation)

#1) DATA PREPARATION

## load raw data
plots_all <- tibble(read.csv("data_raw/plots.csv", sep = ","))
trees_all <- tibble(read.csv("data_raw/trees.csv", sep = ","))

## remove PCQ plots & monodominance

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

## remove plots under 0.2 ha
small_plots <- plots$plot_label[which(plots$area < 0.2)]

trees <- trees %>% 
  filter(!(plot_label %in% small_plots))

plots <- plots %>% 
  filter(!(plot_label %in% small_plots))

## community dataframe
community1 <- tibble(plot = trees$plot_label,
                     species = trees$genus_species)
species_vect <- unique(community1$species)
plot_vect <- unique(plots$plot_label)

community <- matrix(0, ncol = length(species_vect), nrow = length(plot_vect))
for (i in 1:length(plot_vect)){
  temp <- community1 %>%
    filter(plot == plot_vect[i])

  for (j in 1:length(species_vect)){
    community[i,j] <- sum(temp$species == species_vect[j])
  }
}
community[is.na(community)] <- 0

colnames(community) <- species_vect
rownames(community) <- plot_vect

#2) HISTOGRAM OF NUMBER OF SPECIES PER HECTARE

## table for figures
df_temp <- cbind(plots[,c(1,4:5,10)], 
                 nb_sp_per_ha = round(specnumber(community)/plots$area,digits =0)
)
hist_nbspha <- ggplot(df_temp, aes(x=nb_sp_per_ha)) + 
  geom_histogram(color="black", fill="grey", binwidth = 10)+
  geom_vline(aes(xintercept=mean(nb_sp_per_ha)), color="red", 
             linetype="dashed", lwd = 1.3) +
  theme_minimal()+
  ylab("Count")+
  xlab("")+
  ggtitle("Number of species per hectare")

## print & save
pdf(file = paste0("figures/hist_nb_sp_per_ha.pdf", sep = ""), height = 6, width = 6)
print(hist_nbspha)
dev.off()

#3) SPECIES ACCUMULATION CURVE

Ns <- colSums(community)
gsValues <- sapply(1:(sum(Ns)-1), function(r) GenSimp.z(Ns, r))
SAC <- cumsum(gsValues)
sac_plot <- ggplot(data.frame(x = 0:(sum(Ns)-1), 
                  y = c(0, SAC)), 
       aes(x = x, y = y)) +
  geom_line() +
  theme_minimal()+
  # geom_hline(aes(yintercept=quantile(SAC, probs = 0.9)), color="red", 
  #            linetype="dashed", lwd = 1.3) +
  labs(x = "Number of trees", y = "Number of species")

## print & save
pdf(file = paste0("figures/sac.pdf", sep = ""), height = 6, width = 6)
print(sac_plot)
dev.off()

################################################

## number of trees per ha : pareil que nb species mais on garde les monodominées
nb_trees <- as.numeric()
for (i in 1:length(plot_vect)){
  temp <- trees %>%
    filter(plot_label == plot_vect[i])
  nb_trees[i] <- nrow(temp)
}

## table for figures
df_temp <- cbind(plots[,c(1,4:5,10)], 
                    nb_tree_per_ha = nb_trees/plots$area,
                    nb_sp_per_ha = round(specnumber(community)/plots$area,digits =0),
                    simpson_index = diversity(community, index = "simpson")
)

df_figures <- df_temp %>% 
  pivot_longer(c(nb_tree_per_ha, nb_sp_per_ha, simpson_index),
               names_to = "index", values_to = "values")

saveRDS(df_figures, "outputs/table_for_figures.RDS")
saveRDS(community, "outputs/community_table.RDS")





