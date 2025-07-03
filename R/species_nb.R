##############################################################
###             NUMBER OF SPECIES PER HECTARE              ###
##############################################################

## exhaustive & no monodominance plots
## histogram of the number of species per hectare 

library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(vegan)
library(maps)
library(ggplot2)
library(gridExtra)

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

## remove plots under 0.2 ha
small_plots <- plots$plot_label[which(plots$area < 0.2)]

trees <- trees %>% 
  filter(!(plot_label %in% small_plots))

plots <- plots %>% 
  filter(!(plot_label %in% small_plots))

## remove trees without species id & trees < dbh 10cm
trees <- trees %>%
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

#2) HISTOGRAM OF NUMBER OF SPECIES PER HECTARE

## table for figures
df_temp <- cbind(plots[,c(1,4:5,10)], 
                 nb_sp_per_ha = round(specnumber(community)/plots$area,digits =0)
)

hist_nbspha <- ggplot(df_temp, aes(x=nb_sp_per_ha)) + 
  geom_histogram(color="black", fill="darkgrey", binwidth = 10, alpha = 0.6)+
  geom_vline(aes(xintercept=mean(nb_sp_per_ha)), color="red", 
             linetype="dashed", lwd = 1.3) +
  ylab("Count")+
  xlab("")

## print & save
tiff(file = paste0("figures/hist_nb_sp_per_ha.tiff", sep = ""), height = 6, 
     width = 6, units = "in", res = 300)
print(hist_nbspha)
dev.off()

