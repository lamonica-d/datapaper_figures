#######################################################
###               SMALL PLOTS ANALYSIS              ###
#######################################################

## exhaustive & no monodominance plots

library(stringr)
library(dplyr)
library(tibble)
library(vegan)
library(maps)
library(ggplot2)
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

## figure
data <- tibble(plot_label = plot_vect,
               area = plots$area,
               nb_sp_per_ha = round(specnumber(community)/plots$area,digits =0)
)

tiff(file = paste0("figures/small_plots.tiff", sep = ""), height = 6, 
     width = 10, units = "in", res = 300)
ggplot(data)+
  geom_point(aes(x = area, y = nb_sp_per_ha))+
  xlab("Plot area (ha)")+
  ylab("Number of species per ha")+
  geom_vline(aes(xintercept=0.2), colour="red", linetype="dashed", lwd = 1.3)
dev.off()

