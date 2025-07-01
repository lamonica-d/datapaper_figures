##############################################################
###               NUMBER OF TREES PER HECTARE              ###
##############################################################

## all exhaustive plots

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

## remove PCQ plots
plots <- plots_all %>% 
  filter(type == "exhaustive") %>%
  filter(plot_label != "PSE5B-10B")

trees <- trees_all %>%
  filter(plot_label %in% unique(plots$plot_label))

## remove plots under 0.2 ha
small_plots <- plots$plot_label[which(plots$area < 0.2)]

trees <- trees %>% 
  filter(!(plot_label %in% small_plots))

plots <- plots %>% 
  filter(!(plot_label %in% small_plots))

## remove trees < dbh 10cm
trees <- trees %>%
  filter(dbh_before_2000 >= 10 | dbh_after_2000 >= 10) 

#2) COMPUTE & PLOT

nb_trees <- as.numeric()
for (i in 1:nrow(plots)){
  temp <- trees %>%
    filter(plot_label == plots$plot_label[i])
  nb_trees[i] <- nrow(temp)
}

## table for figures
df_temp <- cbind(plots[,c(1,4:5,10)], nb_tree_per_ha = nb_trees/plots$area)

hist_nbtha <- ggplot(df_temp, aes(x=nb_tree_per_ha)) + 
  geom_histogram(color="black", fill="yellow", binwidth = 50, alpha = 0.2)+
  geom_vline(aes(xintercept=mean(nb_tree_per_ha)), color="red", 
             linetype="dashed", lwd = 1.3) +
  ylab("Count")+
  xlab("")

## print & save
pdf(file = paste0("figures/hist_nb_tree_per_ha.pdf", sep = ""), height = 6, width = 6)
print(hist_nbtha)
dev.off()
