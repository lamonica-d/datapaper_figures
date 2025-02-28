##Figures

library(stringr)
library(dplyr)
library(vegan)

## load data
plots <- tibble(read.csv("data/plots.csv", sep = "\t"))
trees <- tibble(read.csv("data/trees.csv", sep = "\t"))

## remove PCQ sampling & plot SPO
trees <- trees %>% 
  filter(subpl_type != "point") %>%
  filter(plot_label != "SPO")

plots <- plots %>% 
  filter(is.na(area) != T) %>%
  filter(plot_label != "SPO")

## select columns needed
subplot_area <- trees$subpl_width*trees$subpl_length

## community dataframe for species accumulation
community1 <- tibble(id_subplot = str_c(trees$plot_label, trees$subpl_label, sep = "", collapse = NULL)
                     , species = trees$species) %>%
  filter(species != "")

species_vect <- unique(community1$species)
subplot_vect <- unique(community1$id_subplot)

community2 <- matrix(0, ncol = length(species_vect), nrow = length(subplot_vect))
for (i in 1:length(subplot_vect)){
  temp <- community1 %>%
    filter(id_subplot == subplot_vect[i])
  community2[i,] <- match(species_vect, temp$species)
     #which(community1$id_subplot == subplot_vect[i] & community1$species == species_vect[j])
}
community2[is.na(community2)] <- 0

## species acc computation and plot
sac <- specaccum(community2)
plot(sac, ci.type="polygon", ci.col="lightgreen")
