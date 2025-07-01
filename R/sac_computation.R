##############################################################
###        SPECIES ACCUMULATION CURVE COMPUTATION          ###
##############################################################

## all plots

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

  for (j in 1:length(species_vect)){
    community[i,j] <- sum(match(temp$species, species_vect[j]), na.rm = T)
  }
}
community[is.na(community)] <- 0

colnames(community) <- species_vect
rownames(community) <- plot_vect

#2) SPECIES ACCUMULATION CURVE COMPUTATION

Ns <- colSums(community)

#### !!!! DO NOT RUN, SUPER LONG !!! ########################################
# gsValues <- sapply(1:(sum(Ns)-1), function(r) GenSimp.z(Ns, r))
# gsValues_sd <- sapply(1:(sum(Ns)-1), function(r) GenSimp.sd(Ns, r))
# ic <- qnorm(1 - 0.05/2) * gsValues_sd/sqrt(sum(Ns))
# 
# df_gsValues <- data.frame(mean = gsValues, sd = gsValues_sd,
#                           lower = gsValues - ic, upper= gsValues + ic)
# 
# ## save
# saveRDS(df_gsValues, file ="outputs/dataframe_gsValues.RDS")
#############################################################################

#3) SPECIES ACCUMULATION CURVE
## load
df_gsValues <- readRDS(file ="outputs/dataframe_gsValues.RDS")

SAC <- cumsum(df_gsValues$mean)
SAC_lower <- cumsum(df_gsValues$lower)
SAC_upper <- cumsum(df_gsValues$upper)
df_plotsac <- data.frame(x = 0:(sum(Ns)-1), ymean = c(0, SAC), 
                         ylower = c(0, SAC_lower),yupper = c(0, SAC_upper))

# nb tree for 80 or 90% of species
x_inter <- min(which(c(0, SAC) > quantile(SAC, probs = 0.8)))

## plot
sac_plot <- ggplot(data = df_plotsac) +
  geom_ribbon(aes(x=x, ymin = ylower, ymax = yupper), fill = "lightblue", alpha = 0.8) +
  geom_line(aes(x = x, y = ymean)) +
  geom_vline(aes(xintercept=x_inter), colour="red", 
             linetype="dashed", lwd = 1.3) +
  labs(x = "Number of trees", y = "Number of species")+
  theme_get()

## print & save
pdf(file = paste0("figures/sac_0.8.pdf", sep = ""), height = 6, width = 6)
print(sac_plot)
dev.off()
