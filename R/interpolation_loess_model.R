##############################################################
###            loess models for interpolation              ###
##############################################################

## load data
region_grid <- readRDS("data/grid_for_interpolation")
df_figures <- readRDS("data/table_for_figures")

## run interpolation for each variable/index of interest
df_plot_variable_list <- list()
for (i in 1:5){
  index_in_plots <- 9 + i
  table_temp <- df_figures[,c(1:3,index_in_plots)]
  colnames(table_temp)[4] <- "variable"
  
  z.loess <- loess(variable ~ long_dd * lat_dd, data = table_temp,
                   span = 0.75, degree = 2, se = T,
                   normalize = TRUE, family = "gaussian",
                   surface = "direct")
  
  grid.z.predict <- predict(z.loess, region_grid_finer, se = T)
  
  min_variable <- min(table_temp$variable)
  max_variable <- max(table_temp$variable)
  grid.z.predict$fit[grid.z.predict$fit < min_variable] <- min_variable
  grid.z.predict$fit[grid.z.predict$fit > max_variable] <- max_variable
  
  df_plot_variable_list[[i]] <- cbind(region_grid_finer, variable = grid.z.predict)
}

saveRDS(df_plot_variable_list, "data/list_table_for_maps")
