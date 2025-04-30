##############################################################
###            loess models for interpolation              ###
##############################################################

## load data
region_grid <- readRDS("outputs/grid_for_interpolation.RDS")
df_interpol <- readRDS("outputs/table_for_interpolation.RDS")

## run interpolation for each variable/index of interest
df_plot_variable_list <- list()
for (i in 1:5){
  index_in_plots <- 4 + i
  table_temp <- df_interpol[,c(1:3,index_in_plots)]
  colnames(table_temp)[4] <- "variable"
  
  z.loess <- loess(variable ~ long_dd * lat_dd, data = table_temp,
                   span = 0.75, degree = 1, se = T,
                   normalize = TRUE, family = "gaussian",
                   surface = "direct")
  
  grid.z.predict <- predict(z.loess, region_grid, se = T)
  df_plot_variable_list[[i]] <- cbind(region_grid, variable = grid.z.predict)
}

saveRDS(df_plot_variable_list, "outputs/list_table_for_maps.RDS")
