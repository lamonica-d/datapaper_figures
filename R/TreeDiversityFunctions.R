## Script for figures and maps of ter Steege et al 2023 "Mapping density, diversity and species-richness of the Amazon tree flora" 
## Communications Biology
## Functions specifically written to map diversity in Amazonia
## (c) Hans ter Steege, Naturalis Biodiversity Center, Leiden, The Netherlands



## Libraries and sources
library(quantreg)
library(raster)
library(rgdal)
library(vegan)
library(ape)





##################### make map based on loess stratified by by forest #####################

make.trait.map = function(w.data = NULL, o = NULL, outlier.rem = F,
                          degree = 2, surface = "direct", loess.se = F,
                          sp = 0.2, one.forest = 0,
                          log.legend = F, fix.legend2sd = 0, fix.legend0 = F,
                          cex.set = 0, offsetx = 0, offsety = 0,
                          name = "", plot.map = T){

  force(surface); force(log.legend); force(degree)
  force(fix.legend2sd); force(fix.legend0)
  force(name); force(offsetx)
  force(offsety); force(cex.set)

  outlier.remove = function(x){
    d = x[,1]
    hi = d > (mean(d) + 1.96*sd(d))
    lo = d < (mean(d) - 1.96*sd(d))
    out = hi+lo
    i = !(out)
    return(x[i,])
  }

  if(is.null(o)) o = 1:length(plot.longitude)
  
  Lon    = plot.longitude[o]
  Lat    = plot.latitude[o]
  forest = plot.forest[o]
  w.data = w.data[o]

  if(is.null(w.data)){
    paste("data is required")
    return(NULL)
  }

  w.data.min = min(w.data, na.rm = T)
  w.data.max = max(w.data, na.rm = T)

  f = (forest == "TFGS" | forest == "TFBS" | forest == "TFPB")
  z = w.data[f]; longitude = Lon[f]; latitude = Lat[f]
  if(outlier.rem == T) data = outlier.remove(data)

  trait.model = loess(z ~ longitude * latitude, span = sp, se = loess.se,
                normalize = TRUE, family = "gaussian",
                surface = surface, degree = degree)
  if(loess.se == T){
    grid.predict  = predict(trait.model, AmazonForestGrid, se = T)
    grid.predict  = grid.predict$se.fit
    plot.predict  = predict(trait.model, se = T)
    plot.predict  = plot.predict$se.fit
  } else {
    grid.predict  = predict(trait.model, AmazonForestGrid, se = F)
    plot.predict  = predict(trait.model, se = F)
  }

  trait.grid.soter = grid.predict
  grid.x = (forestsoter[,3] != 1)
  trait.grid.soter[grid.x] = NA
  trait.pred.obs = cbind(plot.predict,w.data[f])

  f = forest == "PZ" 
  z = w.data[f]; longitude = Lon[f]; latitude = Lat[f]
  if(outlier.rem == T) data = outlier.remove(data)

  trait.model = loess(z ~ longitude * latitude, span = sp,
                normalize = TRUE, family = "gaussian",
                surface = "direct", degree = degree)
  if(loess.se == T){
    grid.predict  = predict(trait.model, AmazonForestGrid, se = T)
    grid.predict  = grid.predict$se.fit
    plot.predict  = predict(trait.model, se = T)
    plot.predict  = plot.predict$se.fit
  } else {
    grid.predict  = predict(trait.model, AmazonForestGrid, se = F)
    plot.predict  = predict(trait.model, se = F)
  }

  grid.x = (forestsoter[,3] == 2) 
  trait.grid.soter[grid.x] = grid.predict[grid.x]

  trait.pred.obs = rbind(trait.pred.obs,cbind(plot.predict, w.data[f]))

  f = forest == "VA" | forest == "IG"
  z = w.data[f]; longitude = Lon[f]; latitude = Lat[f]
  if(outlier.rem == T) data = outlier.remove(data)

  trait.model = loess(z ~ longitude * latitude, span = sp,
                normalize = TRUE, family = "gaussian",
                surface = surface)
  if(loess.se == T){
    grid.predict  = predict(trait.model, AmazonForestGrid, se = T)
    grid.predict  = grid.predict$se.fit
    plot.predict  = predict(trait.model, se = T)
    plot.predict  = plot.predict$se.fit
  } else {
    grid.predict  = predict(trait.model, AmazonForestGrid, se = F)
    plot.predict  = predict(trait.model, se = F)
  }

  grid.x = (forestsoter[,3] == 3)
  trait.grid.soter[grid.x] = grid.predict[grid.x]

  trait.pred.obs = rbind(trait.pred.obs,cbind(plot.predict, w.data[f]))

  f = forest == "SW"
  z = w.data[f]; longitude = Lon[f]; latitude = Lat[f]
  if(outlier.rem == T) data = outlier.remove(data)

  trait.model = loess(z ~ longitude * latitude, span = sp,
                normalize = TRUE, family = "gaussian",
                surface = surface)
  if(loess.se == T){
    grid.predict  = predict(trait.model, AmazonForestGrid, se = T)
    grid.predict  = grid.predict$se.fit
    plot.predict  = predict(trait.model, se = T)
    plot.predict  = plot.predict$se.fit
  } else {
    grid.predict  = predict(trait.model, AmazonForestGrid, se = F)
    plot.predict  = predict(trait.model, se = F)
  }

  grid.x = (forestsoter[,3] == 4) 
  trait.grid.soter[grid.x] = grid.predict[grid.x]

  trait.pred.obs = rbind(trait.pred.obs,cbind(plot.predict, w.data[f]))

  #t0 = trait.grid.soter < 0
  #trait.grid.soter[t0 ] = 0
  #o = (trait.grid.soter > w.data.max) | (trait.grid.soter < w.data.min)
  #trait.grid.soter[o] = NA
  trait.grid.soter[trait.grid.soter > w.data.max] = w.data.max
  trait.grid.soter[trait.grid.soter < w.data.min] = w.data.min


  o = AmazonForestGrid$latitude > 8.5
  trait.grid.soter[o] = NA

  ## to return a map for only one forest 
  if(one.forest > 0 & one.forest < 5){
    grid.min = min(trait.grid.soter, na.rm = T); grid.max = max(trait.grid.soter, na.rm = T)
    trait.grid.soter[forestsoter[,3] != one.forest] = NA
    trait.grid.soter[1]     = grid.min
    trait.grid.soter[98502] = grid.max    
  }

  if(plot.map == T){
    map.grid(trait.grid.soter, res = 0.1, name = name, 
             grid.color = c("green", "red"), pal = 3, 
             log.legend = log.legend, fix.legend2sd = fix.legend2sd, 
             cex.set = cex.set, offsetx = offsetx, offsety = offsety,
             fix.legend0 = fix.legend0)
  }

  x = summary(lm(trait.pred.obs[,2]~trait.pred.obs[,1]))$adj.r.squared
  cat("explained variation :",100*x,"%","\n")

  ## sort pred.obs back to original order
  trait.pred.obs = trait.pred.obs[order(rownames(trait.pred.obs)),] 

  return(list(trait.grid.soter, trait.pred.obs))
}

##################END make map based on loess by forest #####################





##################### map grid on Amazon #####################

map.grid = function(data, res = 1, draw.legend = T, name = "grid", cex.set = 0, 
                    fix.legend2sd = 0, fix.legend0 = F, log.legend = F, offsetx = 0, offsety = 0,
                    grid.color = c("white", "black"), n.colors = 256, pal = 0){
  
  force(log.legend); force(cex.set)

  grid.pal = colorRampPalette(grid.color)(n.colors ) ## (n)

  if(pal == 1) grid.pal = heat.colors(n.colors, alpha = 1)
  if(pal == 2) grid.pal = terrain.colors(n.colors, alpha = 1)
  if(pal == 3) grid.pal = topo.colors(n.colors, alpha = 1)
  if(pal == 4) grid.pal = cm.colors(n.colors, alpha = 1)

  if (res == 0.1){
    cex_pred = 0.2;  data2pred = AmazonForestGrid
  } else if (res == 0.5){
    cex_pred = 1.25; data2pred = data_to_pred05
  } else {
    cex_pred = 2.5;  data2pred = data_to_pred
  } 

  if(cex.set > 0)  cex_pred = cex.set


  if(fix.legend2sd == 1){
    sd.data  = sd(data, na.rm = T)
    grid.min = mean(data, na.rm = T) - sd.data
    grid.max = mean(data, na.rm = T) + sd.data
    if(fix.legend0 == T) {if(grid.min < 0) grid.min = 0}
    data[data < grid.min] = grid.min; data[data > grid.max] = grid.max
  } else if(fix.legend2sd == 2){
    sd.data  = sd(data, na.rm = T)
    grid.min = mean(data, na.rm = T) - 2*sd.data
    grid.max = mean(data, na.rm = T) + 2*sd.data
    if(fix.legend0 == T) {if(grid.min < 0) grid.min = 0}
    data[data < grid.min] = grid.min; data[data > grid.max] = grid.max
  } else {
    grid.min = min(data, na.rm = T)
    grid.max = max(data, na.rm = T)
  }

  grid.range = grid.max - grid.min
  grid.col   = vector(length = length(data2pred$longitude))
  grid.col   = grid.pal[1+round((n.colors-1)*(data - grid.min)/grid.range)]

  plot(data2pred$longitude,data2pred$latitude, 
       main = name,
       xlab = "longitude", ylab = "latitude",
       xaxp = c(-80, -50, 3), yaxp = c(-20, 10, 3),
       xlim = c(-80,-45), ylim = c(-20,10), asp = 30/30,
       xaxp = c(-80, -50, 3), yaxp = c(-20, 10, 3),
       pch = 22, cex = cex_pred,
       col = grid.col, bg  = grid.col)

  if(res == 0.1){
   add.geography(raisg = T)
   plot(Lim_Biogeo, border = "red", add = T, lwd = 2, asp = 1)
  } else {
   add.geography(draw.forestborder = T)
  }

  if (draw.legend == T){ 
      n.round = 0
      if (grid.max <=10) n.round = 1
      if (grid.max <= 1) n.round = 2
      legend.n    = seq(grid.min, grid.max, by = grid.range/4)
      legend.pch  = rep(2,length(legend.n))
      legend.col  = grid.pal[1+round((n.colors-1)*(legend.n - grid.min)/grid.range)]
      legend.fill = legend.col
      legend.nrs  = round(legend.n, n.round)
      if(log.legend == T) legend.nrs = round(10^legend.n, n.round)
      legend(x = -50 + offsetx, y = -13 + offsety, 
             legend = legend.nrs, 
             fill = legend.fill,
             bg = "white")
  }
}
##################END map grid on Amazon #####################





##################END loess model and mapped to Amazon #####################
map.loess = function(z, longitude, latitude, res = 1,
                     span = 0.5, degree = 2, se = T, draw.map = T,
                     predict = T, surface = "direct",
                     r.color = "black",
                     name = "", draw.legend = T, offsetx = 0, offsety = 0, 
                     fix.legend0 = F, fix.legend2sd = 0, 
                     blocks = T, dots = T, c.col = "white",
                     grid.color = c("white", "black"), 
                     n.colors = 256, pal = 0){

#z = plots.abiotic$N; longitude = data_env$longitude; latitude = data_env$latitude; span = 0.25
#surface = "direct"; data2pred = AmazonForestGrid; cex_pred = 0.1

  force(span); force(degree); force(se); 
  force(predict); force(surface); force(r.color)

  grid.pal = colorRampPalette(grid.color)(n.colors ) ## (n)

  if(pal == 1) grid.pal = heat.colors(n.colors, alpha = 1)
  if(pal == 2) grid.pal = terrain.colors(n.colors, alpha = 1)
  if(pal == 3) grid.pal = topo.colors(n.colors, alpha = 1)
  if(pal == 4) grid.pal = cm.colors(n.colors, alpha = 1)

  if (res == 0.1){
    cex_pred = 0.1;  data2pred = AmazonForestGrid
  } else if (res == 0.5){
    cex_pred = 1.25; data2pred = data_to_pred05
  } else {
    cex_pred = 2.5;  data2pred = data_to_pred
  }    

  z.loess = loess(z ~ longitude * latitude,
                  span = span, degree = degree, se = se,
                  normalize = TRUE, family = "gaussian",
                  surface = surface) #!surface is direct to be able to extrapolate

  #calculate explained variation
  SSq = sum((z-mean(z))^2)
  SSqres = sum((z - z.loess$fit)^2)
  expl_var = 100*(SSq-SSqres)/SSq

  #give output for loess regression model and expl variation
  cat("explained variation :",expl_var,"%","\n")

  if(draw.map == F)   return(z.loess)

  #calculate the predicted values for the Amazon grid
  grid.z.predict = predict(z.loess, data2pred, se = T)

  if (blocks != T){
    plot(data2pred$longitude,data2pred$latitude, 
         main = name,
         xlab = "longitude", ylab = "latitude",
         xlim = c(-80, -45), ylim = c(-20,10), asp = 30/30,
         xaxp = c(-80, -45, 7), yaxp = c(-20, 10, 6),
         pch  = 22, cex = 3,
         col  = rgb(0.85,0.95,0.85),
         bg   = rgb(0.85,0.95,0.85))
  }

  #replace all fits outsied data range by min or max
  if(fix.legend0 == T){ 
    grid.z.predict$fit[grid.z.predict$fit < min(z)] = min(z)
    grid.z.predict$fit[grid.z.predict$fit > max(z)] = max(z)
  }

  #show map of expected DCA scores and actual plot locations
  if (blocks == T){
    if(fix.legend2sd == 1){
      sd.z     = sd(grid.z.predict$fit, na.rm = T)
      grid.min = mean(grid.z.predict$fit, na.rm = T) - sd.z
      grid.max = mean(grid.z.predict$fit, na.rm = T) + sd.z
      if(fix.legend0 == T) {if(grid.min < 0) grid.min = 0}
      z[z < grid.min] = grid.min; z[z > grid.max] = grid.max
    } else if(fix.legend2sd == 2){
      sd.z     = sd(grid.z.predict$fit, na.rm = T)
      grid.min = mean(grid.z.predict$fit, na.rm = T) - 2*sd.z
      grid.max = mean(grid.z.predict$fit, na.rm = T) + 2*sd.z
      if(fix.legend0 == T) {if(grid.min < 0) grid.min = 0}
      z[z < grid.min] = grid.min; z[z > grid.max] = grid.max
    } else {
      grid.min = min(grid.z.predict$fit, na.rm = T)
      grid.max = max(grid.z.predict$fit, na.rm = T)
    }

    grid.col   = vector(length = length(data2pred$longitude))
    grid.range = grid.max - grid.min
    grid.col   = 1 -(grid.z.predict$fit - grid.min)/grid.range
    grid.col   = grid.pal[1+round((n.colors-1)*(grid.z.predict$fit - grid.min)/grid.range)]

    plot(data2pred$longitude,data2pred$latitude, 
         main = name,
         xlab = "longitude", ylab = "latitude",
         xlim = c(-80, -45), ylim = c(-20, 10), asp = 30/30,
         xaxp = c(-80, -45, 7), yaxp = c(-20, 10, 6),
         pch = 22, cex = cex_pred,
         col = grid.col, bg = grid.col)
    }

  if (dots == T){
    zmin   = min(z)
    zmax   = max(z)
    zrange = zmax - zmin
    zcex   = 0.1 + round((4*(z - zmin)/zrange),1)
    points(longitude, latitude, cex = zcex, pch = 21, bg = "black", col = c.col)
    }

  if (res == 0.1){
    add.geography(r.color = r.color, raisg = T)
  } else { 
    add.geography()
  }

  if (draw.legend == T){ 
    if (blocks == T){
      n.round = 0
      if (abs(grid.max) <=10) n.round = 1
      if (abs(grid.max) <= 1) n.round = 2
      legend.n   = seq(grid.min, grid.max, by = grid.range/4)
      legend.pch = rep(2,length(legend.n))
      legend.col   = grid.pal[1+round((n.colors-1)*(legend.n - grid.min)/grid.range)]
      legend.fill = legend.col
      legend(x = -49 + offsetx, y = -14 + offsety, 
             legend = round(legend.n, n.round), 
             fill = legend.fill,
             bg = "white")
    }

    if (dots == T){
      legend.n   = round(seq(zmin,zmax,zrange/4),0)
      legend.pch = rep(21,length(legend.n))
      legend.cex = 0.1 + round((4*(legend.n - zmin)/zrange),1)
      legend(x = -48 + offsetx, y = 14 + offsety, 
             legend = legend.n, 
             pch = legend.pch,
             pt.bg = "black",
             col = c.col,
             pt.cex = legend.cex,
             bg = "white")
      }
    }

  return(z.loess)
}
##################END loess model and mapped to Amazon #####################





##################### function to add countries and rivers to maps of Amazon #####

## note that the shapefiles have to be downloaded from ESRI 

#load the shapefiles
countries = readOGR("D:/Documents/GIS Data/ESRIDATA/WORLD", "CNTRY92")
rivers = readOGR("D:/Documents/GIS Data/ESRIDATA/WORLD", "RIVERS")
#Amazon limits
Lim_Biogeo   = readOGR("D:/Documents/GIS Data/neotropics/Amazon shapes", "Lim_Biogeografico")


add.geography = function(draw.countries = T, draw.rivers = T, raisg = F,
                         r.color = "black", border = "black",
                         add.arrow = T, add.scale = T){
  if (draw.countries == T)    plot(countries, xlim = c(-80,-45), ylim = c(-20,10), border = border, add = T, asp = 1)
  if (draw.rivers == T)       plot(rivers, xlim = c(-80,-45), ylim = c(-20,10), col = 'blue', add = T, asp = 1)
  if(raisg == T) plot(Lim_Biogeo, border = "red", add = T, lwd = 2, asp = 1)

  if(add.arrow == T) SpatialPolygonsRescale(layout.north.arrow(), offset = c(-80, 9) , scale = 2, 
                                            fill = c("black", "black"), plot.grid = F)
  if(add.scale == T){ 
    SpatialPolygonsRescale(layout.scale.bar(), offset = c(-80, -20) , scale = 10/1.11, 
                           fill = c("transparent", "black"), plot.grid = F)
    text(-77.8, -18.5 ,"1000 km")
  }
}

##################### function to add countries and rivers to maps of Amazon #####





##################### plot.wa #####################

plot.wa = function(data, xlab = "", ylab = "", main = "", cex = 1){
  force(xlab); force(ylab); force(main); force(cex)
  plot(data[[2]], xlab = xlab, ylab = ylab, main = main, cex = cex)
  x = lm(data[[2]][,2]~data[[2]][,1])
  fline(x, lwd = 2, col = "red"); summary(x)
  r = round(summary(x)$adj.r.squared,2)
  p2 = anova(x)$Pr[1]
  if(p2 <= 0.05)  sig = "*"
  if(p2 <= 0.01)  sig = "**"
  if(p2 <= 0.001) sig = "***"
  if(p2 > 0.05)   sig = "ns"
  y = par("usr")
  text(y[1]+(y[2]-y[1])/9,y[4], paste("r2 = ", round(r,2),sig), pos = 1, offset = 0.5)
}

##################END plot.wa #####################





#################### Boxplot Plus #####################

boxplot.plus = function(dep, ind, main = "", xlab = "", ylab = ""){
  force(xlab); force(ylab); main = main
  x = lm(dep ~ ind)
  r = round(summary(x)$adj.r.squared,2)
  p2 = anova(x)$Pr[1]
  if(p2 <= 0.05)  sig = "*"
  if(p2 <= 0.01)  sig = "**"
  if(p2 <= 0.001) sig = "***"
  if(p2 > 0.05)   sig = "ns"
  o = rank(tapply(dep, ind, median))
  boxplot(dep ~ ind, at = o, xlab = xlab, ylab = ylab, main = main)
  y = par("usr")
  text(y[1]+(y[2]-y[1])/9,y[4], paste("r2 = ", round(r,2),sig), pos = 1, offset = 0.5)
  segments(0,mean(dep),8,mean(dep), col = "red", lty = 3)
}

#################END BoxplotPlus #####################





#################### Histogram Plus #####################

hist.plus = function(dep, trait = "", xlab = "", ylab = ""){
  force(xlab); force(ylab); force(trait)
  mt = mean(dep, na.rm = T); st = sd(dep, na.rm = T)
  x = hist(dep, main = paste(trait, " (avg ",round(mt,2),")", sep = ""), xlab = xlab, ylab = ylab)
  m = max(x$counts)/40
  segments(mt, -m, mt, m, col = "red", lwd = 2)
  segments(mt-st, -.7*m, mt-st, .7*m, col = "red", lwd = 1); segments(mt+st, -.7*m, mt+st, .7*m, col = "red", lwd = 1)
  segments(mt-2*st, -.3*m, mt-2*st, .3*m, col = "red", lwd = 1); segments(mt+2*st, -.3*m, mt+2*st, .3*m, col = "red", lwd = 1)
}

#################END Histogram Plus Traits #####################





##### function to fit a line to the data range only #####
fline = function(object, col.fl = "black", lwd.fl = 1) {
  r = range(object$model[,2])
  y = object$coefficients[1] + object$coefficients[2]*range(object$model[,2])
  lines(r,y, col = col.fl, lwd = lwd.fl)
}

##### End function to fit a line to the data range only #####





############### Functions to make rq maps ###############

####### rq.map1

rq.map1 = function(qrt = 0.5, dep = NULL, ind1 = NULL, make.tif = F, tif.name = "map1",
                   title1 = "", title2 = "", title3 = "", xlab = "", ylab = "", draw.contour = NA){

  x = data.frame(cbind(dep, ind1, plot.longitude, plot.latitude))
  names(x)[3:4] = c("longitude", "latitude")
  x = x[tf,]
  cc = complete.cases(x)
  x = x[cc,]  #; sum(cc); head(x)

  force(xlab); force(ylab)
  if(make.tif == T){
    tiff(paste(tif.name, sep = ""), compression = "lzw",
         width = 28, height = 28, units = "cm", res = 300)
  } else {
    dev.new(width = 19, height = 19, units = "cm")
  }
  par(mfrow = c(2,2))

  map = map.loess(x$dep, x$longitude, x$latitude, pal = 2, res = 0.1, dots = F,
                  name = title1, span = 0.2, offsetx = -1, offsety = 2)
  contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))), add = T)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "A", pos = 1)

  plot(x$ind1, x$dep, ylab = ylab, xlab = xlab, col = "orange")
  rq.tf = rq(dep ~ ind1, tau = 0.5); a = summary(rq.tf)$coefficients
  if(dim(a)[2] == 4) {if(a[2,4] < 0.05) abline(rq.tf, lwd = 2, col = "orange")}
  rq.tf = rq(dep[tf] ~ ind1[tf], tau = 0.9); a = summary(rq.tf)$coefficients
  if(dim(a)[2] == 4) {if(a[2,4] < 0.05) abline(rq.tf, lwd = 2, lty = 2, col = "orange")}
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "B", pos = 1)

  rq.tf = rq(x$dep ~ x$ind1, tau = qrt)#; summary(rq.tf)
  map = map.loess(predict(rq.tf), x$longitude, x$latitude, pal = 2, res = 0.1, dots = F, 
                  name = title2, span = 0.2, offsetx = -1, offsety = 2)
  contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))), add = T)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "C", pos = 1)

  rq.tf.res =  x$dep - predict(rq.tf)
  map = map.loess(rq.tf.res, x$longitude, x$latitude, pal = 2, res = 0.1, dots = F, 
                name = title3, span = 0.2, offsetx = -1, offsety = 2)
  contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))), add = T)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "D", pos = 1)
  if(!is.na(draw.contour)){
    contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, data_to_pred, se = F))),
            levels = draw.contour, add = T, col = "red", lwd = 2)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "D", pos = 1)
  }

  par(mfrow = c(1,1))
  if(make.tif == T) dev.off()
  result = data.frame(cbind(predict(rq.tf), rq.tf.res))
  colnames(result) = c("predict", "residual")
  return(result)
}


####### rq.map2

rq.map2 = function(qrt = 0.5, dep = NULL, ind1 = NULL, ind2 = NULL, ind3 = NULL, ind4 = NULL, pred.rq = F,
                   make.tif = F, tif.name = "map2",
                   title1 = "", title2 = "", title3 = "", xlab = "", ylab = "", draw.contour = NA){

  force(xlab); force(ylab)
  if(make.tif == T){
    tiff(paste(tif.name, sep = ""), compression = "lzw",
         width = 28, height = 28, units = "cm", res = 300)
  } else {
    dev.new(width = 19, height = 19, units = "cm")
  }
  par(mfrow = c(2,2))

  if(is.null(ind2) & is.null(ind3)){
    x = data.frame(cbind(dep, ind1, plot.longitude, plot.latitude))
    names(x)[3:4] = c("longitude", "latitude")
  }  
  if(!is.null(ind2) & is.null(ind3)){
    x = data.frame(cbind(dep, ind1, ind2, plot.longitude, plot.latitude))
    names(x)[4:5] = c("longitude", "latitude")
  }  
  if(!is.null(ind2) & !is.null(ind3)){
    x = data.frame(cbind(dep, ind1, ind2, ind3, plot.longitude, plot.latitude))
    names(x)[5:6] = c("longitude", "latitude")
  }  
  if(!is.null(ind2) & !is.null(ind3)& !is.null(ind4)){
    x = data.frame(cbind(dep, ind1, ind2, ind3, ind4, plot.longitude, plot.latitude))
    names(x)[6:7] = c("longitude", "latitude")
  }  

  x = x[tf,]
  cc = complete.cases(x)
  x = x[cc,]  #; sum(cc); head(x)

  map = map.loess(x$dep, x$longitude, x$latitude, pal = 2, res = 0.1, dots = F,
                  name = title1, span = 0.2, offsetx = -1, offsety = 1)
  contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))), add = T)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "A", pos = 1)

  if(!is.null(ind3)){ 
    rq.tf = rq(x$dep ~ x$ind1 + x$ind2 + x$ind3, tau = qrt)
  } else if(!is.null(ind2)){
    rq.tf = rq(x$dep ~ x$ind1 + x$ind2, tau = qrt)
  } else {
    rq.tf = rq(x$dep ~ x$ind1, tau = qrt)
  }

  map = map.loess(predict(rq.tf), x$longitude, x$latitude, pal = 2, res = 0.1, dots = F, 
                name = title2, span = 0.2, offsetx = -1, offsety = 1)
  contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))), add = T)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "B", pos = 1)

  if(is.null(ind2) & is.null(ind3) & pred.rq == F){
    plot(x$ind1, x$dep, ylab = ylab, xlab = xlab, col = "orange")
    rq.tf = rq(x$dep ~ x$ind1, tau = qrt); summary(rq.tf)
    abline(rq.tf, lwd = 2, col = "orange")
    y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "C", pos = 1)   
  } else {
    plot(predict(rq.tf), x$dep, xlab = "Predicted", ylab = "Observed", col = "orange") 
    rq.pred = rq(x$dep ~ predict(rq.tf), tau = .50); abline(rq.pred, lwd = 2, col = "orange")
    xlm = lm(x$dep ~ predict(rq.tf))
    r   = round(summary(xlm)$adj.r.squared,2)
    p   = anova(xlm)$Pr[1]
    if(p <= 0.05)  sig = "*"
    if(p <= 0.01)  sig = "**"
    if(p <= 0.001) sig = "***"
    if(p > 0.05)   sig = "ns"
    y = par("usr")
    text(y[1]+(y[2]-y[1])/200,y[4]-y[4]/30, paste("r2 = ", round(r,2),sig), pos = 4, offset = 0.5)  
    y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "C", pos = 1)   
  }

  rq.tf.res =  x$dep - predict(rq.tf)
  map = map.loess(rq.tf.res, x$longitude, x$latitude, pal = 2, res = 0.1, dots = F, 
                  name = title3, span = 0.2, offsetx = -1, offsety = 1)
  contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))), add = T)
  if(!is.na(draw.contour)){
    contour(rasterFromXYZ(cbind(AmazonForestGrid$longitude, AmazonForestGrid$latitude, predict(map, AmazonForestGrid, se = F))),
            levels = draw.contour, add = T, col = "red", lwd = 2)
  y = par("usr"); text(y[2]-(y[2]-y[1])/25, y[4], "D", pos = 1)   
  }
  par(mfrow = c(1,1))
  if(make.tif == T) dev.off()
  result = data.frame(cbind(predict(rq.tf), rq.tf.res))
  colnames(result) = c("predict", "residual")
  return(result)
}

############END Functions to make rq maps ###############








