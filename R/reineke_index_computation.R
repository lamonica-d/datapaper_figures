##############################################################
###                      REINEKE INDEX                     ###
##############################################################

## reineke index computation according to 
## Bernard Prevosto. Les indices de compétition en foresterie : exemples d’utilisation, intérêts et limites..
## Revue forestière française, 2005, 57 (5), pp.413-430. 10.4267/2042/5062. hal-03449250

## formula : IR = N * (D_barre / 25.4) ^ 1.625
## with D_barre the quadratic mean of stand diameter (cm), and N the stand density (/ha)

## reineke_index returns the index value for a stand
## arg trees : a vector of all tree diameters in the stand in cm
## arg area : a single value of stand area in ha

reineke_index <- function(diameters, area){
  nb_tree <- length(diameters)
  N <- nb_tree/area
  D_barre <- sqrt(sum(diameters^2)/nb_tree)
  IR <- N * (D_barre / 25.4) ^ 1.625
  return(IR)
}


