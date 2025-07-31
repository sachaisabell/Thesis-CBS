
source("hotspot_functions.R")

## libraries
library(dplyr)
library(ggplot2)
library(lidaRtRee)
library(sf)
library(purrr)
library(sdcSpatial)
library(tigers)
library(raster)








### Box counting dimension
box_counting = function(eps, poly) {
  
  poly = scale.center.poly(poly)
  bb33 = st_bbox(poly)
  bbsize = max(abs(bb33 - 0))
  r <- raster(nrows=1*eps, ncol=1*eps, xmn=-bbsize, ymn=-bbsize, xmx = bbsize, ymx = bbsize)
  vals <- seq(1:length(r))
  
  r2 = st_as_sf(rasterToPolygons(r))
  
  inter2 = st_overlaps(poly, sf::st_set_crs(r2, st_crs(poly)), byid = TRUE)
  ind = inter2[[1]]
  
  
  values(r) = NA
  values(r)[ind] = vals[ind]
  
  plot(r)
  plot(poly, add = TRUE)
  
  return(list("boxes" = length(ind),
              "size" = 2*bbsize / eps,
              "eps" = eps))
  
}


box_counting_dim_func = function(safe, original, eps = 10){
  boxcount_safe = sapply(2*2^(1:eps-1), function (x) box_counting(x, safe$geometry))
  boxcount_og = sapply(2*2^(1:eps-1), function (x) box_counting(x, original$geometry))
  
  bcdim_safe = log(unlist(boxcount_safe[1,])) / log(1/unlist(boxcount_safe[3,]))
  bcdim_og = log(unlist(boxcount_og[1,])) / log(1/unlist(boxcount_og[3,]))
  
  print(bcdim_safe)
  
  plot(unlist(boxcount_safe[3,]), bcdim_safe, type = "l", col = "blue")
  lines(unlist(boxcount_og[3,]), bcdim_og, col = "red")
}





### Examples
# first creating the data and the hotspots
data_og = dwellings[,1:2]
data_safe = mask_random(data_og, r = 100)

hotspot_og = bounded_hotspots_hdr(data_og, minimum = 0.35)
hotspot_safe = bounded_hotspots_hdr(data_safe, minimum = 0.35)

plot(hotspot_safe$geometry, axes = TRUE, col = "blue")              # the original hotspots
plot(hotspot_og$geometry, axes = TRUE, add = TRUE, col = "green") # the safe hotspots
