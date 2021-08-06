
library(chron)
library(data.table)
library(raster)
library(ncdf4)
library(dplyr)
library(doParallel)
library(foreach)
library(zoo)

setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/input_data/")
path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/input_data/"
getwd()

### crop hydro to catchments

## base raster to crop
rl <- list.files(pattern="crop")
rl
r <- raster(rl[1])
r

## catchments shape

shape <- shapefile("ems_weser_merged.shp")

plot(shape)

r <- raster(paste0(path,"input_data/agriculture_land_use.correct.r.tif"))
hpreds_r <- crop(extend(hpr, r), r)
all.equal(extent(r), extent(hpreds_r))
hpreds_rx <- resample(hpreds_r, r)

### list hydro files

net_list <- list.files("all_de/", pattern=".tif")
net_list
i=1

for(i in 1:length(net_list)) {

  ## upload one by one
  hyd <- raster(paste("all_de/", net_list[i], sep=""))
  bioc <- mask(hyd, shape)
  
  biocx <- crop(extend(bioc, r), r)
  all.equal(extent(r), extent(biocx))
  biocx <- resample(biocx, r)

  
  y<- writeRaster(biocx, paste0(path, net_list[i]), format = "GTiff", datatype="INT4U", overwrite=T)
  
  x<- writeRaster(biocx, paste0(path, net_list[i]), format = "GTiff",  overwrite=T)
  
}

dh1x <- as(as(biocx, "SpatialPointsDataFrame"), "data.frame") 
head(dh1x)
dh1x <- dh1x %>%
  mutate(dh1  = (dh1 /1000))
hasValues(biocx)

range(dh1x$dh1) ## 68.6 11421.6

cellStats(biocx, mean) 
cellStats(biocx, min) 
cellStats(biocx, max)



dh1 <- raster("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/input_data/Hydrology/dh1.tif")

dh1x <- as(as(dh1, "SpatialPointsDataFrame"), "data.frame") 
head(dh1x)
dh1x <- dh1x %>%
  mutate(dh1  = (dh1 /1000))

range(dh1x$dh1) ## 68.6 11421.6

cellStats(dh1, mean) 
cellStats(dh1, min) 
cellStats(dh1, max)



