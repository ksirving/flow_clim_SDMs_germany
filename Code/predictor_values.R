library(raster)
library(dplyr)
library(tidyverse)
###################### acc and hydrology
setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_wclim_hydro")
uni_vars <- read.csv(file="all_preds_for_sdms.csv")
head(uni_vars)

vars <- uni_vars$var
#  change vars for new structure
# vars <- vars[-1]
vars
## extract preds to run

setwd("/Users/katie/Documents/manuscript3/pred_data/Acc_climate")
ac <- list.files(patter="tif")


setwd("/Users/katie/Documents/manuscript3/pred_data/Hydrology")
hl <- list.files(pattern=".tif")
hl

setwd("/Users/katie/Documents/manuscript3/pred_data/worldclim")
wl <- list.files(pattern=".tif")



## upload all preds
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/data_2018/sdms/landuse")
setwd("/Users/katie/Documents/manuscript3/pred_data/Acc_climate")
a <- grep(paste(vars, collapse="|"),   ac)
a
apx <- ac[a]
# lpx
apr <- stack(apx)

setwd("/Users/katie/Documents/manuscript3/pred_data/worldclim")
w <- grep(paste(vars, collapse="|"),   wl)
w
wpx <- wl[w]
# lpx
wpr <- stack(wpx)

setwd("/Users/katie/Documents/manuscript3/pred_data/Hydrology")
h <- grep(paste(vars, ".tif", collapse="|", sep=""),   hl)
h
hpx <- hl[h]
# cpx
hpr <- stack(hpx)

#  change extent
r <- raster("/Users/katie/Documents/manuscript2/pred_data/agriculture_land_use.correct.r.tif")
hpreds_r <- crop(extend(hpr, r), r)
all.equal(extent(r), extent(hpreds_r))
hpreds_rx <- resample(hpreds_r, r)

#  stack rasters
preds <- stack(hpreds_rx, wprm, apr)
preds#  stack rasters
?shapefile
#  mask raster by stream network
str_net_points <- shapefile("/Users/katie/Documents/manuscript3/sdms/acc_hydro/binary/binary Alainites_muticus_acc_hyd_ensemble.shp")
str_net_points
df3 <- as.data.frame(str_net_points)
head(df3)
wprm <- mask(preds, str_net_points)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

# species sites
sx2 <-raster::extract(preds,str_net_points) ## extract info from environamtal raster at all sites
class(sx2)
head(sx2)
sx2 <- cbind(sx2, df3)


sx2 <- sx2 %>%
  select(-binary) %>%
  pivot_longer(dh1:bio_12_all_de.r, names_to = "variable", values_to = "value") %>%
  # mutate(coord_code = paste(coords.x1, "_", coords.x1, sep="")) %>%
  group_by(variable) %>%
  summarise(Mean = mean(value), STE = std(value), Min = min(value), Max = max(value))

write.csv(sx2, "/Users/katie/Documents/manuscript3/pred_vals_str_net.csv")

