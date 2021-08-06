#### SDMs
setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/")
path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/"

### both sdms run

library(sdm)
library(usdm)
library(raster)
library(foreach)
library(doParallel)
installAll(sdm)
# library(devtools)
devtools::install_github("babaknaimi/sdm")
# library(sdm)
getmethodNames('sdm')
getmethod("sdm")
library(rgdal)
library(maptools)
library(reshape)
library(caret)
library(raster)
library(dplyr)
# getwd()
# ?getEvaluation
# ?sdmEvaluate

## upload species data
getwd()
load(file="input_data/taxa_over_20_occs_catchments.RData")
taxa <- t
head(taxa)
taxa$taxa_x <- gsub(" ", "_", taxa$taxa_c, fixed=T)

taxa171 <- read.csv("output_data/taxa_list_80.csv")
head(taxa171)
names(taxa171)
spp_list<- taxa171[,-1]
spp_list
## replace . to match taxa df
spp_list <- gsub(".", "_", spp_list, fixed=T)
sdmtaxa <- subset(taxa, taxa_x %in% spp_list) 

head(sdmtaxa)
dim(sdmtaxa) ## 10721     7


sdmtaxa <- sdmtaxa[!duplicated(sdmtaxa), ] ## remove duplicates, only 1 presence at each site
length(unique(sdmtaxa$taxa_c)) ## 93
coords <- sdmtaxa[, 4:5] ## ## separate coords
dim(coords) ## 14854     2
head(coords)
coords <- coords[!duplicated(coords),]
## upload all preds

###################### acc and hydrology

uni_vars <- read.csv(file="output_data/all_preds_for_sdms.csv")
head(uni_vars)

vars <- uni_vars$x
#  change vars for new structure

vars

#  mh21            dh1            
# [4] bio_08_all_de.r bio_09_all_de.r bio_12_all_de.r

## extract preds to run

setwd(paste0(path,"input_data/ac_climate"))
ac <- list.files(patter="tif")


setwd(paste0(path,"input_data/Hydrology"))
hl <- list.files(pattern=".tif")


setwd(paste0(path,"input_data/worldclim"))
wl <- list.files(pattern=".tif")



## upload all preds
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/data_2018/sdms/landuse")
setwd(paste0(path,"input_data/ac_climate"))
a <- grep(paste(vars, collapse="|"),   ac)
a
apx <- ac[a]
# lpx
apr <- stack(apx)

setwd(paste0(path,"input_data/worldclim"))
w <- grep(paste(vars, collapse="|"),   wl)
w
wpx <- wl[w]
# lpx
wpr <- stack(wpx)

setwd(paste0(path,"input_data/Hydrology"))
h <- grep(paste(vars, ".tif", collapse="|", sep=""),   hl)
h
hpx <- hl[h]
class(hpx)
# cpx
hpr <- raster(hpx)

#  change extent
r <- raster(paste0(path,"input_data/agriculture_land_use.correct.r.tif"))
hpreds_r <- crop(extend(hpr, r), r)
all.equal(extent(r), extent(hpreds_r))
hpreds_rx <- resample(hpreds_r, r)

#  stack rasters
preds <- stack(hpreds_rx, apr)
preds

spp_list
###############
setwd(paste0(path,"output_data/sdms/ac_hydro"))
s=1
# cl <- makePSOCKcluster(5, outfile="")
# registerDoParallel(cl)
# getDoParWorkers()
# s=6
foreach(s=1:length(spp_list), .packages=c("sdm", "raster", "rgdal", "maptools")) %dopar% {
  # for(s in 1:length(spp_list)) {
  
  ## species data
  # s
  
  cat("Running species", s, "\n")
  sx <- spp_list[s]
  df3 <- subset(sdmtaxa, taxa_x==sx)
  # head(df3)
  sitesx <- df3[, 4:5] #
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  
  try(coordinates(df3) <- c("join_x","join_y"), silent=T)## convert to spatialpointsdataframe
  
  sx2 <-raster::extract(preds, sitesx) ## extract info from environamtal raster at sampling sites
  df4 <- cbind(df3, sx2)
  
  df4@data <- df4@data[,'taxa_x',drop=F]
  try(coordinates(df4) <- c("join_x","join_y"), silent=T)
  
  d2<-sdmData(taxa_x~.,train=df4, predictors=preds, bg=list(n=2000,method='gRandom',remove=TRUE)) ## 
d2
  
  m2<-sdm(~.,data=d2, methods=c('glm','tree','brt', 'fda','rbf' ),test.percent=30, replicatin="boot", n=10) ##replicatin="boot", n=10
  m2
  
  sts <- getEvaluation(m2, wtest="train", stat=c('TSS','Kappa','AUC', 'sensitivity', 'specificity', "threshold"),opt=2)

  sts$taxa <- sx
  
  try(sts$model[1:10] <- c("glm"),silent=T)
  try(sts$model[11:20] <- c("tree"),silent=T)
  try(sts$model[21:30] <- c("brt"),silent=T)
  try(sts$model[31:40] <- c("fda"),silent=T)
  try(sts$model[41:50] <- c("rbf"),silent=T)
  
  
  ## variable importance of train and testing data
  vi_train <- getVarImp(m2,id=1,wtest='training') # variable importance based on training dataset
  vi_test <- getVarImp(m2,id=1,wtest='test.dep')
  
  ## coerce to df and add species name
  vi_trainx <- as.data.frame(vi_train@varImportance)
  colnames(vi_trainx)[1] <- sx
  
  vi_testx <- as.data.frame(vi_test@varImportance)
  colnames(vi_testx)[1] <- sx
  
  
  
  ## write csv
  write.csv(vi_testx, paste(sx, "_acc_hyd_var_imp_test.csv", sep=""))
  write.csv(vi_trainx, paste(sx, "_acc_hyd_var_imp_train.csv", sep=""))
  
  # # ### create and save plots
  par(mar = rep(2, 4))
  jpeg(paste(sx,'_acc_hyd_var_imp_auc_train.jpg', sep=""))
  plot(vi_train, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_acc_hyd_var_imp_cor_train.jpg', sep=""))
  plot(vi_train, 'cor')
  dev.off()
  #
  jpeg(paste(sx,'_acc_hyd_var_imp_auc_test.jpg', sep=""))
  plot(vi_test, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_acc_hyd_var_imp_cor_test.jpg', sep=""))
  plot(vi_test, 'cor')
  dev.off()
  
  jpeg(paste(sx,'_acc_hyd_roc.jpg', sep=""))
  roc(m2)
  dev.off()
  
  write.csv(sts, paste(sx, "_acc_hyd_stats.csv",sep=""))
  
  e1 <- ensemble(m2,newdata=preds,filename= paste(sx, "_acc_hyd_ensemble.grd",sep=""),
                 setting=list(method='weighted',stat='TSS',opt=2), overwrite=T) # paste(sx, "_uni_ensemble.tif",sep="")
  
  
  rm(e1)
  
}



stopCluster(cl)


sx


# world clim and hydrology


#  stack rasters
preds <- stack(hpreds_rx, wpr)
preds
en_df <- as.data.frame(preds, xy=T, na.rm=T)
dim(en_df)

###############
spp_list

# cl <- makePSOCKcluster(5, outfile="")
# registerDoParallel(cl)
# getDoParWorkers()
# # s=5
s=1
foreach(s=1:length(spp_list), .packages=c("sdm", "raster", "rgdal", "maptools")) %dopar% {
# for(s in 1:length(spp_list)) {
  
  ## species data
  # s
  cat("Running species", s, "\n")
  sx <- spp_list[s]
  
  df3 <- subset(sdmtaxa, taxa_x==sx)
  # head(df4)
  # head(sdmtaxa)
  # head(df3)
  # rownames(df3) <- paste("obs", "_", rownames(df3), sep="")
  sitesx <- df3[, 4:5] #
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  
  
  
  try(coordinates(df3) <- c("join_x","join_y"), silent=T)## convert to spatialpointsdataframe
  
  sx2 <-raster::extract(preds, sitesx) ## extract info from environamtal raster at sampling sites
  df4 <- cbind(df3, sx2)
  # head(df4)
  
  df4@data <- df4@data[,'taxa_x',drop=F]
  try(coordinates(df4) <- c("join_x","join_y"), silent=T)
  
  d2<-sdmData(taxa_x~.,train=df4, predictors=preds, bg=list(n=2000,method='gRandom',remove=TRUE)) ## 
  # d2
  
  
  
  m2<-sdm(~.,data=d2,methods=c('glm','tree','brt', 'fda','rbf' ),test.percent=30, replicatin="boot", n=10) ##replicatin="boot", n=10
  m2
  
  sts <- getEvaluation(m2, wtest="train", stat=c('TSS','Kappa','AUC', 'sensitivity', 'specificity', "threshold"),opt=2)
  sts$taxa <- sx
  
  try(sts$model[1:10] <- c("glm"),silent=T)
  try(sts$model[11:20] <- c("tree"),silent=T)
  try(sts$model[21:30] <- c("brt"),silent=T)
  try(sts$model[31:40] <- c("fda"),silent=T)
  try(sts$model[41:50] <- c("rbf"),silent=T)
  
  # sts
  
  
  ## variable importance of train and testing data
  vi_train <- getVarImp(m2,id=1,wtest='training') # variable importance based on training dataset
  vi_test <- getVarImp(m2,id=1,wtest='test.dep')
  
  ## coerce to df and add species name
  vi_trainx <- as.data.frame(vi_train@varImportance)
  colnames(vi_trainx)[1] <- sx
  
  vi_testx <- as.data.frame(vi_test@varImportance)
  colnames(vi_testx)[1] <- sx
  
  
  setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
  ## write csv
  write.csv(vi_testx, paste(sx, "_wclim_hyd_var_imp_test.csv", sep=""))
  write.csv(vi_trainx, paste(sx, "_wclim_hyd_var_imp_train.csv", sep=""))
  
  # # ### create and save plots
  par(mar = rep(2, 4))
  jpeg(paste(sx,'_wclim_hyd_var_imp_auc_train.jpg', sep=""))
  plot(vi_train, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_hyd_var_imp_cor_train.jpg', sep=""))
  plot(vi_train, 'cor')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_hyd_var_imp_auc_test.jpg', sep=""))
  plot(vi_test, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_hyd_var_imp_cor_test.jpg', sep=""))
  plot(vi_test, 'cor')
  dev.off()
  
  jpeg(paste(sx,'_wclim_hyd_roc.jpg', sep=""))
  roc(m2)
  dev.off()
  
  write.csv(sts, paste(sx, "_wclim_hyd_stats.csv"),sep="")
  
  e1 <- ensemble(m2,newdata=preds,filename= paste(sx, "_wclim_hyd_ensemble.grd",sep=""),
                 setting=list(method='weighted',stat='TSS',opt=2), overwrite=T) # paste(sx, "_uni_ensemble.tif",sep="")
  
  
  rm(e1)
  
}

stopCluster(cl)


# all three


setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_wclim_hydro")
uni_vars <- read.csv(file="all_preds_for_sdms.csv")
head(uni_vars)
vars <- uni_vars$var

vars
# 
# [1] Bio09_worldclim  Bio04_worldclim  Bio015_worldclim
# [4] Bio08_worldclim  mh21             Bio02_worldclim 
# [7] dh1              bio_08_all_de.r  bio_09_all_de.r 
# [10] bio_12_all_de.r 

## extract preds to run

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

#  mask raster by stream network
str_net_points <- shapefile("/Users/katie/Documents/manuscript3/sdms/acc_hydro/binary Acroloxus_lacustris_acc_hyd_ensemble.shp")
str_net_points
wprm <- mask(wpr, str_net_points)


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
preds


###############
spp_list

cl <- makePSOCKcluster(5, outfile="")
registerDoParallel(cl)
getDoParWorkers()
# s=5
s=60
foreach(s=1:length(spp_list), .packages=c("sdm", "raster", "rgdal", "maptools")) %dopar% {
  # for(s in 1:length(spp_list)) {
  
  ## species data
  # s
  sx <- spp_list[s]
  
  
  
  df3 <- subset(sdmtaxa, taxa_x==sx)
  # head(df4)
  # head(sdmtaxa)
  # head(df3)
  sitesx <- df3[, 4:5] #
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  
  
  
  try(coordinates(df3) <- c("join_x","join_y"), silent=T)## convert to spatialpointsdataframe
  
  sx2 <-raster::extract(preds, sitesx) ## extract info from environamtal raster at sampling sites
  df4 <- cbind(df3, sx2)
  # head(df4)
  
  df4@data <- df4@data[,'taxa_x',drop=F]
  try(coordinates(df4) <- c("join_x","join_y"), silent=T)
  
  d2<-sdmData(taxa_x~.,train=df4, predictors=preds, bg=list(n=2000,method='gRandom',remove=TRUE)) ## 
  # d2
  
  
  m2<-sdm(~.,data=d2,methods=c('glm','tree','brt', 'fda','rbf' ),test.percent=30, replicatin="boot", n=10) ##replicatin="boot", n=10
  m2
  
  sts <- getEvaluation(m2, wtest="train",stat=c('TSS','Kappa','AUC', 'sensitivity', 'specificity', "threshold"),opt=2)
  sts$taxa <- sx
  
  try(sts$model[1:10] <- c("glm"),silent=T)
  try(sts$model[11:20] <- c("tree"),silent=T)
  try(sts$model[21:30] <- c("brt"),silent=T)
  try(sts$model[31:40] <- c("fda"),silent=T)
  try(sts$model[41:50] <- c("rbf"),silent=T)
  
  # sts
  
  
  ## variable importance of train and testing data
  vi_train <- getVarImp(m2,id=1,wtest='training') # variable importance based on training dataset
  vi_test <- getVarImp(m2,id=1,wtest='test.dep')
  
  
  ## coerce to df and add species name
  vi_trainx <- as.data.frame(vi_train@varImportance)
  colnames(vi_trainx)[1] <- sx
  
  vi_testx <- as.data.frame(vi_test@varImportance)
  colnames(vi_testx)[1] <- sx
  
  
  setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked")
  ## write csv
  write.csv(vi_testx, paste(sx, "_wclim_acc_hyd_var_imp_test.csv", sep=""))
  write.csv(vi_trainx, paste(sx, "_wclim_acc_hyd_var_imp_train.csv", sep=""))
  
  # # ### create and save plots
  par(mar = rep(2, 4))
  jpeg(paste(sx,'_wclim_acc_hyd_var_imp_auc_train.jpg', sep=""))
  plot(vi_train, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_acc_hyd_var_imp_cor_train.jpg', sep=""))
  plot(vi_train, 'cor')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_acc_hyd_var_imp_auc_test.jpg', sep=""))
  plot(vi_test, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_acc_hyd_var_imp_cor_test.jpg', sep=""))
  plot(vi_test, 'cor')
  dev.off()
  
  jpeg(paste(sx,'_wclim_acc_hyd_roc.jpg', sep=""))
  roc(m2)
  dev.off()
  
  write.csv(sts, paste(sx, "_wclim_acc_hyd_stats.csv"),sep="")
  
  e1 <- ensemble(m2,newdata=preds,filename= paste(sx, "_wclim_acc_hyd_ensemble.grd",sep=""),
                 setting=list(method='weighted',stat='TSS',opt=2), overwrite=T) # paste(sx, "_uni_ensemble.tif",sep="")
  
  
  rm(e1)
  
}

stopCluster(cl)

warnings()


# accumulative climate & worldclim


setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_wclim")
uni_vars <- read.csv(file="all_preds_for_sdms.csv")
head(uni_vars)
vars <- uni_vars$var

vars
# 
# [1] Bio09_worldclim  Bio015_worldclim Bio04_worldclim 
# [4] Bio08_worldclim  Bio02_worldclim  bio_12_all_de.r 
# [7] bio_08_all_de.r  bio_09_all_de.r 

## extract preds to run

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

#  mask raster by stream network
str_net_points <- shapefile("/Users/katie/Documents/manuscript3/sdms/acc_hydro/binary Acroloxus_lacustris_acc_hyd_ensemble.shp")
str_net_points
wprm <- mask(wpr, str_net_points)

#  stack rasters
preds <- stack(wprm, apr)
preds


###############
spp_list <- spp_list[-1]
#  remove missing species

cl <- makePSOCKcluster(5, outfile="")
registerDoParallel(cl)
getDoParWorkers()
# s=5

foreach(s=1:length(spp_list), .packages=c("sdm", "raster", "rgdal", "maptools")) %dopar% {
  # for(s in 1:length(spp_list)) {
  
  ## species data
  # s
  sx <- spp_list[s]
  
  
  
  df3 <- subset(sdmtaxa, taxa_x==sx)
  # head(df4)
  # head(sdmtaxa)
  # head(df3)
  sitesx <- df3[, 4:5] #
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  
  
  
  try(coordinates(df3) <- c("join_x","join_y"), silent=T)## convert to spatialpointsdataframe
  
  sx2 <-raster::extract(preds, sitesx) ## extract info from environamtal raster at sampling sites
  df4 <- cbind(df3, sx2)
  # head(df4)
  
  df4@data <- df4@data[,'taxa_x',drop=F]
  try(coordinates(df4) <- c("join_x","join_y"), silent=T)
  
  d2<-sdmData(taxa_x~.,train=df4, predictors=preds, bg=list(n=2000,method='gRandom',remove=TRUE)) ## 
  # d2
  
  
  m2<-sdm(~.,data=d2,methods=c('glm','tree','brt', 'fda','rbf' ),test.percent=30, replicatin="boot", n=10) ##replicatin="boot", n=10
  m2
  
  sts <- getEvaluation(m2, wtest="train",stat=c('TSS','Kappa','AUC', 'sensitivity', 'specificity', "threshold"),opt=2)
  sts$taxa <- sx
  
  try(sts$model[1:10] <- c("glm"),silent=T)
  try(sts$model[11:20] <- c("tree"),silent=T)
  try(sts$model[21:30] <- c("brt"),silent=T)
  try(sts$model[31:40] <- c("fda"),silent=T)
  try(sts$model[41:50] <- c("rbf"),silent=T)
  
  # sts
  
  
  ## variable importance of train and testing data
  vi_train <- getVarImp(m2,id=1,wtest='training') # variable importance based on training dataset
  vi_test <- getVarImp(m2,id=1,wtest='test.dep')
  
  
  ## coerce to df and add species name
  vi_trainx <- as.data.frame(vi_train@varImportance)
  colnames(vi_trainx)[1] <- sx
  
  vi_testx <- as.data.frame(vi_test@varImportance)
  colnames(vi_testx)[1] <- sx
  
  
  setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc")
  ## write csv
  write.csv(vi_testx, paste(sx, "_wclim_acc_var_imp_test.csv", sep=""))
  write.csv(vi_trainx, paste(sx, "_wclim_acc_var_imp_train.csv", sep=""))
  
  # # ### create and save plots
  par(mar = rep(2, 4))
  jpeg(paste(sx,'_wclim_acc_var_imp_auc_train.jpg', sep=""))
  plot(vi_train, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_acc_var_imp_cor_train.jpg', sep=""))
  plot(vi_train, 'cor')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_acc_var_imp_auc_test.jpg', sep=""))
  plot(vi_test, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_wclim_acc_var_imp_cor_test.jpg', sep=""))
  plot(vi_test, 'cor')
  dev.off()
  
  jpeg(paste(sx,'_wclim_acc_roc.jpg', sep=""))
  roc(m2)
  dev.off()
  
  write.csv(sts, paste(sx, "_wclim_acc_stats.csv",sep=""))
  
  e1 <- ensemble(m2,newdata=preds,filename= paste(sx, "_wclim_acc_ensemble.grd",sep=""),
                 setting=list(method='weighted',stat='TSS',opt=2), overwrite=T) # paste(sx, "_uni_ensemble.tif",sep="")
  
  
  rm(e1)
  
}

stopCluster(cl)

warnings()

