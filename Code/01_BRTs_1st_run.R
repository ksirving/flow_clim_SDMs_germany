#  mnuscript 3 BRTs

library(dismo)
library(raster)

#  taxa upload

#  taxa
setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/")
path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/"
load(file="taxa_over_20_occs_catchments.RData")
head(t)
sort(unique(t$Datum))

head(t)
dim(t)
## make species list
spp_list <- sort(unique(t$taxa_c))
spp_list
###  all sample sites
sites <- t[,c(4:5)] ## subset coords - watch out for x column
sites

## make coords code
sites$coord_code <- paste(sites$join_x, "_", sites$join_y, sep="") ## add coord code
sitesx <- sites[!duplicated(sites[, 3]),] ## remove duplicate sites
dim(sitesx) ## 1260    3
head(sitesx)

t$taxa_t <- gsub("Limnodrilus claparedeianus", "Limnodrilus claparedeanus", t$taxa_c)
t$taxa_c <- t$taxa_t
head(t)
t <- t[, -7]
spp_list <- spp_list[-140]
#  predictor upload

# accumulative climate
getwd()
setwd("input_data/ac_climate")
ac <- list.files(patter="tif")
acpreds <- stack(ac)
acpreds

####### coeficients dataframe

coefs <- data.frame(matrix(ncol=6))
colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")


#### first species to get structure of dataframe
i=1

sx <- paste(spp_list[i])
sx
occ_spx <- subset(t, taxa_c == sx)# subset one species
occ_spx <- unique(occ_spx) 
head(occ_spx)
occ_spx$sp <- 1 ## add column with all presences to 1

occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove duplicate sites - so left with only 1 occurence in 30 years period
dim(data2) ## 171   6

colnames(data2)[7] <- paste(sx) ## species name as column name
head(data2)

## add absences - i.e. other sampling sites
sitesx <- as.data.frame(sitesx)
data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
head(data)
datax <- data[, -c(4:9)] ## remove extra unwanted columns
datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
head(datax)

### add environmental  variables
coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
sx2 <-raster::extract(acpreds,sitesx) ## extract info from environamtal raster at sampling sites
df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt

df <- na.omit(df) ## dim 1248   23 - NAs are due to study area reduction
head(df)
dfx <- df[1:6, c(2,3:8)]
names(df)
dfx
### boosted regression tree

tbrt <- gbm.step(df, gbm.x = 5:23, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5) 
b_res <- as.data.frame(summary(tbrt))
tbrt$cv.statistics$cv.threshold

coefs[i,1] <- paste(sx)
coefs[i,2] <- tbrt$n.trees
coefs[i,3] <- tbrt$cv.statistics$deviance.mean
coefs[i,4] <- tbrt$cv.statistics$correlation.mean
coefs[i,5] <- tbrt$cv.statistics$discrimination.mean
coefs[i,6] <- tbrt$cv.statistics$cv.threshold
getwd()
setwd(paste0(path,"output_data/BRTs_1st/ac_climate"))
save(coefs, file=paste( sx, "_aclim_brt_coefs.RData", sep=""))


colnames(b_res)[2] <- paste(sx)
b_resx <- b_res


##### loop around remaining species

library(doParallel)
library(foreach)
# spp_list
spp_list <- spp_list[-140]
# spp_list <- spp_list[1:20] ## subset data to test loop
# setwd(paste0(path,"input_data/ac_climate"))


cl <- makePSOCKcluster(5, outfile="")
registerDoParallel(cl)
getDoParWorkers()

result <- foreach(i=1:length(spp_list), .combine = rbind, .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% { ## start loop for parallel processing
  
  cat("Running site", i, "\n")
  # for(i in 2:length(spp_list)) {  
  
  sx <- paste(spp_list[i])
  sx
  occ_spx <- subset(t, taxa_c == sx)# subset one species
  occ_spx <- unique(occ_spx) ##184   4
  # head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  
  head(data2)
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  # head(data)
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  # head(datax)
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(acpreds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  df <- na.omit(df)
  
  #  ### boosted regression tree
  
  
  tbrt <- try(gbm.step(df, gbm.x = 5:23, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T) 
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  
  
  bx <- length(b_res)
  
  ## colnames with no brt doesn't work - use if-else and empty dataframe to save
  if (bx == 1) { 
    
    b_res <- data.frame(matrix(ncol=2, nrow=19))
    colnames(b_res)[2] <- try(paste(sx), silent=T)  
    coefs <- data.frame(matrix(ncol=6))
    colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")
    
    
  } else { 
    
    colnames(b_res)[2] <- try(paste(sx), silent=T) 
    coefs <- data.frame(matrix(ncol=6))
    colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")
    coefs[i,1] <- paste(sx)
    coefs[i,2] <- tbrt$n.trees
    coefs[i,3] <- tbrt$cv.statistics$deviance.mean
    coefs[i,4] <- tbrt$cv.statistics$correlation.mean
    coefs[i,5] <- tbrt$cv.statistics$discrimination.mean
    coefs[i,6] <- tbrt$cv.statistics$cv.threshold
  }
  
  # coefs
  
  save(coefs, file=paste( sx, "_aclim_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_aclim_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
}

# b_resx <- na.omit(result)

stopCluster(cl) ## stop parallels

#  hydrology
getwd()
setwd(paste0(path,"input_data/hydrology"))
hl <- list.files(pattern=".tif")
hl
hpreds <- stack(hl)

####### coeficients dataframe

coefs <- data.frame(matrix(ncol=6))
colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")


#### first species to get structure of dataframe
i=1

sx <- paste(spp_list[i])
sx
occ_spx <- subset(t, taxa_c == sx)# subset one species
occ_spx <- unique(occ_spx) 
head(occ_spx)
occ_spx$sp <- 1 ## add column with all presences to 1

occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove duplicate sites - so left with only 1 occurence in 30 years period
dim(data2) ## 171   6

colnames(data2)[7] <- paste(sx) ## species name as column name
head(data2)

## add absences - i.e. other sampling sites
sitesx <- as.data.frame(sitesx)
data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
head(data)
datax <- data[, -c(4:9)] ## remove extra unwanted columns
datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now

### add environmental  variables
coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
sx2 <-raster::extract(hpreds,sitesx) ## extract info from environamtal raster at sampling sites
df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt

df <- na.omit(df) ## dim 1248   23 - NAs are due to study area reduction
head(df)
names(df)
### boosted regression tree

tbrt <- try(gbm.step(df, gbm.x = 5:56, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T) 
b_res <- try(as.data.frame(summary(tbrt)), silent=T)

coefs[i,1] <- paste(sx)
coefs[i,2] <- tbrt$n.trees
coefs[i,3] <- tbrt$cv.statistics$deviance.mean
coefs[i,4] <- tbrt$cv.statistics$correlation.mean
coefs[i,5] <- tbrt$cv.statistics$discrimination.mean
coefs[i,6] <- tbrt$cv.statistics$cv.threshold

setwd(paste0(path,"output_data/BRTs_1st/hydro"))
save(coefs, file=paste( sx, "_hydro_brt_coefs.RData", sep=""))


colnames(b_res)[2] <- paste(sx)
b_resx <- b_res


##### loop around remaining species

library(doParallel)
library(foreach)

# spp_list <- spp_list[-140]
# spp_list <- spp_list[1:20] ## subset data to test loop



cl <- makePSOCKcluster(5, outfile="")
registerDoParallel(cl)
getDoParWorkers()

result <- foreach(i=1:length(spp_list), .combine = rbind, .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% { ## start loop for parallel processing
  
  cat("Running site", i, "\n")
  # for(i in 2:length(spp_list)) {  
  
  sx <- paste(spp_list[i])
  sx
  occ_spx <- subset(t, taxa_c == sx)# subset one species
  occ_spx <- unique(occ_spx) ##184   4
  # head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  
  
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(hpreds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  df <- na.omit(df)
  
  #  ### boosted regression tree
  
  
  tbrt <- try(gbm.step(df, gbm.x = 5:56, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T) 
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  
  
  bx <- length(b_res)
  
  ## colnames with no brt doesn't work - use if-else and empty dataframe to save
  if (bx == 1) { 
    
    b_res <- data.frame(matrix(ncol=2, nrow=52))
    colnames(b_res)[2] <- try(paste(sx), silent=T)  
    coefs <- data.frame(matrix(ncol=6))
    colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")
    
    
  } else { 
    
    colnames(b_res)[2] <- try(paste(sx), silent=T) 
    coefs <- data.frame(matrix(ncol=6))
    colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")
    coefs[i,1] <- paste(sx)
    coefs[i,2] <- tbrt$n.trees
    coefs[i,3] <- tbrt$cv.statistics$deviance.mean
    coefs[i,4] <- tbrt$cv.statistics$correlation.mean
    coefs[i,5] <- tbrt$cv.statistics$discrimination.mean
    coefs[i,6] <- tbrt$cv.statistics$cv.threshold
  }
  
  # coefs
  
  save(coefs, file=paste( sx, "_hydro_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_hydro_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
}

# b_resx <- na.omit(result)

stopCluster(cl) ## stop parallels

# worldclim



setwd("/Users/katie/Documents/manuscript3/pred_data/worldclim")
wl <- list.files(pattern=".tif")
wpreds <- stack(wl)

####### coeficients dataframe

coefs <- data.frame(matrix(ncol=6))
colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")


#### first species to get structure of dataframe
i=1

sx <- paste(spp_list[i])
sx
occ_spx <- subset(t, taxa_c == sx)# subset one species
occ_spx <- unique(occ_spx) 
head(occ_spx)
occ_spx$sp <- 1 ## add column with all presences to 1

occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove duplicate sites - so left with only 1 occurence in 30 years period
dim(data2) ## 171   6

colnames(data2)[7] <- paste(sx) ## species name as column name
head(data2)

## add absences - i.e. other sampling sites
sitesx <- as.data.frame(sitesx)
data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
head(data)
datax <- data[, -c(4:9)] ## remove extra unwanted columns
datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now

### add environmental  variables
coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
sx2 <-raster::extract(wpreds,sitesx) ## extract info from environamtal raster at sampling sites
df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt

df <- na.omit(df) ## dim 1248   23 - NAs are due to study area reduction
head(df)
names(df)
### boosted regression tree

tbrt <- try(gbm.step(df, gbm.x = 5:23, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T) 
b_res <- try(as.data.frame(summary(tbrt)), silent=T)

coefs[i,1] <- paste(sx)
coefs[i,2] <- tbrt$n.trees
coefs[i,3] <- tbrt$cv.statistics$deviance.mean
coefs[i,4] <- tbrt$cv.statistics$correlation.mean
coefs[i,5] <- tbrt$cv.statistics$discrimination.mean
coefs[i,6] <- tbrt$cv.statistics$cv.threshold

setwd("/Users/katie/Documents/manuscript3/BRTs/worldclim")
save(coefs, file=paste( sx, "_wclim_brt_coefs.RData", sep=""))


colnames(b_res)[2] <- paste(sx)
b_resx <- b_res


##### loop around remaining species

library(doParallel)
library(foreach)

# spp_list <- spp_list[-140]
# spp_list <- spp_list[1:20] ## subset data to test loop
setwd("/Users/katie/Documents/manuscript3/BRTs/worldclim")

i=1
cl <- makePSOCKcluster(5, outfile="")
registerDoParallel(cl)
getDoParWorkers()

result <- foreach(i=1:length(spp_list), .combine = rbind, .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% { ## start loop for parallel processing
  
  cat("Running site", i, "\n")
  # for(i in 2:length(spp_list)) {  
  
  sx <- paste(spp_list[i])
  sx
  occ_spx <- subset(t, taxa_c == sx)# subset one species
  occ_spx <- unique(occ_spx) ##184   4
  head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  
  head(data2)
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  head(data)
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(wpreds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  df <- na.omit(df)
  
  #  ### boosted regression tree
  
  
  tbrt <- try(gbm.step(df, gbm.x = 5:23, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T) 
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  
  
  bx <- length(b_res)
  
  ## colnames with no brt doesn't work - use if-else and empty dataframe to save
  if (bx == 1) { 
    
    b_res <- data.frame(matrix(ncol=2, nrow=19))
    colnames(b_res)[2] <- try(paste(sx), silent=T)  
    coefs <- data.frame(matrix(ncol=6))
    colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")
    
    
  } else { 
    
    colnames(b_res)[2] <- try(paste(sx), silent=T) 
    coefs <- data.frame(matrix(ncol=6))
    colnames(coefs) <- c("taxa", "no_of_trees", "dev_mean", "cor_mean", "discrim_mean", "cv_thres")
    coefs[i,1] <- paste(sx)
    coefs[i,2] <- tbrt$n.trees
    coefs[i,3] <- tbrt$cv.statistics$deviance.mean
    coefs[i,4] <- tbrt$cv.statistics$correlation.mean
    coefs[i,5] <- tbrt$cv.statistics$discrimination.mean
    coefs[i,6] <- tbrt$cv.statistics$cv.threshold
  }
  
  # coefs
  
  save(coefs, file=paste( sx, "_wclim_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_wclim_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
}

# b_resx <- na.omit(result)

stopCluster(cl) ## stop parallels

