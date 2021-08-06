### finding important variables for each species
library(dismo)
library(foreach)
library(doParallel)
library(raster)
## upload all datasets - rel imp (results from BRTs)
setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/")
path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/"

setwd(paste0(path,"output_data/BRTs_1st/ac_climate"))

cl <- list.files(pattern="var_imp")
length(cl) ## 430
cl
load(file=paste(cl[1])) ## b_res
dim(b_resx)
b_resx <- b_res
head(b_resx)
b_resx <- b_resx[order(b_resx$var),] ## order by variable name so can cbind 
c=12
for(c in 2:length(cl)) {
  
  cat("Running site", c, "\n")
  load(file=paste(cl[c])) ## b_res ## load file
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  b_res
  
  sp <- colnames(b_res)[2] ## get species name
  
  b_res <- b_res[, -1] ## remove variable column
  
  b_resx <- cbind(b_resx, b_res)  ## combine by column
  colnames(b_resx)[1+c] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
sum(is.na(b_resx)) ## 1083

dim(b_resx) ## 19 431
save(b_resx, file="ac_clim_all_taxa_rel_imp_0001.RData")

############### hydrology
setwd(paste0(path,"output_data/BRTs_1st/hydro"))

hl <- list.files(pattern="var_imp")
length(hl) ## 234
# hl
load(file=paste(hl[2])) ## b_res

b_resx <- b_res
# head(b_resx)
dim(b_resx)
b_resx <- b_resx[order(b_resx$var),] ## order by variable name so can cbind 
h=2
for(h in 2:length(hl)) {
  
  cat("Running site", h, "\n")
  load(file=paste(hl[h])) ## b_res ## load file
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  
  
  sp <- colnames(b_res)[2] ## get species name
 
  b_res <- b_res[, -1] ## remove variable column

  if(length(b_res) == 53) {
  b_res <- b_res[-1]
} else {
  b_res <- b_res
}
  
# length(b_res)
  b_resx <- cbind(b_resx, b_res)  ## combine by column
  colnames(b_resx)[1+h] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
sum(is.na(b_resx)) ## 5824


dim(b_resx) ## 53 235
save(b_resx, file="hydro_all_taxa_rel_imp_0001.RData")



############ world clim


setwd(paste0(path,"output_data/BRTs_1st/worldclim"))

ll <- list.files(pattern="var_imp")
length(ll) ## 430
ll
load(file=paste(ll[1])) ## b_res

b_resx <- b_res
head(b_resx)
b_resx <- b_resx[order(b_resx$var),] ## order by variable name so can cbind 
# h=3
for(l in 2:length(ll)) {
  
  cat("Running site", l, "\n")
  load(file=paste(ll[l])) ## b_res ## load file
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  
  
  sp <- colnames(b_res)[2] ## get species name
  # sp
  b_res <- b_res[, -1] ## remove variable column
  # head(b_res)
  # dim(b_res)
  b_resx <- cbind(b_resx, b_res)  ## combine by column
  colnames(b_resx)[1+l] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
sum(is.na(b_resx)) ## 798


dim(b_resx) ## 7 431
save(b_resx, file="worldclim_all_taxa_rel_imp_0001.RData")

### match species

setwd(paste0(path,"output_data/BRTs_1st/ac_climate"))
load(file="ac_clim_all_taxa_rel_imp_0001.RData") ## b_resx
b_resx1  <- b_resx[, unlist(lapply(b_resx, function(x) !all(is.na(x))))]
ac_clim <- b_resx1
dim(ac_clim)
#  179

setwd(paste0(path,"output_data/BRTs_1st/hydro"))
load(file="hydro_all_taxa_rel_imp_0001.RData") 
b_resx1  <- b_resx[, unlist(lapply(b_resx, function(x) !all(is.na(x))))]
hyd <- b_resx1
dim(hyd)
#  119

setwd(paste0(path,"output_data/BRTs_1st/worldclim"))
load(file="worldclim_all_taxa_rel_imp_0001.RData") ## b_resx1
b_resx1  <- b_resx[, unlist(lapply(b_resx, function(x) !all(is.na(x))))]

wclim <- b_resx1
dim(wclim)
#  194

## match species to only use ones in all groups

aclim_tax <- colnames(ac_clim[-c(1)])
aclim_tax

hyd_tax <- colnames(hyd[-c(1)])
hyd_tax

wclim_tax <- colnames(wclim[-c(1)])
wclim_tax

test <- hyd_tax %in% aclim_tax
test2 <- hyd_tax[test]
test2 ## 99

test2 %in% hyd_tax ## all same

test2 %in% aclim_tax ## all same

test2 %in% wclim_tax ## not same


test3 <- wclim_tax %in% test2

test4 <- wclim_tax[test3]
test4 ## 90

test4 %in% aclim_tax ## all same

test4 %in% hyd_tax ## all same

test4 %in% wclim_tax ## all same

getwd()

setwd(paste0(path,"output_data/"))
write.csv(test4, "taxa_list_90.csv")
# test4 <- read.csv("taxa_list_90.csv")
head(test4)
#  match datasets to matched species

head(ac_clim)
head(hyd)
head(wclim)

taxa_list <- test4

## subset groups to match only taxa in taxa list

hx <- colnames(hyd) %in% taxa_list
sum(hx)
hx
hn <- hyd[, hx]
head(hn)
dim(hn) ## 53 100

cx <- colnames(ac_clim) %in% taxa_list
sum(cx)
cx
cn <- ac_clim[, cx]
head(cn)
dim(cn) ## 19 100

lx <- colnames(wclim) %in% taxa_list
sum(lx)
lx
ln <- wclim[, lx]
head(ln)
dim(ln) ## 19 100

## dataframes with same species 

head(hn)
head(cn)
head(ln)

hyd_var <- hyd[, c(1)]
hyd2 <- cbind(hyd_var, hn)
head(hyd2)
setwd(paste0(path,"output_data/BRTs_1st"))
write.csv(hyd2, "hydro_rel_imp_100_species.csv")

colnames(hyd2)[1] <- "var"

clim_var <- ac_clim[, c(1)]
clim2 <- cbind(clim_var, cn)
head(clim2)
write.csv(clim2, "climate_rel_imp_100_species.csv")

colnames(clim2)[1] <- "var"

wclim_var <- wclim[, c(1)]
wclim2 <- cbind(wclim_var, ln)
head(wclim2)
write.csv(wclim2, "wclim_rel_imp_100_species.csv")
colnames(wclim2)[1] <- "var"

library(raster)
library(maptools)

dim(hyd2)
head(hyd2)
head(wclim2)
head(clim2)

clim2 <- read.csv("climate_rel_imp_100_species.csv")
dim(clim2)
#  get mean of species
clim2$mean <- rowMeans(clim2[3:92]) ## average across all variables
sp1 <- clim2[, c(1, 93)]

sp1 <- sp1[order(sp1[,2]),]

topvars <- tail(sp1, 6) ## 30% of 19
topvars

save(topvars, file=paste("climate_average_top_7_vars_rel_imp.RData", sep=""))
load(file="climate_average_top_7_vars_rel_imp.RData")
#  hydrology

hyd2$mean <- rowMeans(hyd2[2:91])

sp1 <- hyd2[, c(1, 92)]
head(sp1)
tail(sp1)
sp1
sp1 <- sp1[order(sp1[,2]),]

topvars <- tail(sp1, 17) ## 30% of 53
topvars

save(topvars, file=paste("hydro_average_top_17_vars_rel_imp.RData", sep=""))
load(file="hydro_average_top_17_vars_rel_imp.RData")
#  world clim
dim(wclim2)
wclim2$mean <- rowMeans(wclim2[2:91])

sp1 <- wclim2[, c(1, 92)]
# head(sp1)
# tail(sp1)
# sp1
sp1 <- sp1[order(sp1[,2]),]

topvars <- tail(sp1, 6) ## 30% of 19
topvars

save(topvars, file=paste("wclim_sp_average_top_6_vars__rel_imp.RData", sep=""))
load(file="wclim_sp_average_top_6_vars__rel_imp.RData")


########################### brts 2nd run with all categories
### species data

## upload species data
setwd(paste0(path,"input_data"))
load(file="taxa_over_20_occs_catchments.RData")
# taxa <- read.csv("U:/Irving/Assignments/Manuscript2_ubuntu/bio/taxa_workings/taxa_snap_451.csv")
taxa <- t
head(taxa)
# taxa <- taxa[,-1] ## remove weird x column

## match species to ones in BRTs 
spp_list <- taxa_list
spp_list <- as.factor(spp_list)
taxa$taxa_c <- as.character(taxa$taxa_c)

## subset species list from main taxa dataframe

taxa <- taxa[!duplicated(taxa), ] ## remove duplicates, only 1 presence at each site
length(unique(taxa$taxa_c)) ## 236
sites <- taxa[,c(4:5)] ## subset coords - watch out for x column

sites$coord_code <- paste(sites$join_x, "_", sites$join_y, sep="") ## add coord code
# length(unique(sites$coord_code)) ## 7957
sitesx <- sites[!duplicated(sites[, 3]),] ## remove duplicate sites
dim(sitesx) ## 1260    3

#  environmental data

setwd(paste0(path,"input_data/ac_climate"))
ac <- list.files(patter="tif")
acpreds <- stack(ac)
acpreds


setwd(paste0(path,"input_data/Hydrology"))
hl <- list.files(pattern=".tif")
hpreds <- stack(hl)

setwd(paste0(path,"input_data/worldclim"))
wl <- list.files(pattern=".tif")
wpreds <- stack(wl)


setwd(paste0(path,"output_data/BRTs_1st"))
load("climate_average_top_7_vars_rel_imp.RData")

setwd(paste0(path,"input_data/ac_climate"))
clim_vars <- topvars[, 1]
c <- grep(paste(clim_vars, collapse="|"),   ac)
cpx <- ac[c]
acpreds <- stack(cpx)
cpx

setwd(paste0(path,"output_data/BRTs_1st"))
load("hydro_average_top_17_vars_rel_imp.RData")

setwd(paste0(path,"input_data/Hydrology"))


hyd_vars <- topvars[, 1]
h <- grep(paste(hyd_vars, ".tif", sep="",collapse="|"),   hl)
hpx <- hl[h]
hpr <- stack(hpx)
hpx
#  change extent
r <- raster(paste0(path,"input_data/agriculture_land_use.correct.r.tif"))
hpreds_r <- crop(extend(hpr, r), r)
all.equal(extent(r), extent(hpreds_r))
hpreds_rx <- resample(hpreds_r, r)

setwd(paste0(path,"output_data/BRTs_1st"))
load("wclim_sp_average_top_6_vars__rel_imp.RData")

setwd(paste0(path,"input_data/worldclim"))

wclim_vars <- topvars[,1]
w <- grep(paste(wclim_vars, collapse="|"),   wl)
wpx <- wl[w]
wcpreds <- stack(wpx)
wpx
preds <- stack(wcpreds, acpreds, hpreds_rx)
preds

#  brts 2nd run

spp_list 
# spp_list <- spp_list[-1]
spp_list <- gsub("_", " ", spp_list, fixed=T)
s=1


setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim_hydro"))

# cl <- makePSOCKcluster(5, outfile="")
# registerDoParallel(cl)
# getDoParWorkers()

s=1

foreach(s=1:length(spp_list), .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% {
  # for(s in 2:length(spp_list)) {
  
  sx <- paste(spp_list[s])
  # sx
  # # spp_list
  # head(taxa)
  occ_spx <- subset(taxa, taxa_c == sx)# subset one species
  # occ_spx <- unique(occ_spx) ##184   4
  head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  # head(data2)
  
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  # head(data)
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  # dim(datax)
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(preds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  head(df)
  # dim(df)
  
  ###+ boosted regression tree
  tbrt <- try(gbm.step(df, gbm.x = 5:33, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T)
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  head(b_res)
  # b_res
  
  bx <- length(b_res)
  # s=1
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
    coefs[s,1] <- paste(sx)
    coefs[s,2] <- tbrt$n.trees
    coefs[s,3] <- tbrt$cv.statistics$deviance.mean
    coefs[s,4] <- tbrt$cv.statistics$correlation.mean
    coefs[s,5] <- tbrt$cv.statistics$discrimination.mean
    coefs[s,6] <- tbrt$cv.statistics$cv.threshold
  }
  
  # coefs
  
  save(coefs, file=paste(sx, "_all_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_all_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
  
}

getwd()
# stopCluster(cl) ## stop parallels

preds <- stack(acpreds, hpreds_rx)
preds
s=2

#  accumulative climate and hydrology

setwd(paste0(path,"output_data/BRTs_2nd/acc_hydro"))

# cl <- makePSOCKcluster(5, outfile="")
# registerDoParallel(cl)
# getDoParWorkers()

foreach(s=1:length(spp_list), .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% {
  # for(s in 2:length(spp_list)) {
  
  sx <- paste(spp_list[s])
  # sx
  # spp_list
  
  occ_spx <- subset(taxa, taxa_c == sx)# subset one species
  # occ_spx <- unique(occ_spx) ##184   4
  # head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  # head(data2)
  
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  # head(data)
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  # dim(datax)
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(preds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  # head(df)
  # dim(df)
  
  ###+ boosted regression tree
  tbrt <- try(gbm.step(df, gbm.x = 5:27, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T)
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  # head(b_res)
  # b_res
  
  bx <- length(b_res)
  # bx
  ## some species don't wrk with BRT criteria - save as NAs and redo later
  if (bx == 1) { 
    
    b_res <- data.frame(matrix(ncol=2, nrow=27))
    colnames(b_res)[2] <- try(paste(sx), silent=T)  
    coefs <- data.frame(matrix(ncol=5))
    colnames(coefs) <- c("taxa", "no_of_trees", "mean_null", "mean_resid", "correlation")
    
  } else { 
    
    colnames(b_res)[2] <- try(paste(sx), silent=T) 
    coefs <- data.frame(matrix(ncol=5))
    colnames(coefs) <- c("taxa", "no_of_trees", "mean_null", "mean_resid", "correlation")
    coefs[s,1] <- try(paste(sx), silent=T)
    coefs[s,2] <- try(tbrt$n.trees, silent=T)
    coefs[s,3] <- try(tbrt$self.statistics$mean.null, silent=T)
    coefs[s,4] <- try(tbrt$self.statistics$mean.resid, silent=T)
    coefs[s,5] <- try(tbrt$self.statistics$correlation, silent=T)
  }
  
  # coefs
  # getwd()
  
  save(coefs, file=paste(sx, "_all_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_all_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
  
}


# stopCluster(cl) ## stop parallels

#  world clim and hydrology

setwd(paste0(path,"output_data/BRTs_2nd/wclim_hydro"))
preds <- stack(wcpreds, hpreds_rx)
preds
getwd()

# cl <- makePSOCKcluster(5, outfile="")
# registerDoParallel(cl)
# getDoParWorkers()

foreach(s=1:length(spp_list), .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% {
  # for(s in 2:length(spp_list)) {
  
  sx <- paste(spp_list[s])
  # sx
  # spp_list
  
  occ_spx <- subset(taxa, taxa_c == sx)# subset one species
  # occ_spx <- unique(occ_spx) ##184   4
  # head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  # head(data2)
  
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  # head(data)
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  # dim(datax)
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(preds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  # head(df)
  # dim(df)
  
  ###+ boosted regression tree
  tbrt <- try(gbm.step(df, gbm.x = 5:27, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T)
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  # head(b_res)
  # b_res
  
  bx <- length(b_res)
  # bx
  ## some species don't wrk with BRT criteria - save as NAs and redo later
  if (bx == 1) { 
    
    b_res <- data.frame(matrix(ncol=2, nrow=27))
    colnames(b_res)[2] <- try(paste(sx), silent=T)  
    coefs <- data.frame(matrix(ncol=5))
    colnames(coefs) <- c("taxa", "no_of_trees", "mean_null", "mean_resid", "correlation")
    
  } else { 
    
    colnames(b_res)[2] <- try(paste(sx), silent=T) 
    coefs <- data.frame(matrix(ncol=5))
    colnames(coefs) <- c("taxa", "no_of_trees", "mean_null", "mean_resid", "correlation")
    coefs[s,1] <- try(paste(sx), silent=T)
    coefs[s,2] <- try(tbrt$n.trees, silent=T)
    coefs[s,3] <- try(tbrt$self.statistics$mean.null, silent=T)
    coefs[s,4] <- try(tbrt$self.statistics$mean.resid, silent=T)
    coefs[s,5] <- try(tbrt$self.statistics$correlation, silent=T)
  }
  
  # coefs
  
  
  save(coefs, file=paste(sx, "_all_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_all_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
  
}


# stopCluster(cl) ## stop parallels

#  accumulative climate and worldclim

setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim"))
preds <- stack(wcpreds, acpreds)
preds

spp_list 
s=2

# cl <- makePSOCKcluster(5, outfile="")
# registerDoParallel(cl)
# getDoParWorkers()

foreach(s=1:length(spp_list), .packages=c("dismo", "rgdal", "raster","maptools" )) %dopar% {
  # for(s in 2:length(spp_list)) {
  
  sx <- paste(spp_list[s])
  # sx
  # spp_list
  
  occ_spx <- subset(taxa, taxa_c == sx)# subset one species
  # occ_spx <- unique(occ_spx) ##184   4
  # head(occ_spx)
  occ_spx$sp <- 1 ## add column with all presences to 1
  
  occ_spx$coord_code <- paste(occ_spx$join_x, "_", occ_spx$join_y, sep="") ## add coord code
  data2 <- occ_spx[!duplicated(occ_spx[,8]),] ## remove dupliace sites - so left with only 1 occurence in 30 years period
  # head(data2)
  
  colnames(data2)[7] <- paste(sx) ## species name as column name
  
  ## merge with variables
  sitesx <- as.data.frame(sitesx)
  
  data <- merge(sitesx, data2, by="coord_code", all=T) ## merge both dataframes by coord code
  # head(data)
  datax <- data[, -c(4:9)] ## remove extra unwanted columns
  datax[is.na(datax)] <- 0 ## change NAs to 0. df will have binary info now
  # dim(datax)
  ### add environmental  variables
  coordinates(sitesx) <- c("join_x", "join_y") ## make into spatial points dataframe
  sx2 <-raster::extract(preds,sitesx) ## extract info from environamtal raster at sampling sites
  df <- cbind(datax, sx2) ## combine data for species and variables, creates format dataframe as needed for brt
  # head(df)
  # dim(df)
  
  ###+ boosted regression tree
  tbrt <- try(gbm.step(df, gbm.x = 5:16, gbm.y = 4, family ="bernoulli", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5, max.trees = 10000), silent=T)
  b_res <- try(as.data.frame(summary(tbrt)), silent=T)
  # head(b_res)
  # b_res
  
  bx <- length(b_res)
  # bx
  ## some species don't wrk with BRT criteria - save as NAs and redo later
  if (bx == 1) { 
    
    b_res <- data.frame(matrix(ncol=2, nrow=16))
    colnames(b_res)[2] <- try(paste(sx), silent=T)  
    coefs <- data.frame(matrix(ncol=5))
    colnames(coefs) <- c("taxa", "no_of_trees", "mean_null", "mean_resid", "correlation")
    
  } else { 
    
    colnames(b_res)[2] <- try(paste(sx), silent=T) 
    coefs <- data.frame(matrix(ncol=5))
    colnames(coefs) <- c("taxa", "no_of_trees", "mean_null", "mean_resid", "correlation")
    coefs[s,1] <- try(paste(sx), silent=T)
    coefs[s,2] <- try(tbrt$n.trees, silent=T)
    coefs[s,3] <- try(tbrt$self.statistics$mean.null, silent=T)
    coefs[s,4] <- try(tbrt$self.statistics$mean.resid, silent=T)
    coefs[s,5] <- try(tbrt$self.statistics$correlation, silent=T)
  }
  
  # coefs
  
  
  save(coefs, file=paste(sx, "_all_brt_coefs.RData", sep=""))
  
  ## 2nd species add
  
  save(b_res, file=paste(sx, "_all_brt_var_imp.RData", sep=""))
  rm(b_res)
  rm(coefs)
  
  
}


# stopCluster(cl) ## stop parallels




#  correlation analysis
#  add var imp together
#  accumulative climate and hydrology

# accumulative climate and hydrology --------------------------------------


setwd(paste0(path,"output_data/BRTs_2nd/acc_hydro"))
ul <- list.files(pattern="var_imp")
length(ul) ## 100
# ul <- ul[-1]
ul

## make dataframe to add all dfs together
b_resx <- data.frame(matrix(nrow=23, ncol=2))
colnames(b_resx)[1] <- "var"

head(b_resx)
dim(b_resx)

### format first file

# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/all_cats/uni_93_sp_sept_rerun")
load(file=paste(ul[2])) ## b_res
head(b_res)
b_resx[, 1] <-  b_res$var ## 29


b_resx <- merge(b_resx, b_res, by="var", all=T)
b_resx <- b_resx[, c(1, 3)]
sp <- colnames(b_res)[2]
colnames(b_resx)[2] <- sp
b_resx
### uniform
for(c in 2:length(ul)) {
  
  cat("Running site", c, "\n")
  load(file=paste(ul[c])) ## b_res ## load file
  
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  # dim(b_res)
  bx <- dim(b_res)[1]
  
  sp <- colnames(b_res)[2] ## get species name
  
  b_res <- b_res[, -1] ## remove variable column
  
  if (bx == 23){
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  } else{
    b_res <- b_res[c(1:23)]
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  }
  colnames(b_resx)[1+c] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
getwd()
write.csv(b_resx, "acc_hydro_all_var_imp_100_species.csv")


# worldclim climate and hydrology -----------------------------------------



setwd(paste0(path,"output_data/BRTs_2nd/wclim_hydro"))

ul <- list.files(pattern="var_imp")
length(ul) ## 100
# ul <- ul[-101]
ul

## make dataframe to add all dfs together
b_resx <- data.frame(matrix(nrow=23, ncol=2))
colnames(b_resx)[1] <- "var"

head(b_resx)
dim(b_resx)

### format first file

load(file=paste(ul[1])) ## b_res
head(b_res)
b_resx[, 1] <-  b_res$var ## 29


b_resx <- merge(b_resx, b_res, by="var", all=T)
b_resx <- b_resx[, c(1, 3)]
sp <- colnames(b_res)[2]
colnames(b_resx)[2] <- sp
b_resx
### uniform
for(c in 2:length(ul)) {
  
  cat("Running site", c, "\n")
  load(file=paste(ul[c])) ## b_res ## load file
  
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  # dim(b_res)
  bx <- dim(b_res)[1]
  
  sp <- colnames(b_res)[2] ## get species name
  
  b_res <- b_res[, -1] ## remove variable column
  
  if (bx == 23){
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  } else{
    b_res <- b_res[c(1:23)]
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  }
  colnames(b_resx)[1+c] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
dim(b_resx)
getwd()
write.csv(b_resx, "wclim_hydro_all_var_imp_100_species.csv")


# all three ---------------------------------------------------------------


setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim_hydro"))

ul <- list.files(pattern="var_imp")
length(ul) ## 100
# ul <- ul[-1]
ul

## make dataframe to add all dfs together
b_resx <- data.frame(matrix(nrow=29, ncol=2))
colnames(b_resx)[1] <- "var"

head(b_resx)
dim(b_resx)

### format first file

load(file=paste(ul[2])) ## b_res
head(b_res)
b_resx[, 1] <-  b_res$var ## 29


b_resx <- merge(b_resx, b_res, by="var", all=T)
b_resx <- b_resx[, c(1, 3)]
sp <- colnames(b_res)[2]
colnames(b_resx)[2] <- sp
b_resx
### uniform
for(c in 2:length(ul)) {
  
  cat("Running site", c, "\n")
  load(file=paste(ul[c])) ## b_res ## load file
  
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  # dim(b_res)
  bx <- dim(b_res)[1]
  
  sp <- colnames(b_res)[2] ## get species name
  
  b_res <- b_res[, -1] ## remove variable column
  
  if (bx == 29){
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  } else{
    b_res <- b_res[c(1:29)]
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  }
  colnames(b_resx)[1+c] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
getwd()
write.csv(b_resx, "acclim_wclim_hydro_all_var_imp_100_species.csv")


# accumulative climate & worldclim ----------------------------------------



setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim"))

ul <- list.files(pattern="var_imp")
length(ul) ## 100
# ul <- ul[-1]
ul

## make dataframe to add all dfs together
b_resx <- data.frame(matrix(nrow=12, ncol=2))
colnames(b_resx)[1] <- "var"

head(b_resx)
dim(b_resx)

### format first file

load(file=paste(ul[2])) ## b_res
head(b_res)
b_resx[, 1] <-  b_res$var ## 29


b_resx <- merge(b_resx, b_res, by="var", all=T)
b_resx <- b_resx[, c(1, 3)]
sp <- colnames(b_res)[2]
colnames(b_resx)[2] <- sp
b_resx
### uniform
for(c in 2:length(ul)) {
  
  cat("Running site", c, "\n")
  load(file=paste(ul[c])) ## b_res ## load file
  
  colnames(b_res)[1] <- "var" ## 1st colum called var - taxa with no brt have no name
  b_res <- b_res[order(b_res$var),]## order by variable name
  # dim(b_res)
  bx <- dim(b_res)[1]
  
  sp <- colnames(b_res)[2] ## get species name
  
  b_res <- b_res[, -1] ## remove variable column
  
  if (bx == 12){
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  } else{
    b_res <- b_res[c(1:12)]
    b_resx <- cbind(b_resx, b_res)  ## combine by column
  }
  colnames(b_resx)[1+c] <- sp ## add species name
  # b_resx <- merge(b_resx, b_res, by ="var", all=T)
  
}

head(b_resx)
getwd()
write.csv(b_resx, "acclim_wclim_all_var_imp_100_species.csv")


#  correlation 

# match taxa ------------------------------------


library(reshape)
library(caret)
library(raster )

#  list predictor data

setwd(paste0(path,"input_data/ac_climate"))
ac <- list.files(pattern="tif")
acpreds <- stack(ac)
acpreds

setwd(paste0(path,"input_data/Hydrology"))
hl <- list.files(pattern=".tif")
hpreds <- stack(hl)

setwd(paste0(path,"input_data/worldclim"))
wl <- list.files(pattern=".tif")
wpreds <- stack(wl)

#  accumulative climate and hydrology
setwd(paste0(path,"output_data/BRTs_2nd/acc_hydro"))

all_preds <- read.csv("acc_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_predsx)
head(all_preds)
#  remove NAs
all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
head(all_predsx)
acchydpreds <- all_predsx

#  world clim and hydrology


setwd(paste0(path,"output_data/BRTs_2nd/wclim_hydro"))
all_preds <- read.csv("wclim_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
head(all_predsx)
wclimhydpreds <- all_predsx

#  all 3 

setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim_hydro"))
all_preds <- read.csv("acclim_wclim_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
head(all_predsx)
wclimacchydpreds <- all_predsx

#  #  acc climate & worldclim 

setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim"))
all_preds <- read.csv("acclim_wclim_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
head(all_predsx)
wclimaccpreds <- all_predsx


#  species list match and create
aclimhyd_tax <- colnames(acchydpreds[-c(1)])
aclimhyd_tax
# 97
wclimhyd_tax <- colnames(wclimhydpreds[-c(1)])
wclimhyd_tax
# 96
wclimacchyd_tax <- colnames(wclimacchydpreds[-c(1)])
wclimacchyd_tax
# 99
wclimacc_tax <- colnames(wclimaccpreds[-c(1)])
wclimacc_tax

test <- wclimhyd_tax %in% aclimhyd_tax
test2 <- wclimhyd_tax[test]
test2 ## 81

test2 %in% wclimhyd_tax## all same

test2 %in% aclimhyd_tax ## all same

test2 %in% wclimacchyd_tax ## not same


test3 <- wclimacchyd_tax %in% test2

test4 <- wclimacchyd_tax[test3]
test4 ## 84

test4 %in% wclimhyd_tax ## all same

test4 %in% aclimhyd_tax ## all same

test4 %in% wclimacchyd_tax ## all same

test4 %in% wclimacc_tax
test4
#  1 species different - take species out of 

# "Acroloxus.lacustris"
getwd()

setwd(paste0(path,"output_data"))
write.csv(test4, "taxa_list_80.csv")
test4
test4 <- read.csv("taxa_list_80.csv")
spp_list <- test4


# not needed!!!!!!!! ------------------------------------------------------



#  subset only species from spp_list

setwd(paste0(path,"output_data/BRTs_2nd/wclim_hydro"))

all_preds <- read.csv("wclim_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
# all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
al <- which(colnames(all_preds) %in% spp_list)
al
all_predsx <-  all_preds[,c(1,al)]
all_predsx
head(all_predsx)
dim(all_predsx)
#  23 81


all_predsx$mean <- rowMeans(all_predsx[2:81])
all_predsx$mean
## 80 species remain

### get top >1% metrics for each species
all_vars <- all_predsx[, 1]
all_vars

## make df for correlation
vdf <- data.frame(matrix(nrow=23, ncol=1))
vdf[,1] <- all_vars
colnames(vdf)[1] <- "Variables"
vdf 


sp1 <- all_predsx[, c(1, 82)]
sp1



topvars <- sp1 ## 
topvars
dim(topvars) ## 23 2

save(topvars, file=paste0(path,"output_data/BRTs_2nd/wclim_hydro_rel_imp.RData"))

vars <- topvars$var
vars

### extract variable data

setwd(paste0(path,"input_data/worldclim"))
w <- grep(paste(vars, collapse="|"),   wl)
wl
wpx <- wl[w]
# lpx
wpr <- stack(wpx)


setwd(paste0(path,"input_data/Hydrology"))
h <- grep(paste(vars, ".tif", collapse="|", sep=""),   hl)
h
hpx <- hl[h]
# cpx
hpr <- stack(hpx)

#  change extent
r <- raster(paste0(path,"input_data/agriculture_land_use.correct.r.tif"))
            
hpreds_r <- crop(extend(hpr, r), r)
all.equal(extent(r), extent(hpreds_r))
hpreds_rx <- resample(hpreds_r, r)

#  stack rasters
preds <- stack(hpreds_rx, wpr)
preds

preds_df <- as.data.frame(preds)
# head(preds_df)
preds_df <- na.omit(preds_df)
# str(preds_df)



###### multicolinearality

df2 = cor(preds_df)
head(df2)
df2 <- as.data.frame(df2)
getwd()

setwd(paste0(path,"output_data/BRTs_2nd/correlation"))
# write.csv(cor_imp, "wclim_hydro_corrv2.csv")

df2$var <- rownames(df2)

cor_imp <- merge(topvars, df2, by="var")

## make dataframe in order of relative importance
cor_imp <- cor_imp[order(cor_imp[,2], decreasing=TRUE),]

head(cor_imp)
dim(cor_imp)
## remove minus signs from data frame 
cor_imp[] <- lapply(cor_imp, function(x) gsub("-", "", x ))
## change from character to number
cor_imp[, 2:25] <- lapply(cor_imp[,2:25], function(x) as.numeric(as.character(x)))

str(cor_imp)
### if over 0.7 take the variable with the higher importance

## reshape dataframe
library(reshape)
colnames(cor_imp)[2] <- "importance"
cor_imp <- melt(cor_imp, id=c("var", "importance"))
cor_imp$importance2 <- NA

v <- cor_imp$var
## df column for importance of variable 2 - NAs removed as these parwise comparisons are variables < 1%
for(d in 1: length(v)) {
  
  v1 <- v[d]
  imp <- cor_imp$importance[d] 
  
  v2 <- cor_imp$variable %in% v1
  cor_imp$importance2[v2] <- paste(imp)
  
}


cor_imp <- na.omit(cor_imp)

vals <- cor_imp$value


# c=2
### find correlations over 0.7

for (c in 1: length(vals)) {
  
  cv <- vals[c]
  cv
  
  if ( cv > 0.7 && cv < 1){
    cor_imp$corr[c] <- paste("true")
    
  } else {
    cor_imp$corr[c] <- paste("false")
    
  }
}




## subset df to only correlations over 0.7
varcors <- subset(cor_imp, corr=="true")

left_vars <- varcors$var
left_vars
### take the variable of higher relative importance - make list of losing predictors then remove from list of variables.

for (l in 1: length(left_vars)) {
  
  ### first separate pairwise correlations
  ## variable 1
  lvx <- left_vars[l]
  # lvx
  ## variable 2
  ov <- varcors$variable[l]
  # ov
  # left_vars
  ## variable 2 in list to find var importance value
  ovx <- match(ov, left_vars)
  # ovx
  if (varcors[l, 2] < varcors[ovx, 2]) {
    
    varcors$remove[l] <- paste(lvx)
    
  } else { 
    
    varcors$remove[l] <- paste(ov)
    
  }
  
}

varcors

## remove any variables that are correaltied
vb <- unique(varcors$remove)
vb
# vars


# vb
sd <- vars %in% vb
sd
del <- vars[!sd]
del ## remaining variables to use - subset from preds dataframe
# 
# [1] Bio015_worldclim Bio02_worldclim  Bio04_worldclim 
# [4] Bio08_worldclim  Bio09_worldclim  dh1             
# [7] dh2              dh3              dh4             
# [10] ma1              ma12             ma13            
# [13] ma14             ma17             mh21  

# del <- del[-c(7:13)]
del
resx <- subset(cor_imp, var %in% del)
resx <- resx[, 1:2]
resx <- resx[!duplicated(resx),]
resx
# remove the correlated ones not picked up by alorgithm - as were zeros

# [1] Bio015_worldclim Bio02_worldclim  Bio04_worldclim 
# [4] Bio08_worldclim  Bio09_worldclim  dh1             
# [7] ma17             mh21 

getwd()
setwd(paste0(path,"output_data/BRTs_2nd/wclim_hydro"))
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun")
write.csv(resx, file="all_preds_for_sdms.csv")

# accumulative climate and hydrology

#  subset only species from spp_list

setwd(paste0(path,"output_data/BRTs_2nd/acc_hydro"))

all_preds <- read.csv("acc_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
# all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
al <- which(colnames(all_preds) %in% spp_list)
all_predsx <-  all_preds[,c(1,al)]
all_predsx
head(all_predsx)
dim(all_predsx)
#  23   81


all_predsx$mean <- rowMeans(all_predsx[2:81])
all_predsx$var
# all_predsx <- all_predsx[-c(8:14),]
## 80 species remain

### get top >1% metrics for each species
all_vars <- all_predsx[, 1]
all_vars


## make df for correlation
vdf <- data.frame(matrix(nrow=23, ncol=1))
vdf[,1] <- all_vars
colnames(vdf)[1] <- "Variables"
vdf 


sp1 <- all_predsx[, c(1, 82)]
sp1



topvars <- sp1 ## 
topvars
dim(topvars) ## 16 2

# save(topvars, file=paste("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun/uni_95_all_cats_above_1perc_vars_rel_imp.RData", sep=""))
# save(topvars, file=paste("D:/Man_2_August/BRTs/rel_importance/custom_for_sdms/no_precip/no_precip_all_cats_above_1perc_vars_", colnames(sp1)[2], "_rel_imp.RData", sep=""))
save(topvars, file=paste("/Users/katie/Documents/manuscript3/BRTs/2nd_run/rel_imp/acc_hydro_rel_imp.RData", sep=""))

vars <- droplevels(topvars$var)
vars

### extract variable data
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/data_2018/sdms/landuse")
setwd("/Users/katie/Documents/manuscript3/pred_data/Acc_climate")
a <- grep(paste(vars, collapse="|"),   ac)
a
apx <- ac[a]
# lpx
apr <- stack(apx)

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
preds <- stack(hpreds_rx, apr)
preds

preds_df <- as.data.frame(preds)
# head(preds_df)
preds_df <- na.omit(preds_df)
# str(preds_df)
###### multicolinearality

df2 = cor(preds_df)
head(df2)
df2 <- as.data.frame(df2)
getwd()
setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/correlation")
write.csv(cor_imp, "acc_hydro_corrv2.csv")

df2$var <- rownames(df2)

cor_imp <- merge(topvars, df2, by="var")

## make dataframe in order of relative importance
cor_imp <- cor_imp[order(cor_imp[,2], decreasing=TRUE),]

head(cor_imp)
dim(cor_imp)
## remove minus signs from data frame 
cor_imp[] <- lapply(cor_imp, function(x) gsub("-", "", x ))
## change from character to number
cor_imp[, 2:18] <- lapply(cor_imp[,2:18], function(x) as.numeric(as.character(x)))

str(cor_imp)
### if over 0.7 take the variable with the higher importance

## reshape dataframe
library(reshape)
colnames(cor_imp)[2] <- "importance"
cor_imp <- melt(cor_imp, id=c("var", "importance"))
cor_imp$importance2 <- NA

v <- cor_imp$var
## df column for importance of variable 2 - NAs removed as these parwise comparisons are variables < 1%
for(d in 1: length(v)) {
  
  v1 <- v[d]
  imp <- cor_imp$importance[d] 
  
  v2 <- cor_imp$variable %in% v1
  cor_imp$importance2[v2] <- paste(imp)
  
}


cor_imp <- na.omit(cor_imp)

vals <- cor_imp$value


# c=2
### find correlations over 0.7

for (c in 1: length(vals)) {
  
  cv <- vals[c]
  cv
  
  if ( cv > 0.7 && cv < 1){
    cor_imp$corr[c] <- paste("true")
    
  } else {
    cor_imp$corr[c] <- paste("false")
    
  }
}




## subset df to only correlations over 0.7
varcors <- subset(cor_imp, corr=="true")

left_vars <- varcors$var

### take the variable of higher relative importance - make list of losing predictors then remove from list of variables.

for (l in 1: length(left_vars)) {
  
  ### first separate pairwise correlations
  ## variable 1
  lvx <- left_vars[l]
  # lvx
  ## variable 2
  ov <- varcors$variable[l]
  # ov
  # left_vars
  ## variable 2 in list to find var importance value
  ovx <- match(ov, left_vars)
  # ovx
  if (varcors[l, 2] < varcors[ovx, 2]) {
    
    varcors$remove[l] <- paste(lvx)
    
  } else { 
    
    varcors$remove[l] <- paste(ov)
    
  }
  
}

# varcors

## remove any variables that are correaltied
vb <- unique(varcors$remove)
vb
# vars


# vb
sd <- vars %in% vb
sd
del <- vars[!sd]
del ## remaining variables to use - subset from preds dataframe
# 
# [1] bio_04_all_de.r bio_08_all_de.r bio_09_all_de.r
# [4] bio_12_all_de.r dh1             mh21  


del
resx <- subset(cor_imp, var %in% del)
resx <- resx[, 1:2]
resx <- resx[!duplicated(resx),]
resx
# remove the correlated ones not picked up by alorgithm - as were zeros

# [1] Bio015_worldclim Bio02_worldclim  Bio04_worldclim 
# [4] Bio08_worldclim  Bio09_worldclim  dh1             
# [7] ma17             mh21 

getwd()
setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_hydro")
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun")
write.csv(resx, file="all_preds_for_sdms.csv")


#  all three

#  subset only species from spp_list


setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim_hydro"))

all_preds <- read.csv("acclim_wclim_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
# all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
al <- which(colnames(all_preds) %in% spp_list)
all_predsx <-  all_preds[,c(1,al)]
all_predsx
head(all_predsx)
dim(all_predsx)
#  29   94


all_predsx$mean <- rowMeans(all_predsx[2:81])
all_predsx$mean
# all_predsx <- all_predsx[-c(14:20),]
## 93 species remain

### get top >1% metrics for each species
all_vars <- all_predsx[, 1]
all_vars


## make df for correlation
vdf <- data.frame(matrix(nrow=29, ncol=1))
vdf[,1] <- all_vars
colnames(vdf)[1] <- "Variables"
vdf 


sp1 <- all_predsx[, c(1, 82)]
sp1



topvars <- sp1 ## 
topvars
dim(topvars) ## 29 2

# save(topvars, file=paste("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun/uni_95_all_cats_above_1perc_vars_rel_imp.RData", sep=""))
# save(topvars, file=paste("D:/Man_2_August/BRTs/rel_importance/custom_for_sdms/no_precip/no_precip_all_cats_above_1perc_vars_", colnames(sp1)[2], "_rel_imp.RData", sep=""))
setwd(paste0(path,"output_data/BRTs_2nd/"))
save(topvars, file=paste("wclim_acc_hydro_rel_imp.RData", sep=""))

vars <- topvars$var
vars

### extract variable data
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
# cpx
hpr <- stack(hpx)

#  change extent
setwd(paste0(path,"input_data"))
r <- raster("agriculture_land_use.correct.r.tif")
hpreds_r <- crop(extend(hpr, r), r)
all.equal(extent(r), extent(hpreds_r))
hpreds_rx <- resample(hpreds_r, r)

#  stack rasters
preds <- stack(hpreds_rx, apr, wpr)
preds

preds_df <- as.data.frame(preds)
# head(preds_df)
preds_df <- na.omit(preds_df)
# str(preds_df)
###### multicolinearality

df2 = cor(preds_df)
head(df2)
df2 <- as.data.frame(df2)
getwd()
setwd(paste0(path,"output_data/BRTs_2nd/correlation"))
write.csv(df2, "wclim_acc_hydro_corrv2_test.csv")

df2$var <- rownames(df2)

cor_imp <- merge(topvars, df2, by="var")

## make dataframe in order of relative importance
cor_imp <- cor_imp[order(cor_imp[,2], decreasing=TRUE),]

head(cor_imp)
dim(cor_imp)
## remove minus signs from data frame 
cor_imp[] <- lapply(cor_imp, function(x) gsub("-", "", x ))
## change from character to number
cor_imp[, 2:24] <- lapply(cor_imp[,2:24], function(x) as.numeric(as.character(x)))

str(cor_imp)
### if over 0.7 take the variable with the higher importance

## reshape dataframe
library(reshape)
colnames(cor_imp)[2] <- "importance"
cor_imp <- melt(cor_imp, id=c("var", "importance"))
cor_imp$importance2 <- NA

v <- cor_imp$var
## df column for importance of variable 2 - NAs removed as these parwise comparisons are variables < 1%
for(d in 1: length(v)) {
  
  v1 <- v[d]
  imp <- cor_imp$importance[d] 
  
  v2 <- cor_imp$variable %in% v1
  cor_imp$importance2[v2] <- paste(imp)
  
}


cor_imp <- na.omit(cor_imp)

vals <- cor_imp$value


# c=2
### find correlations over 0.7

for (c in 1: length(vals)) {
  
  cv <- vals[c]
  cv
  
  if ( cv > 0.7 && cv < 1){
    cor_imp$corr[c] <- paste("true")
    
  } else {
    cor_imp$corr[c] <- paste("false")
    
  }
}




## subset df to only correlations over 0.7
varcors <- subset(cor_imp, corr=="true")

left_vars <- varcors$var

### take the variable of higher relative importance - make list of losing predictors then remove from list of variables.

for (l in 1: length(left_vars)) {
  
  ### first separate pairwise correlations
  ## variable 1
  lvx <- left_vars[l]
  # lvx
  ## variable 2
  ov <- varcors$variable[l]
  # ov
  # left_vars
  ## variable 2 in list to find var importance value
  ovx <- match(ov, left_vars)
  # ovx
  if (varcors[l, 2] < varcors[ovx, 2]) {
    
    varcors$remove[l] <- paste(lvx)
    
  } else { 
    
    varcors$remove[l] <- paste(ov)
    
  }
  
}

# varcors

## remove any variables that are correaltied
vb <- unique(varcors$remove)
vb
# vars


# vb
sd <- vars %in% vb
sd
del <- vars[!sd]
del ## remaining variables to use - subset from preds dataframe
# 
# [1] bio_08_all_de.r  bio_09_all_de.r  bio_12_all_de.r 
# [4] Bio015_worldclim Bio02_worldclim  Bio04_worldclim 
# [7] Bio08_worldclim  Bio09_worldclim  dh1             
# [10] mh21  

# [1] "bio_04_all_de.r" "bio_08_all_de.r" "bio_09_all_de.r" 
# "Bio01_worldclim" "Bio02_worldclim"
# [6] "Bio03_worldclim" "Bio08_worldclim" "Bio09_worldclim" 
# "dh1"             "mh21"           
# [11] "ta2"  


del
resx <- subset(cor_imp, var %in% del)
resx <- resx[, 1:2]
resx <- resx[!duplicated(resx),]
resx
# remove the correlated ones not picked up by alorgithm - as were zeros

# [1] Bio015_worldclim Bio02_worldclim  Bio04_worldclim 
# [4] Bio08_worldclim  Bio09_worldclim  dh1             
# [7] ma17             mh21 

getwd()
setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_wclim_hydro")
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun")
write.csv(resx, file="all_preds_for_sdms.csv")

#  acc climate & worldclim


#  subset only species from spp_list

setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_wclim")

all_preds <- read.csv("acclim_wclim_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]
dim(all_preds)
head(all_preds)
#  remove NAs
# all_predsx  <- all_preds[, unlist(lapply(all_preds, function(x) !all(is.na(x))))]
al <- which(colnames(all_preds) %in% spp_list)
all_predsx <-  all_preds[,c(1,al)]
all_predsx
head(all_predsx)
dim(all_predsx)
#  12   93


all_predsx$mean <- rowMeans(all_predsx[2:93])
all_predsx$mean
# all_predsx <- all_predsx[-c(8:14),]
## 92 species remain

### get top >1% metrics for each species
all_vars <- all_predsx[, 1]
all_vars


## make df for correlation
vdf <- data.frame(matrix(nrow=12, ncol=1))
vdf[,1] <- all_vars
colnames(vdf)[1] <- "Variables"
vdf 


sp1 <- all_predsx[, c(1, 94)]
sp1



topvars <- sp1 ## 
topvars
dim(topvars) ## 12 2

# save(topvars, file=paste("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun/uni_95_all_cats_above_1perc_vars_rel_imp.RData", sep=""))
# save(topvars, file=paste("D:/Man_2_August/BRTs/rel_importance/custom_for_sdms/no_precip/no_precip_all_cats_above_1perc_vars_", colnames(sp1)[2], "_rel_imp.RData", sep=""))
save(topvars, file=paste("/Users/katie/Documents/manuscript3/BRTs/2nd_run/rel_imp/acc_wclim_rel_imp.RData", sep=""))

vars <- droplevels(topvars$var)
vars

### extract variable data
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


#  stack rasters
preds <- stack(wpr, apr)
preds

preds_df <- as.data.frame(preds)
# head(preds_df)
preds_df <- na.omit(preds_df)
# str(preds_df)
###### multicolinearality

df2 = cor(preds_df)
head(df2)
df2 <- as.data.frame(df2)
getwd()
setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/correlation")
write.csv(cor_imp, "acc_wclim_corrv2.csv")

df2$var <- rownames(df2)

cor_imp <- merge(topvars, df2, by="var")

## make dataframe in order of relative importance
cor_imp <- cor_imp[order(cor_imp[,2], decreasing=TRUE),]

head(cor_imp)
dim(cor_imp)
## remove minus signs from data frame 
cor_imp[] <- lapply(cor_imp, function(x) gsub("-", "", x ))
## change from character to number
cor_imp[, 2:12] <- lapply(cor_imp[,2:12], function(x) as.numeric(as.character(x)))

str(cor_imp)
### if over 0.7 take the variable with the higher importance

## reshape dataframe
library(reshape)
colnames(cor_imp)[2] <- "importance"
cor_imp <- melt(cor_imp, id=c("var", "importance"))
cor_imp$importance2 <- NA

v <- cor_imp$var
## df column for importance of variable 2 - NAs removed as these parwise comparisons are variables < 1%
for(d in 1: length(v)) {
  
  v1 <- v[d]
  imp <- cor_imp$importance[d] 
  
  v2 <- cor_imp$variable %in% v1
  cor_imp$importance2[v2] <- paste(imp)
  
}


cor_imp <- na.omit(cor_imp)

vals <- cor_imp$value


# c=2
### find correlations over 0.7

for (c in 1: length(vals)) {
  
  cv <- vals[c]
  cv
  
  if ( cv > 0.7 && cv < 1){
    cor_imp$corr[c] <- paste("true")
    
  } else {
    cor_imp$corr[c] <- paste("false")
    
  }
}




## subset df to only correlations over 0.7
varcors <- subset(cor_imp, corr=="true")

left_vars <- varcors$var

### take the variable of higher relative importance - make list of losing predictors then remove from list of variables.

for (l in 1: length(left_vars)) {
  
  ### first separate pairwise correlations
  ## variable 1
  lvx <- left_vars[l]
  # lvx
  ## variable 2
  ov <- varcors$variable[l]
  # ov
  # left_vars
  ## variable 2 in list to find var importance value
  ovx <- match(ov, left_vars)
  # ovx
  if (varcors[l, 2] < varcors[ovx, 2]) {
    
    varcors$remove[l] <- paste(lvx)
    
  } else { 
    
    varcors$remove[l] <- paste(ov)
    
  }
  
}

# varcors

## remove any variables that are correaltied
vb <- unique(varcors$remove)
vb
# vars


# vb
sd <- vars %in% vb
sd
del <- vars[!sd]
del ## remaining variables to use - subset from preds dataframe
# 
# 1] bio_08_all_de.r  bio_09_all_de.r  bio_12_all_de.r 
# [4] Bio015_worldclim Bio02_worldclim  Bio04_worldclim 
# [7] Bio08_worldclim  Bio09_worldclim  


del
resx <- subset(cor_imp, var %in% del)
resx <- resx[, 1:2]
resx <- resx[!duplicated(resx),]
resx
# remove the correlated ones not picked up by alorgithm - as were zeros

getwd()
setwd("/Users/katie/Documents/manuscript3/BRTs/2nd_run/acc_wclim")
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/BRTs/rel_importance/uniform_for_sdms/93_sp_sept_rerun")
write.csv(resx, file="all_preds_for_sdms.csv")
