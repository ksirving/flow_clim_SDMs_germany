### extract variable data
setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/")
path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/"

### species
test4 <- read.csv("output_data/taxa_list_80.csv")
spp_list <- test4[, 2]
spp_list
 # correlation
library(reshape)
library(caret)
library(raster)
library(dplyr)

## upload relative importance
setwd(paste0(path,"output_data/BRTs_2nd/acc_wclim_hydro"))

all_preds <- read.csv("acclim_wclim_hydro_all_var_imp_100_species.csv")
all_preds <- all_preds[, -1]


#  remove NAs

al <- which(colnames(all_preds) %in% spp_list)
all_predsx <-  all_preds[,c(1,al)]
all_predsx
head(all_predsx)
dim(all_predsx)
#  29   85


all_predsx$mean <- rowMeans(all_predsx[2:85])
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


sp1 <- all_predsx %>%
  select(var, mean)
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
#  list predictor data
setwd(paste0(path,"input_data/ac_climate"))
ac <- list.files(pattern="tif")

setwd(paste0(path,"input_data/Hydrology"))
hl <- list.files(pattern=".tif")

setwd(paste0(path,"input_data/worldclim"))
wl <- list.files(pattern=".tif")

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
# cpx
hpr <- stack(hpx)

#  change extent
r <- raster(paste0(path,"input_data/agriculture_land_use.correct.r.tif"))
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
setwd(paste0(path,"output_data/tables"))
write.csv(df2, "correlation_matrix.csv")

df2$var <- rownames(df2)

cor_imp <- merge(topvars, df2, by="var")

## make dataframe in order of relative importance
cor_imp <- cor_imp[order(cor_imp[,2], decreasing=TRUE),]

head(cor_imp)
dim(cor_imp)
## remove minus signs from data frame 
cor_imp[] <- lapply(cor_imp, function(x) gsub("-", "", x ))
## change from character to number
cor_imp[, 2:31] <- lapply(cor_imp[,2:31], function(x) as.numeric(as.character(x)))

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
# [7] Bio08_worldclim  Bio09_worldclim  ta2            
# [10] mh21  
topvars
## manual check cor table. TA2 cor with MH21. DH1 correlated
write.csv(del, paste0(path,"output_data/all_preds_for_sdms.csv" ) )
