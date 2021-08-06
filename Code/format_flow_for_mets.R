# install.packages("data.table")
# install.packages("raster")
# install.packages("ncdf4")
# install.packages("dplyr")
# install.packages("doParallel")
# install.packages("foreach")
# install.packages("zoo")
# install.packages("chron")
library(chron)
library(data.table)
library(raster)
library(ncdf4)
library(dplyr)
library(doParallel)
library(foreach)
library(zoo)


### check dh1 metric

dh1 <- raster("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Thesis/Manuscript3/manuscript3/pred_data/dh1_all_de.tif")
plot(dh1)

dh1x <- as(as(dh1, "SpatialPointsDataFrame"), "data.frame") 
head(dh1x)
dim(dh1x)
dh1x <- dh1x %>%
  mutate(dh1_all_de  = (dh1_all_de /1000))

range(dh1x$dh1_all_de)

### crop to catchments
## base raster to crop
setwd("/Users/katieirving/Documents/manuscript3/pred_data/Hydrology")
rl <- list.files(pattern="dh1")

r <- raster(rl[1])
r
plot(r)
setwd("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/input_data/")
getwd()
writeRaster(biocx, "crop_r.tif", format = "GTiff")
## catchments shape

shape <- shapefile("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/input_data/ems_weser_merged.shp")

plot(shape)

setwd(path) ## set wd 

i=1

  bioc <- mask(dh1, shape)
  
  biocx <- crop(extend(bioc, r), r)
  biocx
  # setwd("U:/Irving/Assignments/Manuscript3/pred_data/worldclim")
  
  writeRaster(biocx, "dh1_new.tif", format = "GTiff")
  

  plot(biocx)
  
  dh1x <- as(as(biocx, "SpatialPointsDataFrame"), "data.frame") 
  head(dh1x)
  dh1x <- dh1x %>%
    mutate(dh1  = (dh1_all_de /1000))
  dim(dh1x)
  range(dh1x$dh1)


dh1 <- raster("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Thesis/Manuscript3/manuscript3/pred_data/Hydrology/dh1.tif")

dh1x <- as(as(dh1, "SpatialPointsDataFrame"), "data.frame") 
head(dh1x)
dh1x <- dh1x %>%
  mutate(dh1  = (dh1 /1000))

range(dh1x$dh2)


dh1 <- raster("/Users/katieirving/Documents/manuscript2/pred_data/dh1.tif")
dh1x <- as(as(dh1, "SpatialPointsDataFrame"), "data.frame") 
head(dh1x)
dh1x <- dh1x %>%
  mutate(dh1  = (dh1 /1000))

range(dh1x$dh1)

path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Thesis/Manuscript3/manuscript3/hydro_data/1985_2013/" ## add own directory here (where data is stored)
setwd(path) ## set wd 
getwd()


s=1 ## define s (first file in list)

## upload first file
net_list <- list.files(pattern = "stack_all_de.r")
head(net_list)


years <- seq(1985,2013,1) ## change to years of interest
years
net_list 

### crop to catchments
## base raster to crop
setwd("/Users/katieirving/Documents/manuscript3/pred_data/Hydrology")
rl <- list.files(pattern="dh1")

r <- raster(rl[1])
r

## catchments shape

shape <- shapefile("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/flow_clim_SDMs_germany/input_data/ems_weser_merged.shp")

plot(shape)

setwd(path) ## set wd 

i=1
for(i in 1:length(net_list)) {
  
  bio <- brick(paste(path, net_list[i], sep = "/")) ## upload raster data
  bioc <- mask(bio, shape)
  
  biocx <- crop(extend(bioc, r), r)
  biocx
  # setwd("U:/Irving/Assignments/Manuscript3/pred_data/worldclim")
  
  writeRaster(biocx, paste(path, "cropped_",net_list[i], sep=""), format="netCDF", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

  
  
}
#### first part of dataframe to get structure

path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Thesis/Manuscript3/manuscript3/hydro_data/1985_2013/" ## add own directory here (where data is stored)
setwd(path) ## set wd 
getwd()

s=1 ## define s (first file in list)

## upload first file
net_list <- list.files(pattern = "stack")
head(net_list)
net_list

years <- seq(1985,2013,1) ## change to years of interest
years


net <- brick(paste(path, net_list[1], sep = "/")) ## upload raster data
class(net)
net
plot(net)
yr <- years[1] ## define year
yr

### Add layer names. will display julian day & year
names(net) <- paste0(c("day_"), yr , "_", sprintf("%03d", seq(1:length(names(net)))))

dfx <- as(as(net, "SpatialPointsDataFrame"), "data.frame") ## format raster into df, takes a couple of minutes
## flip data and unlist
dfx <- t(dfx) 
dfx <- data.frame(unlist(dfx))
head(dfx)
## separate df for coordinates
coords <- dfx[(length(dfx[,1])-1):length(dfx[,1]),] 

## take out coords from df and divide by 10000 to get real values
dfx <- dfx[-c((length(dfx[,1])-1):length(dfx[,1])),]/10000 
## add coords back into top of df
dfx <- rbind(coords, dfx)

for(n in 2:length(net_list)) {
  
  cat("Running year", n, "\n")
  
  net <- brick(paste(path, net_list[n], sep = "/")) ## upload each file
  
  yr <- years[n] # define year
  
  ### Add layer names. will display julian day & year
  names(net) <- paste0(c("day_"), yr , "_", sprintf("%03d", seq(1:length(names(net)))))
  
  df <- as(as(net, "SpatialPointsDataFrame"), "data.frame")
  
  
  df <- t(df)## flip data 
  
  df <- data.frame(unlist(df))
  
  df <- df[-c((length(df[,1])-1):length(df[,1])),]/10000
  
  dfx <- rbind(dfx, df)
  
}

dfx[1:10,1:10]

## add year_val column
dfx$year_val <- tstrsplit(rownames(dfx[-c(1:2)]), "[_]")[[2]]
## add julian day column
dfx$jul_day <- tstrsplit(rownames(dfx[-c(1:2)]), "[_]")[[3]] ## this julian day is continuous from origin. need annual jul day i.e. 1:365 for IHA calculations
dfx$jul_day <- as.numeric(as.character(dfx$jul_day))

## create separate df with julian days
jul_day <- as.numeric(as.character(dfx$jul_day))

## convert julian day to dates from origin day
dates <- month.day.year(jul_day, origin.=c(month = 12, day = 31, year = 1949))

## add month and day columns
dfx$month_val <- dates$month
dfx$day_val <- dates$day

## add date column
dfx$date <- as.Date(with(dfx, paste(year_val, month_val, day_val,sep="-")), "%Y-%m-%d")

jul <- dfx
jul$date3 <- as.Date(jul$date, format="%Y-%m-%d") ## format date
jul_x<-jul$date3[1:length(jul$date3)] ## make list of dates
z<-as.POSIXlt(jul_x , "%Y-%m-%d") ## format to UTC
jul_day<-yday(z) ## change date to Julian day
jul$jul_val2[1:length(jul$date)]<-jul_day ## add to df

## format month and year columns
jul$month_val <- as.numeric(as.character(jul$month_val))
jul$year_val <- as.numeric(as.character(jul$year_val))

## wy_val
jul$wy_val <- ifelse(jul$month_val <= 9, jul$year_val, jul$year_val+1)

sim <- jul

## save if needed
path <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/Thesis/Manuscript3/manuscript3/hydro_data/1985_2013/output_data" ## change to directory where data is to be saved
save(sim, file=paste(path,"sim_dis_1985_2013_july_2021.RData")) ## change name 

sim[1:10, 1:10]
cols <- unique(names(sim[-c( 85363:85370)])) ##  define list of sites, do not include the columns: month_val, year_val, day_val, date, date3, jul_val, wy_val. the numbers will need adjusted according to the size of dataframe
head(cols) ## check data
tail(cols) ## check columns are removed

s=1

sx <- paste("^",cols[s],"$", sep="") ## define site within object

hyd_metrics <- data.frame(matrix(nrow = 0, ncol = 4)) ## define dataframe & column names
colnames(hyd_metrics)<-(c("site_no", "X", "Y", "dh1"))

data1 <- sim[,grep(sx, colnames(sim))] ## extract discharge data for site sx

data2 <- cbind(data1,sim[,85363:85370]) # 1 site data + dates etc

# sim[,85363:85370]
date <- "31.12.2013" ## last day (change to last day in data)
date <- as.Date(date, format="%d.%m.%Y")

data3=data2[-c(1:2),] # to remove unimportant rows

colnames(data3)[1] <- "discharge" ## change column name to discharge
data3 <-data3 %>% distinct(date3, .keep_all = TRUE) ## remove duplicated dates
data4 <- data3[order(data3$date3),] ## order df by dates

dim(data4)
head(data4)
sum(is.na(data4))
data4$discharge<-as.numeric(as.character(data4$discharge)) ## change format to number

date.seq <- data.frame("date" = seq.Date((date-10591), date, by="days")) ## define dates sequence 01/01/2050 - 31/12/2013
head(date.seq)
tail(date.seq)
data5 <- na.omit(data4) ## remove NAs
dim(data5)

##Annual maximum daily flow. Compute the maximum of a 1-day moving average flow for each year. DH1 is the 
#' mean (or median-Use Preference option) of these values (cubic feet per second-temporal)
#' need data with discharge column & daily flows
dh1 <- function(qfiletempf, pref = "mean") {
  annualmax <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), max)
  if (pref == "median") {
    dh1 <- median(annualmax$x)
  }
  else {
    dh1 <- round(mean(annualmax$x),digits=2)
  }
  return(dh1)
}

dh1(data5, pref = "mean")

cl <- makePSOCKcluster(2, outfile="")
registerDoParallel(cl)
getDoParWorkers()

# for(s in 1:length(cols)) { ## start loop with single processing
result <- foreach(s=1:length(cols), .combine = rbind, .packages=c("data.table", "dplyr", "zoo")) %dopar% { ## start loop for parallel processing
  
  cat("Running site", s, "\n")
  
  sx <- paste("^",cols[s],"$", sep="") ## define site within object
  
  hyd_metrics <- data.frame(matrix(nrow = 0, ncol = 4)) ## define dataframe & column names
  colnames(hyd_metrics)<-(c("site_no", "X", "Y", "dh1","dh2","dh3","dh4","dh5","dl1", "dl2", "dl3", "dl4", "dl5", "ma1", "ma2", "ma12", "ma13", "ma14", "ma15", "ma16", "ma17", "ma18", "ma19", "ma20", "ma21", "ma22", "ma23", "mh1", "mh2", "mh3", "mh4", "mh5", "mh6", "mh7", "mh8", "mh9", "mh10", "mh11", "mh12", "ra1", "ra3","ta1", "ta2","ml1","ml2","ml3","ml4","ml5","ml6","ml7","ml8","ml9","ml10","ml11","ml12","mh18"))
  
  
  
  data1 <- sim[,grep(sx, colnames(sim))] ## extract discharge data for site sx
  
  data2 <- cbind(data1,sim[,85363:85370]) # 1 site data + dates etc
  
  
  date <- "31.12.2013" ## last day (change to last day in data)
  date <- as.Date(date, format="%d.%m.%Y")
  
  data3=data2[-c(1:2),] # to remove unimportant rows
  
  colnames(data3)[1] <- "discharge" ## change column name to discharge
  data3 <-data3 %>% distinct(date3, .keep_all = TRUE) ## remove duplicated dates
  data4 <- data3[order(data3$date3),] ## order df by dates
  
  
  
  data4$discharge<-as.numeric(as.character(data4$discharge)) ## change format to number
  
  date.seq <- data.frame("date" = seq.Date((date-10591), date, by="days")) ## define dates sequence 01/01/1985 - 31/12/2013
  
  data5 <- na.omit(data4) ## remove NAs
  
  hyd_metrics[s,1] <- cols[s] ## site no
  hyd_metrics[s,2] <- as.numeric(as.character(data2[1,1]))#tstrsplit(cols[s], "[_]")[[1]]                   
  hyd_metrics[s,3] <- as.numeric(as.character(data2[2,1]))#tstrsplit(cols[s], "[_]")[[2]]    
  
  #### dh stats    
  
  hyd_metrics[s,4] <- try(dh1(data5, pref = "mean"), silent=TRUE)#1
  # hyd_metrics[s,5] <- try(dh2(data5, pref = "mean"), silent=TRUE)#2
  # hyd_metrics[s,6] <- try(dh3(data5, pref = "mean"), silent=TRUE)#3
  # hyd_metrics[s,7] <- try(dh4(data5, pref = "mean"), silent=TRUE)#4
  # hyd_metrics[s,8] <- try(dh5(data5, pref = "mean"), silent=TRUE)#5
  # 
  # 
  # 
  # # # ######### DL stats
  # 
  # hyd_metrics[s,9] <- try(dl1(data5, pref = "mean"), silent=TRUE)#6
  # hyd_metrics[s,10] <- try(dl2(data5, pref = "mean"), silent=TRUE)#7
  # hyd_metrics[s,11] <- try(dl3(data5, pref = "mean"), silent=TRUE)#8
  # hyd_metrics[s,12] <- try(dl4(data5, pref = "mean"), silent=TRUE)#9
  # hyd_metrics[s,13] <- try(dl5(data5, pref = "mean"), silent=TRUE)#10
  # 
  # 
  # 
  # 
  # # ############# MA stats
  # 
  # hyd_metrics[s,14] <- try(ma1(data5), silent=TRUE)#11
  # hyd_metrics[s,15] <- try(ma2(data5), silent=TRUE)#12
  # 
  # hyd_metrics[s,16] <- try(unlist(ma12.23(data5)), silent=TRUE)[1]#13
  # hyd_metrics[s,17] <- try(unlist(ma12.23(data5)), silent=TRUE)[2]#14
  # hyd_metrics[s,18] <- try(unlist(ma12.23(data5)), silent=TRUE)[3]#15
  # hyd_metrics[s,19] <- try(unlist(ma12.23(data5)), silent=TRUE)[4]#16
  # hyd_metrics[s,20] <- try(unlist(ma12.23(data5)), silent=TRUE)[5]#17
  # hyd_metrics[s,21] <- try(unlist(ma12.23(data5)), silent=TRUE)[6]#18
  # hyd_metrics[s,22] <- try(unlist(ma12.23(data5)), silent=TRUE)[7]#19
  # hyd_metrics[s,23] <- try(unlist(ma12.23(data5)), silent=TRUE)[8]#20
  # hyd_metrics[s,24] <- try(unlist(ma12.23(data5)), silent=TRUE)[9]#21
  # hyd_metrics[s,25] <- try(unlist(ma12.23(data5)), silent=TRUE)[10]#22
  # hyd_metrics[s,26] <- try(unlist(ma12.23(data5)), silent=TRUE)[11]#23
  # hyd_metrics[s,27] <- try(unlist(ma12.23(data5)), silent=TRUE)[12]#24
  # 
  # ############# MH stats
  # 
  # hyd_metrics[s,28] <- try(unlist(mh1.12(data5)), silent=TRUE)[1]#25
  # hyd_metrics[s,29] <- try(unlist(mh1.12(data5)), silent=TRUE)[2]#26
  # hyd_metrics[s,30] <- try(unlist(mh1.12(data5)), silent=TRUE)[3]#27
  # hyd_metrics[s,31] <- try(unlist(mh1.12(data5)), silent=TRUE)[4]#28
  # hyd_metrics[s,32] <- try(unlist(mh1.12(data5)), silent=TRUE)[5]#29
  # hyd_metrics[s,33] <- try(unlist(mh1.12(data5)), silent=TRUE)[6]#30
  # hyd_metrics[s,34] <- try(unlist(mh1.12(data5)), silent=TRUE)[7]#31
  # hyd_metrics[s,35] <- try(unlist(mh1.12(data5)), silent=TRUE)[8]#32
  # hyd_metrics[s,36] <- try(unlist(mh1.12(data5)), silent=TRUE)[9]#33
  # hyd_metrics[s,37] <- try(unlist(mh1.12(data5)), silent=TRUE)[10]#34
  # hyd_metrics[s,38] <- try(unlist(mh1.12(data5)), silent=TRUE)[11]#35
  # hyd_metrics[s,39] <- try(unlist(mh1.12(data5)), silent=TRUE)[12]#36
  # 
  # 
  # ############# RA stats
  # 
  # hyd_metrics[s,40] <- try(ra1(data5), silent=TRUE)#37
  # hyd_metrics[s,41] <- try(ra3(data5), silent=TRUE)#38
  # 
  # 
  # 
  # ############# TA stats
  # 
  # hyd_metrics[s,42] <- try(unlist(ta1.2(data5)), silent=TRUE)[1]#39
  # hyd_metrics[s,43] <- try(unlist(ta1.2(data5)), silent=TRUE)[2]#40
  # 
  # 
  # ############# ml stats
  # 
  # hyd_metrics[s,44] <- try(unlist(ml1.12(data5)), silent=TRUE)[1]#41
  # hyd_metrics[s,45] <- try(unlist(ml1.12(data5)), silent=TRUE)[2]#42 
  # hyd_metrics[s,46] <- try(unlist(ml1.12(data5)), silent=TRUE)[3]#43
  # hyd_metrics[s,47] <- try(unlist(ml1.12(data5)), silent=TRUE)[4]#44
  # hyd_metrics[s,48] <- try(unlist(ml1.12(data5)), silent=TRUE)[5]#45
  # hyd_metrics[s,49] <- try(unlist(ml1.12(data5)), silent=TRUE)[6]#46
  # hyd_metrics[s,50] <- try(unlist(ml1.12(data5)), silent=TRUE)[7]#47
  # hyd_metrics[s,51] <- try(unlist(ml1.12(data5)), silent=TRUE)[8]#48
  # hyd_metrics[s,52] <- try(unlist(ml1.12(data5)), silent=TRUE)[9]#49
  # hyd_metrics[s,53] <- try(unlist(ml1.12(data5)), silent=TRUE)[10]#50
  # hyd_metrics[s,54] <- try(unlist(ml1.12(data5)), silent=TRUE)[11]#51
  # hyd_metrics[s,55] <- try(unlist(ml1.12(data5)), silent=TRUE)[12]#52
  # 
  # 
  # ###### mh stats
  # 
  # hyd_metrics[s,56] <- try(mh18(data5), silent=TRUE)#53
  
  all_hyd_metrics <- as.data.frame(hyd_metrics)
  
}



hyd_metricsx <- na.omit(result) ## remove nas

hyd_metrics <- hyd_metricsx[!duplicated(hyd_metricsx[,1]),] ## remove duplicates

all_hyd_metrics <- as.data.frame(hyd_metrics) ## format into dataframe

head(all_hyd_metrics) ## check data

# 	site_no        X        Y   dh1  dh2  dh3  dh4  dh5  dl1  dl2  dl3  dl4  dl5
# 1       X1 8.845833 54.90417 10.09 8.00 6.32 4.06 2.81 0.34 0.50 0.61 0.73 1.06
# 3       X2 8.870833 54.90417  9.97 7.80 6.10 3.91 2.70 0.28 0.44 0.57 0.68 1.01
# 6       X3 8.879167 54.90417  9.96 7.78 6.08 3.90 2.69 0.27 0.44 0.56 0.68 1.00
# 10      X4 8.887500 54.90417  9.95 7.76 6.07 3.89 2.68 0.27 0.43 0.56 0.67 1.00
# 15      X5 8.895833 54.90417  9.94 7.75 6.05 3.88 2.67 0.26 0.43 0.55 0.67 1.00
# 21      X6 8.854167 54.89583 10.07 7.97 6.29 4.04 2.80 0.34 0.49 0.61 0.72 1.05

stopCluster(cl) ## stop parallels


save(all_hyd_metrics, file=paste(path, "reran_dh1.RData")) ## save file
