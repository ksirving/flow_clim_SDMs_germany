
rm(list=ls())

install.packages("zoo")

library(zoo)
library(plyr)
library(foreach)
library(doParallel)
library(data.table)

# setwd("/home/shared/KK/predicted_discharge_data/seasonal precipitation/discharge_by_blocks_precip/al")
setwd("/home/shared/KK/predicted_discharge_data/seasonal precipitation/jul_val_precip_zero_discharge/jul_val_al")
sitex <- list.files(pattern = "precip_al_zero_pred_discharge_")
head(sitex)
length(sitex)

l=1

cl <- makePSOCKcluster(2,  outfile="") # only 3 and let run for a while
registerDoParallel(cl) # register parallel backend
getDoParWorkers() # show number of workers

foreach (l = 1:length(sitex), .packages = c("qpcR", "zoo", "plyr")) %dopar% { #qpcR:::rbind.na, , .packages = c("qpcR"))
#for (l in 1:length(sitex)){
    
site <- read.csv(paste("/home/shared/KK/predicted_discharge_data/seasonal precipitation/jul_val_precip_zero_discharge/jul_val_al/", sitex[l], sep = "/"))
head(site,10)
# removing extra spring-summer-fall rows
# site1 <- site[-c(7:337,696:698,1057:1059,1418:1420,1780:1782,2141:2143,2502:2504,2863:2865,3225:3227,3586:3588,3947:3949,4308:4310,4670:4672,5031:5033,5392:5394,5753:5755,6115:6117,6476:6478,6837:6839,7198:7200,7560:7562,7921:7923,8282:8284,8643:8645,9005:9007,9365:9368,9727:9729,10088:10090,10450:10452,10811:10813,11172:11174,11533:11535,11895:11897,12256:12258), -c(4:506)]

# first_year <- seq(1, 328, 1)
# jul_val <- seq(1, 365,1)
# jul_val2 <- rep(jul_val, times=(12508/364))
# zero <- rep(0, times=7)
# julian_val <- c(zero, jul_val2)
total <- site
# head(total,10)
# names(total)
# str(total)
# dim(total)

total2<-total[7:12852,]
headers <- total[1:6,]
head(total2)
# head(headers)

total_3<-total2[!is.na(total2),] ## omit nas - without the nas in header rows
total <- rbind(headers, total_3) ## join back headers and data rows
head(total, 30)

dim(total)
# colnames(total)[which(names(total) == "y_val")] <- "wy_val"
colnames(total)[506] <- "year_val"
colnames(total)[507] <- "day_val" ## change column names around
colnames(total)[505] <- "month_val"

total$month_val <- as.numeric(as.character(total$month_val))
total$year_val <- as.numeric(as.character(total$year_val))
total$wy_val <- if(total$month_val[-c(1:6)] <= 9) total$year_val else total$year_val+1 ### bringing NAs - fix!!!!


# total$month_val[-c(1:6)]
# total$year_val[-c(1:6)]
# head(total$y_val,400)

# head(total$month_val,20)
# total$wy_val <- if(total$month_val < 10) total$year_val else total$year_val+1 

#total$date2 <- as.Date(total$date)

# colnames(total)[which(names(total) == "julian_val")] <- "jul_val"

s=2
scols<-colnames(site[,2:7])
head(scols)
#hyd_metrics <- data.frame(matrix(nrow = 0, ncol = 10))
#colnames(hyd_metrics)<-(c("site_no", "X", "Y", "dh1","dh2","dh3","dh4","dh5","dh6","dh7"))
hyd_metrics <- data.frame(matrix(nrow = 0, ncol = 139))
colnames(hyd_metrics)<-(c("site_no", "X", "Y", "dh1","dh2","dh3","dh4","dh5","dh6","dh7","dh8","dh9","dh10","dh11","dh12","dh13","dh14","dh15","dh16","dh17","dh18","dh19","dh20","dh21","dh22","dh23","dh24", "dl1", "dl2", "dl3", "dl4", "dl5", "dl6", "dl7", "dl8", "dl9", "dl10", "dl11", "dl12", "dl13", "dl14", "dl15", "dl16", "dl17", "dl18", "dl19", "dl20", "fh1", "fh2", "fh3", "fh4", "fh5", "fh6", "fh7", "fh8", "fh9", "fh10", "fh11", "fl1", "fl2", "fl3", "ma1", "ma2", "ma3", "ma4", "ma5", "ma6", "ma7", "ma8", "ma9", "ma10", "ma11", "ma12", "ma13", "ma14", "ma15", "ma16", "ma17", "ma18", "ma19", "ma20", "ma21", "ma22", "ma23", "ma24", "ma25", "ma26", "ma27", "ma28", "ma29", "ma30", "ma31", "ma32", "ma33", "ma34", "ma35", "ma36", "ma37", "ma38", "ma39", "ma40", "mh1", "mh2", "mh3", "mh4", "mh5", "mh6", "mh7", "mh8", "mh9", "mh10", "mh11", "mh12", "mh13", "ml14", "ml15", "ml16", "ml17", "ml18", "ml19", "ml20", "ml21", "ra1", "ra2", "ra3", "ra4", "ra5", "ra6", "ra7", "ta1", "ta2", "ta3", "th1", "th2", "th3", "tl1", "tl2", "tl3", "tl4"))

#for(j in 1:length(datum)) {
#  datum.j <- datum[j]
#  datum.seq <- data.frame("date2" = seq.Date((datum.j-364), datum.j, by="days"))
#  total1 <- merge(total, datum.seq)
  
for (s in 1:length(scols)) {

qfiletempf<-cbind(total[,2:502][s],total[,503:510]) ## dataframe formatted for hydro variables
names(qfiletempf)[1]<-"discharge" ## change column name to discharge
# head(qfiletempf)

head(hyd_metrics)

qfiletempf$month_val<-as.integer(as.character(qfiletempf$month_val))
qfiletempf$day_val<-as.integer(as.character(qfiletempf$day_val))
qfiletempf$wy_val<-as.integer(as.character(qfiletempf$wy_val))
qfiletempf$year_val<-as.integer(as.character(qfiletempf$year_val)) ## ignore warning - all good!!!
qfiletempf$is_neg=ifelse(qfiletempf$discharge<0, 1, 0) # write a 1 if negative
qfiletempf$discharge <- ifelse(qfiletempf$discharge < 0, 0.0001, qfiletempf$discharge) ### change negatives to 0.0001
hyd_metrics[s,1] <- scols[s] ## site number/name of row 1 replaced with s
hyd_metrics[s,2] <- qfiletempf[1][1,] # x
hyd_metrics[s,3] <- qfiletempf[1][2,] # y
hyd_metrics[s,4] <- dh1(qfiletempf, pref = "mean")
hyd_metrics[s,5] <- dh2(qfiletempf, pref = "mean")
hyd_metrics[s,6] <- dh3(qfiletempf, pref = "mean")
hyd_metrics[s,7] <- dh4(qfiletempf, pref = "mean")
hyd_metrics[s,8] <- dh5(qfiletempf, pref = "mean")
hyd_metrics[s,9] <- dh6(qfiletempf)
hyd_metrics[s,10] <- dh7(qfiletempf)
hyd_metrics[s,11] <- dh8(qfiletempf)
hyd_metrics[s,12] <- dh9(qfiletempf)
hyd_metrics[s,13] <- dh10(qfiletempf)
hyd_metrics[s,14] <- dh11(qfiletempf, pref = "mean")
hyd_metrics[s,15] <- dh12(qfiletempf)
hyd_metrics[s,16] <- dh13(qfiletempf)
hyd_metrics[s,17] <- dh14(qfiletempf)
hyd_metrics[s,18] <- unlist(dh15.16(qfiletempf))[1]
hyd_metrics[s,19] <- unlist(dh15.16(qfiletempf))[2]
hyd_metrics[s,20] <- dh17(qfiletempf)
hyd_metrics[s,21] <- dh18(qfiletempf)
hyd_metrics[s,22] <- dh19(qfiletempf)
hyd_metrics[s,23] <- dh20(qfiletempf)
hyd_metrics[s,24] <- dh21(qfiletempf)
hyd_metrics[s,25] <- dh22(qfiletempf, thresh = quantile(qfiletempf$discharge, .99))
hyd_metrics[s,26] <- dh23(qfiletempf, thresh = quantile(qfiletempf$discharge, .99))
hyd_metrics[s,27] <- dh24(qfiletempf, thresh = quantile(qfiletempf$discharge, .99))

######### DL stats

hyd_metrics[s,28] <- dl1(qfiletempf, pref = "mean")
hyd_metrics[s,29] <- dl2(qfiletempf, pref = "mean")
hyd_metrics[s,30] <- dl3(qfiletempf, pref = "mean")
hyd_metrics[s,31] <- dl4(qfiletempf, pref = "mean")
hyd_metrics[s,32] <- dl5(qfiletempf, pref = "mean")
hyd_metrics[s,33] <- dl6(qfiletempf)
hyd_metrics[s,34] <- dl7(qfiletempf)
hyd_metrics[s,35] <- dl8(qfiletempf)
hyd_metrics[s,36] <- dl9(qfiletempf)
hyd_metrics[s,37] <- dl10(qfiletempf)
hyd_metrics[s,38] <- dl11(qfiletempf)
hyd_metrics[s,39] <- dl12(qfiletempf)
hyd_metrics[s,40] <- dl13(qfiletempf)
hyd_metrics[s,41] <- dl14(qfiletempf)
hyd_metrics[s,42] <- dl15(qfiletempf) 
hyd_metrics[s,43] <- unlist(dl16.17(qfiletempf))[1]
hyd_metrics[s,44] <- unlist(dl16.17(qfiletempf))[2]
hyd_metrics[s,45] <- dl18(qfiletempf)
hyd_metrics[s,46] <- dl19(qfiletempf)
hyd_metrics[s,47] <- dl20(qfiletempf)

##### FH stats

hyd_metrics[s,48] <- unlist(fh1.2(qfiletempf))[1]
hyd_metrics[s,49] <- unlist(fh1.2(qfiletempf))[2]
hyd_metrics[s,50] <- fh3(qfiletempf)
hyd_metrics[s,51] <- fh4(qfiletempf)
hyd_metrics[s,52] <- fh5(qfiletempf)
hyd_metrics[s,53] <- fh6(qfiletempf)
hyd_metrics[s,54] <- fh7(qfiletempf)
hyd_metrics[s,55] <- fh8(qfiletempf)
hyd_metrics[s,56] <- fh9(qfiletempf)
hyd_metrics[s,57] <- fh10(qfiletempf)
hyd_metrics[s,58] <- fh11(qfiletempf,thresh = quantile(qfiletempf$discharge, .99)) ## thresh

###### FL stats

hyd_metrics[s,59] <- unlist(fl1.2(qfiletempf))[1]
hyd_metrics[s,60] <- unlist(fl1.2(qfiletempf))[2]
hyd_metrics[s,61] <- fl3(qfiletempf)


############# MA stats

hyd_metrics[s,62] <- ma1(qfiletempf)
hyd_metrics[s,63] <- ma2(qfiletempf)
hyd_metrics[s,64] <- ma3(qfiletempf)
hyd_metrics[s,65] <- unlist(ma4.11(qfiletempf))[1]
hyd_metrics[s,66] <- unlist(ma4.11(qfiletempf))[2]
hyd_metrics[s,67] <- unlist(ma4.11(qfiletempf))[3]
hyd_metrics[s,68] <- unlist(ma4.11(qfiletempf))[4]
hyd_metrics[s,69] <- unlist(ma4.11(qfiletempf))[5]
hyd_metrics[s,70] <- unlist(ma4.11(qfiletempf))[6]
hyd_metrics[s,71] <- unlist(ma4.11(qfiletempf))[7]
hyd_metrics[s,72] <- unlist(ma4.11(qfiletempf))[8]
hyd_metrics[s,73] <- unlist(ma12.23(qfiletempf))[1]#12
hyd_metrics[s,74] <- unlist(ma12.23(qfiletempf))[2]#13
hyd_metrics[s,75] <- unlist(ma12.23(qfiletempf))[3]#14
hyd_metrics[s,76] <- unlist(ma12.23(qfiletempf))[4]#15
hyd_metrics[s,77] <- unlist(ma12.23(qfiletempf))[5]#16
hyd_metrics[s,78] <- unlist(ma12.23(qfiletempf))[6]#17
hyd_metrics[s,79] <- unlist(ma12.23(qfiletempf))[7]#18
hyd_metrics[s,80] <- unlist(ma12.23(qfiletempf))[8]#19
hyd_metrics[s,81] <- unlist(ma12.23(qfiletempf))[9]#20
hyd_metrics[s,82] <- unlist(ma12.23(qfiletempf))[10]#21
hyd_metrics[s,83] <- unlist(ma12.23(qfiletempf))[11]#22
hyd_metrics[s,84] <- unlist(ma12.23(qfiletempf))[12]#23
hyd_metrics[s,85] <- unlist(ma24.35(qfiletempf))[1]#24
hyd_metrics[s,86] <- unlist(ma24.35(qfiletempf))[2]#25
hyd_metrics[s,87] <- unlist(ma24.35(qfiletempf))[3]#26
hyd_metrics[s,88] <- unlist(ma24.35(qfiletempf))[4]#27
hyd_metrics[s,89] <- unlist(ma24.35(qfiletempf))[5]#28
hyd_metrics[s,90] <- unlist(ma24.35(qfiletempf))[6]#29
hyd_metrics[s,91] <- unlist(ma24.35(qfiletempf))[7]#30
hyd_metrics[s,92] <- unlist(ma24.35(qfiletempf))[8]#31
hyd_metrics[s,93] <- unlist(ma24.35(qfiletempf))[9]#32
hyd_metrics[s,94] <- unlist(ma24.35(qfiletempf))[10]#33
hyd_metrics[s,95] <- unlist(ma24.35(qfiletempf))[11]#34
hyd_metrics[s,96] <- unlist(ma24.35(qfiletempf))[12]#35
hyd_metrics[s,97] <- unlist(ma36.40(qfiletempf))[1]#36
hyd_metrics[s,98] <- unlist(ma36.40(qfiletempf))[2]#37
hyd_metrics[s,99] <- unlist(ma36.40(qfiletempf))[3]#38
hyd_metrics[s,100] <- unlist(ma36.40(qfiletempf))[4]#39
hyd_metrics[s,101] <- unlist(ma36.40(qfiletempf))[5]#40

####### MH stats

hyd_metrics[s,102] <- unlist(mh1.12(qfiletempf))[1]#1
hyd_metrics[s,103] <- unlist(mh1.12(qfiletempf))[2]#2
hyd_metrics[s,104] <- unlist(mh1.12(qfiletempf))[3]#3
hyd_metrics[s,105] <- unlist(mh1.12(qfiletempf))[4]#4
hyd_metrics[s,106] <- unlist(mh1.12(qfiletempf))[5]#5
hyd_metrics[s,107] <- unlist(mh1.12(qfiletempf))[6]#6
hyd_metrics[s,108] <- unlist(mh1.12(qfiletempf))[7]#7
hyd_metrics[s,109] <- unlist(mh1.12(qfiletempf))[8]#8
hyd_metrics[s,110] <- unlist(mh1.12(qfiletempf))[9]#9
hyd_metrics[s,111] <- unlist(mh1.12(qfiletempf))[10]#10
hyd_metrics[s,112] <- unlist(mh1.12(qfiletempf))[11]#11
hyd_metrics[s,113] <- unlist(mh1.12(qfiletempf))[12]#12
hyd_metrics[s,114] <- mh13(qfiletempf)#13
hyd_metrics[s,115] <- unlist(ml14.16(qfiletempf))[1]#14
hyd_metrics[s,116] <- unlist(ml14.16(qfiletempf))[2]#15
hyd_metrics[s,117] <- unlist(ml14.16(qfiletempf))[3]#16
hyd_metrics[s,118] <- ml17(qfiletempf)#17
hyd_metrics[s,119] <- ml18(qfiletempf)#18
hyd_metrics[s,120] <- ml19(qfiletempf)#19
hyd_metrics[s,121] <- ml20(qfiletempf)#20
hyd_metrics[s,122] <- ml21(qfiletempf)#21

###### RA stats

hyd_metrics[s,123] <- ra1(qfiletempf)#1
hyd_metrics[s,124] <- ra2(qfiletempf)#2
hyd_metrics[s,125] <- ra3(qfiletempf)#3
hyd_metrics[s,126] <- ra4(qfiletempf)#4
hyd_metrics[s,127] <- ra5(qfiletempf)#5
hyd_metrics[s,128] <- ra6(qfiletempf)#6 log calulation - produces some NAs (negatives??)
hyd_metrics[s,129] <- ra7(qfiletempf)#7 log calulation - produces some NAs (negatives??)
# hyd_metrics[1,130] <- unlist(ra8.9(qfiletempf))[1]#8
# hyd_metrics[1,131] <- unlist(ra8.9(qfiletempf))[2]#9


### TA stats

hyd_metrics[s,130] <- unlist(ta1.2(qfiletempf))[1]#1 log calulation - produces some NAs (negatives??)
hyd_metrics[s,131] <- unlist(ta1.2(qfiletempf))[2]#2 log calulation - produces some NAs (negatives??)
hyd_metrics[s,132] <- ta3(qfiletempf,thresh = quantile(qfiletempf$discharge, .99))#3
# warnings()
##### TH stats

hyd_metrics[s,133] <- unlist(th1.2(qfiletempf))[1]#1
hyd_metrics[s,134] <- unlist(th1.2(qfiletempf))[2]#2
hyd_metrics[s,135] <- th3(qfiletempf,thresh = quantile(qfiletempf$discharge, .99))#3

#### TL stats

hyd_metrics[s,136] <- unlist(tl1.2(qfiletempf))[1]#1
hyd_metrics[s,137] <- unlist(tl1.2(qfiletempf))[2]#2
hyd_metrics[s,138] <- tl3(qfiletempf,thresh = quantile(qfiletempf$discharge, .99))#3
hyd_metrics[s,139] <- tl4(qfiletempf,thresh = quantile(qfiletempf$discharge, .99))#4

  }
  
write.csv2(hyd_metrics, paste("/home/shared/KK/predicted_discharge_data/seasonal precipitation/metrics_calc_prec/alpine_metrics_", sitex[l]))
#write.csv2(hyd_metrics, paste(sitex[l]))

}
                          
