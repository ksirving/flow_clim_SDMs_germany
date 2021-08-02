# head(sampleData)
# 
#   wy_val       date discharge month_val year_val day_val jul_val
# 1   2011 2010-10-01        79        10     2010       1     274
# 2   2011 2010-10-03        55        10     2010       3     276
# 3   2011 2010-10-04        52        10     2010       4     277
# 4   2011 2010-10-05        49        10     2010       5     278
# 5   2011 2010-10-02        62        10     2010       2     275
# 6   2011 2010-10-07        46        10     2010       7     280

# library("zoo")
# 
# setwd("U:/Irving/Data")
# 
# sitex <- read.csv(file.choose())##("Gelnhausen/discharge_by_site/pred_discharge_s501_to_s1000.csv")
# 
# #sitex <- sitex[with(sitex, order(date)), ]
# 
# #length(sitex$X1)
# #head(sitex)
# #tail(sitex)
# jul_val<-seq(1, 365,1)
# 
# jul_val2<-rep(jul_val, times=(12775/365))
# zero<-rep(0, times=3)
# julian_val<-c(zero, jul_val2)
# qfiletempf <- cbind(sitex, julian_val)
# colnames(qfiletempf)[which(names(qfiletempf) == "julian_val")] <- "jul_val"
# #length(jul_val)
# #head(qfiletempf)
# #tail(qfiletempf)



## hydrology variables functions
##https://github.com/USGS-R/EflowStats/tree/master/R

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


# calculated 'by hand'
# max(qfiletempf$discharge)
# mean(c(max(subset(qfiletempf, wy_val == 1980)[,3]), max(subset(qfiletempf, wy_val == 1981)[,3]), max(subset(qfiletempf, wy_val == 1982)[,3])))


#' DH2; Annual maximum of 3-day moving average flows. Compute the maximum of a 3-day moving average flow for 
#' each year. DH2 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).


dh2 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max3daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day3mean <- rollmean(subsetyr$discharge, 3, align = "right", 
                         na.pad = TRUE)
    max3daybyyear[i] <- max(day3mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dh2 <- round(median(max3daybyyear),digits=2)
  }
  else {
    dh2 <- round(mean(max3daybyyear),digits=2)
  }
  return(dh2)
}



#This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH3; Annual maximum of 7-day moving average flows. Compute the maximum of a 7-day moving average flow for 
#' each year. DH3 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh3 numeric containing DH3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh3(qfiletempf)
dh3 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max7daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 7, align = "right", 
                         na.pad = FALSE)
    max7daybyyear[i] <- max(day7mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dh3 <- round(median(max7daybyyear),digits=2)
  }
  else {
    dh3 <- round(mean(max7daybyyear),digits=2)
  }
  return(dh3)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH4; Annual maximum of 30-day moving average flows. Compute the maximum of 30-day moving average flows. Compute 
#' the maximum of a 30-day moving average flow for each year. DH4 is the mean (or median-Use Preference option) 
#' of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh4 numeric containing DH4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh4(qfiletempf)
dh4 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                          na.pad = TRUE)
    max30daybyyear[i] <- max(day30mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dh4 <- round(median(max30daybyyear),digits=2)
  }
  else {
    dh4 <- round(mean(max30daybyyear),digits=2)
  }
  return(dh4)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH5; Annual maximum of 90-day moving average flows. Compute the maximum of a 90-day moving average flow for 
#' each year. DH5 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dh5 numeric containing DH5 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh5(qfiletempf)
dh5 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                          na.pad = TRUE)
    max90daybyyear[i] <- max(day90mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dh5 <- round(median(max90daybyyear),digits=2)
  }
  else {
    dh5 <- round(mean(max90daybyyear),digits=2)
  }
  return(dh5)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH6; Variability of annual maximum daily flows. Compute the standard deviation for the maximum 1-day 
#' moving averages. DH6 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh6 numeric containing DH6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh6(qfiletempf)
dh6 <- function(qfiletempf) {
  meandh6 <- dh1(qfiletempf, pref = "mean")
  maxbyyear <- aggregate(qfiletempf$discharge, 
                         list(qfiletempf$wy_val), max, na.rm=TRUE)
  sddh6 <- sd(maxbyyear$x)
  dh6 <- round((sddh6 * 100)/meandh6,digits=2)
  return(dh6)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH7; Variability of annual maximum of 3-day moving average flows. Compute the standard deviation for the 
#' maximum 3-day moving averages. DH7 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh7 numeric containing DH7 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh7(qfiletempf)
dh7 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandh7 <- dh2(qfiletempf, pref = "mean")
  day3mean <- rollmean(qfiletempf$discharge, 3, align = "right", 
                       na.pad = TRUE)
  day3rollingavg <- data.frame(qfiletempf, day3mean)
  rollingavgs3day <- subset(day3rollingavg, day3rollingavg$day3mean != 
                              "NA")
  max3daybyyear <- aggregate(rollingavgs3day$day3mean, 
                             list(rollingavgs3day$wy_val), max, na.rm=TRUE)
  sddh7 <- sd(max3daybyyear$x)
  dh7 <- round((sddh7 * 100)/meandh7,digits=2)
  return(dh7)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DH8; Variability of annual maximum of 7-day moving average flows. Compute the standard deviation for the 
#' maximum 7-day moving averages. DH8 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh8 numeric containing DH8 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh8(qfiletempf)
dh8 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandh8 <- dh3(qfiletempf, pref = "mean")
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  max7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$wy_val), max, na.rm=TRUE)
  sddh8 <- sd(max7daybyyear$x)
  dh8 <- round((sddh8 * 100)/meandh8,digits=2)
  return(dh8)
}

#' DH9; Variability of annual maximum of 30-day moving average flows. Compute the standard deviation for the 
#' maximum 30-day moving averages. DH9 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dh9 numeric containing DH9 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dh9(qfiletempf)
dh9 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandh9 <- dh4(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                          na.pad = TRUE)
    max30daybyyear[i] <- max(day30mean, na.rm=TRUE)
  }
  sddh9 <- sd(max30daybyyear)
  dh9 <- round((sddh9 * 100)/meandh9,digits=2)
  return(dh9)
}


## DH10; Variability of annual maximum of 90-day moving average flows. Compute the standard deviation for the 
#' maximum 90-day moving averages. DH10 is 100 times the standard deviation divided by the mean (percent-spatial)
#' need data with discharge column & daily flows
#' 
dh10 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandh10 <- dh5(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  max90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                          na.pad = TRUE)
    max90daybyyear[i] <- max(day90mean, na.rm=TRUE)
  }
  sddh10 <- sd(max90daybyyear)
  dh10 <- round((sddh10 * 100)/meandh10,digits=2)
  return(dh10)
}


dh10(qfiletempf)


#' DH11; Annual maximum of 1-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 1-day moving average flow for each year. DH11 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
dh11 <- function(qfiletempf, pref = "mean") {
meanmaxbyyear <- dh1(qfiletempf)
medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
dh11 <- round(meanmaxbyyear/medianflow,digits=2)
return(dh11)
}

#DH12; Annual maximum of 7-day moving average flows divided by the median for the entire record. Compute the 
#' maximum daily average flow for each year. DH12 is the mean of these values divided by the median for the 
#' entire record (dimensionless-spatial).

dh12 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  meanmax7day <- dh3(qfiletempf)
  dh12 <- round(meanmax7day/medianflow,digits=2)
  return(dh12)
}

##DH13; Annual maximum of 30-day moving average flows divided by the median for the entire record. Compute the 
#' maximum of a 30-day moving average flow for each year. DH13 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).

dh13 <- function(qfiletempf) {
  medianflow <- median(qfiletempf$discharge,na.rm=TRUE)
  meanmax30day <- dh4(qfiletempf)
  dh13 <- round(meanmax30day/medianflow,digits=2)
  return(dh13)
}


#DH14; Flood duration. Compute the mean of the mean monthly flow values. Find the 95th percentile for the 
#' mean monthly flows. DH14 is the 95th percentile value divided by the mean of the monthly means 
dh14 <- function(qfiletempf) {
  meanmonthly <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val,qfiletempf$wy_val), mean, na.rm=TRUE)
  meanflow <- mean(meanmonthly$x)
  isolateq <- meanmonthly$x
  sortq <- sort(isolateq)
  hfcrit <- quantile(sortq,.95,type=6)
  dh14 <- round(hfcrit/meanflow,digits=2)
  return(dh14)
}

#' DH15; High flow pulse duration. Compute the average duration for flow events with flows above a threshold equal 
#' to the 75th percentile value for each year in the flow record. DH15 is the median of the yearly average durations 
#' (days/year-temporal). and 
#' DH16; Variability in high flow pulse duration. Compute the standard deviation for the yearly average high pulse 
#' durations. DH16 is 100 times the standard deviation divided by the mean of the yearly average high pulse durations 
#' (percent-spatial).

dh15.16 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.75,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  dh15 <- round(median(hfdur),digits=2)
  dh16 <- round((sd(hfdur)*100)/mean(hfdur),digits=2)
  dh15.16 <- list(dh15=dh15,dh16=dh16)
  return(dh15.16)
}

#' DH17; High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' the median flow value for the entire flow record. DH17 is the average (or median-Use Preference option) duration 
#' of the events (days-temporal).

dh17 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  lfcrit <- median(qfiletempf$discharge)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(as.character(qfiletempf$wy_val)) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  hfdur_pos <- hfdur[hfdur>0]
  if (length(hfdur_pos)>0) {
    dh17 <- round(mean(hfdur_pos),digits=2)
  } else { dh17<-'NA'} 
  return(dh17)
}

#' DH18; High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' three times the median flow value for the entire flow record. DH18 is the average 
#' (or median - Use Preference option) duration of the events (days-temporal).

dh18 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  lfcrit <- median(qfiletempf$discharge)*3
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  hfdur_pos <- hfdur[hfdur>0]
  if (length(hfdur_pos)>0) {
    dh18 <- round(mean(hfdur_pos),digits=2) 
  } else { dh18<-0}
  return(dh18)
}

#' DH19; High flow duration. Compute the average duration of flow events with flows above a threshold equal to 
#' seven times the median flow value for the entire flow record. DH19 is the average (or median-Use Preference option) 
#' duration of the events (days-temporal)

dh19 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  lfcrit <- median(qfiletempf$discharge)*7
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  hfdur_pos <- hfdur[hfdur>0]
  if (length(hfdur_pos)>0) {
    dh19 <- round(mean(hfdur_pos),digits=2) 
  } else { dh19<-0}
  return(dh19)
}


# DH20; High flow duration. Compute the 75th percentile value for the entire flow record. Compute the average 
#' duration of flow events with flows above a threshold equal to the 75th percentile value for the median annual 
#' flows. DH20 is the average (or median-Use Preference option) duration of the events (days-temporal).

dh20 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.75,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  hfdur_pos <- hfdur[hfdur>0]
  if (length(hfdur_pos)>0) {
    dh20 <- round(mean(hfdur_pos),digits=2)
  } else { dh20<-0} 
  return(dh20)
}

#' DH21; High flow duration. Compute the 25th percentile value for the entire flow record. Compute the average 
#' duration of flow events with flows above a threshold equal to the 25th percentile value for the entire set 
#' of flows. DH21 is the average (or median-Use Preference option) duration of the events (days-temporal). 

dh21 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.25,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {hfdur[i]<-pdur/nevents}
  }
  hfdur_pos <- hfdur[hfdur>0]
  if (length(hfdur_pos)>0) {
    dh21 <- round(mean(hfdur_pos),digits=2)
  } else { dh21<-0} 
  return(dh21)
}

# DH22; Flood interval. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Determine the median number of days between flood events for each year. DH22 is the mean (or median-Use 
#' Preference option) of the yearly median number of days between flood events (days-temporal)

dh22 <- function(qfiletempf, thresh) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
  med_yr <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {
        if (flag > 0) {
          dur$dur[nevents]<-pdur
          dur$Year[nevents]<-subsetyr$wy_val[j]
        }
        flag <- 0
        pdur <- 0
      }
    }
    dur_sub <- dur$dur[dur$Year==subsetyr$wy_val[j]]
    med_yr[i] <- median(dur_sub)
  }
  med_yr[is.na(med_yr)]<-0
  dh22 <- round(mean(med_yr,na.rm=TRUE),digits=2)
  return(dh22)
}

#' DH23; Flood duration. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Determine the number of days each year that the flow remains above the flood threshold. DH23 is the mean (or 
#' median-Use Preference option) of the number of flood days for years in which floods occur (days-temporal)

dh23 <- function(qfiletempf, thresh) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
  nevents <- 0
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        dur$Year[nevents] <- subsetyr$wy_val[j]
        dur$dur[nevents] <- dur$dur[nevents]+1
      } else {flag <- 0}
    }
  }
  subset_dur <- dur[1:nevents ,]
  meanbyyr <- aggregate(subset_dur$dur, list(subset_dur$Year), mean)
  colnames(meanbyyr) <- c("Year", "num_mean")
  dh23 <- round(mean(meanbyyr$num_mean),digits=2)
  return(dh23)
}

#' DH24; Flood-free days. Compute the flood threshold as the flow equivalent for a flood recurrence of 1.67 years. 
#' Compute the maximum number of days that the flow is below the threshold for each year. DH24 is the mean 
#' (or median-Use Preference option) of the maximum yearly no-flood days (days-temporal).

dh24 <- function(qfiletempf, thresh) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  dur <- data.frame(Year = rep(0,nrow(qfiletempf)), dur = rep(1,nrow(qfiletempf)))
  max_yr <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {
        if (flag > 0) {
          dur$dur[nevents]<-pdur
          dur$Year[nevents]<-subsetyr$wy_val[j]
        }
        flag <- 0
        pdur <- 0
      }
    }
    dur_sub <- dur$dur[dur$Year==subsetyr$wy_val[j]]
    max_yr[i] <- ifelse(length(dur_sub)>0,max(dur_sub),0)
  }
  max_yr[max_yr==0]<-NA
  dh24 <- round(mean(max_yr,na.rm=TRUE),digits=2)
  return(dh24)
}

# _______________________________________________________________________________________________

# Compute DH indicators

# make data frame to store coefficients
hyd_metrics <- data.frame(matrix(nrow = 0, ncol = 26))
head(sitex)

summary(sitex$discharge)
plot(sitex$discharge)
quantile(sitex$discharge, .99)
nrow(subset(sitex, discharge >= 12.62695))

hyd_metrics[1,1] <- sitex[1,2]
hyd_metrics[1,2] <- sitex[1,3]
hyd_metrics[1,3] <- dh1(qfiletempf, pref = "mean")
hyd_metrics[1,4] <- dh2(qfiletempf, pref = "mean")
hyd_metrics[1,5] <- dh3(qfiletempf, pref = "mean")
hyd_metrics[1,6] <- dh4(qfiletempf, pref = "mean")
hyd_metrics[1,7] <- dh5(qfiletempf, pref = "mean")
hyd_metrics[1,8] <- dh6(qfiletempf)
hyd_metrics[1,9] <- dh7(qfiletempf)
hyd_metrics[1,10] <- dh8(qfiletempf)
hyd_metrics[1,11] <- dh9(qfiletempf)
hyd_metrics[1,12] <- dh10(qfiletempf)
hyd_metrics[1,13] <- dh11(qfiletempf, pref = "mean")
hyd_metrics[1,14] <- dh12(qfiletempf)
hyd_metrics[1,15] <- dh13(qfiletempf)
hyd_metrics[1,16] <- dh14(qfiletempf)
hyd_metrics[1,17] <- unlist(dh15.16(qfiletempf))[1]
hyd_metrics[1,18] <- unlist(dh15.16(qfiletempf))[2]
hyd_metrics[1,19] <- dh17(qfiletempf)
hyd_metrics[1,20] <- dh18(qfiletempf)
hyd_metrics[1,21] <- dh19(qfiletempf)
hyd_metrics[1,22] <- dh20(qfiletempf)
hyd_metrics[1,23] <- dh21(qfiletempf)
hyd_metrics[1,24] <- dh22(qfiletempf, thresh = quantile(sitex$discharge, .99))
hyd_metrics[1,25] <- dh23(qfiletempf, thresh = quantile(sitex$discharge, .99))
hyd_metrics[1,26] <- dh24(qfiletempf, thresh = quantile(sitex$discharge, .99))

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL1; Annual minimum daily flow. Compute the minimum 1-day average flow for each year. DL1 is the mean 
#' (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl1 numeric containing DL1 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl1(qfiletempf)
dl1 <- function(qfiletempf, pref = "mean") {
  annualminimum <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), min)
  if (pref == "median") {
    dl1 <- round(median(annualminimum$x),digits=2)
  }
  else {
    dl1 <- round(mean(annualminimum$x),digits=2)
  }
  return(dl1)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL10; Variability of annual minimum of 90-day moving average flow. Compute the standard deviation for the 
#' minimum 90-day moving averages. DL10 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl10 numeric containing DL10 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl10(qfiletempf)
dl10 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandl10 <- dl5(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                          na.pad = FALSE)
    min90daybyyear[i] <- min(day90mean, na.rm=TRUE)
  }
  sddl10 <- sd(min90daybyyear)
  dl10 <- round((sddl10 * 100)/meandl10,digits=2)
  return(dl10)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL11; Annual minimum daily flow divided by the median for the entire record. Compute the minimum daily flow 
#' for each year. DL11 is the mean of these values divided by the median for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl11 numeric containing DL11 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl11(qfiletempf)
dl11 <- function(qfiletempf) {
  meanmin <- dl1(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl11 <- round(meanmin/medianq,digits=2)
  return(dl11)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL12; Annual minimum of 7-day moving average flow divided by the median for the entire record. Compute the 
#' minimum of a 7-day moving average flow for each year. DL12 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl12 numeric containing DL12 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl12(qfiletempf)
dl12 <- function(qfiletempf) {
  meanmin <- dl3(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl12 <- round(meanmin/medianq,digits=2)
  return(dl12)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL13; Annual minimum of 30-day moving average flow divided by the median for the entire record. Compute the 
#' minimum of a 30-day moving average flow for each year. DL13 is the mean of these values divided by the median 
#' for the entire record (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl13 numeric containing DL13 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl13(qfiletempf)
dl13 <- function(qfiletempf) {
  meanmin <- dl4(qfiletempf)
  medianq <- median(qfiletempf$discharge)
  dl13 <- round(meanmin/medianq,digits=2)
  return(dl13)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL14; Low exceedence flows. Compute the 75-percent exceedence value for the entire flow record. DL14 is 
#' the exceedence value divided by the median for the entire record (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl14 numeric containing DL14 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl14(qfiletempf)
dl14 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit <- quantile(sortq,.25,type=6)
  medianq <- median(qfiletempf$discharge)
  dl14 <- round(hfcrit/medianq,digits=2)
  return(dl14)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL15; Low exceedence flows. Compute the 90-percent exceedence value for the entire flow record. DL15 is the 
#' exceedence value divided by the median for the entire record (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl15 numeric containing DL15 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl15(qfiletempf)
dl15 <- function(qfiletempf) {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit <- quantile(sortq,.1,type=6)
  medianq <- median(qfiletempf$discharge)
  dl15 <- round(hfcrit/medianq,digits=2)
  return(dl15)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL16; Low flow pulse duration. Compute the average pulse duration for each year for flow events below a 
#' threshold equal to the 25th percentile value for the entire flow record. DL16 is the median of the yearly 
#' average durations (number of days-temporal). and 
#' DL17; Variability in low pulse duration. Compute the standard deviation for the yearly average low pulse durations. 
#' DL17 is 100 times the standard deviation divided by the mean of the yearly average low pulse durations 
#' (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl16.17 list containing DL16 and DL17 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl16.17(qfiletempf)
dl16.17 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.25,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  lfdur <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    pdur <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        pdur <- pdur+1
      } else {flag <- 0}
    }
    if (nevents>0) {lfdur[i]<-pdur/nevents}
  }
  dl16 <- round(median(lfdur),digits=2)
  dl17 <- round((sd(lfdur)*100)/mean(lfdur),digits=2)
  dl16.17 <- list(dl16=dl16,dl17=dl17)
  return(dl16.17)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL18; Number of zero-flow days. Count the number of zero-flow days for the entire flow record. DL18 is the 
#' mean (or median-Use Preference option) annual number of zero flow days (number of days/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl18 numeric containing DL18 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl18(qfiletempf)
dl18 <- function(qfiletempf, pref = "mean") {
  subset_zero <- subset(qfiletempf,qfiletempf$discharge==0)
  if (nrow(subset_zero)>0) {
    subset_zero$discharge <- subset_zero$discharge+1
    zero_cnts <- aggregate(subset_zero$discharge, list(subset_zero$wy_val), sum)
    wy_cnts <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),sum)
    colnames(wy_cnts) <- c("wy_val","zero_cnt")
    wy_cnts$zero_cnt <- rep(0,nrow(wy_cnts))
    wy_cnts <- merge(wy_cnts,zero_cnts,by.x="wy_val",by.y="Group.1",all.x=TRUE)
    wy_cnts$zero_cnt <- wy_cnts$zero_cnt+wy_cnts$x
    wy_cnts$zero_cnt[is.na(wy_cnts$zero_cnt)] <- 0
    
  } else {wy_cnts <- data.frame(zero_cnt=rep(0,1))}
  if (pref == "median") {
    dl18 <- round(median(wy_cnts$zero_cnt),digits=2)
  }
  else {
    dl18 <- round(mean(wy_cnts$zero_cnt),digits=2)
  }
  return(dl18)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL19; Variability in the number of zero-flow days. Compute the standard deviation for the annual number of 
#' zero-flow days. DL19 is 100 times the standard deviation divided by the mean annual number of zero-flow days 
#' (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl19 numeric containing DL19 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl19(qfiletempf)
dl19 <- function(qfiletempf) {
  subset_zero <- subset(qfiletempf,qfiletempf$discharge==0)
  if (nrow(subset_zero)>0) {
    subset_zero$discharge <- subset_zero$discharge+1
    zero_cnts <- aggregate(subset_zero$discharge, list(subset_zero$wy_val), sum)
    wy_cnts <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),sum)
    colnames(wy_cnts) <- c("wy_val","zero_cnt")
    wy_cnts$zero_cnt <- rep(0,nrow(wy_cnts))
    wy_cnts <- merge(wy_cnts,zero_cnts,by.x="wy_val",by.y="Group.1",all.x=TRUE)
    wy_cnts$zero_cnt <- wy_cnts$zero_cnt+wy_cnts$x
    wy_cnts$zero_cnt[is.na(wy_cnts$zero_cnt)] <- 0
    meanzero <- mean(wy_cnts$zero_cnt)
    sdzero <- sd(wy_cnts$zero_cnt)
    dl19 <- round((100*sdzero)/meanzero,digits=2)
  } else {
    dl19 <- 0
  }
  return(dl19)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL2; Annual minimum of 3-day moving average flow. Compute the minimum of a 3-day moving average flow for 
#' each year. DL2 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl2 numeric containing DL2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl2(qfiletempf)
dl2 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min3daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day3mean <- rollmean(subsetyr$discharge, 3, align = "right", 
                         na.pad = TRUE)
    min3daybyyear[i] <- min(day3mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl2 <- round(median(min3daybyyear),digits=2)
  }
  else {
    dl2 <- round(mean(min3daybyyear),digits=2)
  }
  return(dl2)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL20; Number of zero-flow months. While computing the mean monthly flow values, count the number of months 
#' in which there was no flow over the entire flow record (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl20 numeric containing DL20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl20(qfiletempf)
dl20 <- function(qfiletempf) {
  sumbymonyr <- aggregate(qfiletempf$discharge,list(qfiletempf$month_val,qfiletempf$year_val),FUN=sum,na.rm=TRUE)
  if (min(sumbymonyr$x)==0) {
    zeromon <- subset(sumbymonyr$x,sumbymonyr$x==0)
    dl20 <- length(zeromon)
  } 
  else {
    dl20 <- 0
  }
  return(dl20)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL3; Annual minimum of 7-day moving average flow. Compute the minimum of a 7-day moving average flow for 
#' each year. DL3 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl3 numeric containing DL3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl3(qfiletempf)
dl3 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min7daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 7, align = "right", 
                         na.pad = TRUE)
    min7daybyyear[i] <- min(day7mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl3 <- round(median(min7daybyyear),digits=2)
  }
  else {
    dl3 <- round(mean(min7daybyyear),digits=2)
  }
  return(dl3)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL4; Annual minimum of 30-day moving average flow. Compute the minimum of a 30-day moving average flow 
#' for each year. DL4 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl4 numeric containing DL4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl4(qfiletempf)
dl4 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                          na.pad = TRUE)
    min30daybyyear[i] <- min(day30mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl4 <- round(median(min30daybyyear),digits=2)
  }
  else {
    dl4 <- round(mean(min30daybyyear),digits=2)
  }
  return(dl4)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL5; Annual minimum of 90-day moving average flow. Compute the minimum of a 90-day moving average flow 
#' for each year. DL5 is the mean (or median-Use Preference option) of these values (cubic feet per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return dl5 numeric containing DL5 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl5(qfiletempf)
dl5 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min90daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day90mean <- rollmean(subsetyr$discharge, 90, align = "right", 
                          na.pad = FALSE)
    min90daybyyear[i] <- min(day90mean, na.rm=TRUE)
  }
  if (pref == "median") {
    dl5 <- round(median(min90daybyyear),digits=2)
  }
  else {
    dl5 <- round(mean(min90daybyyear),digits=2)
  }
  return(dl5)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL6; Variability of annual minimum daily average flow. Compute the standard deviation for the minimum daily 
#' average flow. DL6 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl6 numeric containing DL6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl6(qfiletempf)
dl6 <- function(qfiletempf) {
  meandl6 <- dl1(qfiletempf, pref = "mean")
  annualminimum <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), min)
  sddl6 <- sd(annualminimum$x)
  dl6 <- round((sddl6 * 100)/meandl6,digits=2)
  return(dl6)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL7; Variability of annual minimum of 3-day moving average flow. Compute the standard deviation for the 
#' minimum 3-day moving averages. DL7 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl7 numeric containing DL7 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl7(qfiletempf)
dl7 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandl7 <- dl2(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min3daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day3mean <- rollmean(subsetyr$discharge, 3, align = "right", 
                         na.pad = TRUE)
    min3daybyyear[i] <- min(day3mean, na.rm=TRUE)
  }
  sddl7 <- sd(min3daybyyear)
  dl7 <- round((sddl7 * 100)/meandl7,digits=2)
  return(dl7)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL8; Variability of annual minimum of 7-day moving average flow. Compute the standard deviation for the 
#' minimum 7-day moving averages. DL8 is 100 times the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl8 numeric containing DL8 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl8(qfiletempf)
dl8 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandl8 <- dl3(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min7daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 7, align = "right", 
                         na.pad = TRUE)
    min7daybyyear[i] <- min(day7mean, na.rm=TRUE)
  }
  sddl8 <- sd(min7daybyyear)
  dl8 <- round((sddl8 * 100)/meandl8,digits=2)
  return(dl8)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' DL9; Variability of annual minimum of 30-day moving average flow. Compute the standard deviation for the 
#' minimum 30-day moving averages. DL9 is 100 times the standard deviation divided by the mean (percent-spatial). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return dl9 numeric containing DL9 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' dl9(qfiletempf)
dl9 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meandl9 <- dl4(qfiletempf, pref = "mean")
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  min30daybyyear <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day30mean <- rollmean(subsetyr$discharge, 30, align = "right", 
                          na.pad = TRUE)
    min30daybyyear[i] <- min(day30mean, na.rm=TRUE)
  }
  sddl9 <- sd(min30daybyyear)
  dl9 <- round((sddl9 * 100)/meandl9,digits=2)
  return(dl9)
}
######
###findrank function
findrank <- function(n, p) {
  r <- (1 - p) * (n + 1)
  findrank <- floor(r)
  return(findrank)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH1; High flood pulse count. Compute the average number of flow events with flows above a threshold equal to 
#' the 75th percentile value for the entire flow record. FH1 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal). 
#' FH2; Variability in high pulse count. Compute the standard deviation in the annual pulse counts for FH1. FH2 
#' is 100 times the standard deviation divided by the mean pulse count (number of events/year-spatial). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh1.2 list of FH1 and FH2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh1.2(qfiletempf)
fh1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  frank <- floor(findrank(length(sortq), 0.25))
  hfcrit <- sortq[frank]
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  stdevfh1 <- sd(counter)
  fh2 <- round((stdevfh1 * 100)/mean(counter),digits=2)
  if (pref == "median") {
    fh1 <- round(median(counter),digits=2)
  }
  else {
    fh1 <- round(mean(counter),digits=2)
  }
  fh1.2<-list(fh1=fh1,fh2=fh2)
  return(fh1.2)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH10; Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' median of the annual minima for the entire flow record. FH10 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh10 numeric value of FH10 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh10(qfiletempf)
fh10 <- function(qfiletempf, pref = "mean") {
  minbyyear <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),FUN=min,na.rm=TRUE)
  medmin <- median(minbyyear$x)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>medmin) {
        flag <- flag+1
        hfcountbyyrfh4[i] <- ifelse(flag==1,hfcountbyyrfh4[i]+1,hfcountbyyrfh4[i])
      } else {flag<-0}
    }
  }
  if (pref == "median") {
    fh10 <- round(median(hfcountbyyrfh4),digits=2)
  }
  else {
    fh10 <- round(mean(hfcountbyyrfh4),digits=2)
  }
  return(fh10)
}

#' This function accepts a data frame that contains a column named "discharge" and a threshold value calculated 
#' for the site using the peakdata and peakthresh functions and calculates 
#' FH11; Flood frequency. Compute the average number of flow events with flows above a threshold equal to flow 
#' corresponding to a 1.67-year recurrence interval. FH11 is the average (or median-Use Preference option) number 
#' of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @param pref string containing a "mean" or "median" preference
#' @return fh11 numeric containing FH11 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' sites<-"02178400"
#' peakValues<-getPeakData(sites)
#' thresh<-1158.495
#' fh11(qfiletempf,thresh)
fh11 <- function(qfiletempf, thresh, pref = "mean") {
  lfcrit <- thresh
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  hfcountbyyrfh4 <- rep(0, noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        hfcountbyyrfh4[i] <- ifelse(flag==1,hfcountbyyrfh4[i]+1,hfcountbyyrfh4[i])
      } else {flag<-0}
    }
  }
  if (pref == "median") {
    fh11v <- round(median(hfcountbyyrfh4),digits=2)
  }
  else {
    fh11v <- round(mean(hfcountbyyrfh4),digits=2)
  }
  return(fh11v)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH3; High flood pulse count. Compute the average number of days per year that the flow is above a threshold equal 
#' to three times the median flow for the entire record. FH3 is the mean (or median-Use Preference option) of the 
#' annual number of days for all years (number of days/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh3 numeric value of FH3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh3(qfiletempf)
fh3 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 3 * ma2(qfiletempf)
  highflow <- subset(qfiletempf,qfiletempf$discharge>hfcrit)
  if (nrow(highflow)>0) {
    highbyyr <- aggregate(highflow$discharge,list(highflow$wy_val),FUN=length)
    numrows <- nrow(highbyyr)
    numyrs <- length(unique(qfiletempf$wy_val))
    if (numrows<numyrs) { highbyyr_x <- c(highbyyr$x,rep(0,numyrs-numrows)) } else { highbyyr_x <- highbyyr$x}
    if (pref == "median") {
      fh3 <- round(median(highbyyr_x),digits=2)
    }
    else {
      fh3 <- round(mean(highbyyr_x),digits=2)
    }}
  else {
    fh3 <- 0
  }
  return(fh3)
}

#' FH4; High flood pulse count. Compute the average number of days per year that the flow is above a threshold 
#' equal to seven times the median flow for the entire record. FH4 is the mean (or median - Use Preference option) 
#' of the annual number of days for all years (number of days/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh4 numeric value of high flood pulse count for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh4(qfiletempf)
fh4 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 7 * ma2(qfiletempf)
  highflow <- subset(qfiletempf,qfiletempf$discharge>hfcrit)
  if (nrow(highflow)>0) {
    highbyyr <- aggregate(highflow$discharge,list(highflow$wy_val),FUN=length)
    numrows <- nrow(highbyyr)
    numyrs <- length(unique(qfiletempf$wy_val))
    if (numrows<numyrs) { highbyyr_x <- c(highbyyr$x,rep(0,numyrs-numrows)) } else { highbyyr_x <- highbyyr$x}
    if (pref == "median") {
      fh4 <- round(median(highbyyr_x),digits=2)
    }
  
  else {
    fh4 <- round(mean(highbyyr_x),digits=2)
  }}
else {
  fh4 <- 0
}
return(fh4)
}
    
    
    #' This function accepts a data frame that contains a column named "discharge" and calculates 
    #' FH5; Flood frequency. Compute the average number of flow events with flows above a threshold equal to the 
    #' median flow value for the entire flow record. FH5 is the average (or median - Use Preference option) number 
    #' of events (number of events/year-temporal).
    #' 
    #' @param qfiletempf data frame containing a "discharge" column containing daily flow values
    #' @param pref string containing a "mean" or "median" preference
    #' @return fh5 numeric value of FH5 for the given data frame
    #' @export
    #' @examples
    #' qfiletempf<-sampleData
    #' fh5(qfiletempf)
    fh5 <- function(qfiletempf, pref = "mean") {
      hfcrit <- ma2(qfiletempf)
      noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                           FUN = median, na.rm=TRUE)
      colnames(noyears) <- c("Year", "momax")
      noyrs <- length(noyears$Year)
      counter <- rep(0,noyrs)
      for (i in 1:noyrs) {
        subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
        flag <- 0
        counter[i] <- 0
        for (j in 1:nrow(subsetyr)) {
          if (subsetyr$discharge[j]>hfcrit) {
            flag <- flag+1
            counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
          } else {flag <- 0}
        }}
      if (pref == "median") {
        fh5 <- round(median(counter),digits=2)
      }
      else {
        fh5 <- round(mean(counter),digits=2)
      }
      return(fh5)
    }
   


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH6; Flood frequency. Compute the average number of flow events with flows above a threshold equal to three 
#' times the median flow value for the entire flow record. FH6 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh6 numeric value of FH6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh6(qfiletempf)
fh6 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 3*ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  if (pref == "median") {
    fh6 <- round(median(counter),digits=2)
  }
  else {
    fh6 <- round(mean(counter),digits=2)
  }
  return(fh6)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH7; Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' seven times the median flow value for the entire flow record. FH7 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh7 numeric value of FH7 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh7(qfiletempf)
fh7 <- function(qfiletempf, pref = "mean") {
  hfcrit <- 7*ma2(qfiletempf)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  if (pref == "median") {
    fh7 <- round(median(counter),digits=2)
  }
  else {
    fh7 <- round(mean(counter),digits=2)
  }
  return(fh7)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH8; Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' 25-percent exceedence value for the entire flow record. FH8 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh8 numeric value of FH8 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh8(qfiletempf)
fh8 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit <- quantile(sortq,.75,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  if (pref == "median") {
    fh8 <- round(median(counter),digits=2)
  }
  else {
    fh8 <- round(mean(counter),digits=2)
  }
  return(fh8)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FH9; Flood frequency. Compute the average number of flow events with flows above a threshold equal to 
#' 75-percent exceedence value for the entire flow record. FH9 is the average (or median-Use Preference option) 
#' number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fh9 numeric value of FH9 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fh9(qfiletempf)
fh9 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  hfcrit <- quantile(sortq,.25,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>hfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  if (pref == "median") {
    fh9 <- round(median(counter),digits=2)
  }
  else {
    fh9 <- round(mean(counter),digits=2)
  }
  return(fh9)
}


#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FL1; Low flood pulse count. Compute the average number of flow events with flows below a threshold equal to the 
#' 25th percentile value for the entire flow record. FL1 is the average (or median-Use Preference option) number of 
#' events (number of events/year-temporal). and 
#' FL2; Variability in low pulse count. Compute the standard deviation in the annual pulse counts for FL1. FL2 is 
#' 100 times the standard deviation divided by the mean pulse count (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl1.2 list of FL1 and FL2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fl1.2(qfiletempf)
fl1.2 <- function(qfiletempf, pref = "mean") {
  isolateq <- qfiletempf$discharge
  sortq <- sort(isolateq)
  lfcrit <- quantile(sortq,.25,type=6)
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  counter <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    flag <- 0
    counter[i] <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<lfcrit) {
        flag <- flag+1
        counter[i] <- ifelse(flag==1,counter[i]+1,counter[i])
      } else {flag <- 0}
    }}
  stdevfl1 <- sd(counter)
  fl2 <- round((stdevfl1 * 100)/mean(counter),digits=2)
  if (pref == "median") {
    fl1 <- round(median(counter),digits=2)
  }
  else {
    fl1 <- round(mean(counter),digits=2)
  }
  fl1.2<-list(fl1=fl1,fl2=fl2)
  return(fl1.2)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' FL3; Frequency of low pulse spells. Compute the average number of flow events with flows below a threshold 
#' equal to 5 percent of the mean flow value for the entire flow record. FL3 is the average (or median-Use 
#' Preference option) number of events (number of events/year-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return fl3 numeric containing FL3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' fl3(qfiletempf)
fl3 <- function(qfiletempf, pref = "mean") {
  lfcrit <- 0.05*mean(qfiletempf$discharge)
  lowflow <- subset(qfiletempf,qfiletempf$discharge<lfcrit)
  
  if (nrow(lowflow)>0) {
    lowCounts <- aggregate(lowflow$discharge,list(lowflow$wy_val),FUN=length)
    names(lowCounts) <- c("wy_val","count")
    lowbyyr <- data.frame(wy_val = unique(qfiletempf$wy_val))
    lowbyyr <- merge(lowbyyr,lowCounts, by = "wy_val",all=TRUE)
    lowbyyr$count[is.na(lowbyyr$count)] <- 0
    
    if (pref == "median") {
      fl3 <- round(median(lowbyyr$count),digits=2)
    }
    else {
      fl3 <- round(mean(lowbyyr$count),digits=2)
    }}
  else {
    fl3 <- 0
  }
  return(fl3)
}

#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean of the daily flow values for the entire record (cubic feet per second)
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ma1 numeric value of the mean daily flow for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma1(qfiletempf)
ma1 <- function(x) {
  ma1 <- round(mean(x$discharge,na.rm=TRUE),digits=2)
  return(ma1)
}

#' This function accepts a data frame that contains columns named "discharge" and "month_val" and 
#' calculates the means (or medians - use preference option) of monthly flow values. Compute the means 
#' for each month over the entire record. For example, MA12 is the mean of all January flow values over the 
#' entire record (cubic feet per second - temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ma12.23 data frame containing the mean or medians for each month
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma12.23(qfiletempf)
#' ma12.23(qfiletempf,pref="median")
ma12.23 <- function(qfiletempf, pref = "mean") {
  if (pref == "median") {
    medmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                        median, na.rm=TRUE)
    ma12.23 <- data.frame(round(medmon[2],digits=2))
  }
  else {
    meanmon <- aggregate(qfiletempf$discharge, list(qfiletempf$month_val), 
                         mean, na.rm=TRUE)
    ma12.23 <- data.frame(round(meanmon[2],digits=2))
  }
  return(ma12.23)
}

#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the median of the daily flow values for the entire record (cubic feet per second)
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ma2 numeric value of the median daily flow for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma2(qfiletempf)
ma2 <- function(x) {
  ma2 <- round(median(x$discharge,na.rm=TRUE),digits=2)
  return(ma2)
}


#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates the variability (coefficient of variation) of monthly flow values. compute the standard deviation for each month in each 
#' year over the entire flow record. Divide the standard deviation by the mean for each month. Average (or median - use preference option) these values for 
#' each month across all years (percent - temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ma234.35 data frame containing the MA24-MA35 statistics for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma24.35(qfiletempf)
ma24.35 <- function(qfiletempf, pref = "mean") {
  sdmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                    qfiletempf$month_val), FUN = sd, na.rm=TRUE)
  colnames(sdmonbyyr) <- c("Year", "Month", "sdq")
  meanmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                      qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonbyyr) <- c("Year", "Month", "meanq")
  dfcvmonbyyr <- data.frame(meanmonbyyr$Year, meanmonbyyr$Month, 
                            sdmonbyyr$sdq, meanmonbyyr$meanq)
  colnames(dfcvmonbyyr) <- c("Year", "Month", "sdq", 
                             "meanq")
  cvmonbyyr <- dfcvmonbyyr$sdq/dfcvmonbyyr$meanq
  dfcvmonbyyrf <- data.frame(dfcvmonbyyr, cvmonbyyr)
  colnames(dfcvmonbyyrf) <- c("Year", "Month", "sdq", 
                              "meanq", "cvq")
  if (pref == "median") {
    medmoncv <- aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), 
                          median, na.rm=TRUE)
    ma24.35 <- data.frame(round(medmoncv[2] * 100),digits=2)
  }
  else {
    meanmoncv <- aggregate(dfcvmonbyyrf$cvq, list(dfcvmonbyyrf$Month), 
                           mean, na.rm=TRUE)
    ma24.35 <- data.frame(round(meanmoncv[2] * 100),digits=2)
  }
  return(ma24.35)
}

#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the mean (or median - use preference option) of the coefficients of variation 
#' (standard deviation/mean) for each year. Compute the coefficent of variation for each year 
#' of daily flows. Compute the mean/median of the annual coefficients of variation (percent).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ma3 numeric value of MA3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma3(qfiletempf)
ma3 <- function(qfiletempf, pref = "mean") {
  sdbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                      FUN = sd, na.rm=TRUE)
  colnames(sdbyyr) <- c("Year", "sdq")
  meanbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                        mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year", "meanq")
  dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                         meanbyyr$meanq)
  colnames(dfcvbyyr) <- c("Year", "sdq", "meanq")
  cvbyyr <- dfcvbyyr$sdq/ma1(qfiletempf)
  dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr)
  colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", 
                           "cvq")
  if (pref == "median") {
    medcv <- median(dfcvbyyrf$cvq, na.rm=TRUE)
    ma3 <- round((medcv) * 100,digits=2)
  }
  else {
    meancv <- mean(dfcvbyyrf$cvq, na.rm=TRUE)
    ma3 <- round((meancv) * 100,digits=2)
  }
  return(ma3)
}

#' This function accepts a data frame that contains columns named "discharge" and "month_val" and calculates 
#' MA36; variability across monthly flows. Compute the minimum, maximum, and mean flows for each month 
#' in the entire flow record.  MA36 is the maximum monthly flow minus the minimum monthly flow divided by the 
#' median monthly flow (dimensionless-spatial). 
#' MA37; Variability across monthly flows.  Compute the first (25th 
#' percentile) and the third (75th percentile) quartiles (every month in the flow record).  MA37 is the third 
#' quartile minus the first quartile divided by the median of the monthly means (dimensionless-spatial). 
#' MA38; Variability across monthly flows.  Compute the 10th and 90th percentiles for the monthly means (every month in 
#' the flow record).  MA38 is the 90th percentile minus the 10th percentile divided by the median of the monthly 
#' means (dimensionless-spatial). 
#' MA39; Variability across monthly flows.  Compute the standard deviation for the 
#' monthly means.  MA39 is the standard deviation times 100 divided by the mean of the monthly means (percent-spatial). 
#' MA40; Skewness in the monthly flows.  MA40 is the mean of the monthly flow means minus the median of the monthly 
#' means divided by the median of the monthly means (dimensionless-spatial).
#' 
#' @param qfiletemp data frame containing a "discharge" column containing daily flow values
#' @return ma36.40 list containing MA36-MA40 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma36.40(qfiletempf)
ma36.40 <- function(qfiletemp) {
  meanbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val,qfiletemp$year_val), FUN = mean, na.rm=TRUE)
  colnames(meanbymon) <- c("Month","Year","meanmo")
  maxbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val,qfiletemp$year_val), FUN = max, na.rm=TRUE)
  colnames(maxbymon) <- c("Month","Year","maxmo")
  minbymon <- aggregate(qfiletemp$discharge, list(qfiletemp$month_val,qfiletemp$year_val), FUN = min, na.rm=TRUE)
  colnames(minbymon) <- c("Month","Year","minmo")
  sortmeanbymon <- sort(meanbymon$meanmo)
  perc_10 <- quantile(sortmeanbymon,.1,type=6)
  perc_25 <- quantile(sortmeanbymon,.25,type=6)
  perc_75 <- quantile(sortmeanbymon,.75,type=6)
  perc_90 <- quantile(sortmeanbymon,.9,type=6)
  ma36 <- round((max(meanbymon$meanmo)-min(meanbymon$meanmo))/median(meanbymon$meanmo),digits=2)
  ma37 <- round((perc_75-perc_25)/median(meanbymon$meanmo),digits=2)
  ma38 <- round((perc_90-perc_10)/median(meanbymon$meanmo),digits=2)
  ma39 <- round((sd(meanbymon$meanmo)*100)/mean(meanbymon$meanmo),digits=2)
  ma40 <- round((mean(meanbymon$meanmo)-median(meanbymon$meanmo))/median(meanbymon$meanmo),digits=2)
  ma36.40 <- list(ma36,ma37,ma38,ma39,ma40)
  return(ma36.40)
}

#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates MA4; Standard deviation of the percentiles of the entire flow record divided by the mean of percentiles.  
#' Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 75th,
#' 80th, 85th, 90th, and  95th percentiles for the entire flow record.  Percentiles are computed by 
#' interpolating between the ordered (ascending) flow values.  Compute the standard deviation 
#' and mean for the percentile values.  Divide the standard deviation by the mean to get MA4.  (percent-spatial)
#' MA5; 
#' The skewness of the entire flow record is computed as the mean for the entire flow record (MA1) divided by the median 
#' (MA2) for the entire flow record (dimensionless-spatial). MA6; Range in daily flows is the ratio of the 10-percent 
#' to 90-percent exceedence values for the entire flow record. Compute the 5-percent to 95-percent exceedence values 
#' for the entire flow record. Exceedence is computed by interpolating between the ordered (descending) flow values. 
#' Divide the 10-percent exceedence value by the 90-percent value (dimensionless-spatial). MA7; Range in daily flows is 
#' computed like MA6 except using the 20-percent and 80-percent exceedence values. Divide the 20-percent exceedence 
#' value by the 80-percent value (dimensionless-spatial).
#' MA8; Range in daily flows is computed like MA6 except using the 25-percent and 75-percent exceedence values. 
#' Divide the 25-percent exceedence value by the 75-percent exceedence value (dimensionless-spatial). MA9; Spread in 
#' daily flows is the ratio of the difference between the 90th and 10th percentile of the flow data to median of the 
#' entire flow record.  Compute the 5th, 10th, 15th, 20th, 25th, 30th, 35th, 40th, 45th, 50th, 55th, 60th, 65th, 70th, 
#' 75th, 80th, 85th, 90th, and 95th percentiles for the entire flow record.  Percentiles are computed by interpolating 
#' between the ordered (ascending) flow values.  Compute MA9 as (90th-10th) /MA2 (dimensionless-spatial). MA10; Spread 
#' in daily flows is computed like MA9 except using the 20th and 80th percentiles (dimensionless-spatial). MA11; 
#' Spread in daily flows is computed like MA9 except using the 25th and 75th percentiles (dimensionless-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ma4.11 list of the MA4-MA11 statistics for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ma4.11(qfiletempf)
ma4.11<-function(x) {
  isolateq <- x$discharge
  sortq <- sort(isolateq)
  percentiles<-vector(length=19)
  percentiles[1] <- quantile(sortq,.95,type=6)  
  percentiles[2] <- quantile(sortq,.90,type=6)
  percentiles[3] <- quantile(sortq,.85,type=6)
  percentiles[4] <- quantile(sortq,.80,type=6)
  percentiles[5] <- quantile(sortq,.75,type=6)
  percentiles[6] <- quantile(sortq,.7,type=6)
  percentiles[7] <- quantile(sortq,.65,type=6)
  percentiles[8] <- quantile(sortq,.6,type=6)
  percentiles[9] <- quantile(sortq,.55,type=6)
  percentiles[10] <- quantile(sortq,.5,type=6)
  percentiles[11] <- quantile(sortq,.45,type=6)
  percentiles[12] <- quantile(sortq,.4,type=6)
  percentiles[13] <- quantile(sortq,.35,type=6)
  percentiles[14] <- quantile(sortq,.3,type=6)
  percentiles[15] <- quantile(sortq,.25,type=6)
  percentiles[16] <- quantile(sortq,.2,type=6)
  percentiles[17] <- quantile(sortq,.15,type=6)
  percentiles[18] <- quantile(sortq,.1,type=6)
  percentiles[19] <- quantile(sortq,.05,type=6)
  mean <- mean(percentiles,na.rm=TRUE)
  sdev <- sd(percentiles, na.rm=TRUE)
  ma4 <- round((sdev/mean)*100,digits=2)
  ma5 <- round(ma1(x)/ma2(x),digits=2)
  ma6 <- round(percentiles[2]/percentiles[18],digits=2)
  ma7 <- round(percentiles[4]/percentiles[16],digits=2)
  ma8 <- round(percentiles[5]/percentiles[15],digits=2)
  ma9 <- round((percentiles[2]-percentiles[18])/ma2(x),digits=2)
  ma10 <- round((percentiles[4]-percentiles[16])/(ma2(x)),digits=2)
  ma11 <- round((percentiles[5]-percentiles[15])/(ma2(x)),digits=2)
  ma4.11 <- list(ma4,ma5,ma6,ma7,ma8,ma9,ma10,ma11)
  return(ma4.11)
}

#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the mean flow by year for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return meanflowbyyear numeric value of the mean flow by year for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' meanflowbyyear(qfiletempf)
meanflowbyyear <- function(qfiletempf) {
  meanflowbyyear<-aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                            mean, na.rm=TRUE)
  colnames(meanflowbyyear) <- c("Year", "meanq")
  return(meanflowbyyear)
}


#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the median flow by year for the entire record
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return medflowbyyear numeric value of the median flow by year for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' medflowbyyear(qfiletempf)
medflowbyyear <- function(qfiletempf) {
  medflowbyyear<-aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                           median, na.rm=TRUE)
  colnames(medflowbyyear) <- c("Year","medq")
  return(medflowbyyear)
}

#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates the mean (or median-Use Preference option) maximum flows for each month across all years. Compute 
#' the maximum daily flow for each month over the entire flow record. For example, MH1 is the mean of the maximums 
#' of all January flow values over the entire record (cubic feet per second-temporal).
#' 
#' @param qfiletemp data frame containing a "discharge" column containing daily flow values
#' @return mh1.12 data frame containing the mean or median maximum flows for each month
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh1.12(qfiletempf)
mh1.12 <- function(qfiletemp) {
  maxbymonyr <- aggregate(qfiletemp$discharge, list(qfiletemp$year_val, qfiletemp$month_val), FUN = max, na.rm=TRUE)
  colnames(maxbymonyr) <- c("Year","Month","maxmo")
  meanmaxbymon <- aggregate(maxbymonyr$maxmo, list(maxbymonyr$Month), FUN = mean, na.rm=TRUE)
  colnames(meanmaxbymon) <- c("Month","meanmax")
  mh1 <- round(meanmaxbymon[1,2],digits=2)
  mh2 <- round(meanmaxbymon[2,2],digits=2)
  mh3 <- round(meanmaxbymon[3,2],digits=2)
  mh4 <- round(meanmaxbymon[4,2],digits=2)
  mh5 <- round(meanmaxbymon[5,2],digits=2)
  mh6 <- round(meanmaxbymon[6,2],digits=2)
  mh7 <- round(meanmaxbymon[7,2],digits=2)
  mh8 <- round(meanmaxbymon[8,2],digits=2)
  mh9 <- round(meanmaxbymon[9,2],digits=2)
  mh10 <- round(meanmaxbymon[10,2],digits=2)
  mh11 <- round(meanmaxbymon[11,2],digits=2)
  mh12 <- round(meanmaxbymon[12,2],digits=2)
  mh1.12 <- list(mh1,mh2,mh3,mh4,mh5,mh6,mh7,mh8,mh9,mh10,mh11,mh12)
  return(mh1.12)
}

#' This function accepts a data frame that contains columns named "discharge", "year_val" and "month_val" and 
#' calculates MH13, variability (coefficient of variation) across maximum monthly flow values. Compute the mean 
#' and standard deviation for the maximum monthly flows over the entire flow record. MH13 is the standard deviation 
#' times 100 divided by the mean maximum monthly flow for all years (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return mh13 numeric value of MH13 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' mh13(qfiletempf)
mh13 <- function(qfiletempf) {
  maxmonbyyr <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val, 
                                                     qfiletempf$month_val), FUN = max, na.rm=TRUE)
  colnames(maxmonbyyr) <- c("Year", "Month", "maxmo")
  sdmaxmonflows <- sd(maxmonbyyr$maxmo)
  meanmaxmonflows <- mean(maxmonbyyr$maxmo)
  mh13 <- round((sdmaxmonflows * 100)/meanmaxmonflows,digits=2)
  return(mh13)
}

#' This function accepts a data frame that contains columns named "discharge" and "year_val" and calculates 
#' ML14; Mean of annual minimum annual flows.  ML14 is the mean of the ratios of minimum annual flows to the median 
#' flow for each year (dimensionless-temporal). 
#' ML15; Low flow index.  ML15 is the mean (or median-Use Preference option) of the ratios of minimum annual flows to 
#' the mean flow for each year (dimensionless-temporal). 
#' ML16; Median of annual minimum flows.  ML16 is the median of the ratios of minimum annual flows to the median 
#' flow for each year (dimensionless-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ml14.16 list of ml14-ml16 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml14.16(qfiletempf)
ml14.16 <- function(qfiletempf) {
  minbyyear <- aggregate(qfiletempf$discharge, 
                         list(qfiletempf$wy_val), min, na.rm=TRUE)
  medflow <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       median, na.rm=TRUE)
  meanflow <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), mean, na.rm=TRUE)
  computeml14 <- merge(merge(minbyyear, medflow, by.x="Group.1", by.y="Group.1"),meanflow, by.x="Group.1", by.z="Group.1")
  colnames(computeml14) <- c("year", "minbyyr", "medbyyr", "meanbyyr")
  dfml14 <- computeml14$minbyyr/computeml14$medbyyr
  dfml15 <- computeml14$minbyyr/computeml14$meanbyyr
  ml14 <- round(mean(dfml14),digits=2)
  ml16 <- round(median(dfml14),digits=2)
  ml15 <- round(mean(dfml15),digits=2)
  ml14.16 <- list(ml14,ml15,ml16)
  return(ml14.16)
}

#### base flow index function

bfi <- function(qfiletempf) {
  noyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), 
                       FUN = median, na.rm=TRUE)
  colnames(noyears) <- c("Year", "momax")
  noyrs <- length(noyears$Year)
  compbfi <- rep(0,noyrs)
  for (i in 1:noyrs) {
    subsetyr <- subset(qfiletempf, as.numeric(qfiletempf$wy_val) == noyears$Year[i])
    day7mean <- rollmean(subsetyr$discharge, 6, align = "right", partial = TRUE)
    min7daybyyear <- min(day7mean)
    meanflow <- mean(subsetyr$discharge)
    compbfi[i] <- min7daybyyear/meanflow 
  }
  bfi <- compbfi
  return(bfi)
}
#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the base flow. Compute the mean annual flows. Compute the minimum of a 7-day moving average flow 
#' for each year and divide them by the mean annual flow for that year. ML17 is the mean (or median-Use 
#' Preference option) of those ratios (dimensionless-temporal). 
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ml17 numeric value of ML17 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml17(qfiletempf)
ml17 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  bfibyyear <- bfi(qfiletempf)
  if (pref == "median") {
    ml17 <- round(median(bfibyyear),digits=2)
  }
  else {
    ml17 <- round(mean(bfibyyear),digits=2)
  }
  return(ml17)
}

#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates the variability in base flow index 1. Compute the standard deviation for the ratios of minimum 7-day 
#' moving average flows to mean annual flows for each year.  ML18 is the standard deviation times 100 divided by 
#' the mean of the ratios. (percent-spatial)
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ml18 numeric value of ML18 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml18(qfiletempf)
ml18 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  bfibyyear <- bfi(qfiletempf)  
  sdbfi <- sd(bfibyyear)
  meanbfi <- mean(bfibyyear)
  ml18 <- round((sdbfi/meanbfi)*100,digits=2)
  return(ml18)
}

#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates the base flow index ML19. Compute the ratios of the minimum annual flow to mean annual flow for 
#' each year. ML19 is the mean (or median-Use Preference option) of these ratios times 100 (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ml19 numeric value of ML19 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml19(qfiletempf)
ml19 <- function(qfiletempf, pref = "mean") {
  minbyyr <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),FUN=min,na.rm=TRUE)
  colnames(minbyyr) <- c("Year","yrmin")
  meanbyyr <- aggregate(qfiletempf$discharge,list(qfiletempf$wy_val),FUN=mean,na.rm=TRUE)
  colnames(meanbyyr) <- c("Year","yrmean")
  ratiominmean <- (minbyyr$yrmin/meanbyyr$yrmean)
  if (pref == "median") {
    ml19 <- round(median(ratiominmean)*100,digits=2)
  }
  else {
    ml19 <- round(mean(ratiominmean)*100,digits=2)
  }
  return(ml19)
}

#' This function accepts a data frame that contains a column named "discharge" and 
#' calculates base flow index ML20. Divide the daily flow record into 5-day blocks. Find the minimum flow for 
#' each block. Assign the minimum flow as a base flow for that block if 90 percent of that minimum flow is less 
#' than the minimum flows for the blocks on either side. Otherwise, set it to zero. Fill in the zero values 
#' using linear interpolation. Compute the total flow for the entire record and the total base flow for the 
#' entire record. ML20 is the ratio of total base flow to total flow (dimensionless-spatial).
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml20 numeric value of ML20 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml20(qfiletempf)
ml20 <- function(x) {
  x <- x[order(x$date),]
  sub_flow <- subset(x,na.rm=TRUE)
  numdays <- nrow(sub_flow)
  numsets <- floor(numdays/5)
  sets <- c(1:numsets)
  fiveday <- rep(999999999,numsets)
  for (i in 1:numsets) {
    for (j in (5*i-4):(5*i)) {
      fiveday[i] <- ifelse(fiveday[i]<sub_flow$discharge[j],fiveday[i],sub_flow$discharge[j])
    }
  }
  fiveday_lead <- c(9999999999,fiveday)
  fiveday_lag <- c(fiveday,999999999999)
  fiveday_lead <- fiveday_lead[1:numsets]
  fiveday_lag <- fiveday_lag[2:(numsets+1)]
  zeros <- rep(0,numsets)
  seq_nums <- seq(1,numsets)
  fiveday <- ifelse((.9*fiveday)<fiveday_lag & (.9*fiveday)<fiveday_lead,fiveday,zeros)
  fiveday_seq <- as.data.frame(cbind(seq_nums,fiveday))
  fiveday_seq$fiveday <- ifelse(fiveday_seq$fiveday==0,'',fiveday_seq$fiveday)
  fiveday_interp <- approx(fiveday_seq$seq_nums[fiveday_seq$fiveday!=''],fiveday_seq$fiveday[fiveday_seq$fiveday!=''],xout=fiveday_seq$seq_nums[fiveday_seq$fiveday==''],method="linear",rule=2)
  fiveday_merge <- merge(fiveday_seq,fiveday_interp,by.x="seq_nums",by.y="x",all=TRUE)
  fiveday_merge$y <- ifelse(is.na(fiveday_merge$y),as.numeric(fiveday_merge$fiveday),fiveday_merge$y)
  total_bf <- sum(fiveday_merge$y*5)+((numdays-(numsets*5))*fiveday_merge$y[numsets])
  total_flow <- sum(sub_flow$discharge)
  ml20v <- round(total_bf/total_flow,digits=2)
  return(ml20v)
} 

#' This function accepts a data frame that contains columns named "discharge" and "year_val" and 
#' calculates ML21, variability across annual minimum flows. Compute the mean and standard deviation for the 
#' annual minimum flows. ML21 is the standard deviation times 100 divided by the mean (percent-spatial). 
#' 
#' @param x data frame containing a "discharge" column containing daily flow values
#' @return ml21 numeric value of ML21 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ml21(qfiletempf)
ml21 <- function(x) {
  minbyyr <- aggregate(x$discharge,list(x$wy_val),FUN=min,na.rm=TRUE)
  colnames(minbyyr) <- c("Year","yrmin")
  ml21 <- round((sd(minbyyr$yrmin)*100)/mean(minbyyr$yrmin),digits=2)
  return(ml21)
}

#' This function accepts a data frame containing daily data and returns a data frame containing mean monthly data
#' 
#' @param qfiletempf data frame containing daily value data 
#' @return meanmonts data frame containing mean monthly values
#' @export
#' @examples
#' qfiletempf<-sampleData
#' monthlyMeanTs(qfiletempf)
monthlyMeanTs <- function(qfiletempf) {
  meanmonts <- aggregate(qfiletempf$discharge, list(qfiletempf$year_val,qfiletempf$month_val), FUN = mean, na.rm=TRUE)
  colnames(meanmonts) <- c("Year","Month","Mean_disch")
  meanmonts$datenum <- as.numeric(meanmonts$Year)+as.numeric(meanmonts$Month)*(1/12)
  meanmonts<-meanmonts[order(meanmonts$datenum),]
  return(meanmonts)
}

#' This function accepts a data frame containing daily data and a site number and returns a graph of mean monthly data
#' 
#' @param x data frame containing mean monthly values
#' @param station station number for plot title 
#' @export
#' @examples
#' qfiletempf<-sampleData
#' meanmonts<-monthlyMeanTs(qfiletempf)
#' plotMonthlyMean(meanmonts,'02178400')
plotMonthlyMean <- function(x,station){
  plot(x$datenum,x$Mean_disch,xlab="",ylab="Discharge(cfs)",col="blue",type="o",main=paste("Monthly Average Flow at station ",station,sep=""))
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA1; Rise rate. Compute the change in flow for days in which the change is positive for the entire flow record. 
#' RA1 is the mean (or median-Use Preference option) of these values (cubic feet per second/day-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ra1 numeric containing RA1 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra1(qfiletempf)
ra1 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  if (pref == "median") {
    ra1 <- round(median(findrisevalues),digits=2)
  }
  else {
    ra1 <- round(mean(findrisevalues),digits=2)
  }
  return(ra1)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA2; Variability in rise rate. Compute the standard deviation for the positive flow changes. RA2 is 100 times 
#' the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra2 numeric containing RA2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra2(qfiletempf)
ra2 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meanra2 <- ra1(qfiletempf, pref = "mean")
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  sddra2 <- sd(findrisevalues)
  ra2 <- round((sddra2 * 100)/meanra2,digits=2)
  return(ra2)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA3; Fall rate. Compute the change in flow for days in which the change is negative for the entire flow record. 
#' RA3 is the mean (or median-Use Preference option) of these values (cubic feet per second/day-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param pref string containing a "mean" or "median" preference
#' @return ra3 numeric containing RA3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra3(qfiletempf)
ra3 <- function(qfiletempf, pref = "mean") {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findfallvalueneg <- subset(diffbtdays, diffbtdays < 
                               0)
  findfallvalues <- abs(findfallvalueneg)
  if (pref == "median") {
    ra3 <- round(median(findfallvalues),digits=2)
  }
  else {
    ra3 <- round(mean(findfallvalues),digits=2)
  }
  return(ra3)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA4; Variability in fall rate. Compute the standard deviation for the negative flow changes. RA4 is 100 times 
#' the standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra4 numeric containing RA4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra4(qfiletempf)
ra4 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  meanra4 <- ra3(qfiletempf, pref = "mean")
  diffbtdays <- diff(qfiletempf$discharge, lag = 1, 
                     differences = 1)
  findfallvalueneg <- subset(diffbtdays, diffbtdays < 
                               0)
  findfallvalues <- abs(findfallvalueneg)
  sddra4 <- sd(findfallvalues)
  ra4 <- round((sddra4 * 100)/meanra4,digits=2)
  return(ra4)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA5; Number of day rises. Compute the number of days in which the flow is greater than the previous day. RA5 
#' is the number of positive gain days divided by the total number of days in the flow record (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra5 numeric containing RA5 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra5(qfiletempf)
ra5 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  sub_length <- nrow(qfiletempf)-1
  counter <- 0
  for (i in 1:sub_length) {
    if (qfiletempf$discharge[i+1] - qfiletempf$discharge[i] > 0) { 
      counter <- counter+1
    }
  }
  ra5 <- round(counter/(sub_length+1),digits=2)
  return(ra5)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA6; Change of flow. Compute the loge of the flows for the entire flow record. Compute the change in log of flow 
#' for days in which the change is positive for the entire flow record. RA6 is the median of these values (cubic feet 
#' per second-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra6 numeric containing RA6 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra6(qfiletempf)
ra6 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  disch_log <- ifelse(qfiletempf$discharge>0,log(qfiletempf$discharge),log(.01))
  diffbtdays <- diff(disch_log, lag = 1, 
                     differences = 1)
  findrisevalues <- subset(diffbtdays, diffbtdays > 
                             0)
  ra6 <- round(median(abs(findrisevalues)),digits=2)
  return(ra6)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA7; Change of flow. Compute the loge of the flows for the entire flow record. Compute the change in log of 
#' flow for days in which the change is negative for the entire flow record. RA7 is the median of these log 
#' values (cubic feet per second/day-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra7 numeric containing RA7 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra7(qfiletempf)
ra7 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  disch_log <- ifelse(qfiletempf$discharge>0,log(qfiletempf$discharge),log(.01))
  diffbtdays <- diff(disch_log, lag = 1, 
                     differences = 1)
  findfallvalues <- subset(diffbtdays, diffbtdays < 
                             0)
  ra7 <- round(median(abs(findfallvalues)),digits=2)
  return(ra7)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' RA8; Number of reversals. Compute the number of days in each year when the change in flow from one day to the 
#' next changes direction. RA8 is the average (or median - Use Preference option) of the yearly values (days-temporal).
#' RA9; Variability in reversals. Compute the standard deviation for the yearly reversal values. RA9 is 100 times the 
#' standard deviation divided by the mean (percent-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ra8.9 list containing RA8 and RA9 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ra8.9(qfiletempf)
ra8.9 <- function(qfiletempf) {
  qfiletempf <- qfiletempf[order(qfiletempf$date),]
  noyears <- data.frame(unique(qfiletempf$wy_val),stringsAsFactors=FALSE)
  colnames(noyears) <- c("wy_val")
  noyrs <- nrow(noyears)
  for (j in 1:noyrs) {
    subq <- subset(qfiletempf,qfiletempf$wy_val==noyears$wy_val[j])
    counter <- 0
    sub_length <- nrow(subq)-1
    for (i in 1:sub_length) {
      temp <- subq$discharge[i+1] - subq$discharge[i]
      if (i==1) {
        flag <- 0
        if (temp>0) flag<-1
        if (temp<0) flag<-2
      }
      if (i>1 && temp>0) {
        if (flag==2) counter <- counter+1
        flag <- 1
      }
      if (i>1 && temp<0) {
        if (flag==1) counter <- counter+1
        flag <- 2
      }
    }
    noyears$cnt[j] <- counter
  }
  ra8 <- round(mean(noyears$cnt),digits=2)
  sd_diff <- sd(noyears$cnt)
  ra9 <- round((sd_diff*100)/ra8,digits=2)
  ra8.9 <- list(ra8=ra8,ra9=ra9)
  return(ra8.9)
}

#' This function accepts two data frames containing daily data series and returns the root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmse root mean square error value between the two timeseries
#' @export
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rmse(timeseries1,timeseries2)
rmse<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-(timeseries1-timeseries2)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmse<-sqrt(sumsqerr/n)
  } else {rmse<-NA}
  return(rmse)
}

#' This function accepts two data frames containing daily data series and returns the normalized root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmsne normalized root mean square error value between the two timeseries
#' @export
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rmsne(timeseries1,timeseries2)
rmsne<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-((timeseries1-timeseries2)/timeseries1)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmsne<-sqrt(sumsqerr/n)
  } else {rmsne<-NA}
  return(rmsne)
}

#' This function accepts observed and modeled daily data series and returns the root mean square error/standard deviation
#' 
#' @param timeseries1 data frame containing value data for the observed timeseries
#' @param timeseries2 data frame containing value data for the modeled timeseries
#' @return rsr root mean square error/standard deviation for the two timeseries
#' @export
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rsr(timeseries1,timeseries2)
rsr<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-(timeseries1-timeseries2)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmse<-sqrt(sumsqerr/n)
    sdev <- sd(timeseries1,na.rm=TRUE)
    rsr <- rmse/sdev
  } else {rsr<-NA}
  return(rsr)
}

#' This function accepts a data frame containing daily data and returns the standard deviation
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return sdev standard deviation for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' sdev(qfiletempf$discharge)
sdev <- function(x) {
  sdev <- sd(x,na.rm=TRUE)
  return(sdev)
}

#' This function accepts a data frame containing daily streamflow data, then computes seasonality 
#' variables by first standardizing flows, the fitting relation 
#' A*cos(2*pi*t) + B*sin(2*pi*t)1) Get decimal yearand returns the amplitude and phase
#' 
#' @param timeseries data frame containing daily discharge data
#' @return seasonalityv vector of seasonal factors (amplitude and phase)
#' @export
seasonality <- function(timeseries) {
  rawdates<-timeseries$date
  dateaschar<-as.character(rawdates)
  jday<-strptime(timeseries$date, "%Y-%m-%d")$yday+1
  decimal_year<-as.numeric(timeseries$year_val)+(jday/365.25)
  #2) Standardize flows
  std_flows<-scale(timeseries$discharge, center = TRUE, scale = TRUE)
  #3) Use linear model to fit 
  seasonfit<-lm(std_flows~cos(2*pi*decimal_year)+sin(2*pi*decimal_year))
  seasonA<-as.vector(seasonfit$coefficients[2])
  seasonB<-as.vector(seasonfit$coefficients[3]) 
  #Now compute the amplitude and phase of the seasonal signal
  amplitude<-round(sqrt((seasonA^2)+(seasonB^2)),digits=2)
  phase<-round(atan((-seasonB)/seasonA),digits=2)
  seasonalityv <- cbind(amplitude,phase)
  return(seasonalityv)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' TA1; Constancy. Constancy is computed via the formulation of Colwell (see example in Colwell, 1974). A matrix of values 
#' is compiled where the rows are 365 (no February 29th) days of the year and the columns are 11 flow categories. The 
#' cell values are the number of times that a flow falls into a category on each day. The categories are: 
#' log(flow) < 0.1 ? log(mean flow), 
#' 0.1 ? log(mean flow) ??? log(flow) < 0.25 ? log(mean flow)
#' 0.25 ? log(mean flow) ??? log(flow) < 0.5 ? log(mean flow)
#' 0.5 ? log(mean flow) ??? log(flow) < 0.75 ? log(mean flow)
#' 0.75 ? log(mean flow) ??? log(flow) < 1.0 ? log(mean flow)
#' 1.0 ? log(mean flow) ??? log(flow) < 1.25 ? log(mean flow)
#' 1.25 ? log(mean flow) ???log(flow) < 1.5 ? log(mean flow)
#' 1.5 ? log(mean flow) ??? log(flow) < 1.75 ? log(mean flow)
#' 1.75 ? log(mean flow) ??? log(flow) < 2.0 ? log(mean flow)
#' 2.0 ?log(mean flow) ??? log(flow) < 2.25 ? log(mean flow)
#' log(flow) ??? 2.25 ? log(mean flow)
#' The row totals, column totals, and grand total are computed. Using the equations for Shannon information theory 
#' parameters, constancy is computed as:
#' 1- (uncertainty with respect to state)/log (number of state) 
#' TA2; Predictability. Predictability is computed from the same matrix as constancy (see example in Colwell, 1974). It 
#' is computed as: 
#' 1- (uncertainty with respect to interaction of time and state - uncertainty with respect to time)/log (number of state) 
#' where uncertainty with respect to state = sum((YI_sub/Z)*log10(YI_sub/Z))
#' where YI_sub = the non-zero sums of the 11 categories and Z = the sum total of the Colwell matrix 
#' and where uncertainty with respect to time = sum((XJ_sub/Z)*log10(XJ_sub/Z)) 
#' where XJ_sub = the non-zero sums of the 365 days
#' and where uncertainty with respect to interaction of time and state = sum((colwell_sub/z)*log10(colwell_sub/Z))
#' where colwell_sub = the non-zero sums of the entire matrix 
#' and where number of state = number of categories = 11
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return ta1.2 list containing TA1 and TA2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ta1.2(qfiletempf)
ta1.2 <- function(qfiletempf) {
  colwell_mat <- matrix(-99999,365,11)
  mean_flow <- ma1(qfiletempf)
  for (i in 1:365) {
    m <- ifelse(i<93,i+273,i-92)
    qfile_sub <- qfiletempf[qfiletempf$jul_val==m,]
    colwell_mat[i,1] <- nrow(qfile_sub[log10(qfile_sub$discharge)<(.1*log10(mean_flow)),])
    colwell_mat[i,2] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.1*log10(mean_flow)) & log10(qfile_sub$discharge)<(.25*log10(mean_flow)),])
    colwell_mat[i,3] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.25*log10(mean_flow)) & log10(qfile_sub$discharge)<(.5*log10(mean_flow)),])
    colwell_mat[i,4] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.5*log10(mean_flow)) & log10(qfile_sub$discharge)<(.75*log10(mean_flow)),])
    colwell_mat[i,5] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(.75*log10(mean_flow)) & log10(qfile_sub$discharge)<(1*log10(mean_flow)),])
    colwell_mat[i,6] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1*log10(mean_flow)) & log10(qfile_sub$discharge)<(1.25*log10(mean_flow)),])
    colwell_mat[i,7] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1.25*log10(mean_flow)) & log10(qfile_sub$discharge)<(1.5*log10(mean_flow)),])
    colwell_mat[i,8] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1.5*log10(mean_flow)) & log10(qfile_sub$discharge)<(1.75*log10(mean_flow)),])
    colwell_mat[i,9] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(1.75*log10(mean_flow)) & log10(qfile_sub$discharge)<(2*log10(mean_flow)),])
    colwell_mat[i,10] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(2*log10(mean_flow)) & log10(qfile_sub$discharge)<(2.25*log10(mean_flow)),])
    colwell_mat[i,11] <- nrow(qfile_sub[log10(qfile_sub$discharge)>=(2.25*log10(mean_flow)),])
  }
  XJ <- rowSums(colwell_mat)
  YI <- colSums(colwell_mat)
  Z <- sum(colwell_mat)
  XJ_sub <- XJ[XJ>0]
  HX <- -sum((XJ_sub/Z)*log10(XJ_sub/Z))
  YI_sub <- YI[YI>0]
  HY <- -sum((YI_sub/Z)*log10(YI_sub/Z))
  colwell_sub <- colwell_mat[colwell_mat>0]
  HXY <- -sum((colwell_sub/Z)*log10(colwell_sub/Z))
  HxY <- HXY - HX
  ta1 <- round(1-(HY/log10(11)),digits=2)
  ta2 <- round(100*(1-(HxY/log10(11))),digits=2)
  ta1.2<-list(ta1=ta1,ta2=ta2)
  return(ta1.2)
}


#' This function accepts a data frame that contains a column named "discharge" and threshold value obtained 
#' using the peakdata and peakthresh functions and calculates 
#' TA3; Seasonal predictability of flooding. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, 
#' and so forth). Count the number of flood days (flow events with flows > 1.67-year flood) in each period over 
#' the entire flow record. TA3 is the maximum number of flood days in any one period divided by the total number 
#' of flood days (dimensionless-temporal).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return ta3 numeric containing TA3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' ta3(qfiletempf, 1158)
ta3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  nomonyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val,qfiletempf$month_val), 
                          FUN = median, na.rm=TRUE)
  colnames(nomonyears) <- c("Year","month", "momax")
  nomonyrs <- nrow(nomonyears)
  dur <- data.frame(Year = rep(0,nrow(nomonyears)), month = rep(0,nrow(nomonyears)), dur = rep(1,nrow(nomonyears)))
  for (i in 1:nomonyrs) {
    monyears <- nomonyears[i,]
    colnames(monyears) <- c("wy_val","month_val","momax")
    subsetyr <- merge(qfiletempf,monyears,by = c("wy_val","month_val"))
    flag <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]>lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        dur$Year[i] <- subsetyr$wy_val[j]
        dur$month[i] <- subsetyr$month_val[j]
        dur$dur[i] <- nevents
      } else {flag <- 0}
    }
  }
  dur$season <- ifelse(as.numeric(dur$month)==10,10,ifelse(as.numeric(dur$month)==11,10,ifelse(as.numeric(dur$month)==12,12,ifelse(as.numeric(dur$month)==1,12,ifelse(as.numeric(dur$month)==2,2,ifelse(as.numeric(dur$month)==3,2,ifelse(as.numeric(dur$month)==4,4,ifelse(as.numeric(dur$month)==5,4,ifelse(as.numeric(dur$month)==6,6,ifelse(as.numeric(dur$month)==7,6,ifelse(as.numeric(dur$month)==8,8,ifelse(as.numeric(dur$month)==9,8,99))))))))))))
  dur <- dur[!dur$season==99,]
  num_season <- aggregate(dur$dur, list(dur$season), sum)
  ta3 <- round(max(num_season$x)/sum(num_season$x),digits=2)
  return(ta3)
}

#' This function accepts a data frame containing daily data and returns the skewness
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return skew skewness for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' skew(qfiletempf)
skew <- function(x) {
  x1 <- mean(x$discharge,na.rm=TRUE)
  x2 <- median(x$discharge,na.rm=TRUE)
  x3 <- sd(x$discharge,na.rm=TRUE)
  skew <- (x1-x2)/x3
  return(skew)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' TH1; Julian date of annual maximum. Determine the Julian date that the maximum flow occurs for each year. 
#' Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components 
#' for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by 
#' x-mean. Transform the resultant angle back to Julian date (Julian day-spatial). and 
#' TH2; Variability in Julian date of annual maxima. Compute the coefficient of variation for the mean x and y 
#' components and convert to a date (Julian days-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return th1.2 list containing TH1 and TH2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' th1.2(qfiletempf)
th1.2 <- function(qfiletempf) {
  max1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), max, na.rm=TRUE)
  colnames(max1daybyyear) <- c("wy_val","discharge")
  qfiletempf$wy_day_val <- ifelse(qfiletempf$jul_val>=274,qfiletempf$jul_val-273,qfiletempf$jul_val+92)
  maxjulbyyear <- aggregate(qfiletempf$wy_day_val, list(qfiletempf$wy_val,qfiletempf$discharge),min,na.rm=TRUE)
  colnames(maxjulbyyear) <- c("wy_val","discharge","jul_val")
  maxjulday <- merge(maxjulbyyear,max1daybyyear,by = c("wy_val","discharge"))
  maxjulday$jul_val <- ifelse(maxjulday$jul_val<=92,maxjulday$jul_val+274,maxjulday$jul_val-92)
  maxjulday$np <- cos(maxjulday$jul_val*2*pi/365.25)
  maxjulday$mdata <- sin(maxjulday$jul_val*2*pi/365.25)
  xbar <- mean(maxjulday$np)
  ybar <- mean(maxjulday$mdata)
  if (xbar>0) {
    th1_temp <- atan(ybar/xbar)*180/pi
  } else if (xbar<0) {
    th1_temp <- (atan(ybar/xbar)*180/pi)+180
  } else if (xbar==0 && ybar>0) {
    th1_temp <- 90
  } else if (xbar==0 && ybar<0) {
    th1_temp <- 270
  }
  th1_temp <- ifelse(th1_temp<0,th1_temp+360,th1_temp)
  th1 <- round(th1_temp*365.25/360,digits=2)
  th2_a <- sqrt((xbar*xbar)+(ybar*ybar))
  th2_b <- sqrt(2*(1-th2_a))
  th2 <- round(th2_b*180/pi/360*365.25,digits=2)
  th1.2<-list(th1=th1,th2=th2)
  return(th1.2)
}

#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and getPeakThresh functions and calculates 
#' TH3; Seasonal predictability of nonflooding. Computed as the maximum proportion of a 365-day year that the 
#' flow is less than the 1.67-year flood threshold and also occurs in all years. Accumulate nonflood days that 
#' span all years. TH3 is maximum length of those flood-free periods divided by 365 (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh numeric containing 1.67-year flood threshold calculated by getPeakThresh
#' @return tl4 numeric containing TH3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' th3(qfiletempf, 1158)
th3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  qfiletempf$diff <- (qfiletempf$discharge-lfcrit)
  jul_day_sum <- aggregate(qfiletempf$diff, list(qfiletempf$jul_val), max)
  maxdur <- rep(0,nrow(jul_day_sum))
  flag <- 0
  for (i in 1:365) {
    if (jul_day_sum$x[i]<=0) {
      flag <- flag+1
      maxdur[i]<-flag 
    } else {
      maxdur[i] <- 0
      flag <- 0
    }
  }
  th3 <- round(max(maxdur)/365,digits=2)
  return(th3)
}

#' This function accepts a data frame that contains a column named "discharge" and calculates 
#' TL1; Julian date of annual minimum. Determine the Julian date that the minimum flow occurs for each water year. 
#' Transform the dates to relative values on a circular scale (radians or degrees). Compute the x and y components 
#' for each year and average them across all years. Compute the mean angle as the arc tangent of y-mean divided by 
#' x-mean. Transform the resultant angle back to Julian date (Julian day-spatial). and 
#' TL2; Variability in Julian date of annual minima. Compute the coefficient of variation for the mean x and y 
#' components and convert to a date (Julian day-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @return tl1.2 list containing TL1 and TL2 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' tl1.2(qfiletempf)
tl1.2 <- function(qfiletempf) {
  min1daybyyear <- aggregate(qfiletempf$discharge, 
                             list(qfiletempf$wy_val), min, na.rm=TRUE)
  colnames(min1daybyyear) <- c("wy_val","discharge")
  qfiletempf$wy_day_val <- ifelse(qfiletempf$jul_val>=274,qfiletempf$jul_val-273,qfiletempf$jul_val+92)
  minjulbyyear <- aggregate(qfiletempf$wy_day_val, list(qfiletempf$wy_val,qfiletempf$discharge),min,na.rm=TRUE)
  colnames(minjulbyyear) <- c("wy_val","discharge","jul_val")
  minjulday <- merge(minjulbyyear,min1daybyyear,by = c("wy_val","discharge"))
  minjulday$jul_val <- ifelse(minjulday$jul_val<=92,minjulday$jul_val+274,minjulday$jul_val-92)
  minjulday$np <- cos(minjulday$jul_val*2*pi/365.25)
  minjulday$mdata <- sin(minjulday$jul_val*2*pi/365.25)
  xbar <- mean(minjulday$np)
  ybar <- mean(minjulday$mdata)
  if (xbar>0) {
    tl1_temp <- atan(ybar/xbar)*180/pi
  } else if (xbar<0) {
    tl1_temp <- (atan(ybar/xbar)*180/pi)+180
  } else if (xbar==0 && ybar>0) {
    tl1_temp <- 90
  } else if (xbar==0 && ybar<0) {
    tl1_temp <- 270
  }
  tl1_temp <- ifelse(tl1_temp<0,tl1_temp+360,tl1_temp)
  tl1 <- round(tl1_temp*365.25/360,digits=2)
  tl2_a <- sqrt((xbar*xbar)+(ybar*ybar))
  tl2_b <- sqrt(2*(1-tl2_a))
  tl2 <- round(tl2_b*180/pi/360*365.25,digits=2)
  tl1.2<-list(tl1=tl1,tl2=tl2)
  return(tl1.2)
}

#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and getPeakThresh functions and calculates 
#' TL3; Seasonal predictability of low flow. Divide years up into 2-month periods (that is, Oct-Nov, Dec-Jan, and 
#' so forth). Count the number of low flow events (flow events with flows ??? 5 year flood threshold) in each period 
#' over the entire flow record. TL3 is the maximum number of low flow events in any one period divided by the total 
#' number of low flow events (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh value containing the 5-year recurrence value for the site
#' @return tl3 numeric containing TL3 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' tl3(qfiletempf, 1161.38)
tl3 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  nomonyears <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val,qfiletempf$month_val), 
                          FUN = median, na.rm=TRUE)
  colnames(nomonyears) <- c("Year","month", "momax")
  nomonyrs <- nrow(nomonyears)
  dur <- data.frame(Year = rep(0,nrow(nomonyears)), month = rep(0,nrow(nomonyears)), dur = rep(1,nrow(nomonyears)))
  for (i in 1:nomonyrs) {
    monyears <- nomonyears[i,]
    colnames(monyears) <- c("wy_val","month_val","momax")
    subsetyr <- merge(qfiletempf,monyears,by = c("wy_val","month_val"))
    flag <- 0
    nevents <- 0
    for (j in 1:nrow(subsetyr)) {
      if (subsetyr$discharge[j]<=lfcrit) {
        flag <- flag+1
        nevents <- ifelse(flag==1,nevents+1,nevents)
        dur$Year[i] <- subsetyr$wy_val[j]
        dur$month[i] <- subsetyr$month_val[j]
        dur$dur[i] <- nevents
      } else {flag <- 0}
    }
  }
  dur$season <- ifelse(as.numeric(dur$month)==10,10,ifelse(as.numeric(dur$month)==11,10,ifelse(as.numeric(dur$month)==12,12,ifelse(as.numeric(dur$month)==1,12,ifelse(as.numeric(dur$month)==2,2,ifelse(as.numeric(dur$month)==3,2,ifelse(as.numeric(dur$month)==4,4,ifelse(as.numeric(dur$month)==5,4,ifelse(as.numeric(dur$month)==6,6,ifelse(as.numeric(dur$month)==7,6,ifelse(as.numeric(dur$month)==8,8,ifelse(as.numeric(dur$month)==9,8,99))))))))))))
  dur <- dur[!dur$season==99,]
  num_season <- aggregate(dur$dur, list(dur$season), sum)
  tl3 <- round(max(num_season$x)/sum(num_season$x),digits=2)
  return(tl3)
}

#' This function accepts a data frame that contains a column named "discharge" and a threshold value obtained 
#' using the peakdata and getPeakThresh functions and calculates 
#' TL4; Seasonal predictability of non-low flow. Compute the number of days that flow is above the 5-year flood 
#' threshold as the ratio of number of days to 365 or 366 (leap year) for each year. TL4 is the maximum of the yearly 
#' ratios (dimensionless-spatial).
#' 
#' @param qfiletempf data frame containing a "discharge" column containing daily flow values
#' @param thresh value containing the 5-year recurrence value for the site
#' @return tl4 numeric containing TL4 for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' tl4(qfiletempf, 1161.38)
tl4 <- function(qfiletempf, thresh) {
  lfcrit <- thresh
  subset_crit <- subset(qfiletempf, qfiletempf$discharge>lfcrit)
  num_year <- aggregate(subset_crit$discharge, list(subset_crit$wy_val), function(x) sum(!is.na(x)))
  names(num_year) <- c('wy_val','num_days')
  num_year$ratio <- ifelse((as.numeric(num_year$wy_val)+1)/4==round((as.numeric(num_year$wy_val)+1)/4),num_year$num_days/366,num_year$num_days/365)
  tl4 <- round(max(num_year$ratio),digits=2)
  return(tl4)
}