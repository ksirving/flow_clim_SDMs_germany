### results exploration

## accumlative climate and hydrology

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro")
setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")

st <- list.files(pattern="stats")
length(st) ## 93
st
st <- st[-c(1,88)]
st[2]
# st <-st[-c(1)]
## creat dataframe
all_stats <- data.frame(matrix(nrow=4, ncol=12))
colnames(all_stats) <- c("sens_m", "sens_se", "TSS_m", "TSS_se", "Kappa_m", "Kappa_se", "sens_m", "sens_se", "spec_m", "spec_se", "species", "model")
head(all_stats)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))
s=1
## read 1st file
stats <- read.csv(paste(st[s]))
head(stats)
dim(stats)
length(st)
## loop to add all files together
s
for(s in 1:length(st)) {
  
  statsx <- read.csv(paste(st[s]))
  # head(statsx)
  # dim(statsx)
  model <- unique(statsx$model)
  stats <- rbind(stats, statsx)
  
}

head(stats)
dim(stats) ## 
stats <- stats[, -1]
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/uni_SDMs/95_2brt_run/stats")
setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/stats")
setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/stats")

save(stats, file="acc_hydro_all_stats.csv")

taxa_list <- unique(stats$taxa)
taxa_list

wdf <- data.frame(matrix(nrow=92, ncol=7))
colnames(wdf) <- c("species", "weighted_TSS", "weighted_SENS", "mean_TSS", "mean_SENS", "TSS_SE", "SENS_SE")

for(t in 1:length(taxa_list)) {
  
  tx <- paste(taxa_list[t])
  # tx
  
  tax_stats <- subset(stats, taxa==tx)
  head(tax_stats)
  # dim(tax_stats) ## 300   8
  tax_stats$potW <- tax_stats$TSS
  tax_stats$wTSS <- tax_stats$potW*tax_stats$TSS
  tax_stats$wsensitivity <- tax_stats$potW*tax_stats$sensitivity
  wdf[t, 1] <- paste(tx)
  wdf[t, 2] <- weighted.mean(tax_stats$TSS,tax_stats$potW) ## 0.61469
  wdf[t, 3] <- weighted.mean(tax_stats$sensitivity,tax_stats$potW) ## 0.8446933
  wdf[t, 4] <- mean(tax_stats$TSS)
  wdf[t, 5] <- mean(tax_stats$sensitivity)
  wdf[t, 6] <- std(tax_stats$TSS)
  wdf[t, 7] <- std(tax_stats$sensitivity)
}


# with(tax_stats, sum(tax_stats$TSS*tax_stats$potW)/sum(tax_stats$potW))
head(wdf)
tail(wdf)
dim(wdf) # 92
wdf <- na.omit(wdf)
write.csv(wdf, "weighted_TSS_SENS.csv")

## worldclim and hydrology
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro")
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")

st <- list.files(pattern="stats")
length(st) ## 93
st
st <- st[-c(87)]
# st <-st[-c(1)]
## creat dataframe
all_stats <- data.frame(matrix(nrow=4, ncol=12))
colnames(all_stats) <- c("sens_m", "sens_se", "TSS_m", "TSS_se", "Kappa_m", "Kappa_se", "sens_m", "sens_se", "spec_m", "spec_se", "species", "model")
head(all_stats)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

## read 1st file
stats <- read.csv(paste(st[1]))
head(stats)
dim(stats)

## loop to add all files together

for(s in 2:length(st)) {
  
  statsx <- read.csv(paste(st[s]))
  # head(statsx)
  # dim(statsx)
  model <- unique(statsx$model)
  stats <- rbind(stats, statsx)
  
}

head(stats)
dim(stats) ## 
stats <- stats[, -1]
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/uni_SDMs/95_2brt_run/stats")
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/stats")

save(stats, file="wclim_hydro_all_stats.csv")

taxa_list <- unique(stats$taxa)
taxa_list

wdf <- data.frame(matrix(nrow=92, ncol=7))
colnames(wdf) <- c("species", "weighted_TSS", "weighted_SENS", "mean_TSS", "mean_SENS", "TSS_SE", "SENS_SE")

for(t in 1:length(taxa_list)) {
  
  tx <- paste(taxa_list[t])
  # tx
  
  tax_stats <- subset(stats, taxa==tx)
  head(tax_stats)
  # dim(tax_stats) ## 300   8
  tax_stats$potW <- tax_stats$TSS
  tax_stats$wTSS <- tax_stats$potW*tax_stats$TSS
  tax_stats$wsensitivity <- tax_stats$potW*tax_stats$sensitivity
  wdf[t, 1] <- paste(tx)
  wdf[t, 2] <- weighted.mean(tax_stats$TSS,tax_stats$potW) ## 0.61469
  wdf[t, 3] <- weighted.mean(tax_stats$sensitivity,tax_stats$potW) ## 0.8446933
  wdf[t, 4] <- mean(tax_stats$TSS)
  wdf[t, 5] <- mean(tax_stats$sensitivity)
  wdf[t, 6] <- std(tax_stats$TSS)
  wdf[t, 7] <- std(tax_stats$sensitivity)
}


# with(tax_stats, sum(tax_stats$TSS*tax_stats$potW)/sum(tax_stats$potW))
head(wdf)
tail(wdf)
dim(wdf) # 92
wdf <- na.omit(wdf)
write.csv(wdf, "weighted_TSS_SENS.csv")

#  worldclim  & accumulated climate 

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc")

st <- list.files(pattern="stats")
length(st) ## 92
st
st <- st[-c(87)]
# st <-st[-c(1)]
## creat dataframe
all_stats <- data.frame(matrix(nrow=4, ncol=12))
colnames(all_stats) <- c("sens_m", "sens_se", "TSS_m", "TSS_se", "Kappa_m", "Kappa_se", "sens_m", "sens_se", "spec_m", "spec_se", "species", "model")
head(all_stats)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

## read 1st file
stats <- read.csv(paste(st[1]))
head(stats)
dim(stats)

## loop to add all files together

for(s in 2:length(st)) {
  
  statsx <- read.csv(paste(st[s]))
  # head(statsx)
  # dim(statsx)
  model <- unique(statsx$model)
  stats <- rbind(stats, statsx)
  
}

head(stats)
dim(stats) ## 
stats <- stats[, -1]
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/uni_SDMs/95_2brt_run/stats")
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc/stats")

save(stats, file="wclim_acc_all_stats.csv")

taxa_list <- unique(stats$taxa)
taxa_list

wdf <- data.frame(matrix(nrow=92, ncol=7))
colnames(wdf) <- c("species", "weighted_TSS", "weighted_SENS", "mean_TSS", "mean_SENS", "TSS_SE", "SENS_SE")

for(t in 1:length(taxa_list)) {
  
  tx <- paste(taxa_list[t])
  # tx
  
  tax_stats <- subset(stats, taxa==tx)
  head(tax_stats)
  # dim(tax_stats) ## 300   8
  tax_stats$potW <- tax_stats$TSS
  tax_stats$wTSS <- tax_stats$potW*tax_stats$TSS
  tax_stats$wsensitivity <- tax_stats$potW*tax_stats$sensitivity
  wdf[t, 1] <- paste(tx)
  wdf[t, 2] <- weighted.mean(tax_stats$TSS,tax_stats$potW) ## 0.61469
  wdf[t, 3] <- weighted.mean(tax_stats$sensitivity,tax_stats$potW) ## 0.8446933
  wdf[t, 4] <- mean(tax_stats$TSS)
  wdf[t, 5] <- mean(tax_stats$sensitivity)
  wdf[t, 6] <- std(tax_stats$TSS)
  wdf[t, 7] <- std(tax_stats$sensitivity)
}


# with(tax_stats, sum(tax_stats$TSS*tax_stats$potW)/sum(tax_stats$potW))
head(wdf)
tail(wdf)
dim(wdf) # 92
wdf <- na.omit(wdf)
write.csv(wdf, "weighted_TSS_SENS.csv")


#  worldclim, accumulated climate and hydrology

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro")

st <- list.files(pattern="stats")
length(st) ## 92
st
st <- st[-c(1,88)]
# st <-st[-c(1)]
## creat dataframe
all_stats <- data.frame(matrix(nrow=4, ncol=12))
colnames(all_stats) <- c("sens_m", "sens_se", "TSS_m", "TSS_se", "Kappa_m", "Kappa_se", "sens_m", "sens_se", "spec_m", "spec_se", "species", "model")
head(all_stats)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

## read 1st file
stats <- read.csv(paste(st[1]))
head(stats)
dim(stats)

## loop to add all files together

for(s in 2:length(st)) {
  
  statsx <- read.csv(paste(st[s]))
  # head(statsx)
  # dim(statsx)
  model <- unique(statsx$model)
  stats <- rbind(stats, statsx)
  
}

head(stats)
dim(stats) ## 
stats <- stats[, -1]
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/uni_SDMs/95_2brt_run/stats")
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/stats")

save(stats, file="wclim_acc_hydro_all_stats.csv")

taxa_list <- unique(stats$taxa)
taxa_list

wdf <- data.frame(matrix(nrow=92, ncol=7))
colnames(wdf) <- c("species", "weighted_TSS", "weighted_SENS", "mean_TSS", "mean_SENS", "TSS_SE", "SENS_SE")

for(t in 1:length(taxa_list)) {
  
  tx <- paste(taxa_list[t])
  # tx
  
  tax_stats <- subset(stats, taxa==tx)
  head(tax_stats)
  # dim(tax_stats) ## 300   8
  tax_stats$potW <- tax_stats$TSS
  tax_stats$wTSS <- tax_stats$potW*tax_stats$TSS
  tax_stats$wsensitivity <- tax_stats$potW*tax_stats$sensitivity
  wdf[t, 1] <- paste(tx)
  wdf[t, 2] <- weighted.mean(tax_stats$TSS,tax_stats$potW) ## 0.61469
  wdf[t, 3] <- weighted.mean(tax_stats$sensitivity,tax_stats$potW) ## 0.8446933
  wdf[t, 4] <- mean(tax_stats$TSS)
  wdf[t, 5] <- mean(tax_stats$sensitivity)
  wdf[t, 6] <- std(tax_stats$TSS)
  wdf[t, 7] <- std(tax_stats$sensitivity)
}


# with(tax_stats, sum(tax_stats$TSS*tax_stats$potW)/sum(tax_stats$potW))
head(wdf)
tail(wdf)
dim(wdf) # 92
wdf <- na.omit(wdf)
write.csv(wdf, "weighted_TSS_SENS.csv")

#  worldclim only

#  worldclim, accumulated climate and hydrology

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_only")

st <- list.files(pattern="stats")
length(st) ## 92
st
# st <- st[-c(1,88)]
# st <-st[-c(1)]
## creat dataframe
all_stats <- data.frame(matrix(nrow=4, ncol=12))
colnames(all_stats) <- c("sens_m", "sens_se", "TSS_m", "TSS_se", "Kappa_m", "Kappa_se", "sens_m", "sens_se", "spec_m", "spec_se", "species", "model")
head(all_stats)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))

## read 1st file
stats <- read.csv(paste(st[1]))
head(stats)
dim(stats)

## loop to add all files together

for(s in 2:length(st)) {
  
  statsx <- read.csv(paste(st[s]))
  # head(statsx)
  # dim(statsx)
  model <- unique(statsx$model)
  stats <- rbind(stats, statsx)
  
}

head(stats)
dim(stats) ## 
stats <- stats[, -1]
# setwd("U:/Irving/Assignments/Manuscript2_ubuntu/uni_SDMs/95_2brt_run/stats")
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_only/stats")

save(stats, file="wclim_only_all_stats.csv")

taxa_list <- unique(stats$taxa)
taxa_list

wdf <- data.frame(matrix(nrow=92, ncol=7))
colnames(wdf) <- c("species", "weighted_TSS", "weighted_SENS", "mean_TSS", "mean_SENS", "TSS_SE", "SENS_SE")

for(t in 1:length(taxa_list)) {
  
  tx <- paste(taxa_list[t])
  # tx
  
  tax_stats <- subset(stats, taxa==tx)
  head(tax_stats)
  # dim(tax_stats) ## 300   8
  tax_stats$potW <- tax_stats$TSS
  tax_stats$wTSS <- tax_stats$potW*tax_stats$TSS
  tax_stats$wsensitivity <- tax_stats$potW*tax_stats$sensitivity
  wdf[t, 1] <- paste(tx)
  wdf[t, 2] <- weighted.mean(tax_stats$TSS,tax_stats$potW) ## 0.61469
  wdf[t, 3] <- weighted.mean(tax_stats$sensitivity,tax_stats$potW) ## 0.8446933
  wdf[t, 4] <- mean(tax_stats$TSS)
  wdf[t, 5] <- mean(tax_stats$sensitivity)
  wdf[t, 6] <- std(tax_stats$TSS)
  wdf[t, 7] <- std(tax_stats$sensitivity)
}


# with(tax_stats, sum(tax_stats$TSS*tax_stats$potW)/sum(tax_stats$potW))
head(wdf)
tail(wdf)
dim(wdf) # 92
wdf <- na.omit(wdf)
write.csv(wdf, "weighted_TSS_SENS.csv")
mean(wdf$weighted_TSS) 
mean(wdf$mean_TSS)
mean(wdf$mean_SENS)
mean(wdf$TSS_SE)
mean(wdf$SENS_SE)

#############################################################
#  wilcox tests comparisons

#  combine 2 dataset models
#  test TSS
rm(list = ls())
#  accumulative and hydro Vs world clim and hydro

acc_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/stats/weighted_TSS_SENS.csv")
world_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/stats/weighted_TSS_SENS.csv")
head(acc_hydro)
dim(world_hydro)
#  TSS
acc_hydro_TSS <- acc_hydro[,c(2,3)]
world_hydro_TSS <- world_hydro[,c(2,3)]

colnames(acc_hydro_TSS) <- c("species", "acc_hydro")
colnames(world_hydro_TSS) <- c("species", "world_hydro")

both_TSS <- merge(acc_hydro_TSS, world_hydro_TSS, by="species")
head(both_TSS)
both_TSS$index <- seq(1,92,1)

both_TSS$TSS <- ifelse(both_TSS$acc_hydro > both_TSS$world_hydro, paste("higher"), paste("lower"))

acc_hydro_high <- subset(both_TSS, TSS=="higher")
dim(acc_hydro_high)
# 18
world_hydro_high <- subset(both_TSS, TSS=="lower")
dim(world_hydro_high)
# 74
head(world_hydro_high)
#  models performing better with accumulative climate
### test for significance with original values 

spp_list <- sort(acc_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
astats <- list.files(pattern="acc_hyd_stats")
ind <- acc_hydro_high$index
ind
astats <- sort(astats[ind])
astats ## 18

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
wstats <- list.files(pattern="wclim_hyd_stats")
# ustats <- ustats[-c(69)]
wstats
ind <- acc_hydro_high$index
wstats <- sort(wstats[ind])
wstats ## 18

df <- data.frame(matrix(nrow=18, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/", astats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/",wstats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
dim(sig_higher)
# 7 species perform significantly better with accumulative climate and hydrology
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/acc_hydro_sig_higher_than_world_hydro_new_structure.csv" )
# species w_stat      p_value significant
# 1        Alainites_muticus  828.0 5.519456e-03         yes
# 2      Isoperla_grammatica  454.5 4.232088e-08         yes
# 3   Limnephilus_extricatus  244.5 4.250857e-12         yes
# 4          Lithax_obscurus  443.0 2.685920e-08         yes
# 5 Potamothrix_moldaviensis  688.0 1.083657e-04         yes

### test for significance with original values 

spp_list <- sort(world_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
astats <- list.files(pattern="acc_hyd_stats")
ind <- world_hydro_high$index
ind
astats <- sort(astats[ind])
astats ## 74

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
wstats <- list.files(pattern="wclim_hyd_stats")
# ustats <- ustats[-c(69)]
wstats
ind <- world_hydro_high$index
wstats <- sort(wstats[ind])
wstats ## 74

df <- data.frame(matrix(nrow=74, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/", astats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/",wstats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
sig_higher
dim(sig_higher)
#  63
#  species perform better with world clim and hydrology data
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/world_hydro_sig_higher_than_acc_hydro_new_structure.csv" )


#  world clim & hydrology vs worldclim, worldclim and hydrology

rm(list = ls())
wclim_acc_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/stats/weighted_TSS_SENS.csv")
world_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/stats/weighted_TSS_SENS.csv")
head(wclim_acc_hydro)
dim(world_hydro)
#  TSS
wclim_acc_hydro_TSS <- wclim_acc_hydro[,c(2,3)]
world_hydro_TSS <- world_hydro[,c(2,3)]

colnames(wclim_acc_hydro_TSS) <- c("species", "wclim_acc_hydro")
colnames(world_hydro_TSS) <- c("species", "world_hydro")

both_TSS <- merge(wclim_acc_hydro_TSS, world_hydro_TSS, by="species")
head(both_TSS)
both_TSS$index <- seq(1,92,1)

both_TSS$TSS <- ifelse(both_TSS$wclim_acc_hydro > both_TSS$world_hydro, paste("higher"), paste("lower"))

wclim_acc_hydro_high <- subset(both_TSS, TSS=="higher")
dim(wclim_acc_hydro_high)
# 44
world_hydro_high <- subset(both_TSS, TSS=="lower")
dim(world_hydro_high)
# 48

#  models performing better with accumulative climate
### test for significance with original values 

spp_list <- sort(wclim_acc_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/")
wastats <- list.files(pattern="wclim_acc_hyd_stats")
# wastats <- wastats[-1]
ind <- wclim_acc_hydro_high$index
ind
wastats <- sort(wastats[ind])
wastats ##44

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
wstats <- list.files(pattern="wclim_hyd_stats")
# ustats <- ustats[-c(69)]
wstats
ind <- wclim_acc_hydro_high$index
wstats <- sort(wstats[ind])
wstats ## 44

df <- data.frame(matrix(nrow=44, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/",wstats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
dim(sig_higher)
# 25 species perform significantly better with accumulative climate, hydrology and worldclim
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/world_acc_hydro_sig_higher_than_wclim_hydro_new_structure.csv" )

### test for significance with original values 

spp_list <- sort(world_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked")
wastats <- list.files(pattern="wclim_acc_hyd_stats")
wastats <- wastats[-1]
ind <- world_hydro_high$index
ind
wastats <- sort(wastats[ind])
wastats ## 48

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
wstats <- list.files(pattern="wclim_hyd_stats")
# ustats <- ustats[-c(69)]
# wstats
ind <- world_hydro_high$index
wstats <- sort(wstats[ind])
wstats ## 48

df <- data.frame(matrix(nrow=48, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/",wstats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
sig_higher
dim(sig_higher)

#  38 - worldclim and hydro better than the full model
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/world_hydro_sig_higher_than_world_acc_hydro_new_structure.csv" )

#  acc and H V world, acc, H
rm(list = ls())

wclim_acc_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/stats/weighted_TSS_SENS.csv")
acc_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/stats/weighted_TSS_SENS.csv")
head(wclim_acc_hydro)

#  TSS
wclim_acc_hydro_TSS <- wclim_acc_hydro[,c(2,3)]
acc_hydro_TSS <- acc_hydro[,c(2,3)]

colnames(wclim_acc_hydro_TSS) <- c("species", "wclim_acc_hydro")
colnames(acc_hydro_TSS) <- c("species", "acc_hydro")

both_TSS <- merge(wclim_acc_hydro_TSS, acc_hydro_TSS, by="species")
head(both_TSS)
both_TSS$index <- seq(1,92,1)

both_TSS$TSS <- ifelse(both_TSS$wclim_acc_hydro > both_TSS$acc_hydro, paste("higher"), paste("lower"))

wclim_acc_hydro_high <- subset(both_TSS, TSS=="higher")
dim(wclim_acc_hydro_high)
# 69
acc_hydro_high <- subset(both_TSS, TSS=="lower")
dim(acc_hydro_high)
# 23

#  models performing better with accumulative climate
### test for significance with original values 

spp_list <- sort(wclim_acc_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked")
wastats <- list.files(pattern="wclim_acc_hyd_stats")
# wastats <- wastats[-1]
ind <- wclim_acc_hydro_high$index
ind
wastats <- sort(wastats[ind])
wastats ## 69

## acc clim

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
astats <- list.files(pattern="acc_hyd_stats")
# ustats <- ustats[-c(69)]
astats
ind <- wclim_acc_hydro_high$index
astats <- sort(astats[ind])
astats ## 69

df <- data.frame(matrix(nrow=69, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/",astats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
dim(sig_higher)
# 57 species perform significantly better with accumulative climate, hydrology and worldclim
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/world_acc_hydro_sig_higher_than_acc_hydro_new_structure.csv" )

### test for significance with original values 

spp_list <- sort(acc_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked")
wastats <- list.files(pattern="wclim_acc_hyd_stats")
# wastats <- wastats[-1]
ind <- acc_hydro_high$index
ind
wastats
wastats <- sort(wastats[ind])
wastats ## 23

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
astats <- list.files(pattern="acc_hyd_stats")
# ustats <- ustats[-c(69)]
astats
ind <- acc_hydro_high$index
astats <- sort(astats[ind])
astats ## 23

df <- data.frame(matrix(nrow=23, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/",astats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
sig_higher
dim(sig_higher)

#  13 species perform better with acc clim and hydrology data
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/acc_hydro_sig_higher_than_world_acc_hydro_new_structure.csv" )

#  acc and H V hC & bC
rm(list = ls())

wclim_acc <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_acc/stats/weighted_TSS_SENS.csv")
acc_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/stats/weighted_TSS_SENS.csv")
head(wclim_acc)

#  TSS
wclim_acc_TSS <- wclim_acc[,c(2,3)]
acc_hydro_TSS <- acc_hydro[,c(2,3)]

colnames(wclim_acc_TSS) <- c("species", "wclim_acc")
colnames(acc_hydro_TSS) <- c("species", "acc_hydro")

both_TSS <- merge(wclim_acc_TSS, acc_hydro_TSS, by="species")
head(both_TSS)
both_TSS$index <- seq(1,92,1)

both_TSS$TSS <- ifelse(both_TSS$wclim_acc > both_TSS$acc_hydro, paste("higher"), paste("lower"))

wclim_acc_high <- subset(both_TSS, TSS=="higher")
dim(wclim_acc_high)
# 42
acc_hydro_high <- subset(both_TSS, TSS=="lower")
dim(acc_hydro_high)
# 50

#  models performing better with accumulative climate
### test for significance with original values 

spp_list <- sort(wclim_acc_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc")
wastats <- list.files(pattern="wclim_acc_stats")
# wastats <- wastats[-1]
ind <- wclim_acc_high$index
ind
wastats <- sort(wastats[ind])
wastats ## 42

## acc clim

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
astats <- list.files(pattern="acc_hyd_stats")
# ustats <- ustats[-c(69)]
astats
ind <- wclim_acc_high$index
astats <- sort(astats[ind])
astats ## 42

df <- data.frame(matrix(nrow=42, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/",astats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
dim(sig_higher)
# 26 species perform significantly better with accumulative climate and worldclim
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/acc_wclim_sig_higher_than_acc_hydro_new_structure.csv" )

### test for significance with original values 

spp_list <- sort(acc_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc")
wastats <- list.files(pattern="wclim_acc_stats")
# wastats <- wastats[-1]
ind <- acc_hydro_high$index
ind
wastats
wastats <- sort(wastats[ind])
wastats ## 50

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
astats <- list.files(pattern="acc_hyd_stats")
# ustats <- ustats[-c(69)]
astats
ind <- acc_hydro_high$index
astats <- sort(astats[ind])
astats ## 50

df <- data.frame(matrix(nrow=50, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/",astats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
sig_higher
dim(sig_higher)

#  31 better with acc clim and Hyd
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/acc_hydro_sig_higher_than_world_acc_new_structure.csv" )

#  bioclima and H Vs hC & bC
rm(list = ls())

wclim_acc <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_acc/stats/weighted_TSS_SENS.csv")
wclim_hydro <- read.csv("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/stats/weighted_TSS_SENS.csv")
head(wclim_hydro)

#  TSS
wclim_acc_TSS <- wclim_acc[,c(2,3)]
wclim_hydro_TSS <- wclim_hydro[,c(2,3)]

colnames(wclim_acc_TSS) <- c("species", "wclim_acc")
colnames(wclim_hydro_TSS) <- c("species", "wclim_hydro")

both_TSS <- merge(wclim_acc_TSS, wclim_hydro_TSS, by="species")
head(both_TSS)
both_TSS$index <- seq(1,92,1)

both_TSS$TSS <- ifelse(both_TSS$wclim_acc > both_TSS$wclim_hydro, paste("higher"), paste("lower"))

wclim_acc_high <- subset(both_TSS, TSS=="higher")
dim(wclim_acc_high)
# 6
wclim_hydro_high <- subset(both_TSS, TSS=="lower")
dim(wclim_hydro_high)
# 86

#  models performing better with accumulative climate
### test for significance with original values 

spp_list <- sort(wclim_acc_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc")
wastats <- list.files(pattern="wclim_acc_stats")
# wastats <- wastats[-1]
ind <- wclim_acc_high$index
ind
wastats <- sort(wastats[ind])
wastats ## 6

## acc clim

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
astats <- list.files(pattern="wclim_hyd_stats")
# ustats <- ustats[-c(69)]
astats
ind <- wclim_acc_high$index
astats <- sort(astats[ind])
astats ## 6

df <- data.frame(matrix(nrow=6, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/",astats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
dim(sig_higher)
# 3 species perform significantly better with accumulative climate and worldclim
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/acc_wclim_sig_higher_than_world_hydro_new_structure.csv" )

### test for significance with original values 

spp_list <- sort(wclim_hydro_high$species)
spp_list <- droplevels(spp_list)
spp_list
## upload data
##  accumulative climate

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc")
wastats <- list.files(pattern="wclim_acc_stats")
# wastats <- wastats[-1]
ind <- wclim_hydro_high$index
ind
wastats
wastats <- sort(wastats[ind])
wastats ## 86

## world clim

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
astats <- list.files(pattern="wclim_hyd_stats")
# ustats <- ustats[-c(69)]
astats
ind <- wclim_hydro_high$index
astats <- sort(astats[ind])
astats ## 50

df <- data.frame(matrix(nrow=86, ncol=4))
colnames(df) <- c("species", "w_stat", "p_value", "significant")
t=1


for(t in 1:length(spp_list)) {
  
  sp <- paste(spp_list[t])
  # sp
  data_c <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_acc/", wastats[t], sep=""))
  
  cus <- data_c$TSS
  
  data_u <- read.csv(paste("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/",astats[t], sep=""))
  
  uni <- data_u$TSS
  
  wilx <- wilcox.test(cus,uni)
  
  df[t,1] <- sp
  df[t,2] <- wilx$statistic
  df[t,3] <- wilx$p.value
  df[t,4] <- ifelse(wilx$p.value<=0.05, paste("yes"), paste("no"))  
  
  
}

df
sig_higher <- subset(df, significant=="yes")
sig_higher
dim(sig_higher)
#  80 better with bioclimate and hydro 
write.csv(sig_higher, "/Users/katie/Documents/manuscript3/sdms/comp_stats/world_hydro_sig_higher_than_world_acc_new_structure.csv" )

#  variable importance

#  accumulative climate and hydrology

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
vp <- list.files(pattern="var_imp_train")
# vp <- vp[ind3]
vp 

### world clim and hydrology

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
vpu <- list.files(pattern="var_imp_train")
# vpu <- vpu[-1]
vpu

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/masked")
vp3 <- list.files(pattern="var_imp_train")
# vp3 <- vp3[-1]
vp3


## rank variables first to last for each species

#  acc climate and hydrology - 5 variables total

df <- data.frame(matrix(nrow=92, ncol=6))
colnames(df) <- c("species", "var1","var2",  "var3",  "var4",  "var5")

# ,"var6", "var6_val","var7", "var7_val", "var8", "var8_val", "var9", "var9_val", "var10", "var10_val" )
v=1

setwd("/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure")
## upload csvs one by one
for(v in 1:length(vp)) {
  
  var_imp_c <- read.csv(paste(vp[v]))
  # sum(var_imp_u$AUCtest) ## 0.9854
  # sum(var_imp_u$corTest) ## 2.2245
  
  var_imp_c <- var_imp_c[order(var_imp_c[,3], decreasing=TRUE),] ## order by corTest value
  
  top_vars_c <- var_imp_c
  top_vars_c
  # dim(top_vars_c)[1]
  
  
  df[v,1] <- colnames(top_vars_c)[2]
  df[v,2] <- paste(top_vars_c[1,2])
  df[v,3] <- paste(top_vars_c[2,2])
  df[v,4] <- paste(top_vars_c[3,2])
  df[v,5] <- paste(top_vars_c[4,2])
  df[v,6] <- paste(top_vars_c[5,2])
  # df[v,7] <- paste(top_vars_c[6,2])


  df
  # head(df)
  
  
}

acc_df <- df
acc_df

write.csv(acc_df, "/Users/katie/Documents/manuscript3/sdms/acc_hydro/new_structure/var_imp/var_imp_acc_hydro.csv")


#### worldclim and hydrology
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")

df <- data.frame(matrix(nrow=92, ncol=8))
colnames(df) <- c("species", "var1","var2",  "var3",  "var4",  "var5","var6",  "var7")

v=1
setwd("/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure")
## upload csvs one by one
for(v in 1:length(vpu)) {
  
  var_imp_c <- read.csv(paste(vpu[v]))
  # sum(var_imp_u$AUCtest) ## 0.9854
  # sum(var_imp_u$corTest) ## 2.2245
  
  var_imp_c <- var_imp_c[order(var_imp_c[,3], decreasing=TRUE),] ## order by corTest value
  
  top_vars_c <- var_imp_c
  top_vars_c

  df[v,1] <- colnames(top_vars_c)[2]
  df[v,2] <- paste(top_vars_c[1,2])
  df[v,3] <- paste(top_vars_c[2,2])
  df[v,4] <- paste(top_vars_c[3,2])
  df[v,5] <- paste(top_vars_c[4,2])
  df[v,6] <- paste(top_vars_c[5,2])
  df[v,7] <- paste(top_vars_c[6,2])
  df[v,8] <- paste(top_vars_c[7,2])
  # df[v,9] <- paste(top_vars_c[8,2])
  
  
  df
  
}

wclim_df <- df
wclim_df

write.csv(wclim_df, "/Users/katie/Documents/manuscript3/sdms/wclim_hydro/masked/new_structure/var_imp/var_imp_wclim_hydro.csv")


# #### worldclim accumulative and hydrology
setwd("/Users/katie/Documents/manuscript3/sdms/wwclim_acc_hydro")

df <- data.frame(matrix(nrow=93, ncol=11))
colnames(df) <- c("species", "var1","var2",  "var3",  "var4",  "var5","var6",  "var7","var8", "var9", "var10")
v=1

setwd("/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro")
## upload csvs one by one
for(v in 1:length(vp3)) {
  
  var_imp_c <- read.csv(paste(vp3[v]))
  # sum(var_imp_u$AUCtest) ## 0.9854
  # sum(var_imp_u$corTest) ## 2.2245
  
  var_imp_c <- var_imp_c[order(var_imp_c[,3], decreasing=TRUE),] ## order by corTest value
  
  top_vars_c <- var_imp_c
  # top_vars_c
  
  df[v,1] <- colnames(top_vars_c)[2]
  df[v,2] <- paste(top_vars_c[1,2])
  df[v,3] <- paste(top_vars_c[2,2])
  df[v,4] <- paste(top_vars_c[3,2])
  df[v,5] <- paste(top_vars_c[4,2])
  df[v,6] <- paste(top_vars_c[5,2])
  df[v,7] <- paste(top_vars_c[6,2])
  df[v,8] <- paste(top_vars_c[7,2])
  df[v,9] <- paste(top_vars_c[8,2])
  df[v,10] <- paste(top_vars_c[9,2])
  df[v,11] <- paste(top_vars_c[10,2])
  
  
  df
  
}

wclim_df <- df
wclim_df

write.csv(wclim_df, "/Users/katie/Documents/manuscript3/sdms/wclim_acc_hydro/var_imp/var_imp_wclim_acc_hydro.csv")


