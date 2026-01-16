install.packages("future")
install.packages("lme4")
install.packages("lmerTest")
install.packages("car")
install.packages("AICcmodavg")

library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)





######           local climate variables
#####
#####
#####
Climvar<- read.csv("legacy effect/data/TBFxClimate/Climvar_combined_interpolated.csv")
colnames(Climvar)
TBF_long<- read.csv("data/TBFxClimate/TBF_long_landscape.csv")
colnames(TBF_long)

## summarise by startyear
TP_LA<- Climvar %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarise(T_LA = mean(local_T,na.rm = TRUE),P_LA = mean(local_P,na.rm = TRUE))
## summarise by newMonth (from 15th to 14th_month2)
TP_LM<- Climvar %>%
  group_by(startyear, site, newMonth) %>%    
  summarise(T_LM = mean(local_T,na.rm = TRUE),P_LM = mean(local_P,na.rm = TRUE))

###
### ## annual mean (potential issue with sample size imbalance)


TBF_long$T_LA <- NA
TBF_long$P_LA <- NA


for (i in 1:nrow(TBF_long)) {
  match_LT <- TP_LA$T_LA[TP_LA$site == TBF_long$site[i] & TP_LA$startyear == TBF_long$startyear[i]]
  match_LP <- TP_LA$P_LA[TP_LA$site == TBF_long$site[i] & TP_LA$startyear == TBF_long$startyear[i]]
  if (length(match_LT) >= 1) {  
    TBF_long$T_LA[i] <- match_LT}
  if (length(match_LP) >= 1) {
    TBF_long$P_LA[i] <- match_LP
  }
}
head(TBF_long)


### coldest month min daily T
###
T_LC <- TP_LM [TP_LM$newMonth == 1,] 
minST <- Climvar %>%
  group_by(newMonth, startyear, site) %>%
  summarize(minST = min(local_T, na.rm = TRUE), .groups = "drop")
T_LC <- T_LC %>%
  left_join(minST, by = c("newMonth", "startyear", "site"))

TBF_long$T_LC <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_LC$minST[T_LC$site == TBF_long$site[i] & T_LC$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LC[i] <- match
  }
}
head(TBF_long)

### hottest month T and P

T_LH <- TP_LM [TP_LM$newMonth == 7,] 
maxLH <- Climvar %>%
  group_by(newMonth, startyear, site) %>%
  summarize(maxST = max(local_T, na.rm = TRUE),
            maxLP=  max(local_P, na.rm = TRUE),
            .groups = "drop")  
T_LH <- T_LH %>%
  left_join(maxLH, by = c("newMonth", "startyear", "site"))

TBF_long$T_LH <- NA
TBF_long$P_LH <- NA

for (i in 1:nrow(TBF_long)) {
  match_T <- T_LH$maxST[T_LH$site == TBF_long$site[i] & T_LH$startyear == TBF_long$startyear[i]]
  match_P <- T_LH$maxLP[T_LH$site == TBF_long$site[i] & T_LH$startyear == TBF_long$startyear[i]]
  
  if (length(match_T) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LH[i] <- match_T}
  if (length(match_P) >= 1) {
    TBF_long$P_LH[i] <- match_P
  }
}
head(TBF_long)

any(is.na(TBF_long$T_LH))

### wettest month T


Wettest_L <- TP_LM [TP_LM$newMonth == 8,] 
TBF_long$T_LW <- NA
TBF_long$P_LW <- NA

for (i in 1:nrow(TBF_long)) {
  match_T <- Wettest_L$T_LM[Wettest_L$site == TBF_long$site[i] & Wettest_L$startyear == TBF_long$startyear[i]]
  match_P <- Wettest_L$P_LM[Wettest_L$site == TBF_long$site[i] & Wettest_L$startyear == TBF_long$startyear[i]]
  
  if (length(match_T) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LW[i] <- match_T}
  if (length(match_P) >= 1) {
    TBF_long$P_LW[i] <- match_P
  }
}

head(TBF_long)

### driest month T

driest_L <- TP_LM [TP_LM$newMonth == 12,] 

TBF_long$T_LD <- NA
TBF_long$P_LD <- NA

for (i in 1:nrow(TBF_long)) {
  match_T <- driest_L$T_LM[driest_L$site == TBF_long$site[i] & driest_L$startyear == TBF_long$startyear[i]]
  match_P <- driest_L$P_LM[driest_L$site == TBF_long$site[i] & driest_L$startyear == TBF_long$startyear[i]]
  
  if (length(match_T) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LD[i] <- match_T}
  if (length(match_P) >= 1) {
    TBF_long$P_LD[i] <- match_P
  }
}
head(TBF_long)
####
###
####





###### regional climate variables
####
###

MonthlyRegPrec <- Climvar %>%   
  group_by(newMonth, startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(cumP = sum(regional_P,na.rm = TRUE))
MonthlyRegTemp <- Climvar %>%   
  group_by(newMonth, startyear, site) %>%   
  summarize(meanT = mean(regional_T,na.rm = TRUE))

###regional P 


### cum mean precipitation 

P_RA<- Climvar %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarize(prec = sum(regional_P,na.rm = TRUE))

TBF_long$P_RA <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RA$prec[P_RA$site == TBF_long$site[i] & P_RA$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RA[i] <- match
    
  }
}
head(TBF_long)
### wettest month prec (cum)

P_RW <- MonthlyRegPrec [MonthlyRegPrec$newMonth == 8, ]
TBF_long$P_RW <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RW$cumP[P_RW$site == TBF_long$site[i] & P_RW$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RW[i] <- match
    
  }
}
head(TBF_long)


### driest month prec (cum)

P_RD <- MonthlyRegPrec [MonthlyRegPrec$newMonth == 12, ]
TBF_long$P_RD <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RD$cumP[P_RD$site == TBF_long$site[i] & P_RD$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RD[i] <- match
    
  }
}
head(TBF_long)

### Hottest month prec (cum)
P_RH <- MonthlyRegPrec [MonthlyRegPrec$newMonth == 7, ]

TBF_long$P_RH <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RH$cumP[P_RH$site == TBF_long$site[i] & P_RH$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RH[i] <- match
    
  }
}




#### regional T
####
####
### regional annual mean T
T_RA<- Climvar %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarize(meanT = mean(regional_T,na.rm = TRUE))

TBF_long$T_RA <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_RA$meanT[T_RA$site == TBF_long$site[i] & T_RA$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_RA[i] <- match
    
  }
}

### coldest month min daily T

T_RC <- MonthlyRegTemp [MonthlyRegTemp$newMonth == 1,] 
minT <- Climvar %>%
  group_by(newMonth, startyear, site) %>%
  summarize(minT = min(regional_T, na.rm = TRUE), .groups = "drop")
T_RC <- T_RC %>%
  left_join(minT, by = c("newMonth", "startyear", "site"))

TBF_long$T_RC <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_RC$minT[T_RC$site == TBF_long$site[i] & T_RC$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_RC[i] <- match
  }
}

### hottest month T 

T_RH <- MonthlyRegTemp [MonthlyRegTemp$newMonth == 7,] 
maxT <- Climvar %>%
  group_by(newMonth, startyear, site) %>%
  summarize(maxT = max(regional_T, na.rm = TRUE), .groups = "drop")
T_RH <- T_RH %>%
  left_join(maxT, by = c("newMonth", "startyear", "site"))

TBF_long$T_RH <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_RH$maxT[T_RH$site == TBF_long$site[i] & T_RH$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_RH[i] <- match
  }
}

### wettest month T

T_RW <- MonthlyRegTemp [MonthlyRegTemp$newMonth == 8,] 
TBF_long$T_RW <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_RW$meanT[T_RW$site == TBF_long$site[i] & T_RW$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_RW[i] <- match
  }
}


### driest month T

T_RD <-  MonthlyRegTemp [MonthlyRegTemp$newMonth == 12,] 

TBF_long$T_RD <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_RD$meanT[T_RD$site == TBF_long$site[i] & T_RD$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_RD[i] <- match
  }
}

head(TBF_long)


###########################################################################################
###########################################################################################
##################### scale necessary climate variables ###################################
###########################################################################################
###########################################################################################


TBF_long$s.T_RA <- NA
TBF_long$s.T_RA <- scale(TBF_long$T_RA) 
TBF_long$s.P_RA <- NA
TBF_long$s.P_RA <- scale(TBF_long$P_RA) 
TBF_long$s.P_RW <- NA
TBF_long$s.P_RW <- scale(TBF_long$P_RW) 
TBF_long$s.P_RD <- NA
TBF_long$s.P_RD <- scale(TBF_long$P_RD) 
TBF_long$s.P_RH <- NA
TBF_long$s.P_RH <- scale(TBF_long$P_RH) 
TBF_long$s.T_RH<- NA
TBF_long$s.T_RD<- NA
TBF_long$s.T_RW<- NA
TBF_long$s.T_RC<- NA
TBF_long$s.T_RH <- scale(TBF_long$T_RH) 
TBF_long$s.T_RC <- scale(TBF_long$T_RC) 
TBF_long$s.T_RD <- scale(TBF_long$T_RD) 
TBF_long$s.T_RW <- scale(TBF_long$T_RW) 
TBF_long$s.T_RW <- scale(TBF_long$T_RW) 


TBF_long$sq.P_RA <- (TBF_long$P_RA)^2
TBF_long$sq.T_RA <- (TBF_long$T_RA)^2
TBF_long$sq.T_RH <- (TBF_long$T_RH)^2
TBF_long$s.sq.T_RH <- scale(TBF_long$sq.T_RH)
TBF_long$sq.TSF <- (TBF_long$TSF)^2
TBF_long$s.sq.P_RA <- scale(TBF_long$sq.P_RA)
TBF_long$s.sq.T_RA <- scale(TBF_long$sq.T_RA)
TBF_long$s.TSF <- scale(TBF_long$TSF)
TBF_long$s.sq.TSF <- scale(TBF_long$sq.TSF)
TBF_long$s.TBF <- scale(TBF_long$TBF)


TBF_long$s.T_LA <- NA
TBF_long$s.T_LA <- scale(TBF_long$T_LA) 
TBF_long$s.P_LA <- NA
TBF_long$s.P_LA <- scale(TBF_long$P_LA) 
TBF_long$s.P_LW <- NA
TBF_long$s.P_LW <- scale(TBF_long$P_LW) 
TBF_long$s.P_LD <- NA
TBF_long$s.P_LD <- scale(TBF_long$P_LD) 
TBF_long$s.P_LH <- NA
TBF_long$s.P_LH <- scale(TBF_long$P_LH) 
TBF_long$s.T_LH<- NA
TBF_long$s.T_LD<- NA
TBF_long$s.T_LW<- NA
TBF_long$s.T_LC<- NA
TBF_long$s.T_LH <- scale(TBF_long$T_LH) 
TBF_long$s.T_LC <- scale(TBF_long$T_LC) 
TBF_long$s.T_LD <- scale(TBF_long$T_LD) 
TBF_long$s.T_LW <- scale(TBF_long$T_LW) 

TBF_long$sq.P_LA <- (TBF_long$P_LA)^2
TBF_long$sq.T_LA <- (TBF_long$T_LA)^2
TBF_long$sq.T_LH <- (TBF_long$T_LH)^2
TBF_long$s.sq.T_LH <- scale(TBF_long$sq.T_LH)
TBF_long$s.sq.P_LA <- scale(TBF_long$sq.P_LA)
TBF_long$s.sq.T_LA <- scale(TBF_long$sq.T_LA)
TBF_long$sq.TSF <- (TBF_long$TSF)^2
TBF_long$s.TSF <- scale(TBF_long$TSF)
TBF_long$s.sq.TSF <- scale(TBF_long$sq.TSF)
TBF_long$s.TBF <- scale(TBF_long$TBF)

save.image("data/TBF_long_landscape_with_attr.Rdata")



  