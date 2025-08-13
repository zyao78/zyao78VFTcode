install.packages("future")
library("lme4")
library("lmerTest")
library("car")
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)
HOBO_upto2024$site[HOBO_upto2024$site == "GSP_BI"] <- "GSP-BI"
HOBO_upto2024$site[HOBO_upto2024$site == "GSP_LI"] <- "GSP-LI"
SoilT_upto2024$site[SoilT_upto2024$site == "GSP_BI"] <- "GSP-BI"
SoilT_upto2024$site[SoilT_upto2024$site == "GSP_LI"] <- "GSP-LI"

###### local Precipitation variables
monthlySM <- HOBO_upto2024 %>%
  group_by(newMonth, startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(meanSM = mean(mSWC))
monthlyST <- SoilT_upto2024 %>%   ## fix NAs, check 2000 startyear???
  group_by(newMonth, startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(meanST = mean(Value,na.rm = TRUE))
MonthlyCV <- full_join(monthlySM, monthlyST, by = c("startyear", "site", "newMonth"))   # use full_join to not cut rows


## annual mean precipitation (potential issue with sample size imbalance)

P_LA<- HOBO_upto2024 %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarize(mean = mean(mSWC,na.rm = TRUE))

TBF_long$P_LA <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_LA$mean[P_LA$site == TBF_long$site[i] & P_LA$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_LA[i] <- match
    
  }
}
summary(aov(mean ~ site, P_LA))
ggplot(P_LA, aes(x = site, y = mean, fill = factor(startyear))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Site", y = "Mean", fill = "Start Year")+
  ggtitle("Annual mean local precipitation")

## wettest month SM

P_LW <- monthlySM [monthlySM$newMonth == 8, ]
TBF_long$P_LW <- NA
for (i in 1:nrow(TBF_long)) {
  match <- P_LW$meanSM[P_LW$site == TBF_long$site[i] & P_LW$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_LW[i] <- match
    
  }
}

## driest month  

P_LD <- monthlySM [monthlySM$newMonth == 12, ]  

TBF_long$P_LD <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_LD$meanSM[P_LD$site == TBF_long$site[i] & P_LD$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_LD[i] <- match
  }
}

## hottest month  (big issue with gaps in data availability) fix later

P_LH <- monthlySM [monthlySM$newMonth == 7, ]  

TBF_long$P_LH <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_LH$meanSM[P_LH$site == TBF_long$site[i] & P_LH$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_LH[i] <- match
  }
}




###### local temperature variables
###
### ## annual mean (potential issue with sample size imbalance)

T_LA<- SoilT_upto2024 %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarize(mean = mean(Value,na.rm = TRUE))

TBF_long$T_LA <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_LA$mean[T_LA$site == TBF_long$site[i] & T_LA$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LA[i] <- match
    
  }
}

### coldest month min daily T

T_LC <- monthlyST [monthlyST$newMonth == 1,] 
minST <- SoilT_upto2024 %>%
  group_by(newMonth, startyear, site) %>%
  summarize(minST = min(Value, na.rm = TRUE), .groups = "drop")
T_LC <- T_LC %>%
  left_join(minST, by = c("newMonth", "startyear", "site"))

TBF_long$T_LC <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_LC$minST[T_LC$site == TBF_long$site[i] & T_LC$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LC[i] <- match
  }
}

### hottest month T 

T_LH <- monthlyST [monthlyST$newMonth == 7,] 
maxST <- SoilT_upto2024 %>%
  group_by(newMonth, startyear, site) %>%
  summarize(maxST = max(Value, na.rm = TRUE), .groups = "drop")
T_LH <- T_LH %>%
  left_join(maxST, by = c("newMonth", "startyear", "site"))

TBF_long$T_LH <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_LH$maxST[T_LH$site == TBF_long$site[i] & T_LH$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LH[i] <- match
  }
}

### wettest month T
MonthlyCV$startyear <- as.factor(MonthlyCV$startyear)
MonthlyCV$site <- as.factor(MonthlyCV$site)

T_LW <- monthlyST [monthlyST$newMonth == 8,] 
TBF_long$T_LW <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_LW$meanST[T_LW$site == TBF_long$site[i] & T_LW$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LW[i] <- match
  }
}


### driest month T

T_LD <- monthlyST [monthlyST$newMonth == 12,] 

TBF_long$T_LD <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_LD$meanST[T_LD$site == TBF_long$site[i] & T_LD$startyear == TBF_long$startyear[i]]
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_LD[i] <- match
  }
}
####
###
####





###### regional climate variables
####
###

MonthlyRegPrec <- Reg_clim_upto2024 %>%   
  group_by(newMonth, startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(cumP = sum(prec,na.rm = TRUE))
MonthlyRegTemp <- Reg_clim_upto2024 %>%   
  group_by(newMonth, startyear, site) %>%   
  summarize(meanT = mean(Temp,na.rm = TRUE))

###regional P 

Reg_clim_upto2024$site[Reg_clim_upto2024$site == "GSP_BI"] <- "GSP-BI"
Reg_clim_upto2024$site[Reg_clim_upto2024$site == "GSP_LI"] <- "GSP-LI"

### cum mean precipitation 

P_RA<- Reg_clim_upto2024 %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarize(prec = sum(prec,na.rm = TRUE))

TBF_long$P_RA <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RA$prec[P_RA$site == TBF_long$site[i] & P_RA$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RA[i] <- match
    
  }
}

### wettest month prec (cum)

P_RW <- MonthlyRegPrec [MonthlyRegPrec$newMonth == 8, ]
TBF_long$P_RW <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RW$cumP[P_RW$site == TBF_long$site[i] & P_RW$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RW[i] <- match
    
  }
}


### driest month prec (cum)

P_RD <- MonthlyRegPrec [MonthlyRegPrec$newMonth == 12, ]
TBF_long$P_RD <- NA

for (i in 1:nrow(TBF_long)) {
  match <- P_RD$cumP[P_RD$site == TBF_long$site[i] & P_RD$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$P_RD[i] <- match
    
  }
}

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
T_RA<- Reg_clim_upto2024 %>%
  group_by(startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2022.
  summarize(meanT = mean(Temp,na.rm = TRUE))

TBF_long$T_RA <- NA

for (i in 1:nrow(TBF_long)) {
  match <- T_RA$meanT[T_RA$site == TBF_long$site[i] & T_RA$startyear == TBF_long$startyear[i]]
  
  if (length(match) >= 1) {  # Ensure there's exactly one match
    TBF_long$T_RA[i] <- match
    
  }
}

### coldest month min daily T

T_RC <- MonthlyRegTemp [MonthlyRegTemp$newMonth == 1,] 
minT <- Reg_clim_upto2024 %>%
  group_by(newMonth, startyear, site) %>%
  summarize(minT = min(Temp, na.rm = TRUE), .groups = "drop")
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
maxT <- Reg_clim_upto2024 %>%
  group_by(newMonth, startyear, site) %>%
  summarize(maxT = max(Temp, na.rm = TRUE), .groups = "drop")
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




### model first, then fill gaps in climate data with interpolation
### ###




#### model selection
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

### Regional 
###

### Survival
sur_subset <- 
  TBF_long %>% 
  dplyr::filter(across(c( consur0_1, s.logsize0, TSF, TBF,site, s.T_RA, s.P_RA), ~ !is.na(.)))
GM_RA_sur <- glmer(
  consur0_1 ~ s.logsize0 + TSF * TBF +
    s.T_RA + s.P_RA + s.T_RA:s.P_RA +
    I(s.T_RA^2) + I(s.P_RA^2) + I(s.T_RA^2 * s.P_RA^2) +
    (1 | site),
  data = sur_subset,
  family = "binomial", 
  na.action = "na.fail"
)
summary(GM_RA_sur)
sur_dredge_A <- MuMIn::dredge(GM_RA_sur, trace = 2) # dredge can only work for na.action="na.fail"
sur_mod_RA <- MuMIn::get.models(sur_dredge, 1)[[1]]

sur_subset_M <- 
  TBF_long %>% 
  dplyr::filter(across(c( consur0_1, s.logsize0, TSF, TBF,site, s.P_RD, s.P_RW, s.P_RH, s.T_RC, s.T_RH, 
                          s.T_RD, s.T_RW), ~ !is.na(.)))

GM_RM_sur <- glmer(
  consur0_1 ~ s.logsize0 + TSF * TBF + 
    s.T_RC + I(s.T_RC^2) +
    s.T_RW + s.P_RW + s.T_RW * s.P_RW + I(s.P_RW^2) +
    s.T_RD + s.P_RD + I(s.P_RD^2) + s.P_RD * s.T_RD +
    s.T_RH + s.P_RH + s.T_RH * s.P_RH + I(s.T_RH^2) +
    (1 | site), 
  data = sur_subset_M, 
  family = "binomial", 
  na.action = "na.fail"
)
sur_dredge_M <- MuMIn::dredge(GM_RM_sur, trace = 2) # dredge can only work for na.action="na.fail"
sur_mod_RM <- MuMIn::get.models(sur_dredge_M, 1)[[1]]

### growth

gr_subset_A <- 
  TBF_long %>% 
  dplyr::filter(across(c( logsize1, s.logsize0, TSF, TBF,site, s.T_RA, s.P_RA), ~ !is.na(.)))



### plotting 
ggplot(P_RA, aes(x = factor(startyear), y = prec, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "startyear", y = "annual cum Prec", fill = "site")+
  ggtitle("annual reg prec")
