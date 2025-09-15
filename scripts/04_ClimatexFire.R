install.packages("future")
install.packages("lme4")
install.packages("lmerTest")
install.packages("car")
install.packages("AICcmodavg")
install.packages("tidyverse")
install.packages("MuMIn")
install.packages("glue")
install.packages("dplyr")


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




###  fill gaps in local climate data with interpolation
### ###

LocalP <- read_csv("data/TBFxClimate/HOBO_upto2024.csv")
LocalT <- read_csv("data/TBFxClimate/SoilT_upto2024.csv")
HOBO_upto2024 <- read_csv("data/TBFxClimate/HOBO_upto2024.csv")
RegionalTP<-read_csv("data/TBFxClimate/Reg_clim_upto2024.csv")

### checks for month absence
arr <- with(LocalT, tapply(month, list(site, year, factor(month, levels = 1:12)), length))
arr2 <- aperm(arr, c(2, 3, 1))
arr2

### get daily summaries
LocalT_daily <- LocalT %>%   
  group_by(newMonth, month,day,startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(meanST = mean(Value,na.rm = TRUE))

Combined <- RegionalTP

for (i in 1:nrow(Combined)) {
  match <- LocalT_daily$meanST[LocalT_daily$site == Combined$site[i] & 
                                 LocalT_daily$startyear == Combined$startyear[i] &
                                 LocalT_daily$day == Combined$day[i] &
                                 LocalT_daily$newMonth == Combined$newMonth[i] ] 
  if (length(match) >= 1) {  # Ensure there's exactly one match
    Combined$local_T[i] <- match
  }
}









##### checking large scale climate trend
Monthlyclimsum <- RegionalTP %>%
  group_by(newMonth,site) %>%
  summarize(meanP = mean (prec), meanT = mean(Temp, na.rm = T))

Monthlyclimsum$newMonth <- as.factor()

ggplot(data= Monthlyclimsum, aes(x= newMonth, y= meanT, group = site, color = site)) +
  geom_line(linewidth = 1) + 
  #geom_ribbon(aes(ymin = data_new1$sur$lwr, ymax = data_new1$sur$upr, fill= newTBF), alpha = 0.1) + 
  labs(y= "mean monthly temperature (2015-2024)", x = "Month (M/15 - M+1/14)") +
  #theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  theme(text = element_text(size = 10))
