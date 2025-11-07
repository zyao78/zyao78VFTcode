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

HOBO <- read_csv("F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/HOBO_upto2024.csv")
SoilT <- read_csv("F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/SoilT_upto2024.csv")

table(HOBO$site)

HOBO$site[HOBO$site == "GSP_BI"] <- "GSP-BI"
HOBO$site[HOBO$site == "GSP_LI"] <- "GSP-LI"
SoilT$site[SoilT$site == "GSP_BI"] <- "GSP-BI"
SoilT$site[SoilT$site == "GSP_LI"] <- "GSP-LI"


#### some high values of soilT in CM are in F, fix those
T_inF <- SoilT[which(SoilT$Value >=38 ),]    
Distinct_time <- T_inF %>%
  distinct(site, month, year) %>%
  arrange(year, site, month)   ### problematic entires: CM: 6/20/18 to 6/17/19, 6/16/19 to 6/14/20
                               #                        CH: 6/16/19 to 6/14/20

cm_start <- mdy("6/19/2018")+ days(1) - seconds(1)
cm_end   <- mdy("6/14/2020") + days(1) - seconds(1)
ch_start  <- mdy("6/15/2019")+ days(1) - seconds(1)
ch_end    <- mdy("6/14/2020") + days(1) - seconds(1)

T_inF <- SoilT %>%
  filter(
    (Value >=38&site == "CM" & date_parsed >= cm_start & date_parsed <= cm_end) |
  (Value >=38&site == "CH" & date_parsed >= ch_start  & date_parsed <= ch_end)
  )
T_inF <- T_inF %>%
  filter(!(Value <=50 & month>=4 & month<=9))
T_inF$Value_C <- (T_inF$Value - 32) * 5/9

for (i in 1:nrow(SoilT)) {
  match_value <- T_inF$Value_C[SoilT$Date.Time [i] == T_inF$Date.Time & SoilT$site [i] == T_inF$site]
  if (length(match_value) == 1) {
    SoilT$Value[i] <- match_value
  }
}   ### this might take a long time to run (>2 min)
hist(SoilT$Value)   ## all fixed

write.csv(SoilT, "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/SoilT_upto2024.csv")



######           local climate variables
#####
#####
#####
Climvar<- read.csv("data/TBFxClimate/Climvar_combined_interpolated.csv")
colnames(Climvar)
TBF_long<- read.csv("data/TBFxClimate/TBF_long_lm.csv")
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
### 
write.csv(TBF_long,"F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/TBF_long_lm.csv" )
###








###  fill gaps in local climate data with interpolation
### ###

LocalP <- read.csv("data/TBFxClimate/HOBO_upto2024.csv")
LocalT <- read.csv("data/TBFxClimate/SoilT_upto2024.csv")
table(LocalP$site)
table(LocalT$site)

RegionalTP<-read.csv("data/TBFxClimate/Reg_clim_upto2024.csv")

table(RegionalTP$site)



### checks for month absence
arr <- with(LocalT, tapply(month, list(site, year, factor(month, levels = 1:12)), length))
arr2 <- aperm(arr, c(2, 3, 1))
arr2

### get daily summaries
LocalT_daily <- LocalT %>%   
  group_by(newMonth, month,day,startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(meanST = mean(Value,na.rm = TRUE))

LocalP_daily <- LocalP %>%   
  group_by(newMonth, month,day,startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(meanSP = mean(mSWC,na.rm = TRUE))

Combined <- RegionalTP
Combined$local_T <- NA
Combined$local_P <- NA


### paste daily Local T and P into the Combined
for (i in 1:nrow(Combined)) {
  match <- LocalT_daily$meanST[LocalT_daily$site == Combined$site[i] & 
                                 LocalT_daily$startyear == Combined$startyear[i] &
                                 LocalT_daily$day == Combined$day[i] &
                                 LocalT_daily$newMonth == Combined$newMonth[i] ] 
  if (length(match) >= 1) {  # Ensure there's exactly one match
    Combined$local_T[i] <- match
  }
}
for (i in 1:nrow(Combined)) {
  match <- LocalP_daily$meanSP[LocalP_daily$site == Combined$site[i] & 
                                 LocalP_daily$startyear == Combined$startyear[i] &
                                 LocalP_daily$day == Combined$day[i] &
                                 LocalP_daily$newMonth == Combined$newMonth[i] ] 
  if (length(match) >= 1) {  # Ensure there's exactly one match
    Combined$local_P[i] <- match
  }
}

Combined <- Combined %>%
  rename(
    regional_T = Temp,
    regional_P = prec
  )
colnames(Combined)
table(Combined$site)

plot(Combined$regional_T, Combined$local_T) ### much cleaner correlation after fixing the entries with the wrong unit
plot(Combined$regional_P, Combined$local_P) ### variables on very different scale, hard to visualize

Combined$s.regional_P <- NA
Combined$s.local_P  <- NA
Combined$s.logregional_P <- NA

Combined$s.logregional_P <- scale(log(Combined$regional_P+0.1))
Combined$s.local_P  <- scale(Combined$local_P)
hist(Combined$s.logregional_P)

write.csv(Combined, "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/Climvar_combined.csv")


#####            linear model predicting local temperature       #######
####
####
###
Combined <- read.csv("data/TBFxClimate/Climvar_combined.csv")


## simple linear model that allow slopes to vary among sites 
Local_T_subset <- 
  Combined %>% 
  filter_at(vars(local_T,regional_T,s.logregional_P,site), all_vars(!is.na(.)))

GM_localT <- lm(local_T~regional_T+
                   site+site:regional_T,data=Local_T_subset,
                na.action = "na.fail")    
summary(GM_localT)
hist(GM_localT$residuals) ## great model fit


#### local P
Local_P_subset <- 
  Combined %>% 
  filter_at(vars(local_P,regional_T,s.logregional_P,site), all_vars(!is.na(.)))



GM_localP <- lm(local_P~s.logregional_P+
                  site+site:s.logregional_P ,data=Local_P_subset,
                na.action = "na.fail")    
hist(Local_P_subset$local_P)
summary(GM_localP)
hist(GM_localP$residuals) ## worse R-squared, but not horrible

### trying GAM sine the zero-inflated predictor is lowering the predictive power of the model
## refer to Zak's GAM codes

##check residuals
par(mfrow = c(2, 2))
summary(GM_localP)
plot(GM_localP)

###
###
####                      Gap filling for local T and P

gap_in_LT <- Combined[which(is.na(Combined$local_T)),c("local_T", "regional_T", "site", "X")]
gap_in_LP <- Combined[which(is.na(Combined$local_P)),c("local_P", "s.logregional_P", "site", "X")]

gap_in_LT$local_T<-predict(GM_localT, gap_in_LT, interval = "prediction")[,1]
gap_in_LP$local_P<-predict(GM_localP, gap_in_LP, interval = "prediction")[,1]### the first column is fitted value


for (i in 1:nrow(Combined)) { 
  if(is.na(Combined$local_T[i])){
 match_row <- gap_in_LT[which(Combined$X[i] == gap_in_LT$X),]
 if(nrow(match_row) == 1){
   Combined$local_T[i] <- match_row$local_T
     }
  }}
for (i in 1:nrow(Combined)) { 
  if(is.na(Combined$local_P[i])){
    match_row <- gap_in_LP[which(Combined$X[i] == gap_in_LP$X),]
    if(nrow(match_row) == 1){
      Combined$local_P[i] <- match_row$local_P
    }
  }}


write.csv(Combined, "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/Climvar_combined_interpolated.csv")

#### 
#### since daily local_P is zero inflated, the following codes try to model monthly local_P to see if that improves 
####  the model fit

Combined_2 <- read.csv("data/TBFxClimate/Climvar_combined.csv")

Monthly_Local_P <- Combined_2 %>% 
  group_by(site, newMonth,startyear) %>%
  dplyr::summarise(local_P_M = mean(local_P, na.rm = TRUE), regional_P_M = sum(regional_P))

Monthly_Local_P$local_P_M[which(is.nan(Monthly_Local_P$local_P_M))] <- NA
Monthly_Local_P$s.logregional_P_M <- scale(log(Monthly_Local_P$regional_P_M))


summary(lm(local_P_M~s.logregional_P_M+
             site+site:s.logregional_P_M,data=Monthly_Local_P,
           na.action = "na.omit"))


summary(lm(local_P_M ~ regional_P_M, data=Monthly_Local_P))
summary(lm(local_P ~ s.logregional_P, data=Combined_2))

plot(Monthly_Local_P$local_P_M ~ Monthly_Local_P$s.logregional_P_M)
plot(Combined_2$local_P ~ Combined_2$s.logregional_P)


#### plotting some large scale climate trend

monthly_LP <- Climvar %>%
  group_by(newMonth, site) %>%
  summarise(meanSM = mean(local_P, na.rm = TRUE), .groups = "drop") #, .groups = "drop"
Monthly_Local_P_averaged <- Monthly_Local_P %>%
  group_by(site, newMonth) %>%
  summarise(local_P_M = mean(local_P_M, na.rm = TRUE), .groups = "drop") %>%
  arrange(site, newMonth)

ggplot(Monthly_Local_P_averaged,
       aes(x = factor(newMonth), y = local_P_M, color = site, group = site)) +
  geom_line(size = 1,na.rm = TRUE) +
  geom_point(size = 1.6, na.rm = TRUE) +
  labs (x= "month", y="mean soil moisture")


B1_climate <- Climvar[which(Climvar$site == "B1"),]
B1_climate_monthly <- B1_climate %>%
  group_by(newMonth, startyear) %>%
  summarise(B1_climate_monthly = mean(local_P, na.rm=T), .groups = "drop")

ggplot(B1_climate_monthly,
       aes(x = factor(newMonth), y = B1_climate_monthly)) +
  geom_line(size = 1) +
  geom_point(size = 1.6, na.rm = TRUE) +
  labs (x= "month", y="mean soil moisture")

  