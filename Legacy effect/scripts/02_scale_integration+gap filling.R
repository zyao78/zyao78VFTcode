
#########################################################################
###  fill gaps in local climate data with interpolation
### #####################################################################
library(readr)
library(dplyr)
library(lubridate)

LocalP <- read_csv("F:/VFT/VFT_github/zyao78VFTcode/legacy effect/data/climate data/HOBO_upto2024.csv")
LocalT <- read_csv("F:/VFT/VFT_github/zyao78VFTcode/legacy effect/data/climate data/SoilT_upto2024.csv")
table(LocalP$site)
table(LocalT$site)

RegionalTP<-read_csv("F:/VFT/VFT_github/zyao78VFTcode/legacy effect/data/climate data/Reg_clim_upto2024.csv")

### check that site names are consistent
table(RegionalTP$site)
table(LocalP$site)
table(LocalT$site)

### check for abnormally high soilT (those in F)
#### if present, convert those
T_inF <- LocalT[which(LocalT$Value >=38 ),]    
Distinct_time <- T_inF %>%
  distinct(site, month, year) %>%
  arrange(year, site, month)   

#### go to the raw-data Hobo and pendant folders to check on original units. Current as to 1/12/2026, 
###  no high values are due to incorrect unit. 
### if new issues (of incorrect unit) pop up, the following code fix them

### problematic entires (identified manually): CM: 6/20/18 to 6/17/19, 6/16/19 to 6/14/20
#                        CH: 6/16/19 to 6/14/20

cm_start <- mdy("6/19/2018")+ days(1) - seconds(1)
cm_end   <- mdy("6/14/2020") + days(1) - seconds(1)
ch_start  <- mdy("6/15/2019")+ days(1) - seconds(1)
ch_end    <- mdy("6/14/2020") + days(1) - seconds(1)

T_inF <- LocalT %>%
  filter(
    (Value >=38&site == "CM" & date_parsed >= cm_start & date_parsed <= cm_end) |
      (Value >=38&site == "CH" & date_parsed >= ch_start  & date_parsed <= ch_end)
  )
T_inF <- T_inF %>%
  filter(!(Value <=50 & month>=4 & month<=9))
T_inF$Value_C <- (T_inF$Value - 32) * 5/9

for (i in 1:nrow(LocalT)) {
  match_value <- T_inF$Value_C[LocalT$Date.Time [i] == T_inF$Date.Time & LocalT$site [i] == T_inF$site]
  if (length(match_value) == 1) {
    LocalT$Value[i] <- match_value
  }
}   ### this might take a long time to run (>2 min)
hist(LocalT$Value)


########################################################################################################
########################################################################################################
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



plot(Combined$regional_T, Combined$local_T) ### much cleaner correlation after fixing the entries with the wrong unit
plot(Combined$regional_P, Combined$local_P) ### variables on very different scale, hard to visualize

Combined$s.regional_P <- NA
Combined$s.local_P  <- NA
Combined$s.logregional_P <- NA

Combined$s.logregional_P <- scale(log(Combined$regional_P+0.1))
Combined$s.local_P  <- scale(Combined$local_P)
hist(Combined$s.logregional_P)

write.csv(Combined, "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/Climvar_combined.csv") ## check directory every time you run this code


#####            linear model predicting local temperature       #######
####
####
####
Combined <- read.csv("data/TBFxClimate/Climvar_combined.csv") ## check directory every time you run this code


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

