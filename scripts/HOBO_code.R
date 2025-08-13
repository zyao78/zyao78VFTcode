
library(dplyr)
library(readr)
library(here)
library(tidyverse)
library(lubridate)


## create a dataframe to store climate variable
#site <- unique(HOBO2015$site)

#Climvar <- data.frame(site = rep(site, 9), startyear = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023))


## load in hobo files, rename columns 

#ME_HOBO  <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/ME_20151013_HOBO.csv", skip = 1, header = TRUE)
#IA_HOBO  <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/IA_20151013_HOBO.csv", skip = 1, header = TRUE)
IA_HOBO1 <-  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/IA_20161017_HOBO.csv",skip = 1, header = TRUE)
IA_HOBO2 <-  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/IA_2016620_HOBO.csv", skip = 1,header = TRUE)
colnames(IA_HOBO2) <- colnames(IA_HOBO1)
IA_HOBO <- rbind(IA_HOBO1,IA_HOBO2)

#GSP_LI_HOBO <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2018/VFT HOBO and iButton data 2018/gsp_li_soil_moisture_6_21_2018.csv", skip = 1, header = TRUE)
#GSP_BI_HOBO <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/B1_20150911_HOBO.csv", skip = 1, header = TRUE)

CM_HOBO <- rbind( read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/CM_20160510_HOBO.csv", header = TRUE),
  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/CM_20160909_HOBO.csv", header = TRUE),
  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/CM_20161017_HOBO.csv", header = TRUE))        # in case of multiple files for the same site in one year

#CM_HOBO  <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/CM_20151013_HOBO.csv", skip = 1,header = TRUE)
                     

#CH_HOBO  <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/CH_20151013_HOBO.csv", skip = 1,header = TRUE)

CH_HOBO <- rbind( read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/CH_20160510_HOBO.csv", header = TRUE),
                  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/CH_20160909_HOBO.csv", header = TRUE),
                  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/CH_20161017_HOBO.csv", header = TRUE))        # in case of multiple files for the same site in one year


#B1_HOBO  <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/B1_20150911_HOBO.csv", skip = 1, header = TRUE)

B1_HOBO <- rbind( read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/B1_20160512_HOBO.csv", header = TRUE),
                  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/B1_20161014_HOBO.csv", header = TRUE))

#B2_HOBO  <- read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2015/VFT HOBO and iButton data 2015/B2_20150911_HOBO.csv", skip = 1, header = TRUE)

B2_HOBO <- rbind( read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/B2_20160512_HOBO.csv", header = TRUE),
                  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/B2_20160615_HOBO.csv", header = TRUE),
                  read.csv("F:/VFT/TSF_effect/climate variables/VFT HOBO and iButton data 2016/VFT HOBO and iButton data 2016/B2_20161014_HOBO.csv", header = TRUE))        # in case of multiple files for the same site in one year


#in case of one sensor
#CH_HOBO$SWC2 <- NA


dat.list <- list(
  ME = ME_HOBO,
  IA = IA_HOBO,
  #GSP_LI = GSP_LI_HOBO,
  #GSP_BI = GSP_BI_HOBO,
  CM = CM_HOBO,
  CH = CH_HOBO,  
  B1 = B1_HOBO,
  B2 = B2_HOBO
)

dat.list <- lapply(dat.list, function(HOBO) {           
  HOBO <- HOBO[ , -1]    # delete the first column which contains no useful information
  colnames(HOBO) <- c("date", "SWC1", "SWC2")   ## sometimes there's only one SWC column, modify if needed
  return(HOBO)
})

HOBO2015 <- bind_rows(dat.list, .id = "site") # vertically merge dataframes and create a new column that indicates sites
unique(HOBO2015$site)

# extract months and days
HOBO2015$date_parsed <- mdy_hms(HOBO2015$date)   # first parse the date that is in mdy_hms format
HOBO2015$month <- month(HOBO2015$date_parsed)
HOBO2015$day   <- day(HOBO2015$date_parsed)
HOBO2015$year  <- year(HOBO2015$date_parsed)


HOBO2016$startyear <- ifelse(
  (HOBO2016$year == 2016) & 
    ((HOBO2016$month > 6) | (HOBO2016$month == 6 & HOBO2016$day >= 15)),
  2016,
  2015
)

#delete errors (volumetric water content should be greater than 0 and less than 1)
HOBO2017 <- HOBO2017 %>%
  filter(
    (SWC1 >= 0 & SWC1 <= 1) | 
      (SWC2 >= 0 & SWC2 <= 1)
  )

# take average of  the 2 sensors (if there're two)
HOBO2017$mSWC <- 0

HOBO2017$mSWC <- ifelse(
  HOBO2017$SWC1 >= 0 & HOBO2017$SWC1 <= 1 & HOBO2017$SWC2 >= 0 & HOBO2017$SWC2 <= 1,
  (HOBO2017$SWC1 + HOBO2017$SWC2) / 2,
  ifelse(
    HOBO2017$SWC1 >= 0 & HOBO2017$SWC1 <= 1,
    HOBO2017$SWC1,
    ifelse(
      HOBO2017$SWC2 >= 0 & HOBO2017$SWC2 <= 1,
      HOBO2017$SWC2,
      NA
    )
  )
)

### calculate climate metrics


#mean monthly SWC


monthlySM <- HOBO2017 %>%
  group_by(month, startyear, site) %>%    # for startyear of 2021 for example, months 6-12 were in 2021 and 1-5 were in 2015.
  summarize(mean = mean(mSWC))

monthly_wide <- monthlySM %>%    # transpose and then add to Climvar
    tidyr::pivot_wider(
    names_from = month,
    values_from = mean,
    names_prefix = "moisture_"
  )

#Climvar1 <- left_join(Climvar,monthly_wide, by=c("site", "startyear"))

Climvar <- Climvar %>%
  rows_update(monthly_wide, by = c("site", "startyear"))  # use rows update after initially performing left_join to avoid creating new columns

