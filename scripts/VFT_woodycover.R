library(here)
library(tidyverse)
library(readxl)


#load data

dat.list <- list()

dat.list[[1]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx",  sheet = 1 ) #col_types = rep("text", 9))
dat.list[[1]]$site <- "CH"

dat.list[[2]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 2)
dat.list[[2]]$site <- "CM"

dat.list[[3]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 3)
dat.list[[3]]$site <- "B1"

dat.list[[4]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 4)
dat.list[[4]]$site <- "B2"

#dat.list[[5]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 5)
#dat.list[[5]]$site <- "IA"

dat.list[[5]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 6)
dat.list[[5]]$site <- "GSP-BI"

dat.list[[6]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 7)
dat.list[[6]]$site <- "GSP-LI"

woody25 <- do.call("rbind", dat.list)

## fill in 0s and check for wrong entris
woody25$interc[is.na(woody25$interc)] <- 0
## calculate woody height
woody25$height <- NA
woody25$height <- woody25$legHeight - woody25$distance

table(woody25$legHeight)
hist(woody25$distance)
table(woody25$interc)

wrong<- woody25[!is.na(woody25$height) & woody25$height < 0, ]
woody25[1216, "distance"] <- 40   # fix wrong entries
woody25[122, "distance"] <- NA   # fix wrong entries
woody25$height <- woody25$legHeight - woody25$distance


## percentage cover by intercep

woodyquad <- woody25 %>%
  group_by(site, quad, trans) %>%
  summarise( percent_woody = sum(interc == 1) / n(), meanHeight = mean(height, na.rm=TRUE), count_woody = sum(interc==1,na.rm=TRUE ) )

## left join the note
WoodyNote <-woody25[!is.na(woody25$Note),] 
  
  
woodyquad$note <- NA
for (i in 1:nrow(woodyquad)){
     match_index <- which(WoodyNote$quad == woodyquad$quad[i] &
                            WoodyNote$site == woodyquad$site[i]&
                            WoodyNote$trans == woodyquad$trans[i] )
  
     if (length(match_index) > 0){
       woodyquad$note[i] <- WoodyNote$Note[match_index]
     }
}

nrow(woodyquad[!is.na(woodyquad$note), ])


### add TSF and TBF
woodyquad$startyear <- 2024



### export
write.csv(woodyquad, "F:/VFT/VFT_github/zyao78VFTcode/processed-data/woodycover2025.csv")
