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

## fill in 0s
woody25$interc[is.na(woody25$interc)] <- 0
## calculate woody height
woody25$height <- NA
woody25$height <- woody25$legHeight - woody25$distance

## percentage cover by intercep

woodyquad <- woody25 %>%
  group_by(site, quad) %>%
  summarise(percent_woody_2 = sum(interc == 1) / 81, percent_woody = sum(interc == 1) / n())
