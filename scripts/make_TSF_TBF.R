# project: VFT 
# code summary: calculates TSF and TBF, given fire history dates
# written by: A Louthan 250417
# modified: 
library(tidyverse)

# libraries and functions -----
rm(list = ls())

# load in data
fire_dates_manager <- openxlsx::read.xlsx("F:/TSF_effect/excel fire histories.xlsx", sheet= "manager", startRow=2,detectDates=TRUE)
fire_dates_field_notes <- openxlsx::read.xlsx( "F:/TSF_effect/excel fire histories.xlsx", sheet= "field_notes", startRow=2,detectDates=TRUE)
fire_dates_landsat <- openxlsx::read.xlsx("F:/TSF_effect/excel fire histories.xlsx", sheet= "landsat", startRow=2,detectDates=TRUE)

# reading in the last updated date to make sure you don't accidently say taht a fire was absent, just becuase the dates file hadn't been updated
end_date_manager <- openxlsx::read.xlsx("F:/TSF_effect/excel fire histories.xlsx", sheet= "manager", rows=1,colNames= FALSE) %>% stringr::str_remove("FILE CURRENT AS OF ") %>% as.Date("%m/%d/%y")
end_date_field_notes <- openxlsx::read.xlsx("F:/TSF_effect/excel fire histories.xlsx", sheet= "field_notes", rows=1,colNames= FALSE) %>% stringr::str_remove("FILE CURRENT AS OF ") %>% as.Date("%m/%d/%y")
end_date_landsat <- openxlsx::read.xlsx("F:/TSF_effect/excel fire histories.xlsx", sheet= "landsat", rows=1,colNames= FALSE) %>% stringr::str_remove("FILE CURRENT AS OF ") %>% as.Date("%m/%d/%y")

end_date_landsat <- end_date_manager

sites <- c("B1", "B2", "CM", "CH", "IA", "ME", "GSP-LI", "GSP-BI")
record_types <- c("manager", "field_notes", "landsat")
June15DOY <- cropgrowdays::day_of_year(as.Date("2020-06-15"))

fire_histories <- tidyr::expand_grid(record_type = record_types, site = sites, startyear = 1986:2025, TSF = NA, TBF = NA)

for (j in 1:3){ 
  if (j== 1) {fire_dates_j <- fire_dates_manager
  end_date <-   end_date_manager}
  if (j== 2) {fire_dates_j <- fire_dates_field_notes
  end_date <-   end_date_field_notes}
  if (j== 3) {fire_dates_j <- fire_dates_landsat
  end_date <-   end_date_landsat}
  
  end_startyear <-  ifelse(cropgrowdays::day_of_year(end_date) <= June15DOY, # if the day of year was before June 15
         as.numeric(format(end_date, "%Y"))-2,  # then the last startyear interval you know TSF for was 2 years before
         as.numeric(format(end_date, "%Y"))-1) # but if not, the last start year was 2 years before
  
  fire_dates_j$fire_startyear <- ifelse(cropgrowdays::day_of_year(fire_dates_j$fire_date) <= June15DOY, # if the day of year was before June 15
                                 as.numeric(format(fire_dates_j$fire_date, "%Y"))-1,  # then return the previous years number, b/c the fire happened over the interaval of previous to current year
                                 as.numeric(format(fire_dates_j$fire_date, "%Y"))) # but if not, then month and day must be greater than 6/15, so  return this years number, because the fire happneed over the interval of current year to the following

for (i in 1:length(sites)){  
  fire_dates_j_i <- fire_dates_j[which(fire_dates_j$site == sites[i]), ]
  first_fire_year <- min(fire_dates_j_i$fire_startyear)
   
#NB that if a fire occurs in startyear 2008, that means that a fire occurred over the 2008-2009 interval. 
  TSF <- NA
  for (ii in first_fire_year:end_startyear){
    if (ii %in% fire_dates_j_i$fire_startyear) {TBF <- TSF # if a fire happens, TSF becomes TBF
    TSF <- 0 # a TSF of zero indicates that a fire burned over the startyear to startyear+ 1 interval
    
    } else {TSF <- TSF +1
    }
    fire_histories$TSF[which(fire_histories$ startyear == ii & fire_histories$site == sites[i]  & fire_histories$ record_type == record_types[j])] <- TSF
    fire_histories$TBF[which(fire_histories$ startyear == ii & fire_histories$site == sites[i]  & fire_histories$ record_type == record_types[j])] <- TBF
  #NB that if a fire occurs in startyear 2008, that means that a fire occurred over the 2008-2009 interval.  
  }

  
}
}

save(fire_histories)