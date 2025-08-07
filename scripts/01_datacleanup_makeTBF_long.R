Numextract <- function(string){
  myreturnvalue <- as.numeric(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string))))
  if (is.na(string)) {myreturnvalue <- NA}
  return(myreturnvalue)
}


data <- read.csv("F:/TSF_effect/TSF_effect/alldemodata_upto2024.csv", na.strings = c("", "NA"))

warning("you must keep this na.strings argument in the line above in so that the Numextract function works!")

data$size15 <- data$size16 <- data$size17 <- data$size18 <- data$size19 <- 
  data$size20 <- data$size21 <- data$size22 <- data$size23 <- data$size24 <-

data$rep15 <- data$rep16 <- data$rep17 <- 
  data$rep18 <- data$rep19 <- data$rep20 <- data$rep21 <- data$rep22 <- data$rep23 <- data$rep24<- as.numeric(NA)

# there are commas in sizes for 2015-2018; the below loop fixes those
for (i in 1:dim(data)[1]){
  data$size15[i] <-  sum(Numextract(data$N15[i]) *Numextract(data$L15[i]))
  data$size16[i] <-  sum(Numextract(data$N16[i]) *Numextract(data$L16[i]))
  data$size17[i] <-  sum(Numextract(data$N17[i]) *Numextract(data$L17[i]))
  data$size18[i] <-  sum(Numextract(data$N18[i]) *Numextract(data$L18[i]))
  data$size19[i] <-  sum(Numextract(data$N19[i]) *Numextract(data$L19[i]))
  data$size20[i] <-  sum(Numextract(data$N20[i]) *Numextract(data$L20[i]))
  data$size21[i] <-  sum(Numextract(data$N21[i]) *Numextract(data$L21[i]))
  data$size22[i] <-  sum(Numextract(data$N22[i]) *Numextract(data$L22[i]))
  data$size23[i] <-  sum(Numextract(data$N23[i]) *Numextract(data$L23[i]))
  data$size24[i] <-  sum(Numextract(data$N24[i]) *Numextract(data$L24[i]))
  
  data$rep15[i] <-  sum(Numextract(data$FR15[i]))
  data$rep16[i] <-  sum(Numextract(data$FR16[i]))
  data$rep17[i] <-  sum(Numextract(data$FR17[i]))
  data$rep18[i] <-  sum(Numextract(data$FR18[i]))
  data$rep19[i] <-  sum(Numextract(data$FR19[i]))
  data$rep20[i] <- sum(Numextract(data$FLW20[i]))
  data$rep21[i] <-  sum(Numextract(data$FLW21[i]))
  data$rep22[i] <-  sum(Numextract(data$FLW22[i]))
  data$rep23[i] <-  sum(Numextract(data$FLW23[i]))
  data$rep24[i] <- sum(Numextract(data$FLW24[i])) + sum(Numextract(data$FR24[i]))  }       # NB, REP HERE IS FLOWERS AND FRUITS


data15_16 <- cbind.data.frame(
                  data[,c("site","ID", "quad", "size15", "rep15", "libsur15_16", "consur15_16", "size16", "N16", "L16","rep16", "Comm16")],  rep(2015, dim(data)[1]))
data16_17 <- cbind.data.frame(data[,c("site","ID", "quad", "size16", "rep16", "libsur16_17", "consur16_17", "size17","N17", "L17", "rep17",  "Comm17")], rep(2016, dim(data)[1]))                              
data17_18 <- cbind.data.frame(data[,c("site","ID", "quad", "size17", "rep17", "libsur17_18", "consur17_18", "size18","N18", "L18", "rep18", "Comm18")], rep(2017, dim(data)[1]))                               
data18_19 <- cbind.data.frame(data[,c("site","ID", "quad", "size18", "rep18", "libsur18_19", "libsur18_19", "size19","N19", "L19", "rep19", "Comm19")], rep(2018, dim(data)[1]))                               
data19_20 <- cbind.data.frame(data[,c("site","ID", "quad", "size19", "rep19", "libsur19_20", "libsur19_20", "size20","N20", "L20", "rep20", "Comm20")],rep(2019, dim(data)[1]))                                
data20_21 <- cbind.data.frame(data[,c("site","ID", "quad", "size20", "rep20", "libsur20_21", "libsur20_21", "size21", "N21", "L21","rep21", "Comm21")],rep(2020, dim(data)[1]))                              
data21_22 <- cbind.data.frame(data[,c("site","ID", "quad", "size21", "rep21", "libsur21_22","libsur21_22", "size22", "N22", "L22", "rep22", "Comm22")], rep(2021, dim(data)[1]))                                
data22_23 <- cbind.data.frame(data[,c("site","ID", "quad", "size22", "rep22", "libsur22_23","libsur22_23","size23", "N23", "L23", "rep23", "Comm23")], rep(2022, dim(data)[1]))                                
data23_24 <- cbind.data.frame(data[,c("site","ID", "quad", "size23", "rep23", "libsur23_24","libsur23_24","size24", "N24", "L24", "rep24", "Comm24")], rep(2023, dim(data)[1]))                                

names(data15_16) <-  names(data16_17) <-  names(data17_18) <-  names(data18_19) <-  names(data19_20) <-  names(data20_21) <-  names(data21_22) <- names(data22_23) <- names(data23_24) <-
  c("site", "ID", "quad", "size0", "rep0", "libsur0_1", "consur0_1","size1", "N1", "L1",  "rep1","comm1", "startyear")
head(data23_24)
data_long <- rbind(data15_16, data16_17, data17_18, data18_19, data19_20, data20_21, data21_22, data22_23, data23_24)   
      data_long$site <- factor(data_long$site)    
      data_long$quad <- factor(data_long$quad)   
      data_long$size0 <- as.numeric(data_long$size0) 
      data_long$rep0 <- as.numeric(data_long$rep0)  
      data_long$size1 <- as.numeric(data_long$size1)  
      data_long$rep1 <- as.numeric(data_long$rep1)  
      data_long$ startyear <- factor(data_long$ startyear)

# adding the "fine detailed" fire history from Natalie's FireHistory.rmd file---- 
      
#load in fire file
LC_Fire <- read.csv("F:/TSF_effect/TSF_effect/LC_fire_2025.csv", na.strings = c("", "NA"))
LC_Fire <- LC_Fire[which(LC_Fire$site!= "B1"),] 
data_long$TSF <- NA
data_long$TBF <- NA
#copy an paste TSF and TBF into data_long
for (i in 1:nrow(data_long)) {
  #if (is.na(data_long$quad[i])) {  # Only proceed if quad is NA
    match_TSF <- LC_Fire$TSF[LC_Fire$site == data_long$site[i] & LC_Fire$startyear == data_long$startyear[i]]
    #match_TBF <- LC_Fire$TBF[LC_Fire$site == data_long$site[i] & LC_Fire$startyear == data_long$startyear[i]]
    
    if (length(match_TSF) == 1) {  # Ensure there's exactly one match
      data_long$TSF[i] <- match_TSF
      
  }
}
for (i in 1:nrow(data_long)) {
  #if (is.na(data_long$quad[i])) {  # Only proceed if quad is NA
  #match_TSF <- LC_Fire$TSF[LC_Fire$site == data_long$site[i] & LC_Fire$startyear == data_long$startyear[i]]
  match_TBF <- LC_Fire$TBF[LC_Fire$site == data_long$site[i] & LC_Fire$startyear == data_long$startyear[i]]
  
  if (length(match_TBF) == 1) {  # Ensure there's exactly one match
    data_long$TBF[i] <- match_TBF
    
  }
}
     


#skip this part
TSFs <- data.frame((seq(2004, 2023)))
names(TSFs) <- "startyear"

for (i in 1:length(sites)){
fire_i <- hist(fires_list[[i]],breaks= as.Date(paste(c(TSFs$startyear, TSFs$startyear[length(TSFs$startyear)]+1) # adding in the last year of the survey
                                               , "-03-15", sep= ""), format = "%Y-%m-%d") )$counts 
if (length(which(fire_i >1))>1){warning("more than one fire in a year")}
fire_i <- fire_i-1
fire_i[which(fire_i <0)] <- NA
TSFs <-  cbind(TSFs, fire_i)}



names(TSFs) <- c("startyear", sites)         
TSFs$CH[c(2, 4, 5, 7:13,14:20) ] <- 
         c(1, 1, 2, 1:7, 0:6)
TSFs$CM[c(4:7, 9:13, 15, 17:20) ] <- 
         c(1:4, 1:5,  1,  1:4 )
TSFs$IA[c(9:10, 12:15, 17:20)] <- 
         c(1:2,  1:4,   1:4)
TSFs$ME[c(2:12, 14:15, 17:19, 20)] <- 
         c(1:11,  1:2,   1:3, 0)
TSFs$'GSP-LI'[c(4:5, 7:8, 10, 12:13, 15, 18,20)] <- 
                c(1:2,  1:2, 1,  1:2,1,1,1)
TSFs$'GSP-BI' [c(1:6,7,8,9,10:11,12,13,14:16, 17:18, 19:20)] <- 
c(0:5,0,0,0,1:2,0,0,1:3,0:1,0:1 )
TSFs$B1[c(10:12, 14,15, 16:20)] <- 
  c(1:3,  1,0,   1:5)
TSFs$B2[c(6:12, 14,15, 16:20)] <- 
         c(1:7,  1, 0,  1:5)
write.csv(TSFs, "TSF.csv")
TBFs <- data.frame((seq(2004, 2023)))

names(TBFs) <- "startyear"
TBFs$CH <- TBFs$CM <- TBFs$IA <- TBFs$ME <- TBFs$'GSP-LI' <- TBFs$'GSP-BI' <- TBFs$B1 <- TBFs$B2 <- NA

TBFs$CH[c(6:13) ] <- 2
TBFs$CH[c(3:5) ] <- 1
TBFs$CH[c(1:2) ] <- NA
TBFs$CH[c(14:20) ] <- 7

TBFs$CM[8:13] <- 4
TBFs$CM[14:15] <- 5
TBFs$CM[16:20] <- 1
TBFs$CM[1:7] <- NA


TBFs$IA[1:10] <- NA
TBFs$IA[11:15] <- 2
TBFs$IA[16:20] <- 4

TBFs$ME[1:12] <- NA
TBFs$ME[13:15] <- 11
TBFs$ME[16:20] <- 2

TBFs$'GSP-LI'[1:5] <- NA
TBFs$'GSP-LI'[6:10] <- 2
TBFs$'GSP-LI'[11:13] <- 1
TBFs$'GSP-LI'[14:15] <- 2
TBFs$'GSP-LI'[16] <- 1
TBFs$'GSP-LI'[17:18] <- 0
TBFs$'GSP-LI'[19:20] <- 1


TBFs$'GSP-BI'[1:6] <- NA
TBFs$'GSP-BI'[7] <- 5
TBFs$'GSP-BI'[8:11] <- 0
TBFs$'GSP-BI'[12] <- 2
TBFs$'GSP-BI'[13:16] <- 0
TBFs$'GSP-BI'[17:18] <- 3
TBFs$'GSP-BI'[19:20] <- 1


TBFs$B1[1:12] <- NA
TBFs$B1[13:14] <- 3
TBFs$B1[15:20] <- 1

TBFs$B2[1:12] <-NA
TBFs$B2[13:14] <-7
TBFs$B2[15:20] <- 1


#now, adding in TSF and TBF info into demograpphic data 
data_long$TSF <- NA
data_long$TBF <- NA

for (i in 1:dim(data_long)[1]){ # yes I am sure there is a better way of doing this
  data_long$TBF[i] <- TBFs[which(TBFs$startyear == data_long$startyear[i]),
                           which(colnames(TBFs) == data_long$site[i] )]
  data_long$TSF[i] <- TSFs[which(TSFs$startyear == data_long$startyear[i]),
                           which(colnames(TSFs) == data_long$site[i] )]
}

for (i in 1:nrow(data_long)) {
  
    match_index <- LC_fire_2025$TBF[LC_fire_2025$startyear == data_long$startyear[i] & LC_fire_2025$site == data_long$site[i]]
    
    if (length(match_index) == 1)   # Ensure there's exactly one match
      data_long$TBF[i] <- match_index
  
  
}

for (i in 1:nrow(data_long)) {
  
  match_index <- LC_fire_2025$TSF[LC_fire_2025$startyear == data_long$startyear[i] & LC_fire_2025$site == data_long$site[i]]
  
  if (length(match_index) == 1)   # Ensure there's exactly one match
    data_long$TSF[i] <- match_index
  
  
}

# delete B1 since it has no TBFs

data_long1 <- data_long[data_long$site != "B1",]

# another way 
library(tidyr)
library(dplyr)



TSFs_transposed <- TSFs %>%
  pivot_longer(
    cols = -startyear,  # Keep startyear as is, convert all other columns
    names_to = "site",  # New column for site names
    values_to = "TSF"   # assign values to col TSF
  )

TBFs_transposed <- TBFs %>%
  pivot_longer(
    cols = -startyear,  
    names_to = "site",  
    values_to = "TBF"   
  )

TBFs_transposed$startyear <- as.factor(TBFs_transposed$startyear)
TSFs_transposed$startyear <- as.factor(TSFs_transposed$startyear)

data_long1 <- merge(data_long, TBFs_transposed, by = c("startyear", "site"), all.x = TRUE)
data_long1 <- merge(data_long1, TSFs_transposed, by = c("startyear", "site"), all.x = TRUE)

data_long1 <- data_long1 %>%
  mutate(
    TSF = TSF.y,
    TBF = TBF.y
  ) %>%
  select(-TSF.x, -TSF.y, -TBF.x, -TBF.y) 

write.csv(data_long1, file= "TBF_data_long1.csv")
