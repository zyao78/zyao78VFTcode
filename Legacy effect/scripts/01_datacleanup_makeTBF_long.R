Numextract <- function(string){
  myreturnvalue <- as.numeric(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string))))
  if (is.na(string)) {myreturnvalue <- NA}
  return(myreturnvalue)
}

alldemodata_upto2025 <- read.csv("Legacy effect/data/VFT master data/alldemodata_upto2025.csv")
data <- read.csv("legacy effect/raw-data/VFT master data/alldemodata_upto2025.csv",colClasses = "character")  ## ensure that the column types don't change (otherwise there will be ridiculous N,L)

Sur_cols <- paste0("sur_", 15:24, "_", 16:25)

warning("you must keep this na.strings argument in the line above in so that the Numextract function works!")

data$size15 <- data$size16 <- data$size17 <- data$size18 <- data$size19 <- 
  data$size20 <- data$size21 <- data$size22 <- data$size23 <- data$size24 <-data$size25 <- as.numeric(NA)

data$rep15 <- data$rep16 <- data$rep17 <- 
  data$rep18 <- data$rep19 <- data$rep20 <- data$rep21 <- data$rep22 <- data$rep23 <-data$rep24 <- data$rep25<- as.numeric(NA)

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
  data$size25[i] <-  sum(Numextract(data$N25[i]) *Numextract(data$L25[i]))
  
  data$rep15[i] <-  sum(Numextract(data$FR15[i]))
  data$rep16[i] <-  sum(Numextract(data$FR16[i]))
  data$rep17[i] <-  sum(Numextract(data$FR17[i]))
  data$rep18[i] <-  sum(Numextract(data$FR18[i]))
  data$rep19[i] <-  sum(Numextract(data$FR19[i]))
  data$rep20[i] <- sum(Numextract(data$FLW20[i]))
  data$rep21[i] <-  sum(Numextract(data$FLW21[i]))
  data$rep22[i] <-  sum(Numextract(data$FLW22[i]))
  data$rep23[i] <-  sum(Numextract(data$FLW23[i]))
  data$rep24[i] <- sum(Numextract(data$FLW24[i])) + sum(Numextract(data$FR24[i])) 
  data$rep25[i] <- sum(Numextract(data$FLW25[i])) + sum(Numextract(data$FR25[i])) 
  
  }       # NB, REP HERE IS FLOWERS AND FRUITS

## note that starting in 2025, we enforced the new rule for assigning "dead". The following codes reflect this difference

data15_16 <- cbind.data.frame(data[,c("site","ID", "quad", "size15", "rep15", "sur_15_16", "sur_15_16_tpry", "size16", "N16", "L16","rep16", "Comm16")],  rep(2015, dim(data)[1]))
data16_17 <- cbind.data.frame(data[,c("site","ID", "quad", "size16", "rep16", "sur_16_17", "sur_16_17_tpry", "size17","N17", "L17", "rep17",  "Comm17")], rep(2016, dim(data)[1]))                              
data17_18 <- cbind.data.frame(data[,c("site","ID", "quad", "size17", "rep17", "sur_17_18", "sur_17_18_tpry", "size18","N18", "L18", "rep18", "Comm18")], rep(2017, dim(data)[1]))                               
data18_19 <- cbind.data.frame(data[,c("site","ID", "quad", "size18", "rep18", "sur_18_19", "sur_18_19_tpry", "size19","N19", "L19", "rep19", "Comm19")], rep(2018, dim(data)[1]))                               
data19_20 <- cbind.data.frame(data[,c("site","ID", "quad", "size19", "rep19", "sur_19_20", "sur_19_20_tpry", "size20","N20", "L20", "rep20", "Comm20")],rep(2019, dim(data)[1]))                                
data20_21 <- cbind.data.frame(data[,c("site","ID", "quad", "size20", "rep20", "sur_20_21", "sur_20_21_tpry", "size21", "N21", "L21","rep21", "Comm21")],rep(2020, dim(data)[1]))                              
data21_22 <- cbind.data.frame(data[,c("site","ID", "quad", "size21", "rep21", "sur_21_22","sur_21_22_tpry", "size22", "N22", "L22", "rep22", "Comm22")], rep(2021, dim(data)[1]))                                
data22_23 <- cbind.data.frame(data[,c("site","ID", "quad", "size22", "rep22", "sur_22_23","sur_22_23_tpry","size23", "N23", "L23", "rep23", "Comm23")], rep(2022, dim(data)[1]))                                
data23_24 <- cbind.data.frame(data[,c("site","ID", "quad", "size23", "rep23", "sur_23_24","sur_23_24_tpry","size24", "N24", "L24", "rep24", "Comm24")], rep(2023, dim(data)[1]))                                
data24_25 <- cbind.data.frame(data[,c("site","ID", "quad", "size24", "rep24", "sur_24_25","sur_24_25_tpry","size25", "N25", "L25", "rep25", "Comm25")], rep(2024, dim(data)[1]))                                


names(data15_16) <-  names(data16_17) <-  names(data17_18) <-  names(data18_19) <-  
  names(data19_20) <-  names(data20_21) <-  names(data21_22) <- names(data22_23) <- 
  names(data23_24) <- names(data24_25) <- c("site", "ID", "quad", "size0", "rep0", "sur0_1", "sur0_1_tp","size1", "N1", "L1",  "rep1","comm1", "startyear")
head(data24_25)
data_long <- rbind(data15_16, data16_17, data17_18, data18_19, data19_20, data20_21, data21_22, data22_23, data23_24, data24_25)   
      data_long$site <- factor(data_long$site)    
      data_long$quad <- factor(data_long$quad)   
      data_long$size0 <- as.numeric(data_long$size0) 
      data_long$rep0 <- as.numeric(data_long$rep0)  
      data_long$size1 <- as.numeric(data_long$size1)  
      data_long$rep1 <- as.numeric(data_long$rep1)  
      data_long$startyear <- factor(data_long$ startyear)

# adding the "fine detailed" fire history from Natalie's FireHistory.rmd file---- 
      
#load in fire file
fire_histories <- read.csv("legacy effect/processed-data/fire_histories_landscape.csv") ## load in the most recent fire history fire
      
data_long$TSF <- NA
data_long$TBF <- NA
#copy an paste TSF and TBF into data_long
for (i in 1:nrow(data_long)) {
  #if (is.na(data_long$quad[i])) {  # Only proceed if quad is NA
    match_TSF <- fire_histories$TSF[
                                      fire_histories$site == data_long$site[i] & 
                                      fire_histories$startyear == data_long$startyear[i]]
    
    if (length(match_TSF) == 1) {  # Ensure there's exactly one match
      data_long$TSF[i] <- match_TSF
      
  }
}
for (i in 1:nrow(data_long)) {
  #if (is.na(data_long$quad[i])) {  # Only proceed if quad is NA
  #match_TSF <- fire_histories$TSF[fire_histories$site == data_long$site[i] & fire_histories$startyear == data_long$startyear[i]]
  match_TBF <- fire_histories$TBF[fire_histories$site == data_long$site[i] & fire_histories$startyear == data_long$startyear[i]]
  
  if (length(match_TBF) == 1) {  # Ensure there's exactly one match
    data_long$TBF[i] <- match_TBF
    
  }
}
     

#################################################################################################

write.csv(data_long, file= "legacy effect/data/TBFxClimate/TBF_long_landscape.csv")
