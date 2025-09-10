install.packages("future")
library("lme4")
library("lmerTest")
library("car")


TBF_data_long1$s.logsize0 <- scale(log(TBF_data_long1$size0+0.1)) 
TBF_data_long1$logsize1 <- (log(TBF_data_long1$size1+0.1))
TBF_data_long1$logsize0 <- (log(TBF_data_long1$size0+0.1))


  # survival
sur <- glmer(consur0_1 ~ s.logsize0 +TSF *TBF    + (1|site), data= TBF_data_long1, family= "binomial", na.action = na.omit)
table(sur@frame$site) # for coredata table in vft ltreb # extracting num of observation for each site
gr <- lmer(logsize1 ~ s.logsize0 + TSF* TBF  + (1|site), data= TBF_data_long1[which(TBF_data_long1$consur0_1== 1),])
summary(sur) #TBF is sig; negative effect
anova(gr)#TBF is not sig

TBF_data_long1$prep1 <- NA
TBF_data_long1$prep1[which(TBF_data_long1$rep1==0 & !is.na(TBF_data_long1$rep1))] <- 0 # make sure no values assigned to NA
TBF_data_long1$prep1[which(TBF_data_long1$rep1>0 & !is.na(TBF_data_long1$rep1))] <- 1
TBF_data_long1$logcrep1 <- NA
TBF_data_long1$logcrep1[which(TBF_data_long1$prep1== 1)] <- log(TBF_data_long1$rep1[which(TBF_data_long1$prep1== 1)]) #number of fruit

prep <- glmer(prep1 ~ s.logsize0+ TSF*TBF  + (1|site), data= TBF_data_long1[which(TBF_data_long1$consur0_1== 1),], family= "binomial")
crep1 <- lmer(logcrep1 ~ s.logsize0 +TSF*TBF+ (1|site), data= TBF_data_long1[which(TBF_data_long1$consur0_1== 1),])
summary(prep)#TBF is not sig
anova(crep)#TBF is sig; negative effect

# Car anova

car::Anova(sur, type = 3)
car::Anova(gr, type = 3)
car::Anova(prep, type = 3)
car::Anova(crep1, type = 3)





















# subset data to those observations that have no NAs for objects in global model
sur_subset0 <- 
  TBF_data_long1 %>% 
  dplyr::filter(across(c(consur0_1, logsize0, TSF, TBF,site), ~ !is.na(.)))
# build global model
sur <- glmer(consur0_1 ~ logsize0 + TSF *TBF   + (1|site), data= sur_subset0, family= "binomial", na.action= "na.fail") # exclude NA by specifying the function to stop when it hits NAs
sur_dredge <- MuMIn::dredge(sur_noNA0) # dredge can only work for na.action="na.fail"
#find best model
sur_mod <- MuMIn::get.models(sur_dredge, 1)[[1]]
summary(sur_mod)$coefficients # get coefficients
sur_mod_vcov <- vcov(sur_mod) # get variance-covariance matrix
print(paste("Best fit model weight for survival:", round(sur_dredge$weight[1], 3))) # model weight
