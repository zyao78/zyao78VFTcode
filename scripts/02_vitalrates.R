install.packages("future")
library("lme4")
library("lmerTest")
library("car")
library("dplyr")

################
TBF_long <- read.csv("data/TBFxClimate/TBF_long_landscape.csv") 
################
TBF_long$s.logsize0 <- scale(log(TBF_long$size0+0.1)) 
TBF_long$logsize1 <- (log(TBF_long$size1+0.1))
TBF_long$logsize0 <- (log(TBF_long$size0+0.1))


TBF_long$prep1 <- NA
TBF_long$prep1[which(TBF_long$rep1==0 & !is.na(TBF_long$rep1))] <- 0 # make sure no values assigned to NA
TBF_long$prep1[which(TBF_long$rep1>0 & !is.na(TBF_long$rep1))] <- 1
TBF_long$logcrep1 <- NA
TBF_long$crep1 <- NA
TBF_long$logcrep1[which(TBF_long$prep1== 1)] <- log(TBF_long$rep1[which(TBF_long$prep1== 1)]) #number of fruit
TBF_long$crep1[which(TBF_long$prep1== 1)] <- TBF_long$rep1[which(TBF_long$prep1== 1)] #number of fruit


look <- TBF_long %>%
  filter(prep1 == 1)


################## make sure that prep1 was assigned correctly ############

look <- TBF_long %>%
  filter(is.na(rep1)) #### any living plant should have either 1 or 0 (no NA) under rep1

TBF_long$rep1[which(is.na(TBF_long$rep1) & TBF_long$sur0_1 == 1)] <- 0 

look <- TBF_long %>%
  filter(is.na(rep1)) %>% 
  filter(!is.na(size1) | !is.na(N1) |!is.na(L1))  ### these are all newly added plant (new or missed) which had no logsize0, so these are fine.



#########################################################
####### export if updated ###############################
#########################################################

write.csv(TBF_long ,"data/TBFxClimate/TBF_long_landscape.csv") 

#########################################################


prep <- glmer(prep1 ~ s.logsize0+ TSF*TBF  + (1|site), data= TBF_long[which(TBF_long$consur0_1== 1),], family= "binomial")
crep1 <- lmer(logcrep1 ~ s.logsize0 +TSF*TBF+ (1|site), data= TBF_long[which(TBF_long$consur0_1== 1),])
summary(prep)#TBF is not sig
anova(crep)#TBF is sig; negative effect

# Car anova

car::Anova(sur, type = 3)
car::Anova(gr, type = 3)
car::Anova(prep, type = 3)
car::Anova(crep1, type = 3)


TBF_long$prep2 <- 0
TBF_long$prep2[which(TBF_long$rep1>0 & !is.na(TBF_long$rep1))] <- 1


















# subset data to those observations that have no NAs for objects in global model
sur_subset0 <- 
  TBF_long %>% 
  dplyr::filter(across(c(consur0_1, logsize0, TSF, TBF,site), ~ !is.na(.)))
# build global model
sur <- glmer(consur0_1 ~ logsize0 + TSF *TBF   + (1|site), data= sur_subset0, family= "binomial", na.action= "na.fail") # exclude NA by specifying the function to stop when it hits NAs
sur_dredge <- MuMIn::dredge(sur_noNA0) # dredge can only work for na.action="na.fail"
#find best model
sur_mod <- MuMIn::get.models(sur_dredge, 1)[[1]]
summary(sur_mod)$coefficients # get coefficients
sur_mod_vcov <- vcov(sur_mod) # get variance-covariance matrix
print(paste("Best fit model weight for survival:", round(sur_dredge$weight[1], 3))) # model weight
