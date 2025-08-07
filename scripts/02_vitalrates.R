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


#simulate graphs
data_new1 <- as.data.frame((rep(mean(TBF_data_long1$s.logsize0, na.rm=TRUE), 17*2)))
names(data_new1) <- "s.logsize0"
data_new1$TSF <- rep(seq(0, 16), 2)
data_new1 $TBF <- c(rep(1, 17), rep(15, 17))
data_new1$site <- "GSP-BI"
data_new1$sur <- merTools::predictInterval(sur, data_new1, which= "fixed", type= "probability", 
                                          level= 0.95,n.sims= 10000)
data_new1$gr <- merTools::predictInterval(gr, data_new1, which= "fixed", level= 0.95,n.sims= 10000)
data_new1$prep <- merTools::predictInterval(prep, data_new1, which= "fixed", type= "probability", level= 0.95,n.sims= 10000)
data_new1$crep1 <- exp(merTools::predictInterval(crep1, data_new1, which= "fixed", level= 0.95,n.sims= 10000))
data_new1$newTBF <- NA
data_new1$newTBF[which(data_new1$TBF== 1)] <- "Short TBF"
data_new1$newTBF[which(data_new1$TBF== 15)] <- "Long TBF"

library(ggplot2)
plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur$lwr, ymax = data_new1$sur$upr, fill= newTBF), alpha = 0.1) + 
  ylim(0,1) +xlim(c(0, 16.5)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(text = element_text(size = 20)) 
plt2 <- ggplot(data= data_new1, aes(x= TSF, y= gr$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 16.5)) +
  labs(y= "Growth", x = "Time since fire", tag= "B") + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 20)) 
plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep$fit)) +
  geom_line(aes(color= newTBF)) + 
  ylim(0,1) + xlim(c(0, 16.5)) +
  geom_ribbon(aes(ymin = data_new1$prep$lwr, ymax = data_new1$prep$upr, 
                  fill= newTBF), alpha = 0.1) + 
  labs(y= "Prob. of fruiting", x = "Time since fire",  color= "", fill= "", tag= "C") + 
  theme_bw()  + 
  theme(legend.position = "none", legend.spacing.y = unit(-20, "pt")) +
  theme(text = element_text(size = 20)) 
plt4 <- ggplot(data= data_new1, aes(x= TSF, y= crep1$fit)) +
  geom_line(aes( color= newTBF))+xlim(c(0, 16.5)) +
  geom_ribbon(aes(ymin = data_new1$crep1$lwr, ymax = data_new1$crep1$upr, fill= newTBF), alpha = 0.1) + 
  labs(y= "Number of fruits", x = "Time since fire", tag= "D") +
  theme_bw() +
  theme(legend.position='none') + theme(text = element_text(size = 20))

ggpubr::ggarrange(plt1, plt2, plt3, plt4,
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = "right")

ggpubr::ggarrange(plt1, plt2,plt3,plt4)



















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
