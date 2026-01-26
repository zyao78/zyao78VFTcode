
library(merTools)

############ remove attributes #####################################################
load("Legacy effect/data/TBF_long_landscape_with_attr.Rdata")
load("Legacy effect/data/TBFxClimate/VR_mod_linear.Rdata")
load("Legacy effect/data/TBFxClimate/VR_mod_qd_delta AICc.Rdata")

TBF_long_noattr <- read_csv("Legacy effect/data/TBF_long_export_11_24_25.csv")
TBF_long_noattr <- TBF_long_noattr %>%
  mutate(across(c("s.logsize0", "s.sq.TSF", "s.TSF", "s.TBF"), as.numeric))
TBF_long$s.logsize0 <- scale(TBF_long$logsize0) 

survival_mod <- update(survival_mod,data=TBF_long_noattr %>% 
                         filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                                        s.P_LH, s.P_LD), all_vars(!is.na(.))))
growth_mod <- update(growth_mod,data=TBF_long_noattr %>% 
                       filter_at(vars(s.logsize0,logsize1,s.P_LA ,s.P_LH,s.sq.T_LA,s.T_LA,s.T_LC,s.T_LD , s.TSF), all_vars(!is.na(.))))

prep_mod <- update(prep_mod,data=TBF_long_noattr %>% 
                     filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                                    s.P_RH, ), all_vars(!is.na(.))))
crep_mod <- update(crep_mod,data=TBF_long_noattr %>% 
                     filter_at(vars(logcrep1, s.logsize0, TSF, TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.))))

prep_mod
prep_mod_R


########### make the four_paneled vital rate graphs #####################

#### make newData
####
# Original vector
original_vector <- TBF_long$TSF
TSF <- sort(unique(original_vector), na.last = TRUE)
# Scale the vector
scaled_vector <- TBF_long$s.TSF
sort(unique(scaled_vector), na.last = TRUE)
# vector to used for simulation
sim_TSF <- c(0:max(original_vector))
# Extract the mean and standard deviation used for scaling
mean_val <- mean(original_vector)
sd_val <- sd(original_vector)
# Reconstruct the original vector
scaled_sim_TSF <- (sim_TSF - mean_val) / sd_val

#### s.sq.TSF
####
# Original vector
original_vector <- TBF_long$sq.TSF
sq.TSF <- sort(unique(original_vector), na.last = TRUE)
# Scale the vector
scaled_vector <- TBF_long$s.TSF
sort(unique(scaled_vector), na.last = TRUE)
# vector to used for simulation
sim_sq.TSF <- c(0:max(TSF))^2
# Extract the mean and standard deviation used for scaling
mean_val <- mean(original_vector)
sd_val <- sd(original_vector)
# Reconstruct the original vector
scaled_sim_s.sq.TSF <- (sim_TSF - mean_val) / sd_val

#### TBF
####
# Original vector
original_vector <- TBF_long$TBF
TBF <- sort(unique(original_vector), na.last = TRUE)
# Scale the vector
scaled_vector <- TBF_long$s.TBF
sort(unique(scaled_vector), na.last = TRUE)
# vector to used for simulation
sim_TBF <- c(0,max(TBF))   ### change accordingly
# Extract the mean and standard deviation used for scaling
mean_val <- mean(original_vector)
sd_val <- sd(original_vector)
# Reconstruct the original vector
scaled_sim_TBF <- (sim_TBF - mean_val) / sd_val
############################################################

data_new1 <- as.data.frame((rep(mean(TBF_long$s.logsize0, na.rm=TRUE), (length(TSF)+1)*2)))
#,(rep(mean(TBF_long$s.logsize0, na.rm=TRUE), 17*2)) )

names(data_new1) <- "s.logsize0"
TSF.s <- sort(unique(TBF_long$s.TSF), na.last = NA)    # pull scaled TSF and TBF from data subset from which model was built
TBF.s <- unique(TBF_long$s.TBF)
TSF.sq.s <- sort(unique(TBF_long$s.sq.TSF), na.last = NA)
data_new1$TSF <- c(c(0:max(TSF)),c(0:max(TSF)))
nrow <- length(c(0:max(TSF)))           #### modify this if needed
data_new1$TBF <- c(rep(min(TBF), nrow), rep(max(TBF), nrow))    #### change max(TBF) accordingly
data_new1$s.TSF <- c(scaled_sim_TSF,scaled_sim_TSF)
data_new1$s.sq.TSF <- c(scaled_sim_s.sq.TSF,scaled_sim_s.sq.TSF)
data_new1$s.TBF <- c(rep(min(scaled_sim_TBF), nrow), rep(max(scaled_sim_TBF), nrow))
data_new1$newTBF[which(data_new1$s.TBF== min(scaled_sim_TBF))] <- "Short TBF"
data_new1$newTBF[which(data_new1$s.TBF== max(scaled_sim_TBF))] <- "Long TBF"

data_new1$site <- "GSP-BI"
data_new1$s.P_RD <- rep(mean(TBF_long$s.P_RD, na.rm=TRUE), nrow*2)
data_new1$s.P_RH <- rep(mean(TBF_long$s.P_RH, na.rm=TRUE), nrow*2)
data_new1$s.P_RA <- rep(mean(TBF_long$s.P_RA, na.rm=TRUE), nrow*2)
data_new1$s.sq.P_RA <- rep(mean(TBF_long$s.sq.P_RA, na.rm=TRUE), nrow*2)
data_new1$s.sq.T_RH <- rep(mean(TBF_long$s.sq.T_RH, na.rm=TRUE), nrow*2)
data_new1$s.sq.T_RA <- rep(mean(TBF_long$s.sq.T_RA, na.rm=TRUE), nrow*2)
data_new1$s.T_RA <- rep(mean(TBF_long$s.T_RA, na.rm=TRUE), nrow*2)
data_new1$s.T_RD <- rep(mean(TBF_long$s.T_RD, na.rm=TRUE), nrow*2)
data_new1$s.T_RH <- rep(mean(TBF_long$s.T_RH, na.rm=TRUE), nrow*2)
data_new1$s.T_RC <- rep(mean(TBF_long$s.T_RC, na.rm=TRUE), nrow*2)
data_new1$s.P_LD <- rep(mean(TBF_long$s.P_LD, na.rm=TRUE), nrow*2)

data_new1$s.P_LA <- rep(mean(TBF_long$s.P_LA, na.rm=TRUE), nrow*2)
data_new1$s.P_LH <- rep(mean(TBF_long$s.P_LH, na.rm=TRUE), nrow*2)
data_new1$s.sq.T_LA <- rep(mean(TBF_long$s.sq.T_LA, na.rm=TRUE), nrow*2)
data_new1$s.T_LA <- rep(mean(TBF_long$s.T_LA, na.rm=TRUE), nrow*2)
data_new1$s.T_LD <- rep(mean(TBF_long$s.T_LD, na.rm=TRUE), nrow*2)
data_new1$s.sq.P_LA <- rep(mean(TBF_long$s.sq.P_LA, na.rm=TRUE), nrow*2)
data_new1$s.T_LH <- rep(mean(TBF_long$s.T_LH, na.rm=TRUE), nrow*2)
data_new1$s.sq.P_LA <- rep(mean(TBF_long$s.sq.P_LA, na.rm=TRUE), nrow*2)
data_new1$s.T_LC <- rep(mean(TBF_long$s.T_LC, na.rm=TRUE), nrow*2)
data_new1$s.sq.T_LH <- rep(mean(TBF_long$s.sq.T_LH, na.rm=TRUE), nrow*2)

save(data_new1, file = "Legacy effect/data/TBFxClimate/data_new1_1_21_26.RData")

#################### sur #######################################################
data_new1$sur_fit <- NA
data_new1$sur_se  <-NA
data_new1$sur_lwr <-  NA
data_new1$sur_upr <-NA

sur_pred <- predict(survival_mod, data_new1, type = "response", se.fit = TRUE)
data_new1$sur_fit <- sur_pred$fit
data_new1$sur_se <- sur_pred$se.fit

data_new1$sur_lwr <- sur_pred$fit - qnorm(0.975) * sur_pred$se.fit
data_new1$sur_upr <- sur_pred$fit + qnorm(0.975) * sur_pred$se.fit



plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur_lwr, ymax = data_new1$sur_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 
plt1

################## gr  #################################

data_new1$gr <- merTools::predictInterval(growth_mod, data_new1, which= "fixed",level= 0.95,n.sims= 2000)
plt2 <- ggplot(data= data_new1, aes(x= TSF, y= gr$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= newTBF), alpha = 0.1) + 
  scale_x_continuous(breaks = 0:10) +
  labs(y= "Growth", x = "Time since fire", tag= "B") + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 20)) 
plt2

################## prep (glm)  #################################
data_new1$prep_fit <- NA
data_new1$prep_se  <-NA
data_new1$prep_lwr <-  NA
data_new1$prep_upr <-NA




prep_pred <- predict(prep_mod, data_new1, type = "response", se.fit = TRUE)
data_new1$prep_fit <- prep_pred$fit
data_new1$prep_se <- prep_pred$se.fit

data_new1$prep_lwr <- prep_pred$fit - qnorm(0.975) * prep_pred$se.fit
data_new1$prep_upr <- prep_pred$fit + qnorm(0.975) * prep_pred$se.fit

plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$prep_lwr, ymax = data_new1$prep_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  labs(y= "probability of reproduction", x = "Time since fire", tag= "C") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 
plt3

######## crep ############
data_new1$crep <- merTools::predictInterval(crep_mod, data_new1, which= "fixed",level= 0.95,n.sims= 1000)
plt4 <- ggplot(data= data_new1, aes(x= TSF, y= crep$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$crep$lwr, ymax = data_new1$crep$upr, fill= newTBF), alpha = 0.1) + 
  scale_x_continuous(breaks = 0:10) +
  labs(y= "number of fruit", x = "Time since fire", tag= "D") + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 20)) 
plt4

#################################################################
#################################################################
###################### make "THE" graph #########################
#################################################################
#################################################################

library(ggplot2)

plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur_lwr, ymax = data_new1$sur_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "Survival", x = "Time since fire", tag= "A",color = NULL, fill = NULL) +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 16)) 
plt2 <- ggplot(data= data_new1, aes(x= TSF, y= gr$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= newTBF), alpha = 0.1) + 
  scale_x_continuous(breaks = 0:10) +
  labs(y= "Growth", x = "Time since fire", tag= "B",color = NULL, fill = NULL) + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 16)) 
plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$prep_lwr, ymax = data_new1$prep_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  labs(y= "probability of reproduction", x = "Time since fire", tag= "C",color = NULL, fill = NULL) +
  theme_bw()  +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 16)) 
plt4 <- ggplot(data= data_new1, aes(x= TSF, y= crep$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$crep$lwr, ymax = data_new1$crep$upr, fill= newTBF), alpha = 0.1) + 
  scale_x_continuous(breaks = 0:10) +
  labs(y= "number of fruit", x = "Time since fire", tag= "D",color = NULL, fill = NULL) + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 16)) 

ggpubr::ggarrange(plt1, plt2, plt3, plt4,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")

save.image(file = "my_environment_1_22.RData")
