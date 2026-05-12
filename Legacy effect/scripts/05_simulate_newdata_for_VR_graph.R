

########################################################################
#################################simulate graphs########################
########################################################################

library(merTools)
library(ggplot2)


load("Legacy effect/data/TBFxClimate/VR_mod linear mixed scale.Rdata")# loads in the VR functions
########################################################################
######## TSFxTBF response for each vital rate            ###############
########################################################################

#### TSF
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
original_vector <- (TBF_long$TSF)^2
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
sim_TBF <- seq(from = 0, to = max(TBF, na.rm = TRUE))   ### change accordingly
# Extract the mean and standard deviation used for scaling
mean_val <- mean(original_vector)
sd_val <- sd(original_vector)
# Reconstruct the original vector
scaled_sim_TBF <- (sim_TBF - mean_val) / sd_val

######################################################################################
######################################################################################
######################################################################################
####################### build data.new ###############################################
######################################################################################
######################################################################################


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
data_new1$s.sq.TSF <- as.numeric(data_new1$s.sq.TSF)
data_new1$s.TBF <- c(rep(scaled_sim_TBF[3], nrow), rep(max(scaled_sim_TBF), nrow))   ### modify if needed to set different TBF
data_new1$newTBF[1:11] <- "Short TBF"
data_new1$newTBF[12:22] <- "Long TBF"

data_new1$site <- "GSP-LI"
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



################## sur (GLM) #################################

sur_pred <- predict.glm(survival_mod, data_new1, type = "link", se.fit = TRUE)

sur_lwr_link <- sur_pred$fit + qnorm(0.025) * sur_pred$se.fit
sur_upr_link <- sur_pred$fit + qnorm(0.975) * sur_pred$se.fit

data_new1$sur_fit <- survival_mod$family$linkinv(sur_pred$fit)
data_new1$sur_lwr <- survival_mod$family$linkinv(sur_lwr_link)
data_new1$sur_upr <- survival_mod$family$linkinv(sur_upr_link)

#data_new1$sur <- merTools::predictInterval(survival_mod, data_new1, type = "probability",level= 0.95,n.sims= 2000)


plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = sur_lwr, ymax = sur_upr, fill= newTBF), alpha = 0.1) + 
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
prep_pred <- predict.glm(prep_mod, data_new1, type = "link", se.fit = TRUE)

prep_lwr_link <- prep_pred$fit + qnorm(0.025) * prep_pred$se.fit
prep_upr_link <- prep_pred$fit + qnorm(0.975) * prep_pred$se.fit

data_new1$prep_fit <- prep_mod$family$linkinv(prep_pred$fit)
data_new1$prep_lwr <- prep_mod$family$linkinv(prep_lwr_link)
data_new1$prep_upr <- prep_mod$family$linkinv(prep_upr_link)


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
  labs(y= "Probability of Survival", x = "TSF", tag= "A",color = NULL, fill = NULL) +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 16)) 
plt2 <- ggplot(data= data_new1, aes(x= TSF, y= gr$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= newTBF), alpha = 0.1) + 
  scale_x_continuous(breaks = 0:10) +
  labs(y= "Growth", x = "TSF", tag= "B",color = NULL, fill = NULL) + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 16)) 
plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$prep_lwr, ymax = data_new1$prep_upr, fill= newTBF), alpha = 0.1) + 
  ylim(0,1) +
  labs(y= "Probability of reproduction", x = "TSF", tag= "C",color = NULL, fill = NULL) +
  theme_bw()  +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 16)) 
plt4 <- ggplot(data= data_new1, aes(x= TSF, y= crep$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$crep$lwr, ymax = data_new1$crep$upr, fill= newTBF), alpha = 0.1) + 
  scale_x_continuous(breaks = 0:10) +
  labs(y= "Number of fruit", x = "TSF", tag= "D",color = NULL, fill = NULL) + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 16)) 

ggpubr::ggarrange(plt1, plt2, plt3, plt4,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")





########### recruit ################### 
recruit_df <- read_csv("Legacy effect/data/recruit_df_11_24_2025.csv")
data_new1$log.fr <- rep(mean(recruit_df$log.fr, na.rm=TRUE), nrow*2)

rec_pred <- predict(recruit_mod, data_new1, type = "link", se.fit = TRUE)

rec_lwr_link <- rec_pred$fit + qnorm(0.025) * rec_pred$se.fit
rec_upr_link <- rec_pred$fit + qnorm(0.975) * rec_pred$se.fit

data_new1$rec_fit <- recruit_mod$family$linkinv(rec_pred$fit)
data_new1$rec_lwr <- recruit_mod$family$linkinv(rec_lwr_link)
data_new1$rec_upr <- recruit_mod$family$linkinv(rec_upr_link)

plt5 <- ggplot(data= data_new1, aes(x= TSF, y= rec_fit)) +
  geom_line(aes(color= newTBF)) + 

  geom_ribbon(aes(ymin = data_new1$rec_lwr, ymax = data_new1$rec_upr, 
                  fill= newTBF), alpha = 0.1) + 
  labs(y= "number of recruit", x = "Time since fire",  color= "", fill= "") + 
  theme_bw()  + 
  xlim(c(0, 11)) + 
  theme(legend.position = "right") +
  theme(text = element_text(size = 20)) 
plt5



###################################################################################
###################################################################################
###################################################################################
##############################  Annual climate interaction ########################
###################################################################################
###################################################################################
###################################################################################









data_new1 <- read.csv("data/TBFxClimate/data_new1_2024.csv")

TBF <- sort(unique(original_vector), na.last = TRUE)
s.TBF <- max(sort(unique(TBF_long$s.TBF), na.last = TRUE))
s.TSF <- max(sort(unique(TBF_long$s.TSF), na.last = TRUE))
s.sq.TSF <- max(sort(unique(TBF_loRplotng$s.sq.TSF), na.last = TRUE))
nrow <- length(c(0:max(TBF_long$TSF)))
data_new1$s.TSF <- s.TSF
data_new1$s.TBF <- s.TBF
data_new1$s.sq.TSF <- s.sq.TSF

TLA <- quantile(TBF_long$s.T_LA, probs = seq(0, 1, length.out = 50), na.rm = TRUE)
TLA_sq <- quantile(TBF_long$s.sq.T_LA, probs = seq(0, 1, length.out = 11), na.rm = TRUE)

PLA <- quantile(TBF_long$s.P_LA, probs = c(0.10, 0.90), na.rm = TRUE)
PLA_sq <- quantile(TBF_long$s.sq.P_LA, probs = c(0.10, 0.90), na.rm = TRUE)

TRA <- quantile(TBF_long$s.T_RA, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
TRA_sq <- quantile(TBF_long$s.sq.T_RA, probs = seq(0, 1, length.out = 11), na.rm = TRUE)

PRA <- quantile(TBF_long$s.P_RA, probs = c(0.10, 0.90), na.rm = TRUE)
PRA_sq <- quantile(TBF_long$s.sq.P_RA, probs = c(0.10, 0.90), na.rm = TRUE)


data_new1$s.P_LA <- c(rep(min(PLA), nrow), rep(max(PLA), nrow))
data_new1$s.sq.P_LA <- c(rep(min(PLA_sq), nrow), rep(max(PLA_sq), nrow))
data_new1$s.T_LA <- c(TLA,TLA)
data_new1$s.sq.T_LA <-c(TLA_sq,TLA_sq)

data_new1$s.P_RA <- c(rep(min(PRA), nrow), rep(max(PLA), nrow))
data_new1$s.sq.P_RA <- c(rep(min(PRA_sq), nrow), rep(max(PRA_sq), nrow))
data_new1$s.T_RA <- c(TRA,TRA)
data_new1$s.sq.T_RA <-c(TRA_sq,TRA_sq)

data_new1$precip[which(data_new1$s.P_LA== min(PLA))] <- "low annual precip"
data_new1$precip[which(data_new1$s.P_LA== max(PLA))] <- "high annual precip"

data_new1$gr <- merTools::predictInterval(growth_mod, data_new1, which= "fixed",level= 0.95,n.sims= 2000)


ggplot(data= data_new1, aes(x= s.T_LA, y= gr$fit)) +
  geom_line(aes(color= precip))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= precip), alpha = 0.1) + 
  labs(y= "Growth", x = "annual temperature") + 
  theme_bw() +  theme(text = element_text(size = 20)) 


prep_pred <- predict.glm(prep_mod, data_new1, type = "response", se.fit = TRUE)
data_new1$prep_fit <- prep_pred$fit
data_new1$prep_se <- prep_pred$se.fit

data_new1$prep_lwr <- prep_pred$fit - qnorm(0.975) * prep_pred$se.fit
data_new1$prep_upr <- prep_pred$fit + qnorm(0.975) * prep_pred$se.fit

ggplot(data= data_new1, aes(x= s.T_RA, y= prep_fit)) +
  geom_line(aes(color= precip)) + 
  geom_ribbon(aes(ymin = data_new1$prep_lwr, ymax = data_new1$prep_upr, fill= precip), alpha = 0.1) + 
  #ylim(0,1) +
  labs(y= "prep", x = "annual temperature") +
  theme_bw() 
  theme(legend.position = "right") +
  theme(text = element_text(size = 20)) 








data_new1 <- as.data.frame((rep(mean(recruit_df_5_14_2025$num_fr, na.rm=TRUE), nrow*2)))
names(data_new1) <- "num_fr"

data_new1$num_recruit <- merTools::predictInterval(recruit_mod_g, data_new1, which= "fixed", type= "probability", 
                                           level= 0.95,n.sims= 10000)

data_new1$num_fruit_sim <- merTools::predictInterval(frt_mod, data_new1, which= "fixed", type= "linear.prediction", 
                                                   level= 0.95,n.sims= 10000)

data_new1$newTBF <- NA
data_new1$newTBF[which(data_new1$TBF== 1)] <- "Short TBF"
data_new1$newTBF[which(data_new1$TBF== 15)] <- "Long TBF"

# graph

ggplot(data= data_new1, aes(x= TSF, y= num_recruit$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$num_recruit$lwr, ymax = data_new1$num_recruit$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 17.5)) +
  labs(y= "recruit", x = "Time since fire", tag= "A") +
  theme_bw() + theme() +
  theme(text = element_text(size = 20)) 

ggplot(data= data_new1, aes(x= TSF, y= num_fruit_sim$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$num_fruit_sim$lwr, ymax = data_new1$num_fruit_sim$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 12.5)) +
  labs(y= "num_fruit", x = "Time since fire", tag= "A") +
  theme_bw() + theme() +
  theme(text = element_text(size = 20)) 


look <- TBF_long %>%
  + filter(is.na(rep1))%>%
  