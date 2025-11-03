


########################################################################
#################################simulate graphs########################
########################################################################

TBF_long <- read.csv("data/TBFxClimate/TBF_long_10302025_with_Var.csv")


########################################################################
######## extract original values of the scaled variables ###############
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
sim_TSF <- c(0:16)
# Extract the mean and standard deviation used for scaling
mean_val <- mean(original_vector)
sd_val <- sd(original_vector)
# Reconstruct the original vector
scaled_sim_TSF <- (sim_TSF - mean_val) / sd_val

#### TBF
####
# Original vector
original_vector <- TBF_long$TBF
TBF <- sort(unique(original_vector), na.last = TRUE)
# Scale the vector
scaled_vector <- TBF_long$s.TBF
sort(unique(scaled_vector), na.last = TRUE)
# vector to used for simulation
sim_TBF <- c(1,15)   ### change accordingly
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

attr(terms(sur_mod_G), "term.labels")
attr(terms(gr_mod_G), "term.labels")
attr(terms(prep_mod_G), "term.labels")
attr(terms(crep_mod_G), "term.labels")

data_new1 <- as.data.frame((rep(mean(TBF_long$s.logsize0, na.rm=TRUE), 17*2)))
                           #,(rep(mean(TBF_long$s.logsize0, na.rm=TRUE), 17*2)) )

names(data_new1) <- "s.logsize0"
TSF.s <- sort(unique(TBF_long$s.TSF), na.last = NA)    # pull scaled TSF and TBF from data subset from which model was built
TBF.s <- unique(TBF_long$s.TBF)
TSF.sq.s <- sort(unique(TBF_long$s.sq.TSF), na.last = NA)
data_new1$TSF <- c(c(0:16),c(0:16))
data_new1$TBF <- c(rep(min(TBF), 17), rep(max(TBF), 17))    #### change max(TBF) accordingly
data_new1$s.TSF <- c(scaled_sim_TSF,scaled_sim_TSF)
data_new1$s.TSF <- as.matrix(data_new1$s.TSF)
data_new1$s.sq.TSF <- c(TSF.sq.s,TSF.sq.s)
data_new1$s.sq.TSF <- as.matrix(data_new1$s.sq.TSF)
data_new1$s.TBF <- c(rep(min(scaled_sim_TBF), 17), rep(max(scaled_sim_TBF), 17))
data_new1$s.TBF <- as.matrix(data_new1$s.TBF)
data_new1$newTBF[which(data_new1$s.TBF== min(scaled_sim_TBF))] <- "Short TBF"
data_new1$newTBF[which(data_new1$s.TBF== max(scaled_sim_TBF))] <- "Long TBF"

data_new1$site <- "GSP-BI"
data_new1$s.P_RD <- rep(mean(TBF_long$s.P_RD, na.rm=TRUE), 17*2)
data_new1$s.P_RD <- as.matrix(data_new1$s.P_RD)
data_new1$s.P_RH <- rep(mean(TBF_long$s.P_RH, na.rm=TRUE), 17*2)
data_new1$s.P_RH <- as.matrix(data_new1$s.P_RH)
data_new1$s.P_RA <- rep(mean(TBF_long$s.P_RA, na.rm=TRUE), 17*2)
data_new1$s.P_RA <- as.matrix(data_new1$s.P_RA)
data_new1$s.sq.P_RA <- rep(mean(TBF_long$s.sq.P_RA, na.rm=TRUE), 17*2)
data_new1$s.sq.P_RA <- as.matrix(data_new1$s.sq.P_RA)
data_new1$s.sq.T_RH <- rep(mean(TBF_long$s.sq.T_RH, na.rm=TRUE), 17*2)
data_new1$s.sq.T_RH <- as.matrix(data_new1$s.sq.T_RH)
data_new1$s.sq.T_RA <- rep(mean(TBF_long$s.sq.T_RA, na.rm=TRUE), 17*2)
data_new1$s.sq.T_RA <- as.matrix(data_new1$s.sq.T_RA)
data_new1$s.T_RA <- rep(mean(TBF_long$s.T_RA, na.rm=TRUE), 17*2)
data_new1$s.T_RA <- as.matrix(data_new1$s.T_RA)
data_new1$s.T_RD <- rep(mean(TBF_long$s.T_RD, na.rm=TRUE), 17*2)
data_new1$s.T_RD <- as.matrix(data_new1$s.T_RD)
data_new1$s.T_RH <- rep(mean(TBF_long$s.T_RH, na.rm=TRUE), 17*2)
data_new1$s.T_RH <- as.matrix(data_new1$s.T_RH)
data_new1$s.T_RC <- rep(mean(TBF_long$s.T_RC, na.rm=TRUE), 17*2)
data_new1$s.T_RC <- as.matrix(data_new1$s.T_RC)



data_new1$s.P_LA <- rep(mean(TBF_long$s.P_LA, na.rm=TRUE), 17*2)
data_new1$s.P_LA <- as.matrix(data_new1$s.P_LA)
data_new1$s.P_LH <- rep(mean(TBF_long$s.P_LH, na.rm=TRUE), 17*2)
data_new1$s.P_LH <- as.matrix(data_new1$s.P_LH)
data_new1$s.sq.T_LA <- rep(mean(TBF_long$s.sq.T_LA, na.rm=TRUE), 17*2)
data_new1$s.sq.T_LA <- as.matrix(data_new1$s.sq.T_LA)
data_new1$s.T_LA <- rep(mean(TBF_long$s.T_LA, na.rm=TRUE), 17*2)
data_new1$s.T_LA <- as.matrix(data_new1$s.T_LA)
data_new1$s.T_LD <- rep(mean(TBF_long$s.T_LD, na.rm=TRUE), 17*2)
data_new1$s.T_LD <- as.matrix(data_new1$s.T_LD)
data_new1$s.sq.P_LA <- rep(mean(TBF_long$s.sq.P_LA, na.rm=TRUE), 17*2)
data_new1$s.T_LH <- rep(mean(TBF_long$s.T_LH, na.rm=TRUE), 17*2)
data_new1$s.sq.P_LA <- rep(mean(TBF_long$s.sq.P_LA, na.rm=TRUE), 17*2)


################## sur (GLM) #################################

data_new1$gr_fit <- NA
data_new1$gr_se  <-NA
data_new1$gr_lwr <-  NA
data_new1$gr_upr <-NA

sur_pred <- predict.glm(sur_mod_G, data_new1, type = "response", se.fit = TRUE)
data_new1$sur_fit <- sur_pred$fit
data_new1$sur_se <- sur_pred$se.fit

data_new1$sur_lwr <- sur_pred$fit - qnorm(0.975) * sur_pred$se.fit
data_new1$sur_upr <- sur_pred$fit + qnorm(0.975) * sur_pred$se.fit



plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur_lwr, ymax = data_new1$sur_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  xlim(c(0, 16)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  theme(text = element_text(size = 20)) 
plt1
save(plt1, file = "figures/VFT_climate/sur_graph_TBF.Rdata")

################## gr  #################################

data_new1$gr <- merTools::predictInterval(gr_mod_G, data_new1, which= "fixed",level= 0.95,n.sims= 10000)
plt2 <- ggplot(data= data_new1, aes(x= TSF, y= gr$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 16.5)) +
  labs(y= "Growth", x = "Time since fire", tag= "B") + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 20)) 
plt2
save(plt2, file = "figures/VFT_climate/gr_graph_TBF.Rdata")

################## prep (glm)  #################################
data_new1$prep_fit <- NA
data_new1$prep_se  <-NA
data_new1$prep_lwr <-  NA
data_new1$prep_upr <-NA

data_new1$s.P_LH <- as.numeric(data_new1$s.P_LH)
data_new1$s.sq.T_LA <- as.numeric(data_new1$s.sq.T_LA)
data_new1$s.sq.TSF <- as.numeric(data_new1$s.sq.TSF)
data_new1$s.T_LA <- as.numeric(data_new1$s.T_LA)
data_new1$s.TBF <- as.numeric(data_new1$s.TBF)
data_new1$s.TSF <- as.numeric(data_new1$s.TSF)

prep_pred <- predict.glm(prep_mod_G, data_new1, type = "response", se.fit = TRUE)
data_new1$prep_fit <- prep_pred$fit
data_new1$prep_se <- prep_pred$se.fit

data_new1$prep_lwr <- prep_pred$fit - qnorm(0.975) * prep_pred$se.fit
data_new1$prep_upr <- prep_pred$fit + qnorm(0.975) * prep_pred$se.fit

####
data_new1$prep_fit_2 <- NA
data_new1$prep_se_2  <-NA
data_new1$prep_lwr_2 <-  NA
data_new1$prep_upr_2 <-NA

data_new1$s.P_RA <- as.numeric(data_new1$s.P_RA)
data_new1$s.P_RH <- as.numeric(data_new1$s.P_RH)
data_new1$s.sq.P_RA <- as.numeric(data_new1$s.sq.P_RA)
data_new1$s.sq.T_RA <- as.numeric(data_new1$s.sq.T_RA)
data_new1$s.sq.TSF <- as.numeric(data_new1$s.sq.TSF)
data_new1$s.T_RA <- as.numeric(data_new1$s.T_RA)
data_new1$s.T_RC <- as.numeric(data_new1$s.T_RC)
data_new1$s.T_RH <- as.numeric(data_new1$s.T_RH)
data_new1$s.TBF <- as.numeric(data_new1$s.TBF)
data_new1$s.TSF <- as.numeric(data_new1$s.TSF)


prep_pred_2 <- predict.glm(prep_mod_R_2, data_new1, type = "response", se.fit = TRUE)
data_new1$prep_fit_2 <- prep_pred_2$fit
data_new1$prep_se_2 <- prep_pred_2$se.fit

data_new1$prep_lwr_2 <- prep_pred_2$fit - qnorm(0.975) * prep_pred_2$se.fit
data_new1$prep_upr_2 <- prep_pred_2$fit + qnorm(0.975) * prep_pred_2$se.fit
#####

plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$prep_lwr, ymax = data_new1$prep_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  xlim(c(0, 16)) +
  labs(y= "prep", x = "Time since fire", tag= "C") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  theme(text = element_text(size = 20)) 
plt3
save(plt3, file = "figures/VFT_climate/prep_graph_TBF.Rdata")

######## crep ############
data_new1$crep <- merTools::predictInterval(crep_mod_G, data_new1, which= "fixed",level= 0.95,n.sims= 10000)
plt4 <- ggplot(data= data_new1, aes(x= TSF, y= crep$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$crep$lwr, ymax = data_new1$crep$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 16.5)) +
  labs(y= "crep", x = "Time since fire", tag= "D") + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 20)) 
plt4
save(plt4, file = "figures/VFT_climate/crep_graph_TBF.Rdata")

#################################################################
#################################################################
###################### make "THE" graph #########################
#################################################################
#################################################################

library(ggplot2)
ggpubr::ggarrange(plt1, plt2, plt3, plt4,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")


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














data_new1 <- as.data.frame((rep(mean(recruit_df_5_14_2025$num_fr, na.rm=TRUE), 17*2)))
names(data_new1) <- "num_fr"
data_new1$TSF <- rep(seq(0, 16), 2)
data_new1 $TBF <- c(rep(1, 17), rep(15, 17))
data_new1$site <- "GSP-LI "
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
  