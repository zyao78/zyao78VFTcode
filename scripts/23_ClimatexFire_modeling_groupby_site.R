
library(dplyr)
library(lme4)
library(popbio)
library(parallel)
library(doParallel)
library(foreach)
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)
library("performance")
library(car)

###########################################################
############# try modeling for each site ##################


sur_R_overtopped <- TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD, startyear), all_vars(!is.na(.))) %>% 
  filter(site %in% c("CH", "B1", "B2"))

GM_R_sur <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD + site,
  data = sur_R_overtopped,
  family = "binomial", 
  na.action = "na.fail"
)



n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_R_overtopped"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_R <- MuMIn::dredge(
  GM_R_sur,
  cluster = cluster,
  trace   = 2
)
overtopped_mod_R <- get.models(sur_dredge_R, 1)[[1]]
summary(sur_mod_R_ls)   

stopCluster(cluster)



sur_L_overtopped <- TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                 s.P_LH, s.P_LD, startyear), all_vars(!is.na(.))) %>% 
  filter(site %in% c("CH", "B1", "B2"))
GM_L_sur <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + site,
  data = sur_L_overtopped,
  family = "binomial", 
  na.action = "na.fail"
)
n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_L_overtopped"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})
sur_dredge_L <- MuMIn::dredge(
  GM_L_sur,
  cluster = cluster,
  trace   = 2
)
overtopped_mod_L <- get.models(sur_dredge_L, 1)[[1]]
summary(overtopped_mod_L)   
summary(overtopped_mod_R)   

overtopped_mod <- overtopped_mod_L

########################## not overtopped ########################

sur_R_notovertopped <- TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD, startyear), all_vars(!is.na(.))) %>% 
  filter(site %in% c("CM", "GSP-LI", "GSP-BI", "IA", "ME"))

GM_R_sur <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD,
  data = sur_R_notovertopped,
  family = "binomial", 
  na.action = "na.fail"
)



n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_R_notovertopped"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_R <- MuMIn::dredge(
  GM_R_sur,
  cluster = cluster,
  trace   = 2
)
notovertopped_mod_R <- get.models(sur_dredge_R, 1)[[1]]
summary(sur_mod_R_ls)   

stopCluster(cluster)



sur_L_notovertopped <- TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                 s.P_LH, s.P_LD, startyear), all_vars(!is.na(.))) %>% 
  filter(site %in% c("CM", "GSP-LI", "GSP-BI", "IA", "ME"))
GM_L_sur <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + site,
  data = sur_L_notovertopped,
  family = "binomial", 
  na.action = "na.fail"
)
n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_L_notovertopped"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})
sur_dredge_L <- MuMIn::dredge(
  GM_L_sur,
  cluster = cluster,
  trace   = 2
)
notovertopped_mod_L <- get.models(sur_dredge_L, 1)[[1]]
summary(notovertopped_mod_L)   
summary(notovertopped_mod_R)   

notovertopped_mod <- notovertopped_mod_L

############ graph ####################
data_new1 <- read_csv("data/TBFxClimate/data_new1_2024.csv")

data_new1$s.P_LD <- rep(mean(TBF_long$s.P_LD, na.rm=TRUE), nrow(data_new1))
data_new1$s.P_LD <- as.numeric(data_new1$s.P_LD)
data_new1$site <- "B2"


data_new1$sur_fit <- NA
data_new1$sur_se  <-NA
data_new1$sur_lwr <-  NA
data_new1$sur_upr <-NA

sur_pred <- predict.glm(overtopped_mod, data_new1, type = "response", se.fit = TRUE)
data_new1$sur_fit <- sur_pred$fit
data_new1$sur_se <- sur_pred$se.fit

data_new1$sur_lwr <- sur_pred$fit - qnorm(0.975) * sur_pred$se.fit
data_new1$sur_upr <- sur_pred$fit + qnorm(0.975) * sur_pred$se.fit



plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur_lwr, ymax = data_new1$sur_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  xlim(c(0, 10)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  theme(text = element_text(size = 20)) 
plt1





