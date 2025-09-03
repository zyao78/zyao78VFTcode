
install.packages("popbio")
install.packages("parallel")
install.packages("doParallel")
install.packages("foreach")

library(popbio)
library(parallel)
library(doParallel)
library(foreach)
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)

###
###               make sure to enable parallel computing for faster model building
###
###




#### model selection

### create clim variables   (if not already created)
TBF_long$s.T_RA <- NA
TBF_long$s.T_RA <- scale(TBF_long$T_RA) 
TBF_long$s.P_RA <- NA
TBF_long$s.P_RA <- scale(TBF_long$P_RA) 
TBF_long$s.P_RW <- NA
TBF_long$s.P_RW <- scale(TBF_long$P_RW) 
TBF_long$s.P_RD <- NA
TBF_long$s.P_RD <- scale(TBF_long$P_RD) 
TBF_long$s.P_RH <- NA
TBF_long$s.P_RH <- scale(TBF_long$P_RH) 
TBF_long$s.T_RH<- NA
TBF_long$s.T_RD<- NA
TBF_long$s.T_RW<- NA
TBF_long$s.T_RC<- NA
TBF_long$s.T_RH <- scale(TBF_long$T_RH) 
TBF_long$s.T_RC <- scale(TBF_long$T_RC) 
TBF_long$s.T_RD <- scale(TBF_long$T_RD) 
TBF_long$s.T_RW <- scale(TBF_long$T_RW) 



##
TBF_long <- read_csv("data/TBFxClimate/TBF_long.csv") 
colnames(TBF_long)


###
###                            Regional  climate variables (Annual + Monthly)
###



### Survival
###

## annual clim var
sur_subset_RA <- 
  TBF_long %>% 
  filter_at(vars(consur0_1, s.logsize0, TSF, TBF,site, s.T_RA, s.P_RA), all_vars(!is.na(.)))
GM_RA_sur <- glmer(
  consur0_1 ~ s.logsize0 + TSF * TBF +
    s.T_RA + s.P_RA + s.T_RA:s.P_RA +
    I(s.T_RA^2) + I(s.P_RA^2) + I(s.T_RA^2 * s.P_RA^2) +
    (1 | site),
  data = sur_subset_RA,
  family = "binomial", 
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_RA"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})
sur_dredge_RA <- MuMIn::dredge(
  GM_RA_sur,
  cluster = cluster,
  trace   = 2
)
sur_mod_RA <- get.models(sur_dredge_RA, 1)[[1]]
summary(sur_mod_RA)
stopCluster(cluster)

### monthly clim var


sur_subset_RM <- 
  TBF_long %>% 
  filter_at(vars(consur0_1, s.logsize0, TSF, TBF,site, s.P_RD, s.P_RW, s.P_RH, s.T_RC, s.T_RH, 
                          s.T_RD, s.T_RW), all_vars(!is.na(.)))

GM_RM_sur <- glmer(
  consur0_1 ~ s.logsize0 + TSF * TBF + 
    s.T_RC + I(s.T_RC^2) +
    s.T_RW + s.P_RW + s.T_RW * s.P_RW + I(s.P_RW^2) +
    s.T_RD + s.P_RD + I(s.P_RD^2) + s.P_RD * s.T_RD +
    s.T_RH + s.P_RH + s.T_RH * s.P_RH + I(s.T_RH^2) +
    (1 | site), 
  data = sur_subset_RM, 
  family = "binomial", 
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_RM"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})
sur_dredge_RM <- MuMIn::dredge(
  GM_RM_sur,
  cluster = cluster,
  trace   = 2
)
sur_mod_RM <- get.models(sur_dredge_RM, 1)[[1]]
summary(sur_mod_RM)
stopCluster(cluster)




### growth

gr_subset_A <- 
  TBF_long %>% 
  dplyr::filter(across(c( logsize1, s.logsize0, TSF, TBF,site, s.T_RA, s.P_RA), ~ !is.na(.)))



## save env
save(list = ls(), file = "env_snapshot.RData")

### plotting 
ggplot(P_RA, aes(x = factor(startyear), y = prec, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "startyear", y = "annual cum Prec", fill = "site")+
  ggtitle("annual reg prec")
