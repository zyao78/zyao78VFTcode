
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
###                 Regional  climate variables (Annual + Monthly)
###
###         Define global mode according to Louthan et.al., 2022, Table 2.
###
###
###
###



### Survival
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(consur0_1, s.logsize0, TSF, TBF, site, s.T_RA,s.T_RH, s.T_RD,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))
GM_R_sur <- glmer(
  consur0_1 ~ s.logsize0 + TSF * TBF + I(TSF^2)+
    s.T_RA + s.T_RH + I(s.T_RH^2) + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD + 
    (1 | site),
  data = sur_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_R <- MuMIn::dredge(
  GM_R_sur,
  cluster = cluster,
  trace   = 2
)
sur_mod_R <- get.models(sur_dredge_R, 1)[[1]]
summary(sur_mod_R)   ## large eigenvalue warning, rescale numeric variables

dfs <- sur_subset_R
dfs[,17:ncol(dfs)] <- scale(dfs[,17:ncol(dfs)]) ## scale num variables (not including consur_0_1)
sur_mod_R_s <- update(sur_mod_R,data=dfs)
summary(sur_mod_R_s)


stopCluster(cluster)

### growth

gr_subset_R <- 
  TBF_long %>% 
  filter_at(vars(logsize1, s.logsize0, TSF, TBF, site, s.T_RA, s.P_RA, s.T_RC, 
                 s.T_RD, s.P_RH), all_vars(!is.na(.)))
GM_R_gr <- lmer(
  logsize1 ~ s.logsize0 + TSF * TBF + I(TSF^2)+ s.T_RA + I(s.T_RA^2) + s.P_RA + s.T_RA:s.P_RA +
    s.T_RC + s.T_RD + s.P_RH +
    (1 | site),
  data = gr_subset_R,
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("gr_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

gr_dredge_R <- MuMIn::dredge(
  GM_R_gr,
  cluster = cluster,
  trace   = 2
)
gr_mod_R <- get.models(gr_dredge_R, 1)[[1]]
summary(gr_mod_R)
stopCluster(cluster)

## prob fruiting

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, TSF, TBF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))
GM_R_prep <- glmer(
  prep1 ~ s.logsize0 + TSF * TBF + I(TSF^2)+
    s.T_RA + I(s.T_RA^2) +s.P_RA+ I(s.P_RA^2) + s.T_RA:s.P_RA+ I(s.T_RA^2):I(s.P_RA^2) +
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    (1 | site),
  data = prep_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("prep_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

prep_dredge_R <- MuMIn::dredge(
  GM_R_prep,
  cluster = cluster,
  trace   = 2
)
prep_mod_R <- get.models(prep_dredge_R, 1)[[1]]
summary(prep_mod_R)
stopCluster(cluster)

### number of fruit

crep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, TSF, TBF, site, s.T_RA, s.T_RH), all_vars(!is.na(.)))

GM_R_crep <- lmer(
  logcrep1 ~ s.logsize0 + TSF * TBF + I(TSF^2)+ s.T_RA + s.T_RH +
    (1 | site),
  data = crep_subset_R,
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("crep_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

crep_dredge_R <- MuMIn::dredge(
  GM_R_crep,
  cluster = cluster,
  trace   = 2
)
crep_mod_R <- get.models(crep_dredge_R, 1)[[1]]
summary(crep_mod_R)
stopCluster(cluster)

## 

## save env
save(list = ls(), file = "env_snapshot.RData")

### plotting 
ggplot(P_RA, aes(x = factor(startyear), y = prec, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "startyear", y = "annual cum Prec", fill = "site")+
  ggtitle("annual reg prec")
