library(readr)
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
library(car)
###
###               make sure to enable parallel computing for faster model building
###
###

##################################################################################
#   change data set here using different sources of fire histories              ##
TBF_long <- read_csv("Legacy effect/data/TBF_long_export_11_24_25.csv")
TBF_long$site_ID <- paste(TBF_long$site, TBF_long$ID, sep = "_")                ##
##################################################################################



############################################################################################################
###                Regional   climate variables (Annual + Monthly)
###
###         Define global mode according to Louthan et.al., 2022, Table 2.
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################



### Survival
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

GM_R_sur <- glmer(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + 
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
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
summary(sur_mod_R)   
stopCluster(cluster)

## treating site as fixed effect (if glmer fails to converge) ####
GM_R_sur_2 <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD + 
    site,
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
  GM_R_sur_2,
  cluster = cluster,
  trace   = 2
)
sur_mod_R_ls <- get.models(sur_dredge_R, 1)[[1]]
summary(sur_mod_R_ls)   
save(sur_mod_R_ls, file = "data/TBFxClimate/sur_mod_R_ls.Rdata")
stopCluster(cluster)

### growth

gr_subset_R_2 <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(logsize1, s.logsize0, s.TSF,s.sq.TSF, s.TBF, site, s.sq.T_RA,s.T_RA, s.P_RA, s.T_RC, 
                 s.T_RD, s.P_RH), all_vars(!is.na(.)))
GM_R_gr <- lmer(
  logsize1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF + s.T_RA + s.sq.T_RA + s.P_RA + s.T_RA:s.P_RA +
    s.T_RC + s.T_RD + s.P_RH +
    (1 | site),
  data = gr_subset_R_2,
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("gr_subset_R_2"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

gr_dredge_R <- MuMIn::dredge(
  GM_R_gr,
  cluster = cluster,
  trace   = 2
)
gr_mod_R <- get.models(gr_dredge_R, 1)[[1]]
summary(gr_mod_R)
car::Anova(gr_mod_R, type = 3)
stopCluster(cluster)
save(gr_mod_R, file = "data/TBFxClimate/gr_mod_R_ls.Rdata")

## prob fruiting

prep_subset_R <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))

GM_R_prep <- glmer(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.sq.T_RA +s.P_RA+ s.sq.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    (1|site),
  data = prep_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)   ### this likely won't work due to convergence failure

GM_R_prep <- glm(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.sq.T_RA +s.P_RA+ s.sq.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    site,
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
summary(prep_mod_R)   ## large eiigenvalue ratio value, rescale 

save(prep_mod_R, file = "data/TBFxClimate/prep_mod_R_ls.Rdata")

stopCluster(cluster)

### number of fruit

crep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, TSF, TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.)))

GM_R_crep <- lmer(
  logcrep1 ~ s.logsize0 + TSF * TBF + s.sq.TSF + s.T_RA + s.T_RH +
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
car::Anova(crep_mod_R, type = 3)
stopCluster(cluster)
save(crep_mod_R, file = "data/TBFxClimate/crep_mod_R_ls.Rdata")




