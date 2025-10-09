
install.packages("popbio")
install.packages("parallel")
install.packages("doParallel")
install.packages("carData")
install.packages("performance")
install.packages("glmm.hp")

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
library(performance)
library(car)
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

TBF_long$sq.P_RA <- (TBF_long$P_RA)^2
TBF_long$sq.T_RA <- (TBF_long$T_RA)^2
TBF_long$sq.T_RH <- (TBF_long$T_RH)^2
TBF_long$s.sq.T_RH <- scale(TBF_long$sq.T_RH)
TBF_long$sq.TSF <- (TBF_long$TSF)^2
TBF_long$s.sq.P_RA <- scale(TBF_long$sq.P_RA)
TBF_long$s.sq.T_RA <- scale(TBF_long$sq.T_RA)
TBF_long$s.TSF <- scale(TBF_long$TSF)
TBF_long$s.sq.TSF <- scale(TBF_long$sq.TSF)
TBF_long$s.TBF <- scale(TBF_long$TBF)

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
  filter_at(vars(consur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

GM_R_sur <- glmer(
  consur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD + 
    (1 | site),
  data = sur_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)
## treating site as fixed effect
GM_R_sur_2 <- glm(
  consur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD + 
    site,
  data = sur_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)

summary(GM_R_sur)


n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_R_2 <- MuMIn::dredge(
  GM_R_sur_2,
  cluster = cluster,
  trace   = 2
)
sur_mod_R_2 <- get.models(sur_dredge_R_2, 1)[[1]]
summary(sur_mod_R_2)   

dfs <- sur_subset_R
dfs[,17:ncol(dfs)] <- scale(dfs[,17:ncol(dfs)]) ## scale num variables (not including consur_0_1)
sur_mod_R_s <- update(sur_mod_R,data=dfs)
summary(sur_mod_R_s)

cc <- check_collinearity(sur_mod_R)
cc


stopCluster(cluster)

### growth

gr_subset_R_2 <- 
  TBF_long[which(TBF_long$consur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(logsize1, s.logsize0, s.TSF,s.sq.TSF, s.TBF, site, s.sq.T_RA,s.T_RA, s.P_RA, s.T_RC, 
                 s.T_RD, s.P_RH), all_vars(!is.na(.)))
GM_R_gr <- lmer(
  logsize1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF + s.T_RA + s.sq.T_RA + s.P_RA + s.T_RA:s.P_RA +
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
car::Anova(gr_mod_R, type = 3)
stopCluster(cluster)

## prob fruiting

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
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
summary(prep_mod_R)   ## large eiigenvalue ratio value, rescale 

dfs <- prep_subset_R
dfs[,17:18] <- scale(dfs[,17:18]) ## scale num variables (not including consur_0_1)
prep_mod_R_s <- update(prep_mod_R,data=dfs)
summary(prep_mod_R_s)
car::Anova(prep_mod_R_s, type = 3)

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
car::Anova(crep_mod_R, type = 3)
stopCluster(cluster)

## 
### integrated measure of fruiting 

TBF_long$prep_Int <- NA
TBF_long$prep_Int <- ifelse(
  TBF_long$prep1 == 0 & is.na(TBF_long$logcrep1),
  0,
  TBF_long$prep1 * TBF_long$crep1
)
hist(TBF_long$prep_Int)   ## zero inflated distribution

prep_Int_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep_Int, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, s.sq.T_RA), all_vars(!is.na(.)))

GM_R_prep_Int <- glmer.nb(
  prep_Int ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.sq.T_RA +s.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    (1 | site),
  data = prep_Int_subset_R,
  na.action = "na.fail",
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
) 



### attempt to square then scale

TBF_long$sq.P_RA <- (TBF_long$P_RA)^2
TBF_long$sq.T_RA <- (TBF_long$T_RA)^2
TBF_long$sq.T_RH <- (TBF_long$T_RH)^2
TBF_long$s.sq.T_RH <- scale(TBF_long$sq.T_RH)
TBF_long$sq.TSF <- (TBF_long$TSF)^2
TBF_long$s.sq.P_RA <- scale(TBF_long$sq.P_RA)
TBF_long$s.sq.T_RA <- scale(TBF_long$sq.T_RA)
TBF_long$s.TSF <- scale(TBF_long$TSF)
TBF_long$s.sq.TSF <- scale(TBF_long$sq.TSF)
TBF_long$s.TBF <- scale(TBF_long$TBF)

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, TSF, TBF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH,s.TSF,s.sq.TSF,s.sq.T_RA,s.sq.P_RA), all_vars(!is.na(.)))


GM_R_prep_2 <- glm(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.sq.T_RA +s.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    site,
  data = prep_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)       ##### louthan et al., 2022 table 2 minus the quadratic terms for annual precip (hard to interpret)



n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("prep_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

prep_dredge_R <- MuMIn::dredge(
  GM_R_prep_2,
  cluster = cluster,
  trace   = 2
)

prep_mod_R <- get.models(prep_dredge_R, 1)[[1]]
summary(prep_mod_R)   



stopCluster(cluster)


## save env
save(list = ls(), file = "env_snapshot.RData")
save(prep_mod_R, file = "prep_mod_R.Rdata")
save(prep_mod_R_2, file = "prep_mod_R_2.Rdata")

### plotting 
ggplot(P_RA, aes(x = factor(startyear), y = prec, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "startyear", y = "annual cum Prec", fill = "site")+
  ggtitle("annual reg prec")


r.squaredGLMM(gr_mod_R)
