
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
TBF_long <- read.csv("data/TBFxClimate/TBF_long_10232025.csv") 
colnames(TBF_long)


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
save(sur_mod_R_2, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/sur_mod_R.Rdata")



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
save(gr_mod_R, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/gr_mod_R.Rdata")

## prob fruiting

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))

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

save(prep_mod_R, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/prep_mod_R.Rdata")

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
save(crep_mod_R, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/crep_mod_R.Rdata")

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

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
### 
###                       Local   climate variables (Annual + Monthly)
###
###               Define global mode according to Louthan et.al., 2022, Table 2.
###
###
###
###
TBF_long$s.T_LA <- NA
TBF_long$s.T_LA <- scale(TBF_long$T_LA) 
TBF_long$s.P_LA <- NA
TBF_long$s.P_LA <- scale(TBF_long$P_LA) 
TBF_long$s.P_LW <- NA
TBF_long$s.P_LW <- scale(TBF_long$P_LW) 
TBF_long$s.P_LD <- NA
TBF_long$s.P_LD <- scale(TBF_long$P_LD) 
TBF_long$s.P_LH <- NA
TBF_long$s.P_LH <- scale(TBF_long$P_LH) 
TBF_long$s.T_LH<- NA
TBF_long$s.T_LD<- NA
TBF_long$s.T_LW<- NA
TBF_long$s.T_LC<- NA
TBF_long$s.T_LH <- scale(TBF_long$T_LH) 
TBF_long$s.T_LC <- scale(TBF_long$T_LC) 
TBF_long$s.T_LD <- scale(TBF_long$T_LD) 
TBF_long$s.T_LW <- scale(TBF_long$T_LW) 

TBF_long$sq.P_LA <- (TBF_long$P_LA)^2
TBF_long$sq.T_LA <- (TBF_long$T_LA)^2
TBF_long$sq.T_LH <- (TBF_long$T_LH)^2
TBF_long$s.sq.T_LH <- scale(TBF_long$sq.T_LH)
TBF_long$s.sq.P_LA <- scale(TBF_long$sq.P_LA)
TBF_long$s.sq.T_LA <- scale(TBF_long$sq.T_LA)
TBF_long$sq.TSF <- (TBF_long$TSF)^2
TBF_long$s.TSF <- scale(TBF_long$TSF)
TBF_long$s.sq.TSF <- scale(TBF_long$sq.TSF)
TBF_long$s.TBF <- scale(TBF_long$TBF)


### Survival
sur_subset_L <- 
  TBF_long %>% 
  filter_at(vars(consur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                 s.P_LH, s.P_LD), all_vars(!is.na(.)))

GM_L_sur <- glmer(
  consur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + 
    (1 | site),
  data = sur_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)

## treating site as fixed effect
GM_L_sur_2 <- glm(
  consur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + 
    site,
  data = sur_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)

summary(GM_L_sur_2)


n_cores <- detectCores()
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_L"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_L_2 <- MuMIn::dredge(
  GM_L_sur_2,
  cluster = cluster,
  trace   = 2
)
sur_mod_L_2 <- get.models(sur_dredge_L_2, 1)[[1]]
summary(sur_mod_L_2)   

dfs <- sur_subset_R
dfs[,17:ncol(dfs)] <- scale(dfs[,17:ncol(dfs)]) ## scale num variables (not including consur_0_1)
sur_mod_R_s <- update(sur_mod_R,data=dfs)
summary(sur_mod_R_s)

cc <- check_collinearity(sur_mod_L_2)
cc


stopCluster(cluster)

### growth

gr_subset_L_2 <- 
  TBF_long[which(TBF_long$consur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(logsize1, s.logsize0, s.TSF,s.sq.TSF, s.TBF, site, s.sq.T_LA,s.T_LA, s.P_LA, s.T_LC, 
                 s.T_LD, s.P_LH), all_vars(!is.na(.)))
GM_L_gr <- lmer(
  logsize1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF + s.T_LA + s.sq.T_LA + s.P_LA + s.T_LA:s.P_LA +
    s.T_LC + s.T_LD + s.P_LH +
    (1 | site),
  data = gr_subset_L_2,
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("gr_subset_L_2"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

gr_dredge_L <- MuMIn::dredge(
  GM_L_gr,
  cluster = cluster,
  trace   = 2
)
gr_mod_L <- get.models(gr_dredge_L, 1)[[1]]
summary(gr_mod_L)
car::Anova(gr_mod_L, type = 3)
stopCluster(cluster)

## prob fruiting

prep_subset_L <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_LA,s.sq.T_LA,s.sq.P_LA,
                 s.P_LA, s.T_LH, s.T_LC, 
                 s.P_LH, ), all_vars(!is.na(.)))

GM_L_prep <- glmer(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.sq.T_LA +s.P_LA+ s.sq.P_LA + s.T_LA:s.P_LA+
    s.T_LH + s.P_LH + s.T_LH:s.P_LH + s.T_LC +
    (1 | site),
  data = prep_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)
GM_L_prep_2 <- glm(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.sq.T_LA +s.P_LA+ s.sq.P_LA + s.T_LA:s.P_LA+
    s.T_LH + s.P_LH + s.T_LH:s.P_LH + s.T_LC +
     site,
  data = prep_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)
n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("prep_subset_L"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

prep_dredge_L <- MuMIn::dredge(
  GM_L_prep_2,
  cluster = cluster,
  trace   = 2
)
prep_mod_L <- get.models(prep_dredge_L, 1)[[1]]
summary(prep_mod_L)
save(prep_mod_L, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/prep_mod_L.Rdata")
stopCluster(cluster)

### number of fruit

crep_subset_L <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, TSF,s.sq.TSF ,TBF, site, s.T_LA, s.T_LH), all_vars(!is.na(.)))

GM_L_crep <- lmer(
  logcrep1 ~ s.logsize0 + TSF * TBF + s.sq.TSF+ s.T_LA + s.T_LH +
    (1 | site),
  data = crep_subset_L,
  na.action = "na.fail"
)
car::Anova(GM_L_crep)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("crep_subset_L"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

crep_dredge_L <- MuMIn::dredge(
  GM_L_crep,
  cluster = cluster,
  trace   = 2
)
crep_mod_L <- get.models(crep_dredge_L, 1)[[1]]
summary(crep_mod_L)
car::Anova(crep_mod_L, type = 3)
stopCluster(cluster)
save(crep_mod_L, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/crep_mod_L.Rdata")


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
######################     dredge on local and regional models combined #################################
#########################################################################################################
#########################################################################################################

TBF_long <- read.csv("data/TBFxClimate/TBF_long_10232025.csv")


### Sur

Sur_term_L <- attr(terms(sur_mod_L_2), "term.labels")
Sur_term_R <- attr(terms(sur_mod_R_2), "term.labels")
terms_vec <- union(Sur_term_L, Sur_term_R)
int_terms <- grep(":", terms_vec, value = TRUE)
int_terms_broken <- unique(unlist(strsplit(int_terms, ":", fixed = TRUE)))
main_terms <- terms_vec[!grepl(":", terms_vec)]
Sur_all_terms <- union(main_terms, int_terms_broken)

f_fix <- reformulate(terms_vec, response = "consur0_1")
f_fix    ### don't refer to this, copy and paste to change site into a random effect

Sur_subset_G <- 
  TBF_long %>% 
  filter_at(vars(consur0_1,Sur_all_terms), all_vars(!is.na(.)))

GM_G_sur <- glmer(
  consur0_1 ~ s.logsize0 + s.P_LH + s.sq.T_LH + s.sq.TSF + s.T_LA + 
    s.T_LD + s.T_LH + s.TBF + s.TSF + s.P_LH:s.T_LH + 
    s.TBF:s.TSF + s.P_RD + s.P_RH + s.sq.T_RH + s.T_RA + s.T_RD + 
    s.T_RH + s.P_RH:s.T_RH + (1|site),
  data = Sur_subset_G,
  family = "binomial", 
  na.action = "na.fail"
)
summary(GM_G_sur)
n_cores <- detectCores()
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("Sur_subset_G"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_G <- MuMIn::dredge(
  GM_G_sur,
  cluster = cluster,
  trace   = 2
)
sur_mod_G <- get.models(sur_dredge_G, 1)[[1]]
summary(sur_mod_G)   
save(sur_mod_G, file = "data/TBFxClimate/sur_mod_G.csv")

stopCluster(cluster)

## save env
save(list = ls(), file = "env_snapshot.RData")
save(gr_mod_L, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/gr_mod_L.Rdata")
save(prep_mod_R_2, file = "prep_mod_R_2.Rdata")
write.csv(TBF_long,"data/TBFxClimate/TBF_long_10232025.csv") 

### plotting 
ggplot(P_RA, aes(x = factor(startyear), y = prec, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "startyear", y = "annual cum Prec", fill = "site")+
  ggtitle("annual reg prec")


r.squaredGLMM(gr_mod_R)
