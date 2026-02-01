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

TBF_long <- read_csv("Legacy effect/data/TBF_long_export_11_24_25.csv")
TBF_long$site_ID <- paste(TBF_long$site, TBF_long$ID, sep = "_")



################ here, we are modeling VR against local temperature variables and regional precipitation variables ################

### Survival
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

GM_R_sur <- glmer(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF +
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_RH +
    s.P_RH:s.T_LH + s.P_RD + 
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
head(sur_dredge_R) ########### manually check for comparable models

sur_mod_R <- get.models(sur_dredge_R, 1)[[1]]

summary(sur_mod_R)


## treating site as fixed effect (if the best fit glmer failts to converge)
GM_R_sur_2 <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF +
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_RH +
    s.P_RH:s.T_LH + s.P_RD + 
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

head(sur_dredge_R)

survival_mod<- get.models(sur_dredge_R, 1)[[1]]

stopCluster(cluster)

### growth

gr_subset_R_2 <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(logsize1, s.logsize0, s.TSF,s.sq.TSF, s.TBF, site, s.sq.T_RA,s.T_RA, s.P_RA, s.T_RC, 
                 s.T_RD, s.P_RH), all_vars(!is.na(.)))
GM_R_gr <- lmer(
  logsize1 ~ s.logsize0 + s.TSF * s.TBF  + s.T_LA + s.sq.T_LA + s.P_RA + s.T_LA:s.P_RA +
    s.T_LC + s.T_LD + s.P_RH +
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
head(gr_dredge_R)
growth_mod<- get.models(gr_dredge_R, 1)[[1]]

stopCluster(cluster)

## prob fruiting

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))

 ### this likely won't work due to convergence failure, but feel free to try (take a long time to fit)


GM_R_prep <- glm(
  prep1 ~ s.logsize0 + s.TSF * s.TBF +
    s.T_LA + s.sq.T_LA +s.P_RA+ s.sq.P_RA + s.T_LA:s.P_RA+
    s.T_LH + s.P_RH + s.T_LH:s.P_RH + s.T_LC +
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

prep_dredge_R_2 <- MuMIn::dredge(
  GM_R_prep,
  cluster = cluster,
  trace   = 2
)
head(prep_dredge_R_2)
prep_mod <- get.models(prep_dredge_R_2, 1)[[1]]

stopCluster(cluster)

### number of fruit

crep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, TSF, TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.)))

GM_R_crep <- lmer(
  logcrep1 ~ s.logsize0 + s.TSF * s.TBF  + s.T_LA + s.T_LH +
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
head(crep_dredge_R)
crep_mod <- get.models(crep_dredge_R, 1)[[1]]

stopCluster(cluster)

#################### recruit ##########################

#################################################
########### recruit ############################

recruit_df <- read_csv("Legacy effect/data/recruit_df_11_24_2025.csv")
library(MASS)
recruit_subset <- 
  recruit_df %>% 
  dplyr::filter(across(c(num_news, log.fr, TSF, TBF,site), ~ !is.na(.)))

recruit_mod_g_R <- glm.nb(num_news ~log.fr+s.TSF*s.TBF  +
                            s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_RH +
                            s.P_RH:s.T_LH + s.P_RD + 
                            site,   data = recruit_subset, na.action = "na.fail") 





n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("recruit_subset"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(MASS); library(MuMIn)})

recruit_dredge_R <- MuMIn::dredge(
  recruit_mod_g_R,
  cluster = cluster,
  trace   = 2
)
head(recruit_dredge_R)
recruit_mod <- get.models(recruit_dredge_R, 1)[[1]]

######### vargrowth ################################################
growth_terms <- attr(terms(growth_mod), "term.labels")
int_terms <- grep(":", growth_terms, value = TRUE)
int_terms_broken <- unique(unlist(strsplit(int_terms, ":", fixed = TRUE)))
main_terms <- growth_terms[!grepl(":", growth_terms)]
logsize1 <- c("logsize1")
gr_all_terms <- unique(c(main_terms, int_terms_broken, logsize1))

Vargrowth_subset <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter(across(all_of(gr_all_terms), ~ !is.na(.)))

Vargrowth_subset$vargrowth <- NA
Vargrowth_subset$predgrowth <- NA

Vargrowth_subset$predgrowth <- predict(growth_mod,newdata=Vargrowth_subset)

Vargrowth_subset$vargrowth <-
  (Vargrowth_subset$predgrowth - # variance is equivalent to (predicted-expected)^2
     Vargrowth_subset$logsize1)^2
Vargrowth_subset$og_predgrowth <- exp(Vargrowth_subset$predgrowth)
Vargrowth_subset$og_logsize1 <- exp(Vargrowth_subset$logsize1)


#***** to prevent negative numbers later

GM_vargrowth_R <- lmer(vargrowth ~ s.logsize0 + s.T_LA+ s.TSF * s.TBF  +
                         s.P_RA  + s.sq.P_RA  + s.T_LC +s.T_LD +
                         s.P_RD + s.P_RW
                       + (1|site), #  
                       data = Vargrowth_subset, na.action = "na.fail") 


n_cores <- detectCores()
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("Vargrowth_subset"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})




vargrowth_dredge_R <- MuMIn::dredge(
  GM_vargrowth_R,
  cluster = cluster,
  trace   = 2
)
head(vargrowth_dredge_R)
vargrowth_mod <- get.models(vargrowth_dredge_R, 1)[[1]]

stopCluster(cluster)


################################# save ##############

save(survival_mod, growth_mod, prep_mod, crep_mod,vargrowth_mod,recruit_mod, TBF_long, file = "Legacy effect/data/TBFxClimate/VR_mod linear mixed scale.Rdata")


#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
##########################################################   Using only regional climate ########################################################
#################################################################################################################################################
#################################################################################################################################################

TBF_long <- read_csv("Legacy effect/data/TBF_long_export_11_24_25.csv")
TBF_long$site_ID <- paste(TBF_long$site, TBF_long$ID, sep = "_")



################ here, we are modeling VR against local temperature variables and regional precipitation variables ################

### Survival
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
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
head(sur_dredge_R) ########### manually check for comparable models

sur_mod_R <- get.models(sur_dredge_R, 1)[[1]]

summary(sur_mod_R)


## treating site as fixed effect (if the best fit glmer failts to converge)
GM_R_sur_2 <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF +  +
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

head(sur_dredge_R_2)

survival_mod<- get.models(sur_dredge_R_2, 1)[[1]]
summary(sur_mod_R)   
stopCluster(cluster)

### growth

gr_subset_R_2 <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(logsize1, s.logsize0, s.TSF,s.sq.TSF, s.TBF, site, s.sq.T_RA,s.T_RA, s.P_RA, s.T_RC, 
                 s.T_RD, s.P_RH), all_vars(!is.na(.)))
GM_R_gr <- lmer(
  logsize1 ~ s.logsize0 + s.TSF * s.TBF  + s.T_RA + s.sq.T_RA + s.P_RA + s.T_RA:s.P_RA +
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
head(gr_dredge_R)
growth_mod<- get.models(gr_dredge_R, 1)[[1]]

stopCluster(cluster)

## prob fruiting

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))

GM_R_prep <- glmer(
  prep1 ~ s.logsize0 + s.TSF * s.TBF +
    s.T_RA + s.sq.T_RA +s.P_RA+ s.sq.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    (1|site),
  data = prep_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)   ### this likely won't work due to convergence failure, but feel free to try (take a long time to fit)


GM_R_prep <- glm(
  prep1 ~ s.logsize0 + s.TSF * s.TBF +
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
head(prep_dredge_R)
prep_mod <- get.models(prep_dredge_R, 1)[[1]]

stopCluster(cluster)

### number of fruit

crep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, TSF, TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.)))

GM_R_crep <- lmer(
  logcrep1 ~ s.logsize0 + TSF * TBF  + s.T_RA + s.T_RH +
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
head(crep_dredge_R)
crep_mod <- get.models(crep_dredge_R, 1)[[1]]

stopCluster(cluster)

#################### recruit ##########################

#################################################
########### recruit ############################

recruit_df <- read_csv("Legacy effect/data/recruit_df_11_24_2025.csv")
library(MASS)
recruit_subset <- 
  recruit_df %>% 
  dplyr::filter(across(c(num_news, log.fr, TSF, TBF,site), ~ !is.na(.)))

recruit_mod_g_R <- glm.nb(num_news ~log.fr+s.TSF*s.TBF  +
                            s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
                            s.P_RH:s.T_RH + s.P_RD + 
                            site,   data = recruit_subset, na.action = "na.fail") 





n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("recruit_subset"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(MASS); library(MuMIn)})

recruit_dredge_R <- MuMIn::dredge(
  recruit_mod_g_R,
  cluster = cluster,
  trace   = 2
)
head(recruit_dredge_R)
recruit_mod <- get.models(recruit_dredge_R, 1)[[1]]

######### vargrowth ################################################
growth_terms <- attr(terms(growth_mod), "term.labels")
int_terms <- grep(":", growth_terms, value = TRUE)
int_terms_broken <- unique(unlist(strsplit(int_terms, ":", fixed = TRUE)))
main_terms <- growth_terms[!grepl(":", growth_terms)]
logsize1 <- c("logsize1")
gr_all_terms <- unique(c(main_terms, int_terms_broken, logsize1))

Vargrowth_subset <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter(across(all_of(gr_all_terms), ~ !is.na(.)))

Vargrowth_subset$vargrowth <- NA
Vargrowth_subset$predgrowth <- NA

Vargrowth_subset$predgrowth <- predict(growth_mod,newdata=Vargrowth_subset)

Vargrowth_subset$vargrowth <-
  (Vargrowth_subset$predgrowth - # variance is equivalent to (predicted-expected)^2
     Vargrowth_subset$logsize1)^2
Vargrowth_subset$og_predgrowth <- exp(Vargrowth_subset$predgrowth)
Vargrowth_subset$og_logsize1 <- exp(Vargrowth_subset$logsize1)


#***** to prevent negative numbers later

GM_vargrowth_R <- lmer(vargrowth ~ s.logsize0 + s.T_RA+ s.TSF * s.TBF  +
                         s.P_RA  + s.sq.P_RA  + s.T_RC +s.T_RD +
                         s.P_RD + s.P_RW
                       + (1|site), #  
                       data = Vargrowth_subset, na.action = "na.fail") 


n_cores <- detectCores()
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("Vargrowth_subset"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

vargrowth_dredge_R <- MuMIn::dredge(
  GM_vargrowth_R,
  cluster = cluster,
  trace   = 2
)
head(vargrowth_dredge_R)
vargrowth_mod <- get.models(vargrowth_dredge_R, 1)[[1]]

stopCluster(cluster)


################################# save ##############

save(survival_mod, growth_mod, prep_mod, crep_mod,vargrowth_mod,recruit_mod, TBF_long, file = "Legacy effect/data/TBFxClimate/VR_mod_regional.Rdata")














