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
head(sur_dredge_R) ### visually check to see components of top comparable models (d AICc <2)

sur_mod_R <- get.models(sur_dredge_R, 2)[[1]]  
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
gr_mod_R <- get.models(gr_dredge_R, 3)[[1]]
summary(gr_mod_R)
stopCluster(cluster)

## prob fruiting
prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))
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
prep_mod_R <- get.models(prep_dredge_R, 2)[[1]]
summary(prep_mod_R)   ## large eiigenvalue ratio value, rescale 
stopCluster(cluster)





### number of fruit

crep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.)))

GM_R_crep <- lmer(
  logcrep1 ~ s.logsize0 + s.TSF * s.TBF + s.T_RA + s.T_RH +
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
crep_mod_R <- get.models(crep_dredge_R, 1)[[1]]
summary(crep_mod_R)
stopCluster(cluster)


##############################################################################################################
############################################ local ###########################################################
### Survival
sur_subset_L <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                 s.P_LH, s.P_LD), all_vars(!is.na(.)))

GM_L_sur <- glmer(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + 
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + 
    (1 | site),
  data = sur_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)

## treating site as fixed effect
GM_L_sur <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF +
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + 
    site,
  data = sur_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)


n_cores <- detectCores()
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_L"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

sur_dredge_L <- MuMIn::dredge(
  GM_L_sur,
  cluster = cluster,
  trace   = 2
)
head(sur_dredge_L)
sur_mod_L <- get.models(sur_dredge_L, 2)[[1]]
summary(sur_mod_L)   
stopCluster(cluster)




### growth

gr_subset_L_2 <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(logsize1, s.logsize0, s.TSF,s.sq.TSF, s.TBF, site, s.sq.T_LA,s.T_LA, s.P_LA, s.T_LC, 
                 s.T_LD, s.P_LH), all_vars(!is.na(.)))
GM_L_gr <- lmer(
  logsize1 ~ s.logsize0 + s.TSF * s.TBF + s.T_LA + s.sq.T_LA + s.P_LA + s.T_LA:s.P_LA +
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
head(gr_dredge_L)
gr_mod_L <- get.models(gr_dredge_L, 1)[[1]]
car::Anova(gr_mod_L)
stopCluster(cluster)

## prob fruiting

prep_subset_L <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_LA,s.sq.T_LA,s.sq.P_LA,
                 s.P_LA, s.T_LH, s.T_LC, 
                 s.P_LH ), all_vars(!is.na(.)))
GM_L_prep <- glm(
  prep1 ~ s.logsize0 + s.TSF * s.TBF +
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
  GM_L_prep,
  cluster = cluster,
  trace   = 2
)
head(prep_dredge_L)
prep_mod_L <- get.models(prep_dredge_L, 3)[[1]]
summary(prep_mod_L)
stopCluster(cluster)

### number of fruit

crep_subset_L <- 
  TBF_long %>% 
  filter_at(vars(logcrep1, s.logsize0, TSF,s.sq.TSF ,TBF, site, s.T_LA, s.T_LH), all_vars(!is.na(.)))

GM_L_crep <- lmer(
  logcrep1 ~ s.logsize0 + s.TSF * s.TBF + s.T_LA + s.T_LH +
    (1 | site),
  data = crep_subset_L,
  na.action = "na.fail"
)

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
stopCluster(cluster)


######################### recruit #####################

recruit_df <- read.csv("Legacy effect/data/recruit_df_11_24_2025.csv")
library(MASS)
recruit_subset <- 
  recruit_df %>% 
  dplyr::filter(across(c(num_news, log.fr, TSF, TBF,site), ~ !is.na(.)))

recruit_mod_g_R <- glm.nb(num_news ~log.fr+s.TSF*s.TBF +  
                              s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
                              s.P_RH:s.T_RH + s.P_RD + 
                              site,   data = recruit_subset, na.action = "na.fail") 



recruit_mod_g_L <- glm.nb(num_news ~log.fr+s.TSF*s.TBF + 
                              s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
                              s.P_LH:s.T_LH + s.P_LD +  site,   data = recruit_subset, na.action = "na.fail") 

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
recruit_mod_R <- get.models(recruit_dredge_R, 1)[[1]]

recruit_dredge_L <- MuMIn::dredge(
  recruit_mod_g_L,
  cluster = cluster,
  trace   = 2
)
recruit_mod_L <- get.models(recruit_dredge_L, 1)[[1]]
summary(recruit_mod_L)

############# compare AICc ###################

AICc_list <- AICc(sur_mod_L, sur_mod_R, gr_mod_L, gr_mod_R, prep_mod_L, prep_mod_R, crep_mod_L, crep_mod_R,  recruit_mod_L, recruit_mod_R)

survival_mod <- if (AICc_list$AICc[1] < AICc_list$AICc[2]) sur_mod_L else sur_mod_R
growth_mod <- if (AICc_list$AICc[3] < AICc_list$AICc[4]) gr_mod_L else gr_mod_R
prep_mod <- if (AICc_list$AICc[5] < AICc_list$AICc[6]) prep_mod_L else prep_mod_R
crep_mod <- if (AICc_list$AICc[7] < AICc_list$AICc[8]) crep_mod_L else crep_mod_R
recruit_mod <- if (AICc_list$AICc[9] < AICc_list$AICc[10]) recruit_mod_L else recruit_mod_R

###################################################################
##########################  vargrowth #############################
###################################################################

growth_terms <- attr(terms(growth_mod), "term.labels")
int_terms <- grep(":", growth_terms, value = TRUE)
int_terms_broken <- unique(unlist(strsplit(int_terms, ":", fixed = TRUE)))
main_terms <- growth_terms[!grepl(":", growth_terms)]
logsize1 <- c("logsize1")
gr_all_terms <- unique(c(main_terms, int_terms_broken, logsize1))

Vargrowth_subset <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
  filter_at(vars(gr_all_terms), all_vars(!is.na(.)))

Vargrowth_subset$vargrowth <- NA
Vargrowth_subset$predgrowth <- NA

Vargrowth_subset$predgrowth <- predict(growth_mod,newdata=Vargrowth_subset)

Vargrowth_subset$vargrowth <-
  (Vargrowth_subset$predgrowth - # variance is equivalent to (predicted-expected)^2
     Vargrowth_subset$logsize1)^2
Vargrowth_subset$og_predgrowth <- exp(Vargrowth_subset$predgrowth)
Vargrowth_subset$og_logsize1 <- exp(Vargrowth_subset$logsize1)


#***** to prevent negative numbers later

GM_vargrowth_R <- lmer(vargrowth ~ s.logsize0 + s.T_RA+ s.TSF * s.TBF +
                         s.P_RA  + s.sq.P_RA  + s.T_RC +s.T_RD +
                         s.P_RD + s.P_RW
                       + (1|site), #  
                       data = Vargrowth_subset, na.action = "na.fail") 
GM_vargrowth_L <- lmer(vargrowth ~ s.logsize0 + s.T_LA+ s.TSF * s.TBF+
                         s.P_LA  + s.sq.P_LA  + s.T_LC +s.T_LD +
                         s.P_LD + s.P_LW
                       + (1|site), #  
                       data = Vargrowth_subset, na.action = "na.fail") 

n_cores <- detectCores()
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("Vargrowth_subset"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})


vargrowth_dredge_L <- MuMIn::dredge(
  GM_vargrowth_L,
  cluster = cluster,
  trace   = 2
)
vargrowth_mod_L <- get.models(vargrowth_dredge_L, 1)[[1]]

vargrowth_dredge_R <- MuMIn::dredge(
  GM_vargrowth_R,
  cluster = cluster,
  trace   = 2
)
vargrowth_mod_R<- get.models(vargrowth_dredge_R, 1)[[1]]

stopCluster(cluster)

vargrowth_mod <- if (AICc (vargrowth_mod_R) < AICc(vargrowth_mod_L)) vargrowth_mod_R else vargrowth_mod_L


############# compare AICc ###################

save(survival_mod, growth_mod, prep_mod, crep_mod,vargrowth_mod,recruit_mod, TBF_long, file = "Legacy effect/data/TBFxClimate/VR_mod_linear.Rdata")
