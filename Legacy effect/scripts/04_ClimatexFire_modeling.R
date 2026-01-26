Sys.setenv(LANG = "en")
install.packages("popbio")
install.packages("parallel")
install.packages("doParallel")
install.packages("carData")
install.packages("performance")
install.packages("glmm.hp")
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

TBF_long$s.logsize0 <- scale(log(TBF_long$size0+0.1))

############################ check prep (when first time running this code on a TBF_long) ###############################
## if size1 = NA, then prep must = NA #####
## if sur0_1 = NA, prep could still have value (plants that are first surveyed)
## if size1=0, prep could still have value (senesced) 



look <- TBF_long %>% 
  filter(is.na(sur0_1))%>%
  dplyr::select(prep1, sur0_1, startyear, site_ID, size1, size0)

#all prep1 = NA should be either sur0_1=0 or NA
look <- TBF_long %>% 
  filter(size1 == 0)%>%
  dplyr::select(prep1, sur0_1, startyear, site_ID, size1, size0, crep1)


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
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

GM_R_sur <- glmer(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
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

sur_mod_R <- get.models(sur_dredge_R, 3)[[1]]

## treating site as fixed effect (if the best fit glmer failts to converge)
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
  GM_R_sur,
  cluster = cluster,
  trace   = 2
)
sur_mod_R <- get.models(sur_dredge_R, 1)[[1]]
summary(sur_mod_R)   
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
head(gr_dredge_R)
gr_mod_R <- get.models(gr_dredge_R, 1)[[1]]

stopCluster(cluster)

## prob fruiting

prep_subset_R <- 
  TBF_long %>% 
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
)   ### this likely won't work due to convergence failure, but feel free to try (take a long time to fit)


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
head(prep_dredge_R)
prep_mod_R <- get.models(prep_dredge_R, 1)[[1]]

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
head(crep_dredge_R)
crep_mod_R <- get.models(crep_dredge_R, 1)[[1]]

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
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                 s.P_LH, s.P_LD), all_vars(!is.na(.)))

GM_L_sur <- glmer(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.T_LH + s.sq.T_LH + s.T_LD +s.P_LH +
    s.P_LH:s.T_LH + s.P_LD + 
    (1 | site),
  data = sur_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)

summary(GM_L_sur)
## treating site as fixed effect
GM_L_sur <- glm(
  sur0_1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
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

sur_dredge_L <- MuMIn::dredge(
  GM_L_sur,
  cluster = cluster,
  trace   = 2
)
head(sur_dredge_L)
sur_mod_L <- get.models(sur_dredge_L, 3)[[1]]
summary(sur_mod_L)   

stopCluster(cluster)

### growth

gr_subset_L_2 <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>%      ## make sure to subset data to only alive plants
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
head(gr_dredge_L)
gr_mod_L <- get.models(gr_dredge_L, 1)[[1]]
summary(gr_mod_L)
stopCluster(cluster)

## prob fruiting

prep_subset_L <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_LA,s.sq.T_LA,s.sq.P_LA,
                 s.P_LA, s.T_LH, s.T_LC, 
                 s.P_LH ), all_vars(!is.na(.)))

GM_L_prep <- glmer(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_LA + s.sq.T_LA +s.P_LA+ s.sq.P_LA + s.T_LA:s.P_LA+
    s.T_LH + s.P_LH + s.T_LH:s.P_LH + s.T_LC +
    (1 | site),
  data = prep_subset_L,
  family = "binomial", 
  na.action = "na.fail"
)
GM_L_prep <- glm(
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
  GM_L_prep,
  cluster = cluster,
  trace   = 2
)
head(prep_dredge_L)

prep_mod_L <- get.models(prep_dredge_L, 5)[[1]]
summary(prep_mod_L)
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
head(crep_dredge_L)
crep_mod_L <- get.models(crep_dredge_L, 1)[[1]]
summary(crep_mod_L)
stopCluster(cluster)

#################################################
########### recruit ############################

recruit_df <- read_csv("Legacy effect/data/recruit_df_11_24_2025.csv")
library(MASS)
recruit_subset <- 
  recruit_df %>% 
  dplyr::filter(across(c(num_news, log.fr, TSF, TBF,site), ~ !is.na(.)))

recruit_mod_g_R <- glm.nb(num_news ~log.fr+s.TSF*s.TBF +s.sq.TSF +
                            s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
                            s.P_RH:s.T_RH + s.P_RD + 
                            site,   data = recruit_subset, na.action = "na.fail") 



recruit_mod_g_L <- glm.nb(num_news ~log.fr+s.TSF*s.TBF + s.sq.TSF +
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
head(recruit_dredge_R)
recruit_mod_R <- get.models(recruit_dredge_R, 4)[[1]]

recruit_dredge_L <- MuMIn::dredge(
  recruit_mod_g_L,
  cluster = cluster,
  trace   = 2
)
head(recruit_dredge_L)
recruit_mod_L <- get.models(recruit_dredge_L, 6)[[1]]
summary(recruit_mod_L)

##############################################
AICc_list <- AICc(sur_mod_L, sur_mod_R, gr_mod_L, gr_mod_R, prep_mod_L, prep_mod_R, crep_mod_L, crep_mod_R,  recruit_mod_L, recruit_mod_R)

survival_mod <- if (AICc_list$AICc[1] < AICc_list$AICc[2]) sur_mod_L else sur_mod_R
growth_mod <- if (AICc_list$AICc[3] < AICc_list$AICc[4]) gr_mod_L else gr_mod_R
prep_mod <- if (AICc_list$AICc[5] < AICc_list$AICc[6]) prep_mod_L else prep_mod_R
crep_mod <- if (AICc_list$AICc[7] < AICc_list$AICc[8]) crep_mod_L else crep_mod_R
recruit_mod <- if (AICc_list$AICc[9] < AICc_list$AICc[10]) recruit_mod_L else recruit_mod_R


################################################   
#######   variance in growth ###################
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

GM_vargrowth_R <- lmer(vargrowth ~ s.logsize0 + s.T_RA+ s.TSF * s.TBF + s.sq.TSF +
                         s.P_RA  + s.sq.P_RA  + s.T_RC +s.T_RD +
                         s.P_RD + s.P_RW
                       + (1|site), #  
                       data = Vargrowth_subset, na.action = "na.fail") 
GM_vargrowth_L <- lmer(vargrowth ~ s.logsize0 + s.T_LA+ s.TSF * s.TBF+  s.sq.TSF +
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
head(vargrowth_dredge_L)
vargrowth_mod_L <- get.models(vargrowth_dredge_L, 1)[[1]]

vargrowth_dredge_R <- MuMIn::dredge(
  GM_vargrowth_R,
  cluster = cluster,
  trace   = 2
)
head(vargrowth_dredge_R)
vargrowth_mod_R<- get.models(vargrowth_dredge_R, 1)[[1]]

stopCluster(cluster)

vargrowth_mod <- if (AICc (vargrowth_mod_R) < AICc(vargrowth_mod_L)) vargrowth_mod_R else vargrowth_mod_L

##############################################################
#################### save env ################################
##############################################################
save(survival_mod, growth_mod, prep_mod, crep_mod,vargrowth_mod,recruit_mod, TBF_long, file = "Legacy effect/data/TBFxClimate/VR_mod_qd_delta AICc.Rdata")


