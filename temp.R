prep_subset_R_2 <- 
  TBF_long %>% 
  filter_at(vars(prep2, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.))) %>% 
  filter(consur0_1 == 1)

GM_R_prep_2 <- glm(
  prep2 ~ s.logsize0 + s.TSF * s.TBF + s.sq.TSF+
    s.T_RA + s.sq.T_RA +s.P_RA+ s.sq.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    site,
  data = prep_subset_R_2,
  family = "binomial", 
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("prep_subset_R_2"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

prep_dredge_R_2 <- MuMIn::dredge(
  GM_R_prep_2,
  cluster = cluster,
  trace   = 2
)
prep_mod_R_2 <- get.models(prep_dredge_R_2, 1)[[1]]
summary(prep_mod_R_2)   ## large eiigenvalue ratio value, rescale 
stopCluster(cluster)
library(dplyr)

TBF_long$rep1 [which(is.na(TBF_long$rep1) & TBF_long$consur0_1 == 1)] <- 0

look <- TBF_long %>%
   filter(is.na(rep1)) 
  