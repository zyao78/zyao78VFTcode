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
library(mgcv)
library(merTools)


TBF_long <- read_csv("legacy effect/data/TBFxClimate/TBF_long_landscape_2024.csv")            ##
colnames(TBF_long)

######################### GAM only works with site as factor, not character ################ 

TBF_long <- as.data.frame(TBF_long)
TBF_long$site <- as.factor(TBF_long$site)


############# sur ##################################################
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

GAM_sur <- gam( sur0_1 ~ s.logsize0 +s.TBF+s.TSF:s.TBF+ s(s.TSF,k = 3) +
    s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
    s.P_RH:s.T_RH + s.P_RD + s(site, bs = "re"),method = "REML",
  data = sur_subset_R,
  family = "binomial", 
  na.action = "na.fail"
)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("sur_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(mgcv); library(MuMIn)})
GAM_dregde_sur <- MuMIn::dredge(
  GAM_sur, cluster = cluster,
  trace   = 2
)
GAM_bestfit_sur <- get.models(GAM_dregde_sur, 1)[[1]]

summary(GAM_bestfit_sur)
data_new1$sur <- predict(GAM_bestfit_sur, data_new1, type="response")

AICc(GAM_bestfit_sur)

plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur)) +
  geom_line(aes(color= newTBF)) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 
plt1

summary(GM_R_sur)


############ prep #####################

prep_subset_R <- 
  TBF_long %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))

GAM_prep <- gam( prep1 ~ s.logsize0 + s.TSF:s.TBF + s.TBF+s(s.TSF, k=3)+
                   s.T_RA + s.sq.T_RA +s.P_RA+ s.sq.P_RA + s.T_RA:s.P_RA+
                   s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC + s(site, bs = "re"), method = "REML",
                 data = prep_subset_R,
                 family = "binomial", 
                 na.action = "na.fail")
AICc(GAM_prep)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("prep_subset_R"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(mgcv); library(MuMIn)})
GAM_dregde <- MuMIn::dredge(
  GAM_prep,
  trace   = 2
)
GAM_bestfit <- get.models(GAM_dregde, 1)[[1]]

summary(GAM_bestfit)

data_new1$prep <- predict(GAM_bestfit, data_new1, type="response", se=TRUE)


plt2 <- ggplot(data= data_new1, aes(x= TSF, y= prep$fit)) +
  geom_line(aes(color= newTBF)) + 
  #yli1905m(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "prep", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 

plt2

summary(GM_R_sur)



########## compare to strictly linear ############# 



#### sur #############################################
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

linear_sur <- glmer(sur0_1 ~ s.logsize0 +s.TSF*s.TBF +
                  s.T_RA + s.T_RH + s.sq.T_RH + s.T_RD +s.P_RH +
                  s.P_RH:s.T_RH + s.P_RD + (1|site),
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
linear_dregde_sur <- MuMIn::dredge(
  linear_sur, cluster = cluster,
  trace   = 2
)

linear_bestfit_sur <- get.models(linear_dregde_sur, 1)[[1]]

summary(linear_bestfit_sur)
data_new1$sur <- predictInterval(linear_bestfit_sur, data_new1, type="probability",se=TRUE )

AICc(linear_bestfit_sur)

plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur$lwr, ymax = data_new1$sur$upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 
plt1


####### prep #############################################


linear_prep <- glmer(
  prep1 ~ s.logsize0 + s.TSF * s.TBF + 
    s.T_RA + s.sq.T_RA +s.P_RA+ s.sq.P_RA + s.T_RA:s.P_RA+
    s.T_RH + s.P_RH + s.T_RH:s.P_RH + s.T_RC +
    (1|site),
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
linear_dregde_prep <- MuMIn::dredge(
  linear_prep, cluster = cluster,
  trace   = 2
)

linear_bestfit_prep <- get.models(linear_dregde_prep, 1)[[1]]

summary(linear_bestfit_prep)
data_new1$prep <- predictInterval(linear_bestfit_prep, data_new1, type="probability",se=TRUE )

AICc(linear_bestfit_prep)

plt1 <- ggplot(data= data_new1, aes(x= TSF, y= prep$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$prep$lwr, ymax = data_new1$prep$upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "prep", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 
plt1

#############

prep_subset_R <- 
  TBF_long[which(TBF_long$sur0_1 == 1),] %>% 
  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))
prep_subset_R2 <- 
  TBF_long %>% 
  filter_at(vars(rep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                 s.P_RH, ), all_vars(!is.na(.)))
look <- 
  TBF_long %>% 
  filter (prep1 == 1 & !is.na(s.logsize0))


sur_prep_prob <- TBF_long %>%
  group_by(startyear, site, TSF, TBF) %>%
  summarise(
    sur = mean(sur0_1, na.rm = TRUE),
    prep = mean(prep1, na.rm = TRUE)
  )%>%
  mutate( newTBF=ifelse(TBF<5, "short", "long"))





sur_prep_prob <- TBF_long %>%
  group_by(startyear, site, TSF, TBF) %>%
  summarise(
    sur = mean(sur0_1, na.rm = TRUE),
    prep = mean(prep1, na.rm = TRUE)
  )%>%
  mutate( newTBF=ifelse(TBF<5, "short", "long"))



ggplot(data= sur_prep_prob, aes(x= TSF, y= prep)) +
  geom_point(aes(color= newTBF)) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "prep", x = "TSF") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:11) +
  theme(text = element_text(size = 20)) 

######### examine the linearity of the TSFxsur relationship #############
sur_subset_R <- 
  TBF_long %>% 
  filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_RA,s.T_RH,s.T_RD,s.sq.T_RH,
                 s.P_RH, s.P_RD), all_vars(!is.na(.)))

m_linear <- glm(prep1~TSF, data = TBF_long,
                family = "binomial", 
                na.action = "na.omit")


m_quad <- glm(prep1~poly(TSF, 2), data = TBF_long,
                family = "binomial", 
                na.action = "na.omit")

m_cubic <- glm(prep1~poly(TSF, 3), data = TBF_long,
              family = "binomial", 
              na.action = "na.omit")

m_gam <- gam(prep1~s(TSF, k=3), data = TBF_long,
             family = "binomial", 
             na.action = "na.omit")
anova(m_linear, m_quad, m_cubic, m_gam)

AICc(m_linear, m_quad, m_cubic, m_gam)


