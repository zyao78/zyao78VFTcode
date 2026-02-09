table(TBF_long$startyear)

n_distinct(TBF_long$site_ID) %>%
  group_by(startyear)

Look <- TBF_long %>%
  group_by(startyear) %>%
  summarise(n_distinct_siteID = n_distinct(site_ID))

Look <- TBF_long %>%
  group_by(site, startyear) %>%
  filter(size0>=1) %>%
  summarise(added = n_distinct(site_ID)) %>%
  ungroup()

Look2 <- TBF_long %>%
  group_by(startyear) %>%
  filter(size0>=1) %>%
  summarise(added = n_distinct(site_ID)) %>%
  ungroup()

TBF_long <- TBF_long %>% 
  select(site_ID, everything())


TBF_long_landscape_2024
wanted_cols <- c(
  "ID","site_ID","site","quad","size0","rep0","size1","startyear","comm1",
  "TSF","TBF","s.TSF","s.TBF","s.sq.TSF","s.sq.TBF",
  "s.logsize0","logsize1","prep1","crep1",
  "T_RA","P_RA","s.T_RA","s.P_RA","s.sq.P_RA",
  "T_RC","P_RC","s.T_RC","s.P_RC","s.sq.P_RC",
  "T_RH","P_RH","s.T_RH","s.P_RH","s.sq.T_RH",
  "T_LA","P_LA","s.T_LA","s.P_LA","s.sq.T_LA","s.sq.P_LA",
  "T_LC","P_LC","s.T_LC","s.P_LC","s.sq.T_LC",
  "T_LW","P_LW","s.T_LW","s.P_LW","s.sq.P_LW",
  "T_LD","P_LD","s.T_LD","s.P_LD","s.sq.P_LD",
  "T_LH","P_LH","s.T_LH","s.P_LH","s.sq.T_LH"
)
missing_cols <- setdiff(wanted_cols, colnames(TBF_long_landscape_2024))


TBF_long_landscape_2024$s.sq.P_LW <- scale((TBF_long_landscape_2024$P_LW)^2)
TBF_long_landscape_2024$s.sq.P_RW <- scale((TBF_long_landscape_2024$P_RW)^2)
TBF_long_landscape_2024$s.sq.P_LD <- scale((TBF_long_landscape_2024$P_LD)^2)
TBF_long_landscape_2024$s.sq.P_RD <- scale((TBF_long_landscape_2024$P_RD)^2)

TBF_long_landscape_2024$site_ID <- paste(TBF_long_landscape_2024$site, TBF_long_landscape_2024$ID, sep = "_")

TBF_long$s.sq.T_RC <- scale((TBF_long$s.T_RC)^2)
TBF_long$s.sq.P_RW <- scale((TBF_long$s.P_RW)^2)
TBF_long$s.sq.P_RD <- scale((TBF_long$s.P_RD)^2)
TBF_long$s.sq.T_LC <- scale((TBF_long$s.T_LC)^2)
TBF_long$s.sq.P_LW <- scale((TBF_long$s.P_LW)^2)
TBF_long$s.sq.P_LD <- scale((TBF_long$s.P_LD)^2)

colnames(TBF_long)

TBF_long <- read.csv("data/TBFxClimate/TBF_long_landscape_2024.csv")

TBF_long_export <- TBF_long %>%
  select(ID, site_ID, site, quad, size0, rep0, size1,sur0_1, startyear, comm1, TSF, TBF, s.TSF, s.TBF, s.sq.TSF,
         s.logsize0, logsize1,prep1,crep1,logcrep1,predgrowth ,vargrowth,
         s.T_RA,s.P_RA, s.sq.T_RA, s.sq.P_RA , 
           s.T_RC,s.sq.T_RC, 
           s.T_RW ,s.P_RW ,s.sq.P_RW ,
           s.T_RD , s.P_RD ,s.sq.P_RD,
           s.T_RH , s.P_RH , s.sq.T_RH ,s.T_LA , s.P_LA , s.sq.T_LA , s.sq.P_LA , 
         s.T_LC , s.sq.T_LC , 
         s.T_LW ,s.P_LW , s.sq.P_LW ,
         s.T_LD , s.P_LD ,  s.sq.P_LD ,
         s.T_LH , s.P_LH , s.sq.T_LH )
           

write.csv(TBF_long_export, "data/TBF_long_export_11_24_25.csv")

colnames(TBF_long_export)

save(survival_mod,growth_mod,prep_mod,recruit_mod,crep_mod,
     variance_mod,file = "data/model_export_11_24_25.Rdata")

summary(survival_mod)


AICc(gr_mod_L)
AICc(gr_mod_R)

car::Anova(gr_mod_L)


survival_mod <- sur_mod_L
growth_mod <- gr_mod_L
prep_mod <- prep_mod_R
recruit_mod <- recruit_mod_R
crep_mod <- crep_mod_R
variance_mod <- vargrowth_mod_R




TBF_long %>%
  group_by(site,startyear) %>%
  summarize(p_survival = mean(sur0_1, na.rm = TRUE))

TBF_long %>%
  group_by(startyear) %>%
  summarize(growth = mean(size1 - size0, na.rm = TRUE))




recruit_df$s.T_RA <- NA
recruit_df$s.T_RA <- scale(recruit_df$T_RA) 
recruit_df$s.P_RA <- NA
recruit_df$s.P_RA <- scale(recruit_df$P_RA) 
recruit_df$s.P_RW <- NA
recruit_df$s.P_RW <- scale(recruit_df$P_RW) 
recruit_df$s.P_RD <- NA
recruit_df$s.P_RD <- scale(recruit_df$P_RD) 
recruit_df$s.P_RH <- NA
recruit_df$s.P_RH <- scale(recruit_df$P_RH) 
recruit_df$s.T_RH<- NA
recruit_df$s.T_RD<- NA
recruit_df$s.T_RW<- NA
recruit_df$s.T_RC<- NA
recruit_df$s.T_RH <- scale(recruit_df$T_RH) 
recruit_df$s.T_RC <- scale(recruit_df$T_RC) 
recruit_df$s.T_RD <- scale(recruit_df$T_RD) 
recruit_df$s.T_RW <- scale(recruit_df$T_RW) 
recruit_df$s.T_RW <- scale(recruit_df$T_RW) 


recruit_df$sq.P_RA <- (recruit_df$P_RA)^2
recruit_df$sq.T_RA <- (recruit_df$T_RA)^2
recruit_df$sq.T_RH <- (recruit_df$T_RH)^2
recruit_df$s.sq.T_RH <- scale(recruit_df$sq.T_RH)
recruit_df$sq.TSF <- (recruit_df$TSF)^2
recruit_df$s.sq.P_RA <- scale(recruit_df$sq.P_RA)
recruit_df$s.sq.T_RA <- scale(recruit_df$sq.T_RA)
recruit_df$s.TSF <- scale(recruit_df$TSF)
recruit_df$s.sq.TSF <- scale(recruit_df$sq.TSF)
recruit_df$s.TBF <- scale(recruit_df$TBF)


recruit_df$s.T_LA <- NA
recruit_df$s.T_LA <- scale(recruit_df$T_LA) 
recruit_df$s.P_LA <- NA
recruit_df$s.P_LA <- scale(recruit_df$P_LA) 
recruit_df$s.P_LW <- NA
recruit_df$s.P_LW <- scale(recruit_df$P_LW) 
recruit_df$s.P_LD <- NA
recruit_df$s.P_LD <- scale(recruit_df$P_LD) 
recruit_df$s.P_LH <- NA
recruit_df$s.P_LH <- scale(recruit_df$P_LH) 
recruit_df$s.T_LH<- NA
recruit_df$s.T_LD<- NA
recruit_df$s.T_LW<- NA
recruit_df$s.T_LC<- NA
recruit_df$s.T_LH <- scale(recruit_df$T_LH) 
recruit_df$s.T_LC <- scale(recruit_df$T_LC) 
recruit_df$s.T_LD <- scale(recruit_df$T_LD) 
recruit_df$s.T_LW <- scale(recruit_df$T_LW) 

recruit_df$sq.P_LA <- (recruit_df$P_LA)^2
recruit_df$sq.T_LA <- (recruit_df$T_LA)^2
recruit_df$sq.T_LH <- (recruit_df$T_LH)^2
recruit_df$s.sq.T_LH <- scale(recruit_df$sq.T_LH)
recruit_df$s.sq.P_LA <- scale(recruit_df$sq.P_LA)
recruit_df$s.sq.T_LA <- scale(recruit_df$sq.T_LA)
recruit_df$sq.TSF <- (recruit_df$TSF)^2
recruit_df$s.TSF <- scale(recruit_df$TSF)
recruit_df$s.sq.TSF <- scale(recruit_df$sq.TSF)
recruit_df$s.TBF <- scale(recruit_df$TBF)














sur_pred_link <- predict.glm(survival_mod, data_new1, type = "link", se.fit = TRUE)
fit <- sur_pred_link$fit

link_lwr <- sur_pred_link$fit + qnorm(0.025) * sur_pred_link$se.fit
link_upr <- sur_pred_link$fit + qnorm(0.975) * sur_pred_link$se.fit

fit2 <- survival_mod$family$linkinv(fit)
upr2 <- survival_mod$family$linkinv(link_upr)
lwr2 <- survival_mod$family$linkinv(link_lwr)

data_new1$sur_fit


m1 <- predict.glm(survival_mod, data_new1, type = "link", se.fit = TRUE)
m1$se.fit
fit <- m1$fit
fit2 <- survival_mod$family$linkinv(fit)


m2 <- predict.glm(survival_mod, data_new1, type = "response", se.fit = TRUE)
m2$se.fit


