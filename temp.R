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

colnames(TBF_long_landscape_2024)

TBF_long_export <- TBF_long %>%
  select(ID, site_ID, site, quad, size0, rep0, size1, startyear, comm1, TSF, TBF, s.TSF, s.TBF, s.sq.TSF,
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

summary(crep_mod_R)


AICc(gr_mod_L)
AICc(gr_mod_R)

car::Anova(gr_mod_L)


survival_mod <- sur_mod_L
growth_mod <- gr_mod_L
prep_mod <- prep_mod_R
recruit_mod <- recruit_mod_R
crep_mod <- crep_mod_R
variance_mod <- vargrowth_mod_R













