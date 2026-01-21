library(AICcmodavg)
library(lme4)
library(tidyverse)
library(dplyr)
library(glue)
library(zoo)
library(MASS)

survival_mod <- L_survival_mod
growth_mod<-L_growth_mod
prep_mod<-L_prep_mod
crep_mod<-L_crep_mod
vargrowth_mod<-L_vargrowth_mod
recruit_mod<-L_recruit_mod






### load data and remove model attr
load("~/zyao78VFTcode2/Legacy effect/data/TBFxClimate/VR_mod_linear.Rdata")# loads in the VR functions
TBF_long_noattr <- read_csv("Legacy effect/data/TBF_long_export_11_24_25.csv")
TBF_long_noattr <- TBF_long_noattr %>%
  mutate(across(c("s.logsize0", "s.sq.TSF", "s.TSF", "s.TBF"), as.numeric))
TBF_long$s.logsize0 <- scale(TBF_long$logsize0) 

survival_mod <- update(survival_mod,data=TBF_long_noattr %>% 
                         filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                                        s.P_LH, s.P_LD), all_vars(!is.na(.))))
growth_mod <- update(growth_mod,data=TBF_long_noattr %>% 
                       filter_at(vars(s.logsize0,logsize1,s.P_LA ,s.P_LH,s.sq.T_LA,s.T_LA,s.T_LC,s.T_LD , s.TSF), all_vars(!is.na(.))))

prep_mod <- update(prep_mod,data=TBF_long_noattr %>% 
                     filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                                    s.P_RH, ), all_vars(!is.na(.))))
crep_mod <- update(crep_mod,data=TBF_long_noattr %>% 
                     filter_at(vars(logcrep1, s.logsize0, TSF, TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.))))
variance_mod <- update(variance_mod,data=TBF_long_noattr %>% 
                         filter_at(vars(vargrowth, s.logsize0,s.P_RD,s.P_RW,s.sq.P_RA,s.T_RD  ), all_vars(!is.na(.))))
recruit_mod <- recruit_mod


#size bounds for IPM
IPM_bounds <- range(TBF_long$logsize0, na.rm = TRUE)  # our IPM will be based on logsize as a size metric. 
no_bins <- 100
bin_edges <- seq(IPM_bounds[1], IPM_bounds[2], length.out= no_bins+1)
bin_mids <- zoo::rollmean(bin_edges,2)


##################################
rows_with_new <- TBF_long[grepl("\\bnew", TBF_long$comm1), ]
rows_with_new <- rows_with_new[!grepl("no new", rows_with_new$comm1), ] # delete "no new"
rows_with_new <- rows_with_new[!grepl("no news", rows_with_new$comm1), ] # delete "no new" 
rows_with_new <- rows_with_new[!grepl("not possible to see new plants", rows_with_new$comm1), ]
rows_with_new <- rows_with_new[!grepl("No news", rows_with_new$comm1), ]
rows_with_new <- rows_with_new[!grepl("new lvs", rows_with_new$comm1), ]
rows_with_new <- rows_with_new[!grepl("new nail", rows_with_new$comm1), ]

mean_seedling_logsize <- mean(rows_with_new$logsize1, na.rm=T) # get the mean logsize of seedings   ### the mean seedling size is very large
var_seedling_logsize <- var(rows_with_new$logsize1,na.rm=T)# get the var in logsize of seedings                







##################################  
possible_scenarios <- 
  as.data.frame(matrix(NA, nrow= 22, ncol = 28))
names(possible_scenarios) <- c("FRI","s.TBF","s.T_LA", "s.T_LH",
                               "s.sq.T_LH",
                               "s.sq.P_LA",
                               "s.T_LC",
                               "s.T_LD",
                               "s.P_LD",
                               "s.sq.T_LA",
                               "s.P_LA",
                               "s.P_LW",
                               "s.P_LH",
                               "s.T_RA",
                               "s.sq.T_RA",
                               "s.P_RA",
                               "s.T_RH",
                               "s.P_RH",
                               "s.T_RC",
                               "s.T_RW",
                               "s.T_RD",
                               "s.sq.T_RC",
                               "s.P_RW",
                               "s.P_RD",
                               "s.sq.T_RH",
                               "s.sq.P_RA",
                               "s.sq.P_RW",
                               "s.sq.P_RD")


possible_scenarios$FRI <- rep(1:11, 2) # we want FRI to range from 1-11
possible_scenarios$s.TBF <- scale(rep(c(3, 11), each= 11) ,# raw TBF values
                                  center= attr(TBF_long$s.TBF,"scaled:center"), 
                                  scale= attr(TBF_long$s.TBF,"scaled:scale")) # for each set of FRI's, we want to set a short TBF (of 3) and a long TBF (of 11)

possible_scenarios[, c(  "s.T_LA",  "s.T_LH",  "s.sq.T_LH",  "s.sq.P_LA",  "s.T_LC",  "s.T_LD",  "s.P_LD",  "s.sq.T_LA",  "s.P_LA",  
                         "s.P_LW",  "s.P_LH",  "s.T_RA",  "s.sq.T_RA",  "s.P_RA",  "s.T_RH",  "s.P_RH",  "s.T_RC",  "s.T_RW",  "s.T_RD",
                         "s.sq.T_RC","s.P_RW","s.P_RD", "s.sq.T_RH",  "s.sq.P_RA",  "s.sq.P_RW",  "s.sq.P_RD")]     <- 0 # setting all climate variable to their mean

log.fr <- log(1) ### 1 fruit is not log(1), it is log(1+0.1)
my_sites <- unique(TBF_long$site) 
no_reps <- 1000 # or somethign similar

all_pop_growth_rates <- matrix(data= NA, nrow= dim(possible_scenarios)[1], ncol= no_reps)


###
i=8
ii=7

s="B2"
###

##################################################################
###################### initiate IPM ##############################
##################################################################

for (j in 1:no_reps){
  # make randomly selected set of fixed effect estimates
  # (analogously, for glm/lm's, I am not randomly seleting site effects)
  rsur_coefs <- coefficients(survival_mod) # first, assign all "randomy selected" coefficients to the mean estimated coefficeints
  ### will excluding site factors disrupt the covariance structure?
  rsur_coefs[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")] <- MASS::mvrnorm(mu= coefficients(survival_mod), Sigma = vcov(survival_mod))[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")]
  rgr_coefs <- lme4::fixef(growth_mod)
  rgr_coefs <-  MASS::mvrnorm(mu= lme4::fixef(growth_mod) , Sigma = vcov(growth_mod)) # no site effect in this model
  rprep_coefs <- coefficients(prep_mod)
  rprep_coefs[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")] <- MASS::mvrnorm(mu= coefficients(prep_mod), Sigma = vcov(prep_mod))[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")]
  rcrep_coefs <- lme4::fixef(crep_mod)
  rcrep_coefs <-  MASS::mvrnorm(mu= lme4::fixef(crep_mod) , Sigma = vcov(crep_mod)) # no site effect in this model
  rvar_coefs <- lme4::fixef(variance_mod)
  rvar_coefs <-  MASS::mvrnorm(mu= lme4::fixef(variance_mod) , Sigma = vcov(variance_mod)) # no site effect in this model
  rrec_coefs <- coefficients(recruit_mod)
  rrec_coefs[!names(coefficients(recruit_mod)) %in% paste("site", my_sites, sep="")] <-  MASS::mvrnorm(mu= coefficients(recruit_mod), Sigma = vcov(recruit_mod))[!names(coefficients(recruit_mod)) %in% paste("site", my_sites, sep="")]
  
  
  # no site effect in this model
  j_pop_gr_rate <- rep(NA, dim(possible_scenarios)[1]) ### changed to no_scenarios
  
  
  for (i in 1: dim(possible_scenarios)[1]) {
    
    i_scenario <- possible_scenarios[i,] # which combination of FRI, TBF, and climate do I want to use
    
    TSFs <- 0:(i_scenario$FRI-1) # TSF range from 0 to (FRI-1) such that an FRI of 1 will always have TSF= 0
    i_scenario <- i_scenario[rep(seq_len(nrow(i_scenario)), length(TSFs)), ]
    i_scenario$TSF <- TSFs
    i_scenario$s.TSF <- scale(TSFs, center= attr(TBF_long$s.TSF,"scaled:center"), 
                              scale= attr(TBF_long$s.TSF,"scaled:scale"))
    i_scenario$s.sq.TSF <- scale(TSFs^2, center= attr(TBF_long$s.sq.TSF,"scaled:center"), 
                                 scale= attr(TBF_long$s.sq.TSF,"scaled:scale"))
    
    mx_prod <- diag(no_bins+1) # setting up an identity matrix before you enter the loop
    
    for (ii in TSFs){
      i_ii_scenario <- i_scenario[which(i_scenario$TSF==ii),]
      data_for_prediction <- i_ii_scenario[rep(seq_len(nrow(i_ii_scenario)), length(bin_mids)), ]
      data_for_prediction$s.logsize0 <- scale(bin_mids, center= attr(TBF_long$s.logsize0,"scaled:center"), 
                                                scale= attr(TBF_long$s.logsize0,"scaled:scale"))
      data_for_prediction_rec <- cbind(i_ii_scenario, log.fr)
      
      predicted_sur <- predicted_gr <-predicted_pr <- predicted_cr <- predicted_vg <- matrix(data=NA, nrow= length(my_sites), ncol= length(bin_mids))
      predicted_rec <- rep(NA, length(my_sites))
      
      ### make empty mxes before entering the site loop
      
      predicted_sur_allsites <-predicted_gr_allsites<-predicted_pr_allsites <-predicted_cr_allsites<-predicted_vg_allsites <-matrix(NA, nrow = length(my_sites), ncol = no_bins, dimnames = list(my_sites, NULL))
      predicted_rec_allsites <- matrix(NA, nrow = length(my_sites), ncol = 1, dimnames = list(my_sites, NULL))
      
      
      for (s in my_sites){ #looping through the sites
        s_data_for_prediction <- data_for_prediction
        s_data_for_prediction_rec <- data_for_prediction_rec
        s_data_for_prediction$site <- s
        s_data_for_prediction_rec$site <- s
        
        s_data_for_prediction$site     <- factor(s_data_for_prediction$site,  levels = my_sites)   ## it is important to add the levels=my_sites here to ensure that the model matrix is built correctly
        s_data_for_prediction_rec$site <- factor(s_data_for_prediction_rec$site, levels = my_sites)
        
        s_data_for_prediction <- s_data_for_prediction %>% mutate(across(c("s.logsize0", "s.sq.TSF", "s.TSF", "s.TBF"), as.numeric))   ### change col type to numeric to avoid matrix col type
        
        
        
        # Zheng-- you will have to change the predict() call in the below to be a custom predict function that uses the random coefficients generated earlier in the code (e.g., rsur_coefs)
        
        ### build model matrices and use mx multiplication to obtain predicted values. Don't forget to transform if needed!!!!!!!!!
        
        m_sur <- model.matrix(~s.logsize0 + s.P_LD + s.P_LH + s.sq.T_LH + s.T_LA  + s.T_LD + s.T_LH +  s.TBF +  s.TSF + site + s.P_LH:s.T_LH ,data=s_data_for_prediction)   ### change these coefficient if switching models
        p2_sur <- rsur_coefs %*% t(m_sur) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_sur <- plogis(p2_sur)   ## transform to binomial 
        predicted_sur_allsites[s,] <- p2_sur
        
        m_gr <- model.matrix(~s.logsize0 + s.P_LA  + s.P_LH  + s.sq.T_LA  +s.T_LA+s.T_LC  + s.T_LD+s.TSF+s.P_LA:s.T_LA ,data=s_data_for_prediction)
        p2_gr <- rgr_coefs %*% t(m_gr) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_gr_allsites[s,] <- p2_gr
        
        m_prep <- model.matrix(~s.logsize0 +s.P_LA+s.P_LH +s.sq.P_LA+ s.sq.T_LA + s.T_LA+s.T_LH +s.TSF +site+s.P_LA:s.T_LA+s.P_LH:s.T_LH  ,data=s_data_for_prediction)
        p2_prep <- rprep_coefs %*% t(m_prep) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_prep <- plogis(p2_prep)   ## transform to binomial 
        predicted_pr_allsites[s,] <- p2_prep
        
        m_crep <- model.matrix(~s.logsize0 +s.T_RA+s.T_RH  ,data=s_data_for_prediction)
        p2_crep <- rcrep_coefs %*% t(m_crep) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_cr_allsites[s,] <- p2_crep
        
        m_vg <- model.matrix(~s.logsize0 +s.P_RD + s.P_RW + s.sq.P_RA+s.T_RD,data=s_data_for_prediction)
        p2_vg <- rvar_coefs %*% t(m_vg) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_vg_allsites[s,] <- p2_vg
        
        m_rec <- model.matrix(~log.fr +s.P_LH +s.sq.T_LH+ s.T_LA+ s.T_LD+ s.T_LH , data=s_data_for_prediction_rec)
        p2_rec <- rrec_coefs %*% t(m_rec) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_rec <- exp(p2_rec)   ## transform to negative binomial 
        predicted_rec_allsites[s,1] <- p2_rec
        
        
        # for mixed models, the above functions will predict site-specific results (i.e., will incorporate random effects)
        rm(s_data_for_prediction) # this part removes the site-specific dataframe used in this loop, so you do not accidentaly use the previous sites' dataframe instead of the current one-- in case there was an error somehow
      }
      predicted_sur <- apply(predicted_sur_allsites, MARGIN= 2, FUN= mean) # average size-specific vital rates across sites
      predicted_gr <- apply(predicted_gr_allsites, MARGIN= 2, FUN= mean)
      predicted_pr <- apply(predicted_pr_allsites, MARGIN= 2, FUN= mean)
      predicted_cr <- apply(predicted_cr_allsites, MARGIN= 2, FUN= mean)
      predicted_vg <- apply(predicted_vg_allsites, MARGIN= 2, FUN= mean)
      predicted_rec <- mean(predicted_rec_allsites)
      
      gmx <- matrix(NA, length(bin_mids),length(bin_mids)) ## create empty gmx before entering the no_bins loop
      
      # make the TSF-specific matrix
      for (ss in 1:no_bins) {
        growcdf <- pnorm(bin_edges,predicted_gr[ss],sqrt(predicted_vg[ss])) # Yes. Vargrowth=(pred - obs)^2
        grows <- growcdf[2:length(bin_edges)]-growcdf[1:(length(bin_edges)-1)]
        if(sum(grows)>0){grows <- grows/sum(grows)
        gmx[,ss] <- grows
        } else if (sum(grows)==0 & grow_vals[ss]< min(bin_edges)) { 
          gmx[,ss] <- c(1, rep(0, length(binmids)-1))  
        } else if (sum(grows)==0 & grow_vals[ss]> max(bin_edges)) { 
          gmx[,ss] <- c(rep(0, length(binmids)-1), 1)  } else { # deals with complete eviction-- OUT OF THE BOUNDS  of the kernel
            gmx[,ss] <- NA} 
        # this if statement breaks the code (puts NA's into the matrix) if the sum of the PDF is zero (which happens if all the probability is outside of the size bounds)
      } # end ss loop
      
      # make the surv*growth mx
      survgmx <- gmx*t(matrix( rep(predicted_sur,no_bins),no_bins)) # Yes, this is doing element-wise multiplication 
      reprow <-  predicted_sur*predicted_pr*predicted_cr # prob that you survive, reproduce, then number fruits|reproduce; this multiplcation does it element by element
      
      mx <- matrix(0, no_bins+1, no_bins+1)
      mx[2:(no_bins+1), 2:(no_bins+1)] = survgmx
      mx[1,2:(no_bins+1)] = reprow # FRUIT ROW
      
      sdlng_cdf <- pnorm(bin_edges,mean_seedling_logsize,sqrt(var_seedling_logsize)) 
      sdlng_pdf <- sdlng_cdf[2:length(bin_edges)]-sdlng_cdf[1:(length(bin_edges)-1)]
      if(sum(sdlng_pdf)>0){
        sdlng_pdf <- sdlng_pdf/sum(sdlng_pdf)} else {
          sdlng_pdf <- NA} 
      
      mx[2:(no_bins+1),1]  = predicted_rec *sdlng_pdf 
      
      mx_prod <- mx_prod %*% mx # in the first iteration of the loop, mx just becomes mx_prod
      
    } # closes ii (TSFs) loop
    
    j_pop_gr_rate[i] <- (popbio::lambda(mx_prod))^(1/length(TSFs)) # see Gross et al. : https://conbio.onlinelibrary.wiley.com/doi/10.1111/j.1523-1739.1998.97285.x
    # multiply them together to get a matrix, etc. 
    
  } # closes i (scenarios) loop
  # store lambda for each rep
  all_pop_growth_rates[,j] <- j_pop_gr_rate # in "all_pop_growth_rates" the rows are  scenarios and the columns are reps
}

all_pop_growth_rates <- cbind(possible_scenarios, all_pop_growth_rates)
names(all_pop_growth_rates) <- c(names(possible_scenarios), paste("rep", 1:no_reps, sep = ""))

# Zheng, you can calculate the 95% CI's on the predictions (which incorporate parameter uncertainty) from these reps




################## graph #############################
rep_cols <- grep("^rep", names(all_pop_growth_rates), value = TRUE)

mean_lambda_across_scenarios <- all_pop_growth_rates %>%
  rowwise() %>%
  mutate(
    mean_lambda = mean(c_across(all_of(rep_cols)), na.rm = TRUE),
    lower_bound = quantile(c_across(all_of(rep_cols)), 0.025, na.rm = TRUE),
    upper_bound = quantile(c_across(all_of(rep_cols)), 0.975, na.rm = TRUE)
  ) %>%
  dplyr::select(FRI,s.TBF,mean_lambda,lower_bound, upper_bound) %>%
  mutate(TBF=NA) %>%
  ungroup()

mean_lambda_across_scenarios$TBF[1:11] <- "Short TBF"
mean_lambda_across_scenarios$TBF[12:22] <- "Long TBF"


plt1 <- ggplot(data= mean_lambda_across_scenarios, aes(x= FRI, y= mean_lambda)) +
  geom_line(aes(color= TBF)) + 
  geom_ribbon(aes(ymin = mean_lambda_across_scenarios$lower_bound, ymax = mean_lambda_across_scenarios$upper_bound, fill= TBF), alpha = 0.1) + 
  #ylim(0,1) +
  #xlim(c(0, 12)) +
  labs(y= "Growth Rate", x = "Fire Return Interval",color = NULL, fill = NULL) +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 16)) 


















