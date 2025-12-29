load("/Users/amlouthan/Desktop/TBF_long_landscape_with_attr.RData") # loads the vital rate functions & TBF_long with attributes

# Zheng-- There are a bunch of unused columns at the beginning of this database that need to be removed. Also when I call str(TBF_long), 
# all of the column names seem to have attributes too. They should not! Something has gone wrong. 
# in addition, s.logsize0 needs to have attributes too so the code works

#size bounds for IPM
IPM_bounds <- range(TBF_long$logsize0, na.rm = TRUE)  # our IPM will be based on logsize as a size metric. 
no_bins <- 100
bin_edges <- seq(IPM_bounds[1], IPM_bounds[2], length.out= no_bins)
bin_mids <- zoo::rollmean(bin_edges,2)

mean_seedling_logsize <- # get the mean logsize of seedings
var_seedling_logsize <- # get the var in logsize of seedings
  
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
  "s.P_RD ",
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
                         "s.sq.T_RC","s.P_RW","s.P_RD ", "s.sq.T_RH",  "s.sq.P_RA",  "s.sq.P_RW",  "s.sq.P_RD")]     <- 0 # setting all climate variable to their mean

log.fr <- log(1)
my_sites <- unique(TBF_long$site) 
no_reps <- 1000 # or somethign similar

all_pop_growth_rates <- matrix(data= NA, nrow= dim(possible_scenarios)[1], ncol= no_reps)
for (j in 1:no_reps){
  # make randomly selected set of fixed effect estimates
  # (analogously, for glm/lm's, I am not randomly seleting site effects)
  rsur_coefs <- coefficients(survival_mod) # first, assign all "randomy selected" coefficients to the mean estimated coefficeints
  rsur_coefs[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")] <- # then, replace the non-site coefficients with randomly selected coefficients
    MASS::mvrnorm(mu= coefficients(survival_mod), Sigma = vcov(survival_mod))[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")]
  rgr_coefs <- lme4::fixef(growth_mod)
  rgr_coefs <-  MASS::mvrnorm(mu= lme4::fixef(growth_mod) , Sigma = vcov(growth_mod)) # no site effect in this model
  rprep_coefs <- coefficients(prep_mod)
  rprep_coefs[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")] <- 
    MASS::mvrnorm(mu= coefficients(prep_mod), Sigma = vcov(prep_mod))[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")]
  rcrep_coefs <- lme4::fixef(crep_mod)
  rcrep_coefs <-  MASS::mvrnorm(mu= lme4::fixef(crep_mod) , Sigma = vcov(crep_mod)) # no site effect in this model
  rvar_coefs <- lme4::fixef(variance_mod)
  rvar_coefs <-  MASS::mvrnorm(mu= lme4::fixef(variance_mod) , Sigma = vcov(variance_mod)) # no site effect in this model
  rrec_coefs <- lme4::fixef(recruit_mod)
  rrec_coefs <-  MASS::mvrnorm(mu= lme4::fixef(recruit_mod) , Sigma = vcov(recruit_mod)) # no site effect in this model
  j_pop_gr_rate <- rep(NA, no_reps)
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
  i_ii_scenario <- i_scenario[which(i_scenario$TSF== ii),]
  data_for_prediction <- i_ii_scenario[rep(seq_len(nrow(i_ii_scenario)), length(bin_mids)), ]
  data_for_prediction$s.logsize0 <-   scale(bin_mids, center= attr(TBF_long$s.logsize0,"scaled:center"), 
                                                    scale= attr(TBF_long$s.logsize0,"scaled:scale"))
  data_for_prediction_rec <- cbind(i_ii_scenario, log.fr)
  
  predicted_sur <- predicted_gr <-predicted_pr <- predicted_cr <- predicted_vg <- matrix(data=NA, nrow= length(my_sites), ncol= length(bin_mids))
  predicted_rec <- rep(NA, length(my_sites))
  
  for (s in my_sites){ #looping through the sites
    s_data_for_prediction <- data_for_prediction
    s_data_for_prediction_rec <- data_for_prediction_rec
    s_data_for_prediction$site <- s
    s_data_for_prediction_rec$site <- s
    
    # Zheng-- you will have to change the predict() call in the below to be a custom predict function that uses the random coefficients generated earlier in the code (e.g., rsur_coefs)
    # Sarah has a nice (albeit verbose) example of how to do this, I will email this to you
  predicted_sur_allsites[s,] <- predict(survival_mod_r, newdata= s_data_for_prediction, type='response') 
  predicted_gr_allsites[s,] <- predict(growth_mod_r, newdata= s_data_for_prediction) # predicting logsize--- perfect for our IPM
  predicted_pr_allsites[s,] <- predict(prep_mod_r, newdata= s_data_for_prediction, type='response')
  predicted_cr_allsites[s,] <- predict(crep_mod_r, newdata= s_data_for_prediction)
  predicted_vg_allsites[s,] <- predict(variance_mod_r, newdata= s_data_for_prediction, type='response') # Zheng check that the residual  you used in the fitting of the variance_mod
  # is right-- you need to regress the squared residual, which I don't htink is the default output
  # and you need to ensure that the variance_mod function is predicting the variance in log size, not variance in size per se
  predicted_rec_allsites[s] <- predict(recruit_mod_r, newdata= s_data_for_prediction, type='response')
  
  # for mixed models, the above functions will predict site-specific results (i.e., will incorporate random effects)
  rm(s_data_for_prediction) # this part removes the site-specific dataframe used in this loop, so you do not accidentaly use the previous sites' dataframe instead of the current one-- in case there was an error somehow
  }
  predicted_sur <- apply(predicted_sur_allsites, MARGIN= 2, FUN= mean) # average size-specific vital rates across sites
  predicted_gr <- apply(predicted_gr_allsites, MARGIN= 2, FUN= mean)
  predicted_pr <- apply(predicted_pr_allsites, MARGIN= 2, FUN= mean)
  predicted_cr <- apply(predicted_cr_allsites, MARGIN= 2, FUN= mean)
  predicted_vg <- apply(predicted_vg_allsites, MARGIN= 2, FUN= mean)
  predicted_rec <- mean(predicted_rec_allsites)
  
  # make the TSF-specific matrix
  for (ss in 1:nobins) {
    growcdf <- pnorm(binedges,predicted_gr[ss],sqrt(predicted_vg[ss])) # Zheng, note the sqrt here-- you should have fit the squared residual when fitting variance_mod. See Easterling paper if confused
    grows <- growcdf[2:length(binedges)]-growcdf[1:(length(binedges)-1)]
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
  survgmx <- gmx*t(matrix( rep(predicted_sur,nobins),nobins)) # survs* growth # ZHENG CHECK TAHT this survival multiplication IS IS ORIENTED CORRECTLY
  reprow <-  predicted_sur*predicted_pr*predicted_cr # prob that you survive, reproduce, then number fruits|reproduce; this multiplcation does it element by element
  
  mx <- matrix(0, nobins+1, nobins+1)
  mx[2:(nobins+1), 2:(nobins+1)] = survgmx
  
  mx[1,2:(nobins+1)] = reprow # FRUIT ROW
  
  sdlng_cdf <- pnorm(binedges,mean_seedling_logsize,sqrt(var_seedling_logsize)) 
  sdlng_pdf <- sdlng_cdf[2:length(binedges)]-sdlng_cdf[1:(length(binedges)-1)]
  if(sum(sdlng_pdf)>0){
    sdlng_pdf <- sdlng_pdf/sum(sdlng_pdf)} else {
      sdlng_pdf <- NA} 
  
  mx[2:(nobins+1),1]  = predicted_rec *sdlng_pdf 
  
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
