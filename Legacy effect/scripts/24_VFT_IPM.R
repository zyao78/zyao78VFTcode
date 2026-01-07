load("data/TBF_long_landscape_with_attr.RData") # loads in the VR functions
TBF_long_noattr <- read.csv("data/TBF_long_export_11_24_25.csv")
TBF_long_noattr <- TBF_long_noattr %>%
  mutate(across(c("s.logsize0", "s.sq.TSF", "s.TSF", "s.TBF"), as.numeric))
TBF_long$s.logsize0 <- scale(TBF_long$logsize0) 
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(dplyr)
library(glue)
library(zoo)

### refit all models with fields that don't contain attr

m_sur <- update(survival_mod,data=TBF_long_noattr %>% 
         filter_at(vars(sur0_1, s.logsize0, s.TSF,s.TBF, s.sq.TSF, site, s.T_LA,s.T_LH,s.T_LD,s.sq.T_LH,
                        s.P_LH, s.P_LD), all_vars(!is.na(.))))
m_gr <- update(growth_mod,data=TBF_long_noattr %>% 
                  filter_at(vars(s.logsize0,logsize1,s.P_LA ,s.P_LH,s.sq.T_LA,s.T_LA,s.T_LC,s.T_LD , s.TSF), all_vars(!is.na(.))))

m_prep <- update(prep_mod,data=TBF_long_noattr %>% 
                  filter_at(vars(prep1, s.logsize0, s.TSF, s.TBF,s.sq.TSF, site, s.T_RA,s.P_RA, s.T_RH, s.T_RC, 
                                 s.P_RH, ), all_vars(!is.na(.))))
m_crep <- update(crep_mod,data=TBF_long_noattr %>% 
                   filter_at(vars(logcrep1, s.logsize0, TSF, TBF,s.sq.TSF, site, s.T_RA, s.T_RH), all_vars(!is.na(.))))
m_vargrowth <- update(variance_mod,data=TBF_long_noattr %>% 
                        filter_at(vars(vargrowth, s.logsize0,s.P_RD,s.P_RW,s.sq.P_RA,s.T_RD  ), all_vars(!is.na(.))))
m_recruit <- recruit_mod

#size bounds for IPM
IPM_bounds <- range(TBF_long$logsize0, na.rm = TRUE) # ZHENG, NEED TO TRANSFORM TO LOG SIZE NOT SCALED LOG SIZE
no_bins <- 100
bin_edges <- seq(IPM_bounds[1], IPM_bounds[2], length.out= no_bins+1)
bin_mids <- zoo::rollmean(bin_edges,2)  ## calculate k=2: rolling mean with a moving-window width of 2 (average of two bin edges)

# global model for each demographic rate could include these effects:
{# s.logsize0
  # s.TSF
  # s.TBF
  # s.sq.TSF
  # s.T_LA
  # s.T_LH
  # s.sq.T_LH
  # s.sq.P_LA
  # s.T_LC
  # s.T_LD
  # s.P_LH:s.T_LH
  # s.P_LD
  # s.sq.T_LA
  # s.P_LA
  # s.T_LA:s.P_LA
  # s.P_LW
  # s.P_LH
  # s.T_RA
  # s.sq.T_RA
  # s.P_RA
  # s.T_RA:s.P_RA
  # s.T_RH
  # s.P_RH
  # s.T_RH:s.P_RH
  # s.T_RC
  # siteB2
  # siteCH
  # siteCM
  # siteGSP-BI
  # siteGSP-LI
  # siteIA
  # siteME
  # s.TSF:s.TBF
  # s.T_RW
  # s.T_RD
  # num_fr
  # s.sq.T_RC
  # s.P_RW
  # s.P_RD 
  # s.T_RW:s.P_RW
  # s.T_RD:s.P_RD
  # s.sq.T_RH
  # s.sq.P_RA
  # s.sq.P_RW
  # s.sq.P_RD
  # as well as a RE of site!!!! 
}

possible_scenarios <- 
  as.data.frame(matrix(NA, nrow= 22, ncol = 35))   ## nrow: TSF maximum*2     ncol: # of all possible predictors
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
  "s.sq.P_RD",
  
  "siteB2",
  "siteCH",
  "siteCM",
  "siteGSP-BI",
  "siteGSP-LI",
  "siteIA",
  "siteME")
possible_scenarios$FRI <- rep(1:11, 2)
possible_scenarios$s.TBF <- scale(rep(c(0, 10), each= 11) ,# raw TBF values
                                  center= attr(TBF_long$s.TBF, "scaled:center"),   ### center and scale are rounded to 2 digits. Is this okay?
                                  scale= attr(TBF_long$s.TBF, "scaled:scale")) # make sure to avoid recalculating mean and sd 
                                                                               # of TBF_long$s.TBF by directly refering to the col attr
possible_scenarios[, c(
  "s.T_LA",
  "s.T_LH",
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
  "s.sq.P_RD")] <- 0 # ZHENG WORKAROUND, this is just to show you how the code is going-- setting all climate variable to their mean


possible_scenarios[, c("site")] <- "B1" # ZHENG CHANGE LATER; this only predicts pop gr rate for B1
log.fr <- log(1)  ### 1 fruit is not log(1), it is log(1+0.1)


###################
#### test i=10  ###
i=10            ###    
ii=10           ####
###################


for (i in 1: dim(possible_scenarios)[1]) {

  i_scenario <- possible_scenarios[i,]
  
  TSFs <- 0:(i_scenario$FRI-1)  
  i_scenario <- i_scenario[rep(seq_len(nrow(i_scenario)), length(TSFs)), ]
  i_scenario$TSF <- TSFs
  i_scenario$s.TSF <- scale(TSFs, center= attr(TBF_long$s.TSF, "scaled:center"), scale= attr(TBF_long$s.TSF, "scaled:scale")) #
  i_scenario$s.sq.TSF <- scale(TSFs^2, center= attr(TBF_long$s.sq.TSF, "scaled:center"), scale= attr(TBF_long$s.sq.TSF, "scaled:scale")) # Zheng change this too!!!!?? to something like: attr(TBF_long$s.TSF, "center")
  
  mxes <- array(NA, dim= c(length(TSFs), no_bins+1, no_bins+1))
  for (ii in TSFs){
  i_ii_scenario <- i_scenario[which(i_scenario$TSF== TSFs[ii]),]
  data_for_prediction <- i_ii_scenario[rep(seq_len(nrow(i_ii_scenario)), length(bin_mids)), ]
  data_for_prediction$s.logsize0 <-  scale(bin_mids, center= attr(TBF_long$s.logsize0, "scaled:center"), scale= attr(TBF_long$s.logsize0, "scaled:scale"))
  data_for_prediction_rec <- cbind(i_ii_scenario, log.fr)
  
  data_for_prediction <- data_for_prediction %>% mutate(across(c("s.logsize0", "s.sq.TSF", "s.TSF", "s.TBF"), as.numeric))   ### change col type to numeric to avoid matrix col type
  
  predicted_sur <- predict(m_sur, newdata= data_for_prediction, type='response') # Zheng-- need to re-fit all vital rate functions iwth data that has no attributes. and nothing in matrix format!!!!!
  predicted_gr <- predict(m_gr, newdata= data_for_prediction) # predicting log size
  predicted_pr <- predict(m_prep, newdata= data_for_prediction, type='response')
  predicted_cr <- predict(m_crep, newdata= data_for_prediction)# predicting log size
  predicted_vg <- predict(m_vargrowth, newdata= data_for_prediction, type='response') # this prediction predicting the variance in log size, not variance in SIZE per se
  predicted_rec <- predict(m_recruit, newdata= data_for_prediction_rec, type='response')
  
  # make the TSF-specific matrix
  gmx <- matrix(NA, length(bin_mids),length(bin_mids))  ## added, define gmx first
  
  for (ss in 1:(no_bins-1)) {   # changed from no_bins to no_bins-1 # variance was defined as (pred - observed)^2
    growcdf <- pnorm(bin_edges,predicted_gr[ss],sqrt(predicted_vg[ss])) # TRANSOFRMATIONS NOT RIGHT-- DID YOU SQUARE WHEN YOU WERE FITTING THE VARIANCE IN THE 
    grows <- growcdf[2:length(bin_edges)]-growcdf[1:(length(bin_edges)-1)]
    my_size_range <-range(TBF_long$logsize1, na.rm = T) ## added, my_size_range defined by logsize1
    
    if(sum(grows)>0){grows <- grows/sum(grows) ## scale to 1 in case sum(grows) = 0.999999, for example
    gmx[,ss] <- grows
    } else if (sum(grows)==0 & predicted_gr[ss]< my_size_range[1]) { 
      gmx[,ss] <- c(1, rep(0, length(bin_mids)-1))  
    } else if (sum(grows)==0 & predicted_gr[ss]> my_size_range[2]) { 
      gmx[,ss] <- c(rep(0, length(bin_mids)-1), 1)  } else { # deals with complete eviction-- OUT OF THE BOUNDS  of the kernel
        gmx[,ss] <- NA} 
    # this if statement breaks the code (puts NA's into the matrix) if the sum of the PDF is zero (which happens if all the probability is outside of the size bounds)
  } # end ss loop
  
  # make the surv*growth mx
  survgmx <- gmx*t(matrix( rep(predicted_sur,no_bins),no_bins)) # survs* growth # ZHENG CHECK TAHT IS IS ORIENTED CORRECTLY
  reprow <-  sur_vals*prep_vals*crep_vals # prob that you survive, reproduce, then number fruits|reproduce; this multiplcation does it element by element
  
  mx <- matrix(0, no_bins+1, no_bins+1)
  mx[2:(no_bins+1), 2:(no_bins+1)] = survgmx
  
  mx[1,2:(no_bins+1)] = reprow # FRUIT ROW
  
  sdlng_cdf <- pnorm(binedges,mean_seedling_size,sqrt(var_seedling_size)) # TRANSOFRMATIONS NOT RIGHT
  sdlng_pdf <- sdlng_cdf[2:length(binedges)]-sdlng_cdf[1:(length(binedges)-1)]
  if(sum(sdlng_pdf)>0){
    sdlng_pdf <- sdlng_pdf/sum(sdlng_pdf)} else {
      sdlng_pdf <- NA} 
  
  mx[2:(no_bins+1),1]  = predicted_rec *sdlng_pdf 
  
  mxes[ii,,] <- mx

  } # closes ii (TSFs) loop
  # multiply them together to get a matrix, etc. 
} # closes i (scenarios) loop
  
  
 data_to_predict <- data.frame()