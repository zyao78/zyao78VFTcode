load("/Users/amlouthan/Desktop/model_export_11_24_25.RData") # loads in the VR functions
TBF_long <- read.csv("/Users/amlouthan/Desktop/TBF_long_export_11_24_25.csv")


#size bounds for IPM
IPM_bounds <- range(TBF_long$s.logsize0, na.rm = TRUE) # ZHENG, NEED TO TRANSFORM TO LOG SIZE NOT SCALED LOG SIZE
no_bins <- 100
bin_edges <- seq(IPM_bounds[1], IPM_bounds[2], length.out= no_bins)
bin_mids <- zoo::rollmean(bin_edges,2)

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
  as.data.frame(matrix(NA, nrow= 22, ncol = 35))
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
possible_scenarios$s.TBF <- scale(rep(c(3, 11), each= 11) ,# raw TBF values
                                  center= mean(TBF_long$TBF, na.rm=TRUE), 
                                  scale= sd(TBF_long$TBF, na.rm=TRUE)) # ZHENG. deal with squares in a particular way-- this is bad b/c it both re-calculates mean & sd (thus your original scaling may not be used, by accident) &
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
  "s.P_RD ",
  "s.sq.T_RH",
  "s.sq.P_RA",
  "s.sq.P_RW",
  "s.sq.P_RD")]     <- 0 # ZHENG WORKAROUND, this is just to show you how the code is going-- setting all climate variable to their mean

possible_scenarios[, c("site")] <- "B1" # ZHENG CHANGE LATER; this only predicts pop gr rate for B1
log.fr <- log(1)

for (i in 1: dim(possible_scenarios)[1]) {

  i_scenario <- possible_scenarios[i,]
  
  TSFs <- 0:(i_scenario$FRI-1) # Zheng-- check this carefully. Does TSF range from 0 to (FRI-1) OR from 1 to FRI? 
  i_scenario <- i_scenario[rep(seq_len(nrow(i_scenario)), length(TSFs)), ]
  i_scenario$TSF <- TSFs
  i_scenario$s.TSF <- scale(TSFs, center= mean(TBF_long$TSF), scale= sd(TBF_long$TSF)) # Zheng change this too!!!!?? to something like: attr(TBF_long$s.TSF, "center")
  i_scenario$s.sq.TSF <- scale(TSFs^2, center= mean(TBF_long$TSF^2), scale= sd(TBF_long$TSF^2)) # Zheng change this too!!!!?? to something like: attr(TBF_long$s.TSF, "center")
  
  mxes <- array(NA, dim= c(length(TSFs), no_bins+1, no_bins+1))
  for (ii in TSFs){
  i_ii_scenario <- i_scenario[which(i_scenario$TSF== TSFs[ii]),]
  data_for_prediction <- i_ii_scenario[rep(seq_len(nrow(i_ii_scenario)), length(bin_mids)), ]
  data_for_prediction$s.logsize0 <-  bin_mids
  data_for_prediction_rec <- cbind(i_ii_scenario, log.fr)
  
  predicted_sur <- predict(survival_mod, newdata= data_for_prediction, type='response') # Zheng-- need to re-fit all vital rate functions iwth data that has no attributes. and nothing in matrix format!!!!!
  predicted_gr <- predict(growth_mod, newdata= data_for_prediction) # predicting log size
  predicted_pr <- predict(prep_mod, newdata= data_for_prediction, type='response')
  predicted_cr <- predict(crep_mod, newdata= data_for_prediction)# predicting log size
  predicted_vg <- predict(variance_mod, newdata= data_for_prediction, type='response') # this prediction predicting the variance in log size, not variance in SIZE per se
  predicted_rec <- predict(recruit_mod, newdata= data_for_prediction_rec, type='response')
  
  # make the TSF-specific matrix
  
  for (ss in 1:nobins) {
    growcdf <- pnorm(binedges,predicted_gr[ss],sqrt(predicted_vg[ss])) # TRANSOFRMATIONS NOT RIGHT-- DID YOU SQUARE WHEN YOU WERE FITTING THE VARIANCE IN THE 
    grows <- growcdf[2:length(binedges)]-growcdf[1:(length(binedges)-1)]
    if(sum(grows)>0){grows <- grows/sum(grows)
    gmx[,ss] <- grows
    } else if (sum(grows)==0 & grow_vals[ss]< my_size_range[1]) { 
      gmx[,ss] <- c(1, rep(0, length(binmids)-1))  
    } else if (sum(grows)==0 & grow_vals[ss]> my_size_range[2]) { 
      gmx[,ss] <- c(rep(0, length(binmids)-1), 1)  } else { # deals with complete eviction-- OUT OF THE BOUNDS  of the kernel
        gmx[,ss] <- NA} 
    # this if statement breaks the code (puts NA's into the matrix) if the sum of the PDF is zero (which happens if all the probability is outside of the size bounds)
  } # end ss loop
  
  # make the surv*growth mx
  survgmx <- gmx*t(matrix( rep(predicted_sur,nobins),nobins)) # survs* growth # ZHENG CHECK TAHT IS IS ORIENTED CORRECTLY
  reprow <-  sur_vals*prep_vals*crep_vals # prob that you survive, reproduce, then number fruits|reproduce; this multiplcation does it element by element
  
  mx <- matrix(0, nobins+1, nobins+1)
  mx[2:(nobins+1), 2:(nobins+1)] = survgmx
  
  mx[1,2:(nobins+1)] = reprow # FRUIT ROW
  
  sdlng_cdf <- pnorm(binedges,mean_seedling_size,sqrt(var_seedling_size)) # TRANSOFRMATIONS NOT RIGHT
  sdlng_pdf <- sdlng_cdf[2:length(binedges)]-sdlng_cdf[1:(length(binedges)-1)]
  if(sum(sdlng_pdf)>0){
    sdlng_pdf <- sdlng_pdf/sum(sdlng_pdf)} else {
      sdlng_pdf <- NA} 
  
  mx[2:(nobins+1),1]  = predicted_rec *sdlng_pdf 
  
  mxes[ii,,] <- mx

  } # closes ii (TSFs) loop
  # multiply them together to get a matrix, etc. 
} # closes i (scenarios) loop
  
  
 data_to_predict <- data.frame()