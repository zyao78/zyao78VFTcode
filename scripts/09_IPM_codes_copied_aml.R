#############################################################
## Sarah Herzog
## 2024
#############################################################

##############################
### using demo data separetaly from C and R data, due to singularity issue when combined
##############################

##############################
#### 0 - Load libraries ####
##############################
install.packages("AICcmodavg", "lme4", "tidyverse", "MuMIn", "glue")

library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)

############################## 
##### 1 - Source files ####
##############################

sedum_long_full <- read_csv("ipm-example1-sedumlong.csv", col_types = 
                              cols(
                                ID = col_double(),
                                uniqueIDold = col_double(),
                                site = col_factor(),
                                transect = col_factor(),
                                TP = col_character(),
                                x = col_double(), 
                                y = col_double(),
                                trmt = col_factor(),
                                year = col_factor(),
                                rosnum = col_double(),
                                totalvol = col_double(),
                                frtno = col_double(),
                                BM_wlitter = col_double(),
                                BM_woutlitter = col_double(),
                                sur_to_next = col_double(),
                                year_next = col_factor(),
                                rosnum_next = col_double(),
                                totalvol_next = col_double(),
                                frtno_next = col_double(),
                                BM_wlitter_next = col_double(),
                                BM_woutlitter_next = col_double(),
                                bifrt = col_double(),
                                condfrtno = col_double(),
                                bifrt_next = col_double(),
                                condfrtno_next = col_double(),
                                log_totalvol = col_double(),
                                log_totalvol_next = col_double(),
                                elevation = col_factor(),
                                site2 = col_factor()
                              ))

seedlings_per_fruit <- read_csv("ipm-example1-seedlingRate.csv") %>% 
  mutate(elevation = as.factor(elevation))

source("ipm-example-1-helper.R")

# removal trmt seedling rate multiplier:
r_seedling_mult <- 2

##############################
##### 2 - Start code ####
##############################

#### vital rate functions for each vital rate: #### ----------------------------
# survival, binary fruits, conditional fruit number,growth, variance in growth



#### survival ####
# subset data to those observations that have no NAs for objects in global model
 sur_subset <- 
  TBF_data_long1 %>% 
  dplyr::filter(across(c(consur0_1, s.logsize0, TSF, TBF,site), ~ !is.na(.)))

# global model
sur <- glmer(consur0_1 ~ s.logsize0 + TSF *TBF   + (1|site), data= TBF_data_long1, family= "binomial", na.action = "na.omit")


sur_noNA <- glmer(consur0_1 ~ s.logsize0 + TSF *TBF   + (1|site), data= sur_subset, family= "binomial", na.action= "na.fail") # exclude NA by specifying the function to stop when it hits NAs


summary(sur_noNA)


sur_dredge <- MuMIn::dredge(sur_noNA) # dredge can only work for na.action="na.fail"
summary(sur_mod)

#'Best' model
sur_mod <- MuMIn::get.models(sur_dredge, 1)[[1]]
summary(sur_mod)
summary(sur_mod)$coefficients # get coefficients
sur_mod_vcov <- vcov(sur_mod) # get variance-covariance matrix
print(paste("Best fit model weight for survival:", round(sur_dredge$weight[1], 3))) # model weight

#### binary fruits ####
# subset 
bifrt_subset <- 
  TBF_data_long1 %>% 
  dplyr::filter(across(c(prep1, consur0_1, s.logsize0, site, TSF, TBF), ~ !is.na(.)))
bifrt_subset <- bifrt_subset[which(bifrt_subset$consur0_1== 1),]

#global
prep <- glmer(prep1 ~ s.logsize0+ TSF*TBF  + (1|site), data= bifrt_subset, family= "binomial", na.action = "na.fail")
summary(prep)

summary(glmer(prep1 ~ s.logsize0 + TSF + (1|site), data=bifrt_subset, family = "binomial", na.action="na.fail"))
# model subsets
bifrt_dredge <- MuMIn::dredge(prep)
# best model
bifrt_mod <- get.models(bifrt_dredge, 1)[[1]]
summary(bifrt_mod)
bifrt_mod_coeffs <- summary(bifrt_mod)$coefficients # get coefficients
bifrt_mod_vcov <- vcov(bifrt_mod) # get variance-covariance matric
print(paste("Best fit model weight for binary_fruiting:", round(bifrt_dredge$weight[1], 3))) # model weight


#### conditional fruit number ####

# subset 
frt_subset <- 
  TBF_data_long1 %>% 
  dplyr::filter(across(c(logcrep1, consur0_1, s.logsize0, site, TSF, TBF), ~ !is.na(.)))
frt_subset <- frt_subset[which(frt_subset$consur0_1== 1),]

#global

crep <- lmer(logcrep1 ~ s.logsize0 +TSF*TBF+ (1|site), data= frt_subset, na.action="na.fail")
summary(crep)
# model subsets
frt_dredge <- MuMIn::dredge(crep)
#best model:
frt_mod <- get.models(frt_dredge, 1)[[1]]
summary(frt_mod)
frt_mod_coeffs <- summary(frt_mod)$coefficients # get coefficients
frt_mod_vcov <- vcov(frt_mod) # get variance-covariance matric
print(paste("Best fit model weight for #fruits:", round(frt_dredge$weight[1], 3))) # model weight


#### growth ####
# subset 
growth_subset <- 
  TBF_data_long1 %>% 
  dplyr::filter(across(c( logsize1, s.logsize0, site, TSF, TBF), ~ !is.na(.)))
#global
growth_mod_g <- lmer(logsize1 ~ s.logsize0 + TSF*TBF + (1|site),
                     data = growth_subset, na.action = "na.fail") 
summary(growth_mod_g)
# # model subsets
growth_dredge <- MuMIn::dredge(growth_mod_g)
#best model:
growth_mod <- get.models(growth_dredge, 1)[[1]]
summary(growth_mod)
growth_mod_coeffs <- summary(growth_mod)$coefficients # get coefficients
growth_mod_vcov <- vcov(growth_mod) # get variance-covariance matric
print(paste("Best fit model weight for growth:", round(growth_dredge$weight[1], 3))) # model weight

#### variance in growth ####
TBF_data_long1$vargrowth <- NA
TBF_data_long1$predgrowth <- NA    # see predicted values
TBF_data_long1$predgrowth[which(!is.na(TBF_data_long1$s.logsize0) & 
                                  !is.na(TBF_data_long1$logsize1) & !is.na(TBF_data_long1$site) &
                                  !is.na(TBF_data_long1$TBF)& 
                                  !is.na(TBF_data_long1$TSF))] <- predict(growth_mod, newdata=TBF_data_long1[which(!is.na(TBF_data_long1$s.logsize0) & !is.na(TBF_data_long1$logsize1) & !is.na(TBF_data_long1$site) & !is.na(TBF_data_long1$TBF)& !is.na(TBF_data_long1$TSF)),])


TBF_data_long1$vargrowth[which(!is.na(TBF_data_long1$s.logsize0) & 
                                 !is.na(TBF_data_long1$logsize1) & !is.na(TBF_data_long1$site) &
                                 !is.na(TBF_data_long1$TBF)& 
                                 !is.na(TBF_data_long1$TSF))] <-
  (predict(growth_mod) - # variance is equivalent to (predicted-expected)^2
     TBF_data_long1$logsize1[which(!is.na(TBF_data_long1$s.logsize0) & 
                                     !is.na(TBF_data_long1$logsize1) & !is.na(TBF_data_long1$site) &
                                     !is.na(TBF_data_long1$TBF)& 
                                     !is.na(TBF_data_long1$TSF))])^2





# subset 
vargrowth_subset <- 
  TBF_data_long1 %>% 
  dplyr::filter(across(c(logsize1, s.logsize0, site, TSF, TBF,vargrowth ), ~ !is.na(.))) %>% 
  mutate(exp_vargrowth = exp(vargrowth)) #***** to prevent negative numbers later

vargrowth_mod_g <- lmer(vargrowth ~ s.logsize0 + TSF*TBF + (1|site), #  
                        data = vargrowth_subset, na.action = "na.fail") 
summary(vargrowth_mod_g)
# model subsets
vargrowth_dredge <- MuMIn::dredge(vargrowth_mod_g)
# ***** changed year to fixed effect to prevent "singular" problem

#best model:
vargrowth_mod <- get.models(vargrowth_dredge, 1)[[1]]
range(predict(vargrowth_mod)) # predicted variance can't be/shoudln't be negative
summary(vargrowth_mod)
vargrowth_mod_coeffs <- summary(vargrowth_mod)$coefficients # get coefficients
vargrowth_mod_vcov <- vcov(vargrowth_mod) # get variance-covariance matric
print(paste("Best fit model weight for var. in growth:", round(vargrowth_dredge$weight[1], 3))) # model weight


# ####### Make IPM ############## --------------------------------------------
# make sure nothing is weird
str(TBF_data_long1)

#make bounds for matrices and # of bins
maxsize <- max(c(TBF_data_long1$logsize0, TBF_data_long1$logsize1), na.rm=TRUE)
minsize <- min(c(TBF_data_long1$logsize0, TBF_data_long1$logsize1), na.rm=TRUE)

binno <- 100 # this is arbitrary
binedges <- seq(minsize, maxsize, length.out= binno+1)
binmidpoints <- (binedges[1:(length(binedges)-1)] + binedges[2:length(binedges)])/2

#size dist of plants recruiting into the pop

new_plants <- TBF_data_long1[grepl("\\bnew", TBF_data_long1$comm1), ]
new_plants <- new_plants[!grepl("no new", new_plants$comm1), ]     # identify and extract recruit from each year

mean_recruit_logsize <- mean(new_plants$logsize1, na.rm = TRUE)
  
var_recruit_logsize <- var(new_plants$logsize1, na.rm = TRUE)

recruitcdf <- pnorm(binedges,mean_recruit_logsize,sqrt(var_recruit_logsize))


# given the above cdf, how much probability falls within that bin?   or cdf_2 - cdf_1
smallplants_sizedistribution <- recruitcdf[2:length(binedges)]-recruitcdf[1:(length(binedges)-1)]


#### IPM #### ----------------------------------------------------------------
# want to predict survival, growth, var growth, binrep, condrep for each binmidpoint
# so let's make a new data frame we will use to do that
# dont need to include year since they have mean of 0???



TBF <- unique(TBF_data_long1$TBF)
TSF <- unique(TBF_data_long1$TSF)



vital_rates <- c("log_totalvol", "TSF", "TBF", "sur_vals", "grow_vals", "bifrt_vals", "frt_vals", "var_vals")

vital_rates_sensitivity <- vital_rates <- c("sur_vals", "grow_vals", "bifrt_vals", "frt_vals", "var_vals", "plantsperfruit")

# make blank arrays
mat_array <- array(data = NA, dim = c(binno, 
                                      binno, 
                              
                                      length(TSF), 
                                      length(TBF)),
                   dimnames = list(paste("bin",1:binno, sep = "_"),
                                   paste("bin",1:binno, sep = "_"),
                                  
                                   TSF,
                                   TBF))

lam_array <- array(data = NA, dim = c( 
                                      length(TSF), 
                                      length(TBF)),
                   dimnames = list(
                                   TSF,
                                   TBF))


pred_array <- array(data = NA, dim = c(binno, # predicted values
                                       length(vital_rates_sensitivity), 
                                       length(TSF), 
                                       length(TBF)),
                    dimnames = list(paste("bin",1:binno, sep = "_"), 
                                    vital_rates,
                                    TSF,
                                    TBF))# predicted values; for figures later

pred_df <- matrix(nrow = 1, ncol = length(vital_rates),
                  dimnames = list(c(), vital_rates))
flag = "lowered" # in case of an error



# initiate 
  
  for (i in 1:length(TSF)) { # TSF
    for (j in 1:length(TBF)) { # TBF
      
      # data to predict from
      mynewdata <- data.frame(binmidpoints, TSF[i], TBF[j])
      names(mynewdata) <- c("log_totalvol", "TSF", "TBF")
      
      # seedlings per fruit for site2 i
      smallplantsperfruit <- seedlings_per_fruit %>%
        full_join(distinct(select(sedum_long, site2, elevation)), by = "elevation") %>%  #add site2 names
        filter(site2 == sites[i]) %>%
        pull(seedlings_per_fruit)
      # # **** averaging across elevations
      # smallplantsperfruit <- seedlings_per_fruit %>%
      #   full_join(distinct(select(sedum_long, site2, elevation)), by = "elevation") %>%  #add site2 names
      #   mutate(seedlings_per_fruit = mean(seedlings_per_fruit)) %>%
      #   filter(site2 == sites[i]) %>%
      #   pull(seedlings_per_fruit)
      
      
      if (trmts[j] == "R") {smallplantsperfruit <- smallplantsperfruit * r_seedling_mult} # add multiplier (specified above) to removal trmt if needed:
      pred_array[, 'plantsperfruit', i, j, z] <- smallplantsperfruit
      
      #predict values
      sur_vals <- predict(sur_mod, mynewdata, type= "response")
      pred_array[,'sur_vals', i, j] <- sur_vals
      
      grow_vals <- predict(growth_mod, mynewdata, type= "response")
      pred_array[,'grow_vals', i, j, z] <- grow_vals
      
      bifrt_vals <- predict(bifrt_mod, mynewdata, type= "response")
      pred_array[,'bifrt_vals', i, j] <- bifrt_vals
      
      frt_vals <- predict_custom(frt_mod, mynewdata, type= "response")
      pred_array[,'frt_vals', i, j] <- frt_vals
      # Zheng-- note that I am not sure if fruits should be log-transformed or something here....depends on the structure of frt_mod
      
      var_vals <- predict(vargrowth_mod, mynewdata, type= "response")
      var_vals[var_vals < 0] <- 0.00000001 # replace values that are negative with small #
      pred_array[,'var_vals', i, j] <- var_vals
      
      # make the matrix
      gmx <- matrix(data=NA, nrow=length(binmidpoints), ncol=length(binmidpoints))
      
      # get transition matrix -- transitions among size classes, derived from growth and variance in growth
      for (ss in 1:binno) {
        # for a given starting size class, what is the cdf of the distribution of sizes in the next time step? (alert: tricky! )
        growcdf <- pnorm(binedges,grow_vals[ss],sqrt(var_vals[ss])) 
        # given the above cdf, how much probability falls within that bin?
        grows <- growcdf[2:length(binedges)]-growcdf[1:(length(binedges)-1)] 
        if(sum(grows)>0){
          grows <- grows/sum(grows) # renormalizes the pdf
          gmx[,ss] <- grows} else {gmx[,ss] <- NA} 
        # this if statement breaks the code (puts NA's into the matrix) if the sum of the PDF is zero (which happens if all the probability is outside of the max/ min size bounds)
      } # end ss loop
      
      # add in survival to the transition matrix
      survgmx <- gmx*t(matrix( rep(sur_vals,binno),binno)) # survs* growth
      # now, let's add reproduction! warning: THIS IS VERY TRICKY. don't want to introduce a delay... and need to include survival... 
      # in this case, adult plants produce seeds at a rate predicted by the frts per logsize relationship, and then to produce small plants in the 
      # next time step, those fruits shed seeds and those seeds germinate and survive to the next time step. 
      # Thus, we don't have a seedling class, or a seed class, so, here is what we do: 
      repmatrix <-   smallplants_sizedistribution %*%   # this gives you the size distribution of small plants produced in the next time step
        t(bifrt_vals*(frt_vals)*smallplantsperfruit) # this gives you the number of small plants produced by each largeplant's bin -- 
      # recall that conditional fruits was fitted as fruits/ size, hence the grow_vals term
      
      mx <- survgmx + repmatrix # this is our matrix/kernel whatever it's called
      lambda <- log(popbio::lambda(mx))    # NB: we could also calculate a series of matrices for different years, and then calculate stochastic lambda rather than just
      # getting the 'dominant eigenvalue' of the matrix, as we are here. 
      
      # add to array
      mat_array[,,i,j] <- mx
      lam_array[i,j] <- lambda
      
      # Sensitivity analysis -------------------------------------------------
      
      for (q in 1:length(vital_rates_sensitivity)) { # vital rate loop
        for (y in 1:2) { # add or subtract perturbations (1 for adding, 2 for subtracting)
          if (y == 1) {perturb_temp = perturb # add
          } else if (y == 2) {
            perturb_temp = -perturb # make this negative to later subtract
          } else {perturb_temp <- NA; print("ERROR: TOO MANY Ys GIVEN")}
          
          # reassign values from above so we dont overwrite them
          sur_vals_s <- sur_vals
          grow_vals_s  <- grow_vals
          bifrt_vals_s <- bifrt_vals
          frt_vals_s <- frt_vals
          var_vals_s <- var_vals
          smallplantsperfruit_s <- smallplantsperfruit
          
          # increase/decrease selected vital rate
          if (vital_rates_sensitivity[q] == "sur_vals") {
            sur_vals_s <- sur_vals_s * (1 + perturb_temp)
            sur_vals_s[sur_vals_s < 0] <- 0 # no values below 0
            sur_vals_s[sur_vals_s > 1] <- 1 # no values greater than 1
            delta_vitalrate_array[z, , i, j, q, y] <- sur_vals_s - sur_vals # store differences between perturbed and original
            
          } else if (vital_rates_sensitivity[q] == "grow_vals") {
            grow_vals_s <- grow_vals_s * (1 + perturb_temp)
            delta_vitalrate_array[z, , i, j, q, y] <- grow_vals_s - grow_vals # store differences between perturbed and original
            
          } else if (vital_rates_sensitivity[q] == "bifrt_vals") {
            bifrt_vals_s <- bifrt_vals_s * (1 + perturb_temp)
            bifrt_vals_s[bifrt_vals_s < 0] <- 0 # no values below 0
            bifrt_vals_s[bifrt_vals_s > 1] <- 1 # no values greater than 1
            delta_vitalrate_array[z, , i, j, q, y] <- bifrt_vals_s - bifrt_vals # store differences between perturbed and original
            
          } else if (vital_rates_sensitivity[q] == "frt_vals") {
            frt_vals_s <- frt_vals_s * (1 + perturb_temp)
            frt_vals_s[frt_vals_s < 0] <- 0 # no values below 0
            delta_vitalrate_array[z, , i, j, q, y] <- frt_vals_s - frt_vals # store differences between perturbed and original
            
          } else if (vital_rates_sensitivity[q] == "var_vals") {
            var_vals_s <- var_vals_s * (1 + perturb_temp)
            var_vals_s[var_vals_s < 0] <- 0 # no values below 0
            delta_vitalrate_array[z, , i, j, q, y] <- var_vals_s - var_vals # store differences between perturbed and original
            
          } else if (vital_rates_sensitivity[q] == "plantsperfruit") {
            smallplantsperfruit_s <- smallplantsperfruit_s * (1 + perturb_temp)
            smallplantsperfruit_s[smallplantsperfruit_s < 0] <- 0 # no values below 0
            delta_vitalrate_array[z, , i, j, q, y] <- smallplantsperfruit_s - smallplantsperfruit # store differences between perturbed and original
          } else print("too many vital rates given or dont match? In sensitivity loop")
          
          # find new lambda  
          # make the matrix
          gmx <- matrix(data=NA, nrow=length(binmidpoints), ncol=length(binmidpoints))
          
          # get transition matrix -- transitions among size classes, derived from growth and variance in growth
          for (ss in 1:binno) {
            # for a given starting size class, what is the cdf of the distribution of sizes in the next time step? (alert: tricky! )
            growcdf <- pnorm(binedges,grow_vals_s[ss],sqrt(var_vals_s[ss])) 
            # given the above cdf, how much probability falls within that bin?
            grows <- growcdf[2:length(binedges)]-growcdf[1:(length(binedges)-1)] 
            if(sum(grows)>0){
              grows <- grows/sum(grows) # renormalizes the pdf
              gmx[,ss] <- grows} else {gmx[,ss] <- NA} 
            # this if statement breaks the code (puts NA's into the matrix) if the sum of the PDF is zero (which happens if all the probability is outside of the max/ min size bounds)
          } # end ss loop
          
          # add in survival to the transition matrix
          survgmx <- gmx*t(matrix( rep(sur_vals_s,binno),binno)) # survs* growth
          repmatrix <-   smallplants_sizedistribution %*%   # this gives you the size distribution of small plants produced in the next time step
            t(bifrt_vals_s*(frt_vals_s)*smallplantsperfruit_s) # this gives you the number of small plants produced by each largeplant's bin -- 
          
          mx <- survgmx + repmatrix # this is our matrix/kernel whatever it's called
          lambda <- log(popbio::lambda(mx))    # NB: we could also calculate a series of matrices for different years, and then calculate stochastic lambda rather than just
          # getting the 'dominant eigenvalue' of the matrix, as we are here. 
          
          # add to array
          mat_array_s[,,i,j,z,q,y] <- mx
          lam_array_s[i,j,z,q,y] <- lambda
        }
      }
      
    } # end of iteration loop
    
    # average across predicted vals and add i and j info to cols
    # pred_df_temp <- as.data.frame(apply(pred_array, 1:2, median))
    # pred_df_temp[,1:ncol(mynewdata)] <- mynewdata
    # pred_df <- rbind(pred_df, pred_df_temp)
    
    
    #cat("done with i=", i,"j =",j, '\n') 
  }
  if(flag == "raised") {
    print("an error occured with a predict function")
    break
  }
  
  cat("done with itteration ", z, '\n')

# remove blank row from predicted
# pred_df <- pred_df %>% 
#   drop_na()

# average across lambdas
lam_mean <- 
  exp(as.data.frame(apply(lam_array, 1:2, median))) %>%  # note that lam is log(lam) and is therefore being exponentiated
  rownames_to_column(var = "site2") %>%
  pivot_longer(cols = R:C, names_to = "treatment", values_to = "lambda")
lam_95conf <- 
  exp(as.data.frame(apply(lam_array, 1:2, quantile, probs = c(0.025, 0.975)))) %>% 
  rownames_to_column(var = "quant") %>%
  mutate(quant = str_replace(quant, '\\%', ''),
         quant = paste0("quant_", quant)) %>%  # remove symbols and add prefix for column names later
  pivot_longer(L1.R:H2.C, names_to = c("site2", "treatment"), values_to = "conf", names_sep = "[.]") %>% 
  pivot_wider(names_from = quant, values_from = conf) # make confidence int. values into columns, not rows

## is the effect of neighbors on population growth rate different across populations? ----------------------



# calculate actual sensitivities
sensitivities_array <- array(data = NA, # easier format for analyses
                             dim = c(binno, 3, length(sites), length(vital_rates_sensitivity)),
                             dimnames = list(paste0("bin", 1:binno), c("mean_sens", "ci_low", "ci_high"), sites, vital_rates_sensitivity))
sensitivities_long <- array(data = NA, # easier to save format and better for making ggplots
                            dim = c(binno*length(sites)*length(vital_rates_sensitivity), 6),
                            dimnames = list(rep(paste0("bin", 1:binno), times = length(sites)*length(vital_rates_sensitivity)), 
                                            c("mean_sens", "ci_low", "ci_high", "combined", "site", "vital")))
z = 1
for (i in 1:length(sites)) { # site2
  for (q in 1:length(vital_rates_sensitivity)) {
    
    mn <- # mean
      apply(rbind(
        # up perturbation
        (lam_array_s[i, "C", , q, 1]- lam_array[i, "C",])/# delta_lambda 
          delta_vitalrate_array[,,i, "C", q, 1],  # delta vital rate
        # down perturbation
        (lam_array_s[i, "C", , q, 2]- lam_array[i, "C",])/# delta_lambda 
          delta_vitalrate_array[,,i, "C", q, 2]), # this  is delta vital rate
        2, mean) # average values across rows 
    
    ci <- # confidence interval
      apply(rbind(
        # up perturbation
        (lam_array_s[i, "C", , q, 1]- lam_array[i, "C",])/# delta_lambda 
          delta_vitalrate_array[,,i, "C", q, 1],  # delta vital rate
        # down perturbation
        (lam_array_s[i, "C", , q, 2]- lam_array[i, "C",])/# delta_lambda 
          delta_vitalrate_array[,,i, "C", q, 2]), # this  is delta vital rate
        2, quantile, probs = c(0.025, 0.975)) # average values across rows 
    
    # save to 
    sensitivities_array[, 1, i, q] <- mn
    sensitivities_array[, 2, i, q] <- ci[1,]
    sensitivities_array[, 3, i, q] <- ci[2,]
    
    sensitivities_long[z:(z+99), 1] <- mn
    sensitivities_long[z:(z+99), 2] <- ci[1,]
    sensitivities_long[z:(z+99), 3] <- ci[2,]
    sensitivities_long[z:(z+99), 4] <- paste0(round(mn,3),", (", round(ci[1,], 3),",",  round(ci[2,], 3), ")")
    sensitivities_long[z:(z+99), 5] <- as.character(sites[i])
    sensitivities_long[z:(z+99), 6] <- vital_rates_sensitivity[q]
    
    z = z + 100 # row to start at next loop
    
  }
}
sensitivities <- as.data.frame(sensitivities_long) %>% 
  rownames_to_column(var = "bin")


