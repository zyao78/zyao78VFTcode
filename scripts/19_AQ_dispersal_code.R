##############################################################################
#                             Load libraries                                 #
##############################################################################
library(lme4)
library(MuMIn)
library(tidyverse)
library(popbio)
library(parallel)

library(doParallel)
library(foreach)

#############################################################################
#                             Bring in OG data                              #
#############################################################################

#This is the age structure data; used for building the fruiting function
data1<-read.csv(file="input_data/ages_with_conditions.csv", header=T)

# Replace with zeros and 1s for fruiting rate
data1$frt <- ifelse(data1$frt %in% c("n", "N", "", NA), 0,
                    ifelse(data1$frt %in% c("Y", "y"), 1, data1$frt))
# Force fruiting to numeric just in case
data1$frt <- as.numeric(data1$frt)

#If tsb is NA, assume 100 yrs
data1$tsb <-  ifelse(is.na(data1$tsb), 100, data1$tsb)

#Make cattle a binary
data1$cattle_binary <- ifelse(data1$cattle %in% c("n", "N", "", NA), 0,
                              ifelse(data1$cattle %in% c("Y", "y"), 1, data1$cattle))

#make cattle numeric
data1$cattle_binary <- as.numeric(data1$cattle_binary)


str(data1)


#Site conditions data with groups based on similar burn and graze
sitedata<-read.csv("input_data/site_data_with_groups.csv", header=T)

#We only want the one set of conditions for k20b and k20c, which had 2 collection years
sitedata_fixed <- sitedata %>%
  mutate(collection_yr = ifelse(plotgroup %in% c("k20b_10", "k20c_11") &
                                  collection_yr == 2021, 2022, collection_yr))


#Take only the site data for each unique plot group
sitedata_unique <- sitedata_fixed %>%
  select(plotgroup, Year, collection_yr, cattle_binary, MAT, MAP, TSB, interval) %>%
  distinct()

sitedata_unique$cattle_binary <- as.numeric(sitedata_unique$cattle_binary)


#################################################################################

                                # Constants #

################################################################################

#################################################################################
# Fruit fxn #
#################################################################################

#This function was selected previously by dredging
finfrt<- glm(frt~totprecip+tsb+cattle_binary+ (totprecip:tsb) + (totprecip:cattle_binary) + (cattle_binary:tsb) + age, data=data1,
             family = "binomial",
             na.action = "na.fail")



#################################################################################
# Cone stuff#
################################################################################

#Cones need to be produced, then ripen, then germinate

crop <- 805000 # avg crop per tree from holthuijzen, across a high and low yield year

#These values used if dispersal happening (From Holthuijzen)
prop_bird <-0.653 #rate at which ripe cones removed by birds
prop_tree <- 0.116 #rate at which ripe cones dispersed near tree
birdcrop <- crop * prop_bird #Amount of crop moved by birds
nonbirdcrop <- crop * prop_tree # Amount of crop direcly dropped near tree

#This mean value used with no dispersal
conecrop <-(birdcrop + nonbirdcrop)/2 #Let's use the mean crop size for no dispersal

#These values are used if dispersal happening
birdgerm<-0.413 #rate at which bird dispersed cones germinate (Holthuiijzen)
nonbirdgerm <- 0.1 #rate at which cones which have not been dispersed by birds OR been depulped by others germinates (Holthuijzen but I think he got from someone else)

#Mean value used if no dispersal
germination <- (birdgerm+nonbirdgerm)/2 #Let's use the mean for doing the model without dispersal

#Other values: Necessary regardless of whether dispersal happening or not
germ_seedbank <- 0.05 #rate of germination from seedbank (Holthuijzen)
seed_sur = 0.8545 #Survival of newly germinated seedlings from Spring-Summer (Walton ppr)


#################################################################################
                      # Parameters for survival equation#
################################################################################

a=1.24770384
b=-0.17157273
c=0.49217549
d=0.04528923
e=-0.34301611
f=5.87967016
g=-3.2649320



################################################################################

                               # For iteration #

###############################################################################
ages_forfunction<- c(1:102) #this is specifically for survival
binedges <- 1:105
set_start_no <- 50

ages <- c("seedbank","birdcones", "nonbirdcones",1:102) #for use in making storage Npop
n_ages <- length(ages)

sites<-unique(sitedata$site_orig)
burnintervals<-c(1,2,5,10,20)
cattle<- c("ungraze", "graze") #yes then no
cattle_bin <- c(0,1)
locations <- 1:200

#Shape parameters for rgamma
alpha<-0.9
theta<-1.5

#Number of "tiles" in dispersal
tiles <-200

#total number of years
totyears<-100
years <- 1:totyears #vector of years for burn simul


#################################################################################

                              # Kernel function #

################################################################################


my_kernel <- function(a,b,c,d,e,f,g,
                      ages_forfunction,
                      totprecip_forfunction, tsb_forfunction, cattle_forfunction, # make these direct inputs so you don't have to worry about format of a data frame; also, don't use the same label for objects inside & outside of the function
                      finfrt, # function describing fruiting rates
                      birdcrop,
                      nonbirdcrop,
                      birdgerm, 
                      nonbirdgerm,
                      seed_sur, 
                      germ_seedbank) { 
  
  n_age_classes <- length(ages_forfunction)  # should be 102 (ages to be used for survival fxn; not relevant for cones)
  n_states <- 3 + n_age_classes  # seedbank, birdcones, notbirdcones, ages 1–102
  kernel <- matrix(0, nrow = n_ages, ncol = n_ages, dimnames=list(ages, ages))
  
  # Assign indices
  idx_seedbank <- 1
  idx_cones_bird <- 2
  idx_cones_notbird <- 3
  idx_age_first <- 4
  idx_age_last <- n_ages
  
  # Recruitment pathways (recruit from seedbank or on tree)
  kernel[idx_age_first, idx_seedbank] <- germ_seedbank * seed_sur
  kernel[idx_age_first, idx_cones_bird] <- birdgerm * seed_sur
  kernel[idx_age_first, idx_cones_notbird] <- nonbirdgerm * seed_sur
  
  # Cones that didn’t germinate become part of the seedbank
  kernel[idx_seedbank, idx_cones_bird] <- (1 - birdgerm)
  kernel[idx_seedbank, idx_cones_notbird] <- (1 - nonbirdgerm)
  
  # Survival: logistic function using a, b, c, d
  survival_vec <- arm::invlogit(
    (a * (ages_forfunction)) + #Baseline age effect- survival is very low until about age 20; decreases toward 0 as density changes
      (b*(totprecip_forfunction)) +
      (c * (totprecip_forfunction)*(ages_forfunction)) +
      (d* (tsb_forfunction)) +
      (e* (cattle_forfunction))+
      (f* (cattle_forfunction) * (tsb_forfunction) )+ 
      (g * (tsb_forfunction) * (ages_forfunction))
    ) 
  
  
  # Place survival rates along rows corresponding to ages
  for (i in 1:(length(survival_vec) - 1)) {
    kernel[idx_age_first + i, idx_age_first + i - 1] <- survival_vec[i]
  }
  
  
  # Predict fruiting probabilities using finfrt
  dat_pred <- data.frame(
    age = ages_forfunction,
    totprecip = totprecip_forfunction,
    tsb = tsb_forfunction,
    cattle_binary = cattle_forfunction
  )
  
  frt_probs <- predict(finfrt, newdata = dat_pred, type = "response")
  
  # Fruiting contributions to cone states
  birdcone_contributions <- frt_probs * birdcrop * survival_vec
  nonbirdcone_contributions <- frt_probs * nonbirdcrop * survival_vec
  
  kernel[idx_cones_bird, idx_age_first:idx_age_last] <- birdcone_contributions
  kernel[idx_cones_notbird, idx_age_first:idx_age_last] <- nonbirdcone_contributions
  
  return(kernel)
}

###############################################################################

                                #Iterate kernel#

##############################################################################


n_cores <- parallel::detectCores() - 4  # Reserve one core for the OS
cl <- makeCluster(n_cores)
registerDoParallel(cl)


#this code spreads different treatment combos of my dispersal code across cores
treatments <- expand.grid(site = sites,
                          burn = burnintervals,
                          cattle = cattle_bin,
                          stringsAsFactors = FALSE)

#do paralell across sites
Npop_list <- foreach(i = 1:nrow(treatments), .packages = c("dplyr"), .export = ls()) %dopar% {
                                   
  site <- treatments$site[i]
  burni <- treatments$burn[i]
  cattlei <- treatments$cattle[i]
  
  
   totprecip_forfunction <- mean(sitedata$MAP[sitedata$site_orig == site])
   
   tsblist <- (years - 1) %% burni
   
   pop_list <- list()
   for (yr in years) {
     mat <- matrix(0, nrow = n_ages, ncol = tiles,
                   dimnames = list(age = as.character(ages), loc = locations))
     pop_list[[yr]] <- mat
   }
   
   # Set initial population (e.g., age 13 in location 1, year 1)
   pop_list[[1]][13, 1] <- set_start_no
   
   for (yr in 2:(totyears)) { #Starting from year 2
     
     tsb_forfunction <- tsblist[yr]
     cattle_forfunction <- cattlei
     
     dispersed_in_timestep<-vector("list", length(locations))
     
     for (loc in 1:tiles) {
       popvec <- pop_list[[yr - 1]][, loc]
       
       kernel <- my_kernel(a,b,c,d,e,f,g,
                           ages_forfunction,
                           totprecip_forfunction,
                           tsb_forfunction,
                           cattle_forfunction,
                           finfrt,
                           birdcrop,
                           nonbirdcrop,
                           birdgerm,
                           nonbirdgerm,
                           seed_sur,
                           germ_seedbank)
       
       # Multiply kernel × population vector
       newvec <- kernel %*% popvec
       
       # update age classes except birdcones
       pop_list[[yr]][c(1, 3:105), loc] <- as.numeric(newvec[c(1, 3:105)])
       birdcones<- newvec[2] #take birdcones at location/yr for dispersal
                                           
       if (birdcones > 0) {
         n_disp <- round(birdcones / 2)
         disp_pos <- round(rgamma(n_disp, shape = alpha, scale = theta))
         disp_neg <- round(-rgamma(n_disp, shape = alpha, scale = theta))
         all_dispersal_dists <- c(disp_pos, disp_neg)
         disp_summary <- table(all_dispersal_dists)
         disp_df <- data.frame(disp_summary)
         disp_df$all_dispersal_dists <- as.integer(as.character(disp_df$all_dispersal_dists))
         disp_df <- disp_df %>%
           filter(all_dispersal_dists + loc > 0 & all_dispersal_dists + loc <= tiles)
         disp_df$new_loc <- disp_df$all_dispersal_dists + loc
         dispersed_in_timestep[[loc]] <- disp_df
       }
     }
     
     # Apply dispersal
     for (entry in dispersed_in_timestep) {
       if (!is.null(entry)) {
         for (j in 1:nrow(entry)) {
           new_loc <- entry$new_loc[j]
           freq <- entry$Freq[j]
           pop_list[[yr]][2, new_loc] <- pop_list[[yr]][2, new_loc] + freq
         }
       }
     }
   }
   
   # Return result for one treatment combo
   list(site = site, burn = burni, cattle = cattlei, pop = pop_list)
}

names(Npop_list) <- sites
Npop <- Npop_list
stopCluster(cl)


save(Npop, file="redcedar_sites_dispersal.Rdata")
