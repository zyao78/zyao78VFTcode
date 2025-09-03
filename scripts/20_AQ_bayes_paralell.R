##############################################################################
#                             Load libraries                                 #
##############################################################################
#Base packages
library(lme4)
library(MuMIn)
library(tidyverse)
library(popbio)
library(stringr)

#Packages for running parallel + loading bar
library(parallel)
library(doParallel)
library(doSNOW)
library(foreach)
##############################################################################

                       # Settings for running parallel

##############################################################################

# Set number of cores to use
n_cores <- parallel::detectCores() - 4  # Leave some cores free
#Make a Snow cluster based on specs using doSNOW package
cl <- makeCluster(n_cores)
#Register cluster object using doParallel package
registerDoParallel(cl)

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

#Make cattle a factor
data1$cattle <- factor(data1$cattle, levels=c("n", "y"))

str(data1)


#Site data; has climate, tsb, cattle, etc in each year in each plot
sitedata<-read.csv(file="input_data/all_years_climate_fire_withint.csv") 

#Let's assign plots at the same site with the same collection year, graze regime, and timesinceburn to a site-group
sitedata <- sitedata %>%
  group_by(site_orig, graze, interval) %>%
  mutate(plotgroup = paste0(site_orig, "_", cur_group_id())) %>%
  ungroup()

#make grazing a factor
sitedata$graze <- factor(sitedata$graze, levels=c("n", "y"))


#Take only the site data for each unique plot group
sitedata_unique <- sitedata %>%
  select(plotgroup, Year, collection_yr, graze, MAT, MAP, TSB, interval) %>%
  distinct()

#Save this info for use in creating site groups in real data

# write.csv(sitedata, file="input_data/site_data_with_groups.csv")

################################################################################
#                                                                              #
#                      Population Vectors (Real Data)                          #
#                                                                              #
################################################################################

load("input_data/sitegroup_maxadult_densities.RData") #mean adult count per plot (adult_counts)
adult_counts$max_count <- ceiling(adult_counts$total_std_count)


#  Add a site column (prefix before "_")
adult_counts <- adult_counts %>%
  mutate(site = str_extract(plotgroup, "^[^_]+"))

# Get the max adult count per site
site_max <- adult_counts %>%
  group_by(site) %>%
  summarise(max_count = max(total_std_count, na.rm = TRUE)) %>%
  ungroup()

#Make named vector to inc speed
site_max_vec <- setNames(site_max$max_count, site_max$site)


load("input_data/age_matrix.RData") #sitegroup_vector of individuals each age (age_matrix)

#Let's cut off the matrix at age 100, since we don't need anything older/simualtion is only 100 yrs

real_pops<-t(age_matrix[1:15, 1:100])


#################################################################################

                                # Constants #

################################################################################

#################################################################################
                              # Fruit fxn #
#################################################################################

#This function was selected previously by dredging
finfrt<- glm(frt~totprecip+tsb+cattle+ (totprecip:tsb) + (totprecip:cattle) + (cattle:tsb) + age, data=data1,
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


################################################################################

                    # Definitions for use in loop #

###############################################################################

ages_forfunction<- 1:100 #this is specifically for survival equation; only est survival for ages 1-100 (not cones/seedbank)

# Define age bins (same as used for real_pops)
age_bins <- unique(cut(
  ages_forfunction,
  breaks = seq(1, 101, by = 5),  # 1–5, 6–10, ..., 96–100
  right = FALSE,
  labels = paste(seq(1, 96, by = 5), seq(5, 100, by = 5), sep = "-")
))

ages <- c("seedbank","cones",ages_forfunction) #for use in making storage Npop

site_group <- unique(sitedata_unique$plotgroup) #vector of site_groups (used in loop)

totyears<-100#total number of years
years <- 1:100 #vector of years for initializing storage at each site_orig
set_start_no <- 5 #Starting population size
min_adult_age <- 22 #(age 20 in new vector, assuming first two entries are cones)
adults <- 22:102 #Vector of adult ages

#################################################################################

                        # Kernel function #

################################################################################


my_kernel <- function(a, b, b2, c, c2, d, d2, d3, #Survival equation parameters
                      young_decay, #modifies survival based on age
                      density_decay, #Modifies survival based on densities
                      ages_forfunction,
                      totprecip_forfunction, tsb_forfunction, cattle_forfunction, # make these direct inputs so you don't have to worry about format of a data frame; also, don't use the same label for objects inside & outside of the function
                      finfrt, # function describing fruiting rates
                      conecrop,
                      germination,
                      seed_sur, 
                      germ_seedbank) { 
  
  n_age_classes <- length(ages_forfunction)  # should be 100 (ages to be used for survival fxn; not relevant for cones)
  n_states <- 2 + n_age_classes  # seedbank, birdcones, notbirdcones, ages 1–100
  kernel <- matrix(0, nrow = n_states, ncol = n_states, dimnames=list(ages, ages))
  
  # Assign indices
  idx_seedbank <- 1
  idx_cones <- 2
  idx_age_first <- 3
  idx_age_last <- idx_age_first + n_age_classes - 1
  
  # Recruitment pathways (recruit from seedbank or on tree)
  kernel[idx_age_first, idx_seedbank] <- germ_seedbank * seed_sur
  kernel[idx_age_first, idx_cones] <- germination * seed_sur
  
  # Cones that didn’t germinate become part of the seedbank
  kernel[idx_seedbank, idx_cones] <- (1 - germination)
  
  
  young_modifier <- exp(-young_decay * (ages_forfunction )-1) #Determines how different params affect different ages
  density_modifier <- exp(density_decay * (ages_forfunction)) #Determines how different params affect different ages
  
  # Survival: logistic function using a, b, c, d
  survival_vec <- arm::invlogit(
    (a * (ages_forfunction - 20) * density_modifier) +          #Baseline age effect- survival is very low until about age 20; decreases toward 0 as density changes
      (b * totprecip_forfunction) +                               # Main effect of precipitation--Less precip decreases survival
      (b2 * totprecip_forfunction * young_modifier) +             # Youngest trees see a boost to survival with precipitation
      (c * tsb_forfunction) +                                     # Time since burn (linear)
      (c2 * tsb_forfunction^2) +                                  # Time since burn (quadratic) -----We assume that survival will inc after tsb, but later decrease
      (d * as.numeric(cattle_forfunction)) +                      # Cattle decrease survival of trees
      (d2 * as.numeric(cattle_forfunction) * young_modifier) +    # Stronger cattle effect for young trees
      (d3 * as.numeric(cattle_forfunction) * (tsb_forfunction == 0))  # Cattle × fire-year interaction--Conditional multiplier, cattle modifies survival if TSB==0; In this case increasing (Briggs 2002) 
  )
  
  for (i in 1:(length(survival_vec) - 1)) {
    kernel[idx_age_first + i, idx_age_first + i - 1] <- survival_vec[i]
  }
  
  # Predict fruiting probabilities using finfrt
  dat_pred <- data.frame(
    age = ages_forfunction,
    totprecip = totprecip_forfunction,
    tsb = tsb_forfunction,
    cattle = cattle_forfunction
  )
  
  frt_probs <- predict(finfrt, newdata = dat_pred, type = "response")
  
  # Fruiting contributions to cone states
  cone_contributions <- (frt_probs/2) * conecrop * survival_vec #Divide frt prob by 2--dioecious

  kernel[idx_cones, idx_age_first:idx_age_last] <- cone_contributions

  return(kernel)
}


################################################################################
                   # Parameters for survival equation#
################################################################################

# # Given priors
# a_priors <- 0.5   # age
# 
# b_priors <- 0.002  # precip
# b2_priors <- 0.004  # precip effect x age
# 
# c_priors <- 0.15    # tsb
# c2_priors <- -0.005 #quadratic tsb
# 
# d_priors <- -0.2   # cattle
# d2_priors <- -0.4 #cattle x age
# d3_priors <- 0.3 #cattle x fire year
# 
# ######### Function to generate priors
# make_prior_seq <- function(center, dev_frac = 0.5, n_vals = 4, min_dev = 0.001) {
#   # dev_frac = the fraction of 'center' to use for max_dev
#   # min_dev = minimum allowed deviation (helps for very small or zero centers)
#   
#   # If center is zero, fall back to min_dev
#   #Otherwise, allow it to deviate by half of absolute value
#   # max_dev <- max(abs(center) * dev_frac, min_dev)
#   
#   max_dev<-0.5
#   # Generate sequence
#   seq_vals <- seq(center - max_dev, center + max_dev, length.out = n_vals)
#   round(seq_vals, 6)
# }
# 
# # Generate sequences
# a_vals <- make_prior_seq(a_priors)
# 
# b_vals <- make_prior_seq(b_priors)
# b2_vals <- make_prior_seq(b2_priors)
# 
# 
# c_vals <- make_prior_seq(c_priors)
# c2_vals <- make_prior_seq(c2_priors)
# 
# 
# d_vals <- make_prior_seq(d_priors)
# d2_vals <- make_prior_seq(d2_priors)
# d3_vals <- make_prior_seq(d3_priors)
# 
# # Define acceptance threshold for rejection sampling
# accept_threshold <- 20  
# 
# param_grid <- expand.grid(a = a_vals,
#                           b = b_vals, b2=b2_vals,
#                           c = c_vals, c2=c2_vals,
#                           d = d_vals, d2 = d2_vals, d3=d3_vals)


###################################################################
##                  Equation Modifiers                           ##
###################################################################

young_decay=0.3 #Decay rate
density_decay=4 #Steep drop off in juvenile survival as density approaches max

################################################################################

                    #Testing: Make fake real populations #

################################################################################

# # Define age classes
# ages_forfake <- 1:100
# 
# # Define fake site groups (if not already defined)
# # Example: site_group <- c("siteA", "siteB", "siteC")
# # Uncomment and edit below if needed
# # site_group <- c("siteA", "siteB", "siteC")
# 
# # Create a matrix of fake population values
# 
# set.seed(42)  # for reproducibility
# 
# real_pops <- sapply(site_group, function(s) {
#   base <- 1000 * exp(-0.05 * ages_forfake)  # exponential decay
#   jittered <- base + rnorm(length(ages_forfake), mean = 0, sd = 10)  # add some noise
#   jittered[jittered < 0] <- 0  # ensure no negative values
#   round(jittered)
# })
# 
# # Convert to matrix with proper row names
# real_pops <- matrix(real_pops, nrow = length(ages_forfake), dimnames = list(age = ages_forfake, site = site_group))
# 
# # Check it
# head(real_pops)

################################################################################

#                          Storing and Tracking Progress                       #

###############################################################################
# Predefine final_populations as an empty list
final_populations <- list()

# Create progress bar
registerDoSNOW(cl)
#Rows in pairwise combos of params to be distributed across cores
n_iters <- nrow(param_grid)

#Progress bar
pb <- txtProgressBar(max = n_iters, style = 3)

# Create progress function
progress <- function(n) setTxtProgressBar(pb, n)

#Set options for loop to show progress bar as loops complete 
opts <- list(progress = progress)

################################################################################
################################################################################
#                                                                              #
#                                                                              #
#                       !     Ye Olde Loope !                                  #
#                                                                              #
#                                                                              #
################################################################################
################################################################################

n_iters=1000
#this uses doparalell to run the outer foreach loop, show below
#specify required iterations (which are spread across n cores)
#also specify packages
results <- foreach(i = 1:n_iters, .options.snow = opts,
                   .packages = c("dplyr", "stringr")) %dopar% {
  
#The rows in param_grid (difft combos of param values) are distributed across cores

 #Randomly draw from distributions
                     
a <- runif(1, min = 0, max = 1)         # centered on 0.5
b <- runif(1, min = -0.005, max = 0.005)    # centered on 0.002
b2 <- runif(1, min = -0.005, max = 0.005)    # centered on 0.004
c <- runif(1, min = 0, max = 0.3)       # centered on 0.15
c2 <- runif(1, min = -0.015, max = 0.005)   # centered on -0.005
d <- runif(1, min = -0.4, max = 0.4)          # centered on -0.2
d2 <- runif(1, min = -0.6, max = 0.6)      # centered on -0.4
d3 <- runif(1, min = 0, max = 0.5)        # centered on 0.3

young_decay <-runif(1, min = 0, max = 1)
density_decay <-runif(1, min = 0, max = 5)

  
  ### 1. Initialize population vectors for all years at a site_group   ##
  Npop <- list()
  for (s in site_group) { #List for each site group
    
    mat <- matrix(0, nrow = length(ages), ncol = length(years), #Vector of ages across years
                  dimnames = list(age = as.character(ages), year = as.character(years)))
    
    mat["1", 1] <- set_start_no
    
    Npop[[s]] <- mat
    
  }
  
  for (s in site_group) {
    
    ##  2. Getting site conditions    ##
    
    #Filter conditions to site group
    site_conditions <- sitedata_unique %>% filter(plotgroup == s)
    
    #Take data for the 99 years prior to collection
    collectyr <- unique(site_conditions$collection_yr)
    
    conditions_for_iteration <- site_conditions %>%
      filter(Year %in% (collectyr - 99):(collectyr - 1))
    
    #We will use data to iterate out population growth for 99 years (final vector will be collection year)
    
    #Cattle conditions will not change in this simulation; assume similar history over 100 years
    cattle_forfunction <- unique(conditions_for_iteration$graze)
    
    #Get the years to be used in simul; sort from oldest to newest
    yearsvec <- sort(unique(conditions_for_iteration$Year))
    
    for (yr in 1:(totyears - 1)) {
      year_i <- yearsvec[yr] #For first year...
      
      #Get TSB in year
      tsb_forfunction <- conditions_for_iteration$TSB[conditions_for_iteration$Year == year_i]
      
      #Get total precip in year
      totprecip_forfunction <- conditions_for_iteration$MAP[conditions_for_iteration$Year == year_i]
      
      ##  3.  Population projection    ##
            #Get vector for that year
      popvec <- Npop[[s]][, yr]
      
      #Get max density for site
      site_prefix <- str_extract(s, "^[^_]+")
      max_adults <- site_max_vec[[site_prefix]]
      
      #Get density modifier
      adult_density <- sum(popvec[adults])
      density_modifier <- exp(density_decay * ((adult_density / max_adults)^3))
      
      # get pop kernel based on conditions
      kernel <- my_kernel(a, b, b2, c, c2, d, d2, d3,
                          young_decay,
                          density_decay,
                          ages_forfunction,
                          totprecip_forfunction,
                          tsb_forfunction,
                          cattle_forfunction,
                          finfrt,
                          conecrop,
                          germination,
                          seed_sur,
                          germ_seedbank)
      

      #Multiply pop vector by kernel
      newvec <- kernel %*% popvec
     
      #Store as starting vector for next year
      Npop[[s]][, yr + 1] <- as.numeric(newvec)
    }
  }
  
  ##  4.   Evaluate parameter set by getting error    ##
  
  param_id <- sprintf("a=%.6f|b=%.6f|b2=%.6f|c=%.6f|c2=%.6f|d=%.6f|d2=%.6f|d3=%.6f|", a, b, b2, c, c2, d, d2, d3) #First store param IDs
  
  param_accepted <- TRUE #Baseline we assume params are accepted; may reject
  
  site_vectors <- list() #Create list to store year 100 vectors for site_groups
  
  total_error <- 0  # Initialize total error before loop
  
  for (s in site_group) {
  
    sim_pop <- Npop[[s]][3:102, length(years)]
    
    # Aggregate sim_pop into the same bins as realpops using tapply
    sim_pop_binned <- tapply(sim_pop, age_bins, sum)
    
    # Convert to numeric vector in correct order
    sim_pop_binned <- as.numeric(sim_pop_binned)
    names(sim_pop_binned) <- levels(age_bins)
    
    real_pop <- real_pops[, s]
    
    # Only compare entries where real_pop is not zero
    
    nonzero_idx <- real_pop != 0
    error <- sum(abs(sim_pop[nonzero_idx] - real_pop[nonzero_idx]))
    
    if (is.na(error)) {
      param_accepted <- FALSE
      break
    }
   
     if (isTRUE(error > accept_threshold)) {
      
      param_accepted <- FALSE
      break  # Stop early if error is too high
      
    } else if(error <= accept_threshold) {
      
      site_vectors[[s]] <- sim_pop
      total_error <- total_error + error  # Accumulate total error
      
    }
  }
  
  
  if (param_accepted==TRUE) {
    
    #return list for param_id with params, vectors and error
    #This will store in the appropriate list in "results"
    
    list(
      
      param_id = param_id,
      data = list(a = a,
                  b = b, b2 = b2,
                  c = c, c2 = c2,
                  d = d, d2 = d2, d3=d3,
                  site_vectors = site_vectors,
                  error = total_error)
      
    )
  } else {
    
    #If the params don't work, the list will store as NULL
    NULL
    
  }
}

#Close the loading bar
close(pb)


# Remove NULLs and reformat to named list
results_clean <- results[!sapply(results, is.null)]
final_populations <- setNames(lapply(results_clean, `[[`, "data"),
                              sapply(results_clean, `[[`, "param_id"))

stopCluster(cl)
