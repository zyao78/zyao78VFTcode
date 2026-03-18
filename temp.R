
url <- "https://www.dropbox.com/scl/fi/kxzc8fmomkigcrxyjjmdp/Butler-et-al.-Table-1.csv?rlkey=61ey3q1jc4rsor257uy4080j3&dl=1"
df1 <- read.csv(url)

plot(df1$Winter, df1$N, xlab = "Year", ylab = "Population count (z)", 
     xlim = c(1940, 2030), ylim = c(0, 1000), typ = "b", cex = 0.8, 
     pch = 20, col = rgb(0.7,0.7,0.7,0.9))




# Required functions
calc.lambda <- function(t,t0,gamma,lambda0){lambda0*exp(gamma*(t-t0))}
rdunif <- function(n,min,max){sample(min:max,size=n,replace=TRUE)}

# Algorithm settings
K.tries <- 10^4 # Number of simulated data sets to make

# Known random variables and parameters
t0 <- 1949
t <- 1950:2010
n <- 61

# Unknown random variables and parameters
parameters <- matrix(,K.tries,3) # Matrix to samples of save unknown parameters
colnames(parameters) <- c("gamma","lambda0","p")

y <- matrix(,K.tries,n) # Matrix to samples of save true unknown number of whooping cranes
z <- matrix(,K.tries,n) # Matrix to samples of save number of observed (counted) whooping cranes

set.seed(3110)
for(k in 1:K.tries){
  # Simulate from the prior predictive distribution
  gamma.try <- runif(1,0,0.1) # Parameter model for process model
  lambda0.try <- rdunif(1,2,50) # Parameter model for process model
  lambda.try <- calc.lambda(t = t,t0 = t0,gamma = gamma.try,lambda0 = lambda0.try) # Mathematical equation for process model
  y.try <- rpois(n,lambda.try) # Process model
  p.try <- runif(1,0.75,1) # Parameter model for process model
  z.try <- rbinom(n,y.try,p.try) # Data model
  
  # Save unkown random variables and parameters
  y[k,] <- y.try
  z[k,] <- z.try
  parameters[k,] <- c(gamma.try,lambda0.try,p.try)
}

# Plot data and 3 draws (k=1, 5 and 30) of z from the simulated data
plot(df1$Winter, df1$N, xlab = "Year", ylab = "True population size (y)", xlim = c(1940, 2010), ylim = c(0, 1000), typ = "b", cex = 0.8, pch = 20, col = rgb(0.7,0.7,0.7,0.9))
points(t,z[1,],col="red")
points(t,z[5,],col="purple")
points(t,z[30,])


################################################ WC example 2 ############

# Download and plot the data
url <- "https://www.dropbox.com/scl/fi/kxzc8fmomkigcrxyjjmdp/Butler-et-al.-Table-1.csv?rlkey=61ey3q1jc4rsor257uy4080j3&dl=1"
df1 <- read.csv(url)

plot(df1$Winter, df1$N, xlab = "Year", ylab = "Population count (z)", 
     xlim = c(1940, 2030), ylim = c(0, 300), typ = "b", cex = 0.8, 
     pch = 20, col = rgb(0.7,0.7,0.7,0.9))




# Approximate Bayes Computation algorithm (rejection sampling with error) used
# to fit whooping crane model to data
# Required functions
calc.lambda <- function(t,t0,gamma,lambda0){lambda0*exp(gamma*(t-t0))}
rdunif <- function(n,min,max){sample(min:max,size=n,replace=TRUE)}

# Algorithm settings
K.tries <- 10^6 # Number of simulated data sets to make
diff <- matrix(,K.tries,1) # Vector to save the measure of discrepancy between simulated data and real data
error <- 1000 # Allowable difference between simulated data and real data

# Known random variables and parameters
t0 <- 1949
t <- 1950:2050
z <- df1$N
n <- length(t) 

# Unknown random variables and parameters
posterior.samp.parameters <- matrix(,K.tries,3) # Matrix to samples of save unknown parameters
colnames(posterior.samp.parameters) <- c("gamma","lambda0","p")

y <- matrix(,K.tries,n) # Matrix to samples of save unknown number of whooping cranes

for(k in 1:K.tries){
  # Simulate from the prior predictive distribution
  gamma.try <- runif(1,0,0.1) # Prior or Parameter model
  lambda0.try <- rdunif(1,2,50) # Prior Parameter model 
  lambda.try <- calc.lambda(t = t,t0 = t0,gamma = gamma.try,lambda0 = lambda0.try) # Mathematical equation for process model
  y.try <- rpois(n,lambda.try) # Process model
  p.try <- runif(1,0.75,1) # Prior for data model
  z.try <- rbinom(n,y.try,p.try) # Data model
  
  # Record difference between draw of z from prior predictive distribution and observed data
  diff[k,] <- sum(abs(z - z.try[1:length(z)]))
  
  # Save unknown random variables and parameters
  y[k,] <- y.try
  posterior.samp.parameters[k,] <- c(gamma.try,lambda0.try,p.try)
}

# Calculate acceptance rate
length(which(diff<error))/K.tries

# Plot approximate posterior distribution of parameters
install.packages("latex2exp")    
library(latex2exp)
hist(posterior.samp.parameters[which(diff<error),1],col="grey",freq=FALSE,xlim=c(0,0.1),main="",
     xlab= TeX('$\\gamma  | \\textbf{z}$'),
     ylab = TeX('$\\lbrack\\gamma  | \\textbf{z}\\rbrack$'))
curve(dunif(x,0,0.1),col="deepskyblue",lwd=3,add=TRUE)

hist(posterior.samp.parameters[which(diff<error),2],col="grey",freq=FALSE,xlim=c(2,50),main="",
     breaks=seq(2,50,by=1),
     xlab= TeX('$\\lambda_0  | \\textbf{z}$'),
     ylab = TeX('$\\lbrack\\lambda_0  | \\textbf{z}\\rbrack$'))
curve(dunif(x,2,50),col="deepskyblue",lwd=3,add=TRUE)

hist(posterior.samp.parameters[which(diff<error),3],col="grey",freq=FALSE,xlim=c(0,1),main="",
     xlab= TeX('$\\p  | \\textbf{z}$'),
     ylab = TeX('$\\lbrack\\p  | \\textbf{z}\\rbrack$'))
curve(dunif(x,0,1),col="deepskyblue",lwd=3,add=TRUE)

# Plot data (z) and approximate posterior predictive distribution of y
plot(df1$Winter, df1$N, xlab = "Year", ylab = "True population size (y)", xlim = c(1940, 2050), ylim = c(0, 2000), typ = "b", cex = 0.8, pch = 20, col = rgb(0.7,0.7,0.7,0.9))
e.y <- colMeans(y[which(diff<error),])
points(t[which(t>2010)],e.y[which(t>2010)],typ="l",lwd = 2)
lwr.CI <- apply(y[which(diff<error),] , 2, FUN = quantile, prob = c(0.025))
upper.CI <- apply(y[which(diff<error),] , 2, FUN = quantile, prob = c(0.975))
polygon(c(t[which(t>2010)],rev(t[which(t>2010)])), c(lwr.CI[which(t>2010)],rev(upper.CI[which(t>2010)])), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
legend(x = 1940, y = 1000, cex = 1.3, legend = c(expression("E(" * y[pred] *"|" * z * ")"), "95% CI"), bty = "n", lty = 1, lwd = 2, col = c("black",rgb(0.5, 0.5, 0.5, 0.5)))
points(2025,557,pch=20,lwd=5,col="gold")

#https://www.fws.gov/press-release/2025-06/2025-wintering-whooping-crane-count

# Plot approximate posterior distribution of the year when y greater than or equal to 1000
hist(t[apply(y[which(diff<error),],1,FUN=function(x){which(x>999)[1]})],col="grey",freq=FALSE,xlim=c(2020,2060),main="",
     xlab= TeX('$\\t_{1000}  | \\textbf{z}$'),
     ylab = TeX('$\\lbrack\\t_{1000}  | \\textbf{z}\\rbrack$'))




save.image(file = "model assessment/data/model_export_mar_2026.RData")


n_cores <- detectCores()
cl <- makeCluster(n_cores - 1)
registerDoParallel(cl)
j_pop_gr_rate <- rep(NA, dim(possible_scenarios)[1]) ### changed to no_scenarios


all_pop_growth_rates <- foreach(
  j = 1:no_reps,
  .combine = cbind,
  .packages = c("MASS", "lme4", "popbio", "dplyr")
) %dopar% {
  rsur_coefs <- coefficients(survival_mod) # first, assign all "randomy selected" coefficients to the mean estimated coefficeints
  rsur_coefs[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")] <- MASS::mvrnorm(mu= coefficients(survival_mod), Sigma = vcov(survival_mod))[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")]
  #rsur_coefs <- lme4::fixef(survival_mod)
  #rsur_coefs <-  MASS::mvrnorm(mu= lme4::fixef(survival_mod) , Sigma = vcov(survival_mod))
  rgr_coefs <- lme4::fixef(growth_mod)
  rgr_coefs <-  MASS::mvrnorm(mu= lme4::fixef(growth_mod) , Sigma = vcov(growth_mod)) # no site effect in this model
  rprep_coefs <- coefficients(prep_mod)
  rprep_coefs[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")] <- MASS::mvrnorm(mu= coefficients(prep_mod), Sigma = vcov(prep_mod))[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")]
  rcrep_coefs <- lme4::fixef(crep_mod)
  rcrep_coefs <-  MASS::mvrnorm(mu= lme4::fixef(crep_mod) , Sigma = vcov(crep_mod)) # no site effect in this model
  rvar_coefs <- lme4::fixef(vargrowth_mod)
  rvar_coefs <-  MASS::mvrnorm(mu= lme4::fixef(vargrowth_mod) , Sigma = vcov(vargrowth_mod)) # no site effect in this model
  rrec_coefs <- lme4::fixef(recruit_mod)
  rrec_coefs <-  MASS::mvrnorm(mu= lme4::fixef(recruit_mod) , Sigma = vcov(recruit_mod)) # no site effect in this model
  #rrec_coefs <-  MASS::mvrnorm(mu= lme4::fixef(recruit_mod) , Sigma = vcov(recruit_mod)) 
  
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
      data_for_prediction$s.logsize0 <-   scale(bin_mids, center= attr(TBF_long$s.logsize0,"scaled:center"), 
                                                scale= attr(TBF_long$s.logsize0,"scaled:scale"))
      data_for_prediction_rec <- cbind(i_ii_scenario)
      
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
        
        m_sur <- model.matrix(sur_term ,data=s_data_for_prediction)
        p2_sur <- rsur_coefs %*% t(m_sur) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_sur <- plogis(p2_sur)   ## transform to binomial 
        predicted_sur_allsites[s,] <- p2_sur
        
        m_gr <- model.matrix(gr_term, data=s_data_for_prediction)
        p2_gr <- rgr_coefs %*% t(m_gr) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_gr_allsites[s,] <- p2_gr
        
        m_prep <- model.matrix(prep_term,data=s_data_for_prediction)
        p2_prep <- rprep_coefs %*% t(m_prep) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_prep <- plogis(p2_prep)   ## transform to binomial 
        predicted_pr_allsites[s,] <- p2_prep
        
        m_crep <- model.matrix(crep_term  ,data=s_data_for_prediction)
        p2_crep <- rcrep_coefs %*% t(m_crep) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_cr_allsites[s,] <- p2_crep
        
        m_vg <- model.matrix(vargrowth_term,data=s_data_for_prediction)
        p2_vg <- rvar_coefs %*% t(m_vg) # 
        predicted_vg_allsites[s,] <- exp(p2_vg)  
        
        m_rec <- model.matrix(rec_term , data=s_data_for_prediction_rec)
        p2_rec <- rrec_coefs %*% t(m_rec) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_rec_allsites[s,1] <- p2_rec*log(1.1)            #### account for composite rec variable (rec/log.fr)
        
        
        # for mixed models, the above functions will predict site-specific results (i.e., will incorporate random effects)
        rm(s_data_for_prediction) # this part removes the site-specific dataframe used in this loop, so you do not accidentaly use the previous sites' dataframe instead of the current one-- in case there was an error somehow
      }
      predicted_sur <- apply(predicted_sur_allsites, MARGIN= 2, FUN= mean) # average size-specific vital rates across sites
      predicted_gr <- apply(predicted_gr_allsites, MARGIN= 2, FUN= mean)
      predicted_pr <- apply(predicted_pr_allsites, MARGIN= 2, FUN= mean)
      predicted_cr <- apply(predicted_cr_allsites, MARGIN= 2, FUN= mean)
      predicted_vg <- apply(predicted_vg_allsites, MARGIN= 2, FUN= mean)
      predicted_vg[which(predicted_vg<0)] <- 0                 #### to prevent negative variance
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
    
    j_pop_gr_rate[i] <- (popbio::lambda(mx_prod))^(1/length(TSFs)) 
    # multiply them together to get a matrix, etc. 
  }
    j_pop_gr_rate
  
}


hist(as.numeric(all_pop_growth_rates[1, grepl("rep", colnames(all_pop_growth_rates))]))

histograms_gIPM <- lapply(1:22, function(i) {
  hist(
    as.numeric(all_pop_growth_rates[i, grepl("rep", colnames(all_pop_growth_rates))])
  )
})

plot(histograms_gIPM[[3]])

cols <- c("red", "blue", "green")

hist(
  as.numeric(all_pop_growth_rates[1, grepl("rep", colnames(all_pop_growth_rates))]),
  col = adjustcolor(cols[1]),
  xlab = "Lambda",
  main = "Overlay: scenarios 1–3"
)

hist(
  as.numeric(all_pop_growth_rates[2, grepl("rep", colnames(all_pop_growth_rates))]),
  col = adjustcolor(cols[2]),
  xlab = "Lambda",
  main = "Overlay: scenarios 1–3",
  add=TRUE
)















all_pop_growth_rates_Cro <- foreach(
  j = 1:no_reps,
  .combine = cbind,
  .packages = c("MASS", "lme4", "popbio", "dplyr")
) %dopar% {
  rsur_coefs <- coefficients(survival_mod) # first, assign all "randomy selected" coefficients to the mean estimated coefficeints
  rsur_coefs[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")] <- MASS::mvrnorm(mu= coefficients(survival_mod), Sigma = vcov(survival_mod))[!names(coefficients(survival_mod)) %in% paste("site", my_sites, sep="")]
  #rsur_coefs <- lme4::fixef(survival_mod)
  #rsur_coefs <-  MASS::mvrnorm(mu= lme4::fixef(survival_mod) , Sigma = vcov(survival_mod))
  rgr_coefs <- lme4::fixef(growth_mod)
  rgr_coefs <-  MASS::mvrnorm(mu= lme4::fixef(growth_mod) , Sigma = vcov(growth_mod)) # no site effect in this model
  rprep_coefs <- coefficients(prep_mod)
  rprep_coefs[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")] <- MASS::mvrnorm(mu= coefficients(prep_mod), Sigma = vcov(prep_mod))[!names(coefficients(prep_mod)) %in% paste("site", my_sites, sep="")]
  rcrep_coefs <- lme4::fixef(crep_mod)
  rcrep_coefs <-  MASS::mvrnorm(mu= lme4::fixef(crep_mod) , Sigma = vcov(crep_mod)) # no site effect in this model
  rvar_coefs <- lme4::fixef(vargrowth_mod)
  rvar_coefs <-  MASS::mvrnorm(mu= lme4::fixef(vargrowth_mod) , Sigma = vcov(vargrowth_mod)) # no site effect in this model
  rrec_coefs <- lme4::fixef(recruit_mod)
  rrec_coefs <-  MASS::mvrnorm(mu= lme4::fixef(recruit_mod) , Sigma = vcov(recruit_mod)) # no site effect in this model
  #rrec_coefs <-  MASS::mvrnorm(mu= lme4::fixef(recruit_mod) , Sigma = vcov(recruit_mod)) 
  
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
      data_for_prediction$s.logsize0 <-   scale(bin_mids, center= attr(TBF_long$s.logsize0,"scaled:center"), 
                                                scale= attr(TBF_long$s.logsize0,"scaled:scale"))
      data_for_prediction_rec <- cbind(i_ii_scenario)
      
      predicted_sur <- predicted_gr <-predicted_pr <- predicted_cr <- predicted_vg <- matrix(data=NA, nrow= length(my_sites), ncol= length(bin_mids))
      predicted_rec <- rep(NA, length(my_sites))
      
      ### make empty mxes before entering the site loop
      
      predicted_sur_allsites <-predicted_gr_allsites<-predicted_pr_allsites <-predicted_cr_allsites<-predicted_vg_allsites <-matrix(NA, nrow = length(my_sites), ncol = no_bins, dimnames = list(my_sites, NULL))
      predicted_rec_allsites <- matrix(NA, nrow = length(my_sites), ncol = 1, dimnames = list(my_sites, NULL))
      
      
      for (s in my_sites[3:4]){ #looping through the sites
        s_data_for_prediction <- data_for_prediction
        s_data_for_prediction_rec <- data_for_prediction_rec
        s_data_for_prediction$site <- s
        s_data_for_prediction_rec$site <- s
        
        s_data_for_prediction$site     <- factor(s_data_for_prediction$site,  levels = my_sites)   ## it is important to add the levels=my_sites here to ensure that the model matrix is built correctly
        s_data_for_prediction_rec$site <- factor(s_data_for_prediction_rec$site, levels = my_sites)
        
        s_data_for_prediction <- s_data_for_prediction %>% mutate(across(c("s.logsize0", "s.sq.TSF", "s.TSF", "s.TBF"), as.numeric))   ### change col type to numeric to avoid matrix col type
        # Zheng-- you will have to change the predict() call in the below to be a custom predict function that uses the random coefficients generated earlier in the code (e.g., rsur_coefs)
        
        ### build model matrices and use mx multiplication to obtain predicted values. Don't forget to transform if needed!!!!!!!!!
        
        m_sur <- model.matrix(sur_term ,data=s_data_for_prediction)
        p2_sur <- rsur_coefs %*% t(m_sur) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_sur <- plogis(p2_sur)   ## transform to binomial 
        predicted_sur_allsites[s,] <- p2_sur
        
        m_gr <- model.matrix(gr_term, data=s_data_for_prediction)
        p2_gr <- rgr_coefs %*% t(m_gr) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_gr_allsites[s,] <- p2_gr
        
        m_prep <- model.matrix(prep_term,data=s_data_for_prediction)
        p2_prep <- rprep_coefs %*% t(m_prep) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        p2_prep <- plogis(p2_prep)   ## transform to binomial 
        predicted_pr_allsites[s,] <- p2_prep
        
        m_crep <- model.matrix(crep_term  ,data=s_data_for_prediction)
        p2_crep <- rcrep_coefs %*% t(m_crep) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_cr_allsites[s,] <- p2_crep
        
        m_vg <- model.matrix(vargrowth_term,data=s_data_for_prediction)
        p2_vg <- rvar_coefs %*% t(m_vg) # 
        predicted_vg_allsites[s,] <- exp(p2_vg)  
        
        m_rec <- model.matrix(rec_term , data=s_data_for_prediction_rec)
        p2_rec <- rrec_coefs %*% t(m_rec) # check on whether this coef(survival_mod is a column or a row!!!!!!!!
        predicted_rec_allsites[s,1] <- p2_rec*log(1.1)            #### account for composite rec variable (rec/log.fr)
        
        
        # for mixed models, the above functions will predict site-specific results (i.e., will incorporate random effects)
        rm(s_data_for_prediction) # this part removes the site-specific dataframe used in this loop, so you do not accidentaly use the previous sites' dataframe instead of the current one-- in case there was an error somehow
      }
      predicted_sur <- apply(predicted_sur_allsites, MARGIN= 2, FUN= mean,na.rm = TRUE) # average size-specific vital rates across sites
      predicted_gr <- apply(predicted_gr_allsites, MARGIN= 2, FUN= mean,na.rm = TRUE)
      predicted_pr <- apply(predicted_pr_allsites, MARGIN= 2, FUN= mean,na.rm = TRUE)
      predicted_cr <- apply(predicted_cr_allsites, MARGIN= 2, FUN= mean,na.rm = TRUE)
      predicted_vg <- apply(predicted_vg_allsites, MARGIN= 2, FUN= mean,na.rm = TRUE)
      predicted_vg[which(predicted_vg<0)] <- 0                 #### to prevent negative variance
      predicted_rec <- mean(predicted_rec_allsites,na.rm = TRUE)
      
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
    
    j_pop_gr_rate_Cro[i] <- (popbio::lambda(mx_prod))^(1/length(TSFs)) 
    # multiply them together to get a matrix, etc. 
  }
  j_pop_gr_rate_Cr
}
