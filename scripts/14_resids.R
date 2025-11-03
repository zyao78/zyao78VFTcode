install.packages("DHARMa")
library(DHARMa)

par(mfrow = c(1, 1))
# simulate residuals  #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html extended reading on DHARMa
residuals <- simulateResiduals(fittedModel = gr_mod_G, plot = F)
plot(residuals)  # plot qq plot and simulated residual plot

#plotQQunif(residuals_sur) ### KS test (test for correct distribution), dispersion test, outlier test
#plotResiduals(residuals_sur, quantreg = TRUE)

##  plotResiduals(residuals_sur, form = sur_subset_R$site)   plot residual against one of the model variable
hist(gr_mod_G$residuals)

### get raw residuals

residualPlots (crep_mod_G)
plot(fitted(prep_mod_R_s), residuals(prep_mod_R_s))
r_raw <- residuals(gr_mod_G, type = "response")

r_pear <- residuals(crep_mod_G, type = "pearson")

r_dev <- residuals(crep_mod_G, type = "deviance")
hist(r_raw)

summary(crep_mod_G)


#### check for outlier effect

install.packages("influence.ME")
library(influence.ME)

subset <- model.frame(sur_mod_G)
str(subset)
str(data_new1)

# reconstruct data used to fit
infl <- influence(gr_mod_G, obs = TRUE)
cooks.distance(infl)
plot(infl, which = "cook")
cooksD_data<-as.data.frame(cooks.distance(infl)) 
cooksD_data_select<-cooksD_data[cooksD_data>0.003,drop=FALSE,] 
cooksD_oultiers<-as.numeric(rownames(cooksD_data_select))
