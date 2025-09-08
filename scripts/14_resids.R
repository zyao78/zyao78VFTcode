install.packages("DHARMa")
library(DHARMa)

par(mfrow = c(1, 1))
# simulate residuals  #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html extended reading on DHARMa
residuals <- simulateResiduals(fittedModel = crep_mod_R, plot = F)
#plotQQunif(residuals_sur) ### KS test (test for correct distribution), dispersion test, outlier test
#plotResiduals(residuals_sur, quantreg = TRUE)

##  plotResiduals(residuals_sur, form = sur_subset_R$site)   plot residual against one of the model variable
plot(residuals)  # plot qq plot and simulated residual plot


