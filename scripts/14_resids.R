install.packages("DHARMa")
library(DHARMa)

par(mfrow = c(1, 1))
# simulate residuals  #https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html extended reading on DHARMa
residuals_sur_g <- simulateResiduals(fittedModel = sur, plot = F)
plotResiduals(residuals_sur_g, quantreg = TRUE)

plotResiduals(residuals_sur_g) 



residuals_vargrowth <- simulateResiduals(fittedModel=vargrowth_mod)   
plotResiduals(residuals_vargrowth)


plot(vargrowth_mod)
