### fix convergence warninings 
# source : https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
install.packages("numDeriv")
install.packages("RCurl")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("plyr")
install.packages("RColorBrewer")

library("numDeriv")
library("RCurl") 
library("ggplot2"); theme_set(theme_bw())
library("reshape2")
library("plyr")
library("RColorBrewer")

##
m1 <- sur_mod_R
## check singularity

tt <- getME(m1,"theta")
ll <- getME(m1,"lower")
min(tt[ll==0])

#### double check gradient calculations
derivs1 <- m1@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

max(pmin(abs(sc_grad1),abs(derivs1$gradient)))

# similar process: check gradient using numDeriv package

dd <- update(m1,devFunOnly=TRUE)
pars <- unlist(getME(m1,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))

## try restarting from previous fit ..

ss <- getME(m1,c("theta","fixef"))
m2 <- update(m1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e5)))  ## if no warnings, stop from here

GM_RM_sur_2 <- m2

### try a different optimizer
m3 <- update(m1,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
sur_mod_R <- m3

summary(sur_mod_R)
