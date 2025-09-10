#simulate graphs
data_new1 <- as.data.frame((rep(mean(dfs$s.logsize0, na.rm=TRUE), 17*2))
                           ,(rep(mean(TBF_long$s.logsize0, na.rm=TRUE), 17*2)) )

names(data_new1) <- "s.logsize0"
TSF.s <- sort(unique(dfs$TSF), na.last = NA)    # pull scaled TSF and TBF from data subset from which model was built
TBF.s <- unique(dfs$TBF)
data_new1$TSF <- c(TSF.s,TSF.s)
data_new1$TBF <- c(rep(min(TBF.s), 17), rep(max(TBF.s), 17))
data_new1$site <- "GSP-BI"
data_new1$s.P_RD <- rep(mean(dfs$s.P_RD, na.rm=TRUE), 17*2)
data_new1$s.P_RH <- rep(mean(dfs$s.P_RH, na.rm=TRUE), 17*2)
data_new1$s.T_RA <- rep(mean(dfs$s.T_RA, na.rm=TRUE), 17*2)
data_new1$s.T_RD <- rep(mean(dfs$s.T_RD, na.rm=TRUE), 17*2)
data_new1$s.T_RH <- rep(mean(dfs$s.T_RH, na.rm=TRUE), 17*2)

data_new1$sur <- merTools::predictInterval(sur_mod_R_s, data_new1, which= "fixed", type= "probability", 
                                           level= 0.95,n.sims= 10000)

plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur$lwr, ymax = data_new1$sur$upr, fill= newTBF), alpha = 0.1) + 
  ylim(0,1) +xlim(c(-0.9, 3)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  theme(text = element_text(size = 20)) 
plt1



data_new1$gr <- merTools::predictInterval(gr, data_new1, which= "fixed", level= 0.95,n.sims= 10000)
data_new1$prep <- merTools::predictInterval(prep, data_new1, which= "fixed", type= "probability", level= 0.95,n.sims= 10000)
data_new1$crep1 <- exp(merTools::predictInterval(crep1, data_new1, which= "fixed", level= 0.95,n.sims= 10000))
data_new1$newTBF <- NA
data_new1$newTBF[which(data_new1$TBF== min(TBF.s))] <- "Short TBF"
data_new1$newTBF[which(data_new1$TBF== max(TBF.s))] <- "Long TBF"

library(ggplot2)
plt1 <- ggplot(data= data_new1, aes(x= TSF, y= sur$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$sur$lwr, ymax = data_new1$sur$upr, fill= newTBF), alpha = 0.1) + 
  ylim(0,1) +xlim(c(0, 16.5)) +
  labs(y= "Survival", x = "Time since fire", tag= "A") +
  theme_bw() + theme(legend.position='none') +
  theme(text = element_text(size = 20)) 
plt2 <- ggplot(data= data_new1, aes(x= TSF, y= gr$fit)) +
  geom_line(aes(color= newTBF))+ 
  geom_ribbon(aes(ymin = data_new1$gr$lwr, ymax = data_new1$gr$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 16.5)) +
  labs(y= "Growth", x = "Time since fire", tag= "B") + 
  theme_bw() + theme(legend.position='none') + theme(text = element_text(size = 20)) 
plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep$fit)) +
  geom_line(aes(color= newTBF)) + 
  ylim(0,1) + xlim(c(0, 16.5)) +
  geom_ribbon(aes(ymin = data_new1$prep$lwr, ymax = data_new1$prep$upr, 
                  fill= newTBF), alpha = 0.1) + 
  labs(y= "Prob. of fruiting", x = "Time since fire",  color= "", fill= "", tag= "C") + 
  theme_bw()  + 
  theme(legend.position = "none", legend.spacing.y = unit(-20, "pt")) +
  theme(text = element_text(size = 20)) 
plt4 <- ggplot(data= data_new1, aes(x= TSF, y= crep1$fit)) +
  geom_line(aes( color= newTBF))+xlim(c(0, 16.5)) +
  geom_ribbon(aes(ymin = data_new1$crep1$lwr, ymax = data_new1$crep1$upr, fill= newTBF), alpha = 0.1) + 
  labs(y= "Number of fruits", x = "Time since fire", tag= "D") +
  theme_bw() +
  theme(legend.position='none') + theme(text = element_text(size = 20))

ggpubr::ggarrange(plt1, plt2, plt3, plt4,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE,
                  legend = "right")

ggpubr::ggarrange(plt1, plt2,plt3,plt4)














data_new1 <- as.data.frame((rep(mean(recruit_df_5_14_2025$num_fr, na.rm=TRUE), 17*2)))
names(data_new1) <- "num_fr"
data_new1$TSF <- rep(seq(0, 16), 2)
data_new1 $TBF <- c(rep(1, 17), rep(15, 17))
data_new1$site <- "GSP-LI "
data_new1$num_recruit <- merTools::predictInterval(recruit_mod_g, data_new1, which= "fixed", type= "probability", 
                                           level= 0.95,n.sims= 10000)

data_new1$num_fruit_sim <- merTools::predictInterval(frt_mod, data_new1, which= "fixed", type= "linear.prediction", 
                                                   level= 0.95,n.sims= 10000)

data_new1$newTBF <- NA
data_new1$newTBF[which(data_new1$TBF== 1)] <- "Short TBF"
data_new1$newTBF[which(data_new1$TBF== 15)] <- "Long TBF"

# graph

ggplot(data= data_new1, aes(x= TSF, y= num_recruit$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$num_recruit$lwr, ymax = data_new1$num_recruit$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 17.5)) +
  labs(y= "recruit", x = "Time since fire", tag= "A") +
  theme_bw() + theme() +
  theme(text = element_text(size = 20)) 

ggplot(data= data_new1, aes(x= TSF, y= num_fruit_sim$fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$num_fruit_sim$lwr, ymax = data_new1$num_fruit_sim$upr, fill= newTBF), alpha = 0.1) + 
  xlim(c(0, 12.5)) +
  labs(y= "num_fruit", x = "Time since fire", tag= "A") +
  theme_bw() + theme() +
  theme(text = element_text(size = 20)) 