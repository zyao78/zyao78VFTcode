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