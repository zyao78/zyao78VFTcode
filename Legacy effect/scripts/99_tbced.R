data_new1$prep_fit <- NA
data_new1$prep_se  <-NA
data_new1$prep_lwr <-  NA
data_new1$prep_upr <-NA

data_new1$s.sq.TSF <- as.matrix(data_new1$s.sq.TSF)
data_new1$s.TBF <- as.matrix(data_new1$s.TBF)
data_new1$s.TSF <- as.matrix(data_new1$s.TSF)


prep_pred <- predict.glm(test, data_new1, type = "response", se.fit = TRUE)
data_new1$prep_fit <- prep_pred$fit
data_new1$prep_se <- prep_pred$se.fit

data_new1$prep_lwr <- prep_pred$fit - qnorm(0.975) * prep_pred$se.fit
data_new1$prep_upr <- prep_pred$fit + qnorm(0.975) * prep_pred$se.fit

#####

plt3 <- ggplot(data= data_new1, aes(x= TSF, y= prep_fit)) +
  geom_line(aes(color= newTBF)) + 
  geom_ribbon(aes(ymin = data_new1$prep_lwr, ymax = data_new1$prep_upr, fill= newTBF), alpha = 0.1) + 
  #ylim(0,1) +
  labs(y= "probability of reproduction", x = "Time since fire", tag= "C") +
  theme_bw() + theme(legend.position='none') +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = 0:10) +
  theme(text = element_text(size = 20)) 
plt3