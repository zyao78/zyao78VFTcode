library(here)
library(readxl)
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)
library(MASS)
install.packages("DHARMa")
library(glmmTMB)
library(DHARMa)

#load data

dat.list <- list()

dat.list[[1]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx",  sheet = 1 ) #col_types = rep("text", 9))
dat.list[[1]]$site <- "CH"

dat.list[[2]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 2)
dat.list[[2]]$site <- "CM"

dat.list[[3]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 3)
dat.list[[3]]$site <- "B1"

dat.list[[4]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 4)
dat.list[[4]]$site <- "B2"

#dat.list[[5]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 5)
#dat.list[[5]]$site <- "IA"

dat.list[[5]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 6)
dat.list[[5]]$site <- "GSP-BI"

dat.list[[6]] <- read_xlsx("F:/VFT/data entry/2025/woody cover blank/woody cover template.xlsx", sheet = 7)
dat.list[[6]]$site <- "GSP-LI"

woody25 <- do.call("rbind", dat.list)

## fill in 0s and check for wrong entris
woody25$interc[is.na(woody25$interc)] <- 0

woody25$height <- NA
woody25$height <- woody25$legHeight - woody25$distance

wrong<- woody25[!is.na(woody25$height) & woody25$height < 0, ]
woody25[1216, "distance"] <- 40   # fix wrong entries
woody25[122, "distance"] <- NA   # fix wrong entries

table(woody25$legHeight)
hist(woody25$distance)
table(woody25$interc)




## percentage cover by intercep

woodyquad <- woody25 %>%
  group_by(site, quad, trans) %>%
  summarise( percent_woody = sum(interc == 1) / n(), meanHeight = mean(height, na.rm=TRUE), count_woody = sum(interc==1,na.rm=TRUE ) )

## left join the note
WoodyNote <-woody25[!is.na(woody25$Note),] 
  
  
woodyquad$note <- NA
for (i in 1:nrow(woodyquad)){
     match_index <- which(WoodyNote$quad == woodyquad$quad[i] &
                            WoodyNote$site == woodyquad$site[i]&
                            WoodyNote$trans == woodyquad$trans[i] )
  
     if (length(match_index) > 0){
       woodyquad$note[i] <- WoodyNote$Note[match_index]
     }
}

nrow(woodyquad[!is.na(woodyquad$note), ])


### add TSF and TBF
woodyquad$startyear <- 2024
woodyquad$TSF <- NA
woodyquad$TBF <- NA
woodyquad$TSF_M <- NA
woodyquad$TBF_M<- NA

fire_histories<- read_csv("processed-data/fire_histories_8_27_2025.csv")
LC_fire_2025 <- fire_histories[(fire_histories$record_type == "landsat"),]
M_fire_2025 <- fire_histories[(fire_histories$record_type == "manager"),]

for (i in 1:nrow(woodyquad)){
  match_index <- which( LC_fire_2025$site == woodyquad$site[i]&
                          LC_fire_2025$startyear == woodyquad$startyear[i] )
  
  if (length(match_index) > 0){
    woodyquad$TSF[i] <- LC_fire_2025$TSF[match_index]
    woodyquad$TBF[i] <- LC_fire_2025$TBF[match_index]
    
  }
}

for (i in 1:nrow(woodyquad)){
  match_index <- which( M_fire_2025$site == woodyquad$site[i]&
                          M_fire_2025$startyear == woodyquad$startyear[i] )
  
  if (length(match_index) > 0){
    woodyquad$TSF_M[i] <- M_fire_2025$TSF[match_index]
    woodyquad$TBF_M[i] <- M_fire_2025$TBF[match_index]
    
  }
}

###
### model building
## count woody
# since variance is significantly higher than the mean, I use negative binomial dist over poisson

woodysub1 <- woodyquad %>% 
  filter_at(vars(TSF,TBF, count_woody), all_vars(!is.na(.)))
woody.count.nb <- glmer.nb(count_woody ~ TSF*TBF+(1|site) , data = woodysub1, na.action = "na.fail")

woodysub4 <- woodyquad %>% 
  filter_at(vars(TSF_M,TBF_M, count_woody), all_vars(!is.na(.)))
woody.count.nb.M <- glmer.nb(count_woody ~ TSF_M*TBF_M+(1|site) , data = woodysub4, na.action = "na.fail")


## percent woody
woodysub2 <- woodyquad %>% 
  filter_at(vars(TSF,TBF, percent_woody), all_vars(!is.na(.)))
hist(woodysub2$percent_woody)

woody.percent <- glmmTMB(percent_woody ~ TSF*TBF+(1|site) , zi=~TSF*TBF+(1|site), data = woodysub2,    # zero-inflated beta distribution
                          beta_family(link = "logit")
                          ,na.action = "na.fail")


### double check whether poisson is indeed overdispersed
woody.count.poisson <- glmer(count_woody ~ TSF*TBF+(1|site) , data = woodysub1, 
                             family = poisson(link="log"),na.action = "na.fail")
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(woody.count.poisson) 

## woody height
woodysub3 <- woodyquad %>% 
  filter_at(vars(TSF,TBF, logHeight), all_vars(!is.na(.)))
hist(woodysub2$percent_woody)
woodyquad$logHeight <- NA
woodyquad$logHeight <- log(woodyquad$meanHeight+1)
hist(woodyquad$logHeight)
woody.height <- lmer(logHeight~TSF*TBF+(1|site), data=woodysub3)

### check anova
summary(woody.percent)
summary(woody.count.nb)
summary(woody.height)

car::Anova(woody.percent, type = 3)
car::Anova(woody.count.nb, type = 3)
car::Anova(woody.height, type = 3)



### export
read_csv("F:/VFT/VFT_github/zyao78VFTcode/processed-data/fire_histories_8_27_2025.csv")
