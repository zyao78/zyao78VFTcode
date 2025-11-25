install.packages("ACImodavg")
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)

#############
TBF_long <- read.csv("data/TBF_long_export_11_24_25.csv")
alldemodata_upto2025 <- read.csv("data/VFT master data/alldemodata_upto2025.csv")

str(TBF_long)
TBF_long$ID[which(TBF_long$ID == "71.099999999999994")] <- "71.1"
TBF_long$ID[which(TBF_long$ID == "73.099999999999994")] <- "73.1"
TBF_long$ID[which(TBF_long$ID == "68.099999999999994")] <- "68.1"

table(TBF_long$quad) ### pull unique values in quad
TBF_long$quad[which(TBF_long$quad == "E")] <- "east"
TBF_long$quad[which(TBF_long$quad == "S")] <- "south"
TBF_long$quad[which(TBF_long$quad == "W")] <- "west"
TBF_long$quad[which(TBF_long$quad == "N")] <- "north"

sum(is.na(TBF_long$quad))
unique(TBF_long$quad)

############################### if not present, create these columns #################


TBF_long$qs <- "N"
TBF_long$x <- NA
TBF_long$y <- NA
TBF_long$site_ID <- paste(TBF_long$site, TBF_long$ID, sep = "_")
TBF_long <- TBF_long %>%
  select(x,y,site_ID, everything())
for (i in 1:nrow(TBF_long)) {
   #Find the matching index in TBF_long based on the 'site_ID' column
  match_index <- which(alldemodata_upto2025$site_ID == TBF_long$site_ID[i])
  
  # If there's a match, update the 'quad' value in TBF_long
 if (length(match_index) > 0) {
    TBF_long$x[i] <- alldemodata_upto2025$x[match_index]
    TBF_long$y[i] <- alldemodata_upto2025$y[match_index]
 }} 
empty_coord <- TBF_long[is.na(TBF_long$x),]
empty_coord <- TBF_long[is.na(TBF_long$y),]



#################### add quad num to GSP sites ##################################

GSP_sites <- TBF_long[TBF_long$site %in% c("GSP-BI", "GSP-LI"), ]
str(GSP_sites)
GSP_sites$quad_num <- NA
GSP_sites$quad_num <- cut(
  GSP_sites$x,
  breaks = seq(0, 1000, by = 50),  # Adjust 1000 if needed for higher ranges
  labels = 1:20,                   # Labels quadrats as 1, 2, 3, ..., 20
  right = F ,    # Exclude the upper boundary in each interval
  include.lowest = TRUE     
)
GSP_sites$quad <- paste(GSP_sites$quad, GSP_sites$quad_num, sep = "-")
table(GSP_sites$quad)

for (i in 1:nrow(TBF_long)) {
  # Find the matching index in GSP_sites based on the 'key' column
  match_index <- which(GSP_sites$site_ID == TBF_long$site_ID[i])
  
  # If there's a match, 
  if (length(match_index) > 0) {
    TBF_long$quad[i] <- GSP_sites$quad[[match_index[1]]]   
  }
} 
table(TBF_long$quad)   ### quad nums for transect-sites were successfully updated 

# find qs
rows_with_qs <- TBF_long[grep("\\bqs", TBF_long$comm1, ignore.case = TRUE), ]  # case insensitive
rows_with_qs <- rows_with_qs[!grepl("qs/ns", rows_with_qs$comm1,ignore.case = TRUE), ]
rows_with_qs <- rows_with_qs[!grepl("qs/qns", rows_with_qs$comm1,ignore.case = TRUE), ]
rows_with_qs <- rows_with_qs[!grepl("qns/qs?", rows_with_qs$comm1,ignore.case = TRUE), ]

TBF_long$qs <- "N"

TBF_long$key <- paste(TBF_long$site, TBF_long$ID, TBF_long$startyear, sep = "_")
TBF_long$key_qsy <- paste(TBF_long$quad, TBF_long$site, TBF_long$startyear, sep = "_")

rows_with_qs$key <- paste(rows_with_qs$site, rows_with_qs$ID, rows_with_qs$startyear, sep = "_") # make keys for matching by site_ID_year
rows_with_qs$key_qsy <- paste(rows_with_qs$quad, rows_with_qs$site, rows_with_qs$startyear, sep = "_") # make keys for matching by site_ID_year

TBF_long$qs[TBF_long$key_qsy %in% rows_with_qs$key_qsy] <- "Y"

table(TBF_long$qs) #check correct number of Y and N

# define search range x

rows_with_qs$startx <- as.numeric(sub(".*[qQ][sS][ -]*(\\d+).*", "\\1", rows_with_qs$comm1))
rows_with_qs$endx <- as.numeric(sub(".*[qQ][sS][ -]*\\d+[ -]+(\\d+).*", "\\1", rows_with_qs$comm1))
rows_with_qs<- rows_with_qs %>%
  select(startx, endx, everything())
rows_with_qs$startx[ rownames(rows_with_qs) == "34545" ] <- -25   ## manually assign the negative start x 
rows_with_qs$startx[ rownames(rows_with_qs) == "35770" ] <- -25

rows_with_qs <- rows_with_qs[!is.na(rows_with_qs$startx) & !is.na(rows_with_qs$endx), ] # delete rows with empty start/end x

for (i in 1:nrow(rows_with_qs)) {
  if (!is.na(rows_with_qs$startx[i])) 
    if (rows_with_qs$startx[i] > rows_with_qs$endx[i]) {
      temp <- rows_with_qs$startx[i]
      rows_with_qs$startx[i] <- rows_with_qs$endx[i]
      rows_with_qs$endx[i] <- temp
    }
  
}   ## in case start and end xs were labeled backward, flip them

nrow(look <- rows_with_qs %>%
  filter(startx > endx))   ### 0, good!




#################################################################################
################################## count num of news ############################


rows_with_new <- TBF_long[grepl("\\bnew", TBF_long$comm1), ]
rows_with_new <- rows_with_new[!grepl("no new", rows_with_new$comm1), ] # delete "no new"
rows_with_new <- rows_with_new[!grepl("no news", rows_with_new$comm1), ] # delete "no new" 
rows_with_new <- rows_with_new[!grepl("not possible to see new plants", rows_with_new$comm1), ]
rows_with_new <- rows_with_new[!grepl("No news", rows_with_new$comm1), ]
rows_with_new <- rows_with_new[!grepl("new lvs", rows_with_new$comm1), ]
rows_with_new <- rows_with_new[!grepl("new nail", rows_with_new$comm1), ]
table(rows_with_new$comm1) ## manual checks for weird phrasings 

rows_with_new$startx <- NA
rows_with_new$endx <- NA
rows_with_qs$num_news <- NA

for (i in 1:nrow(rows_with_qs)) { # count num of fruit
  quad_i <- rows_with_qs $ quad [i]
  startyear_i <- rows_with_qs $ startyear [i]
  startx_i <- rows_with_qs $ startx[i]
  endx_i <- rows_with_qs$endx[i]
  site_i <- rows_with_qs$site[i]
  starty_i <- 25
  endy_i <- 50
  rows_with_qs$num_news [i]= nrow(
    rows_with_new [rows_with_new$quad == quad_i & rows_with_new$x>=startx_i & rows_with_new$x<=endx_i & rows_with_new$startyear==startyear_i&
                      rows_with_new$site==site_i & rows_with_new$y>=starty_i & rows_with_new$y<=endy_i,]
  )
  } # find number of news that satisify these rules within the rows_with_new1 df


rows_with_qs$num_fr <- NA 

i=1

for (i in 1:nrow(rows_with_qs)) {
  quad_i <- rows_with_qs $ quad [i]
  startyear0_i <- rows_with_qs $ startyear [i]    ### count fruits produced in the previous startyear
  startx_i <- rows_with_qs $ startx[i]
  endx_i <- rows_with_qs$endx[i]
  site_i <- rows_with_qs$site[i]
  starty_i <- 25
  endy_i <- 50
  rows_with_qs$num_fr [i]= sum(TBF_long$rep0[TBF_long$quad == quad_i & TBF_long$startyear == startyear0_i & TBF_long$site==site_i 
                                                   & TBF_long$x>=startx_i 
                                                   & TBF_long$x<=endx_i 
                                                   & TBF_long$y>=starty_i 
                                                   & TBF_long$y<=endy_i],na.rm = TRUE)}

# average size0 in plot
rows_with_qs$mean_size0 <- NA 


i=1

for (i in 1:nrow(rows_with_qs)) {
  # Extract values for the current row in rows_with_qs
  key_i <- rows_with_qs$key_qsy[i]
  startx_i <- rows_with_qs$startx[i]
  endx_i <- rows_with_qs$endx[i]
  site_i <- rows_with_qs$site[i]
  starty_i <- 25
  endy_i <- 50
  
  rows_with_qs$mean_size0[i] <- mean(
    TBF_long$size0[
      TBF_long$key_qsy == key_i & 
        TBF_long$x >= startx_i & 
        TBF_long$x <= endx_i & 
        TBF_long$y >= starty_i & 
        TBF_long$y <= endy_i
    ],
    na.rm = TRUE
  )
}

# export recruit_df

recruit_df <- rows_with_qs %>%
  select(quad,site, num_news, num_fr, site_ID, startyear, everything())%>%
  select(-y, -x, -X, -key, -qs, -key_qsy, -mean_size0)

recruit_df$log.fr <- log(recruit_df$num_fr + 0.1)

look <- TBF_long %>%
  filter(site == "IA")  %>%
  filter(quad == 15 )%>%
  filter(startyear == 2017)
  
  
write.csv(recruit_df, file = "F:/VFT/VFT_github/zyao78VFTcode/data/TBFxClimate/recruit_df_11_7_2025.csv")

write.csv(recruit_df_11_7_2025, file = "F:/VFT/VFT_github/zyao78VFTcode/data/recruit_df_11_24_2025.csv")


#####
#####   fit mod

library(lme4)
library(popbio)
library(parallel)
library(doParallel)
library(foreach)
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)
library("performance")
library(car)
library(glmmTMB)
library(MASS)


recruit_df <- read.csv("data/TBFxClimate/recruit_df_11_7_2025.csv")

colnames(recruit_df)

var(recruit_df$num_news)
mean(recruit_df$num_news)

mod.poisson <- glm(num_news ~  num_fr + TSF*TBF,  data= recruit_df, na.action= "na.fail",family= "poisson")
mod.nb <- glm.nb(num_news ~  num_fr + TSF*TBF, data = recruit_df, na.action = "na.fail")

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(mod.nb) # is this over dispersed?


aic_poisson <- AIC(mod.poisson)
aic_nb <- AIC(mod.nb)



# recruit mod


recruit_subset <- 
  recruit_df %>% 
  dplyr::filter(across(c(num_news, log.fr, TSF, TBF,site), ~ !is.na(.)))

recruit_mod_g <- glmer.nb(num_news ~num_fr+s.TSF*s.TBF + s.sq.TSF + 
                            s.T_RA + s.P_RA + s.sq.T_RA + s.sq.P_RA + s.T_RA:s.P_RA +
                            s.T_RC + s.sq.T_RC + 
                            s.T_RW +s.P_RW + s.T_RW:s.P_RW + s.sq.P_RW +
                            s.T_RD + s.P_RD + s.T_RD: s.P_RD + s.sq.P_RD +
                            s.T_RH + s.P_RH + s.sq.T_RH + s.T_RH:s.P_RH +
                            (1|site),
                           data = recruit_subset, na.action = "na.fail") 



recruit_mod_g_2 <- glmer.nb(num_news ~log.fr+s.TSF*s.TBF + s.sq.TSF + 
                            s.T_LA + s.sq.T_LA + s.P_LA + s.sq.P_LA + s.T_LA:s.P_LA +
                            s.T_LH + s.T_LC +s.P_LH +
                            (1|site),
                          data = recruit_subset, na.action = "na.fail") 

summary(recruit_mod_g)

n_cores <- detectCores()
n_cores
cluster <- makeCluster(n_cores - 1)
registerDoParallel(cluster)
clusterExport(cluster, c("recruit_subset"))   ### replace with different subset (different global models)
clusterEvalQ(cluster, {library(lme4); library(MuMIn)})

recruit_dredge_R_2 <- MuMIn::dredge(
  recruit_mod_g_2,
  cluster = cluster,
  trace   = 2
)
recruit_mod_R_2 <- get.models(recruit_dredge_R_2, 1)[[1]]

summary(recruit_mod_R)
summary(recruit_mod_R_2)

stopCluster(cluster)

save(recruit_mod_R, file = "data/TBFxClimate/recruit_mod_G_ls.Rdata")



