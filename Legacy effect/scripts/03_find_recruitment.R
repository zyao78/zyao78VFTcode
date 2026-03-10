install.packages("ACImodavg")
library(AICcmodavg)
library(lme4)
library(tidyverse)
library(MuMIn)
library(glue)
library(MASS)

load alldemodata_upto2025 and data_long

# check NAs in columns

sum(is.na(data_long$y))

# start

data_long$x <- NA
data_long$y <- NA
data_long$qs <- "N"
data_long$site_ID <- paste(data_long$site, data_long$ID, sep = "_")

for (i in 1:nrow(data_long)) {
  # Find the matching index in data_long based on the 'site_ID' column
  match_index <- which(alldemodata_upto2025$site_ID == data_long$site_ID[i])
  
  # If there's a match, update the 'quad' value in data_long
  if (length(match_index) > 0) {
    data_long$x[i] <- alldemodata_upto2025$x[match_index]
    data_long$y[i] <- alldemodata_upto2025$y[match_index]
    
  }
} 


data_long <- data_long %>% # move columns to the front
  select(x, y,site_ID, everything())

# add quad num to GSP sites

GSP_sites <- data_long[data_long$site %in% c("GSP-BI", "GSP-LI"), ]
GSP_sites$x <- as.numeric(GSP_sites$x)
GSP_sites$y


GSP_sites$quad_num <- NA
GSP_sites$quad_num <- cut(
  GSP_sites$x,
  breaks = seq(0, 1000, by = 50),  # Adjust 1000 if needed for higher ranges
  labels = 1:20,                   # Labels quadrats as 1, 2, 3, ..., 20
  right = F ,    # Exclude the upper boundary in each interval
  include.lowest = TRUE     
)

GSP_sites$quad <- recode(GSP_sites$quad,         # change into consistent quad names
                         "E" = "east",
                         "W" = "west",
                         "N" = "north",
                         "S" = "south")
table(GSP_sites$quad)

GSP_sites$quad <- paste(GSP_sites$quad, GSP_sites$quad_num, sep = "-")
data_long$quad <- as.character(data_long$quad)
for (i in 1:nrow(data_long)) {
  # Find the matching index in GSP_sites based on the 'key' column
  match_index <- which(GSP_sites$site_ID == data_long$site_ID[i])
  
  # If there's a match, 
  if (length(match_index) > 0) {
    data_long$quad[i] <- GSP_sites$quad[[match_index[1]]]   # since match_index is a vector (multiple rows), assign quad of the first row (or the second or the third)
  }
} 
table(data_long$quad)

# find qs
rows_with_qs <- data_long[grep("\\bqs", data_long$comm1, ignore.case = TRUE), ]  # case insensitive
rows_with_qs <- rows_with_qs[!grepl("qs/ns", rows_with_qs$comm1,ignore.case = TRUE), ]
rows_with_qs <- rows_with_qs[!grepl("qs/qns", rows_with_qs$comm1,ignore.case = TRUE), ]

data_long$qs <- "N"

data_long$key <- paste(data_long$site, data_long$ID, data_long$startyear, sep = "_")
data_long$key_qsy <- paste(data_long$quad, data_long$site, data_long$startyear, sep = "_")

rows_with_qs$key <- paste(rows_with_qs$site, rows_with_qs$ID, rows_with_qs$startyear, sep = "_") # make keys for matching by site_ID_year
rows_with_qs$key_qsy <- paste(rows_with_qs$quad, rows_with_qs$site, rows_with_qs$startyear, sep = "_") # make keys for matching by site_ID_year

data_long$qs[data_long$key_qsy %in% rows_with_qs$key_qsy] <- "Y"

table(data_long$qs) #check correct number of Y and N


# define search range x

rows_with_qs$startx <- as.numeric(sub(".*[qQ][sS][ -]*(\\d+).*", "\\1", rows_with_qs$comm1))
rows_with_qs$endx <- as.numeric(sub(".*[qQ][sS][ -]*\\d+[ -]+(\\d+).*", "\\1", rows_with_qs$comm1))


rows_with_qs <- rows_with_qs[!is.na(rows_with_qs$startx) & !is.na(rows_with_qs$endx), ] # delete rows with empty start/end x

for (i in 1:nrow(rows_with_qs)) {                             # fix cases where startx and endx are reversed
  
  if (rows_with_qs$startx[i] > rows_with_qs$endx[i]) {
    temp <- rows_with_qs$startx[i]
    rows_with_qs$startx[i] <- rows_with_qs$endx[i]
    rows_with_qs$endx[i] <- temp
  }
  
}






# count num of news


rows_with_new <- data_long[grepl("\\bnew", data_long$comm1), ]
rows_with_new1 <- rows_with_new[!grepl("no new", rows_with_new$comm1), ] # delete "no new"
rows_with_new1 <- rows_with_new1[!grepl("no news", rows_with_new1$comm1), ] # delete "no new" 
rows_with_new1 <- rows_with_new1[!grepl("not possible to see new plants", rows_with_new1$comm1), ]
rows_with_new1 <- rows_with_new1[!grepl("No news", rows_with_new1$comm1), ]  
rows_with_new1 <- rows_with_new1[!grepl("new lvs", rows_with_new1$comm1), ] 
rows_with_new1 <- rows_with_new1[!grepl("new nail", rows_with_new1$comm1), ] 


rows_with_qs$num_news <- NA

for (i in 1:nrow(rows_with_qs)) {
  quad_i <- rows_with_qs $ quad [i]
  startyear_i <- rows_with_qs $ startyear [i]
  startx_i <- rows_with_qs $ startx[i]
  endx_i <- rows_with_qs$endx[i]
  site_i <- rows_with_qs$site[i]
  starty_i <- 25
  endy_i <- 50
  rows_with_qs$num_news [i]= nrow(
    rows_with_new1 [rows_with_new1$quad == quad_i & rows_with_new1$x>=startx_i & rows_with_new1$x<=endx_i & rows_with_new1$startyear==startyear_i&
                      rows_with_new1$site==site_i & rows_with_new1$y>=starty_i & rows_with_new1$y<=endy_i,]
  )
} # find number of news that satisify these rules within the rows_with_new1 df

# count num of fruit

rows_with_qs$num_fr <- NA 

for (i in 1:nrow(rows_with_qs)) {
  key_i <- rows_with_qs $ key_qsy [i]
  startx_i <- rows_with_qs $ startx[i]
  endx_i <- rows_with_qs$endx[i]
  site_i <- rows_with_qs$site[i]
  starty_i <- 25
  endy_i <- 50
  rows_with_qs$num_fr [i]= sum(data_long$rep0[data_long$key_qsy == key_i & data_long$x>=startx_i 
                                                   & data_long$x<=endx_i 
                                                   & data_long$y>=starty_i 
                                                   & data_long$y<=endy_i],na.rm = TRUE)
  
}

# export recruit_df

recruit_df_5_14_2025 <-  rows_with_qs[, c("quad", "site", "num_news", "num_fr", "key_qsy", "startyear", "TSF", "TBF")]
write.csv(recruit_df_5_14_2025, file = "recruit_df_5_14_2025.csv")


# average size0 in plot
rows_with_qs$mean_logsize0 <- NA 



for (i in 1:nrow(rows_with_qs)) {
  # Extract values for the current row in rows_with_qs
  key_i <- rows_with_qs$key_qsy[i]
  startx_i <- rows_with_qs$startx[i]
  endx_i <- rows_with_qs$endx[i]
  site_i <- rows_with_qs$site[i]
  starty_i <- 25
  endy_i <- 50
  
  rows_with_qs$mean_logsize0[i] <- mean(
    data_long$s.logsize0[
      data_long$key_qsy == key_i & 
        data_long$x >= startx_i & 
        data_long$x <= endx_i & 
        data_long$y >= starty_i & 
        data_long$y <= endy_i
    ],
    na.rm = TRUE
  )
}




# fit mod

recruit_df_5_14_2025$num_news_m2 <- 4* recruit_df_5_14_2025$num_news
recruit_df_5_14_2025$num_fr_m2 <- 4* recruit_df_5_14_2025$num_fr

par(mfrow = c(2, 2)) # 2x2 plot layout
plot(mod.nb)

plot(recruit_df$num_news_m2~recruit_df$num_fr_m2)

mod.poisson <- glm(num_news_m2 ~  num_fr_m2,  data= recruit_df_5_14_2025, na.action= "na.fail",family= "poisson")
mod.nb <- glm.nb(num_news_m2 ~ num_fr_m2, data = recruit_df_5_14_2025, na.action = "na.fail")

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

# fix things

GSP_sites$first_word <- sub("^([A-Za-z]+).*", "\\1", GSP_sites$quad) # extract first word 


# recruit mod
recruit_subset <- 
  rows_with_qs %>% 
  dplyr::filter(across(c(num_news, num_fr, TSF, TBF,site), ~ !is.na(.)))

recruit_mod_g <- glmer.nb(num_news ~num_fr+TSF*TBF + (1|site),
                          data = recruit_df_5_14_2025, na.action = "na.fail") 
overdisp_fun(recruit_mod_g)

car::Anova(recruit_mod_g, type=3)
  
recruit_dredge <- MuMIn::dredge(recruit_mod_g)
MuMIn::dredge(recruit_mod_g)
recruit_mod <- get.models(recruit_dredge, 1)[[1]]
summary(recruit_mod)
growth_mod_coeffs <- summary(growth_mod)$coefficients # get coefficients
growth_mod_vcov <- vcov(growth_mod) # get variance-covariance matric
print(paste("Best fit model weight for growth:", round(growth_dredge$weight[1], 3))) # model weight

