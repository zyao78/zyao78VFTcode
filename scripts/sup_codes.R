lapply(dat.list, names)    # print column names in data list to check for mismatch
 
col_names <- c(
  "ID", "Quad", "X", "Y", "Mrk", "N23", "L23", "FR23", "FL23", 
  "Notes23", "Comm23", "N24", "L24", "FR24", "FL24", "Comm24", "site"
)
dat.list <- lapply(dat.list, function(df) {
  colnames(df) <- col_names
  return(df)
})     # re assign col names manually

names24 <- names(dat24)[1:(ncol(dat24)-1)]   # extract column names from 1:x  columns


for(i in 1:nrow(fdat24)){
  if(dat24$Notes23[i] != "") {
    if(str_detect(fdat24$Notes23[i], "mpd24") | str_detect(fdat24$Notes23[i], "dead24")) {
      fdat24$Alive24[i] <- "0"  }}
  }     # B2 93 says pendant at 16 disc; dead24


for(i in 1:nrow(fdat24)){
  if(dat24$Comm24[i] != "") {
    if(str_detect(fdat24$Comm24[i], "disc") ) 
    if(fdat24$N24[i] == ""){
  fdat24$Alive24[i] <- as.character("")}
}  }   # losts of disc under comm24, no N24, assign these to ""


testDF <- alldemodata_2023[alldemodata_2023$Alive23 == 1, ]


write.csv(fdat24, file = "alldemodata_2024_v1.csv", row.names=FALSE)


xy_subset <- allexist_data[
  , 
  c("x_annual", "y_annual", "x_wide", "y_wide", "mark_annual", "mark_wide", "site_ID", "x","y", "mark")
]

xy_mismatch <- xy_subset[
  xy_subset$x_annual != xy_subset$x_wide | xy_subset$y_annual != xy_subset$y_wide, 
] # check for mismatched xy rows


duplicate_search <- as.data.frame(table(annual_data_v2$site_ID))   # check duplicates

annual_data_v2 <- annual_data_v2[!(annual_data_v2$site_ID %in% c("B2_NA", "CH_NA", "IA_NA")), ]  # delete empty entries


allexist_data <- allexist_data[,!colnames(allexist_data) %in% "X"]

str(all_data)


all_data2$Comm24[all_data2$site_ID %in% c("GSP-LI_269", "CM_24.89.")] <- "lacking L24"  # update comm24 for specific plants
all_data2$size24[all_data2$site_ID %in% c("GSP-LI_269", "CM_24.89.")] <- NA


all_test2 <- all_data2[,colnames(all_data2) %in% c("site_ID", "Comm24", "size24", "Notes23_24", "N24", "L24", "FLW24", "FR24", "N23", "size23")]

IA_alldemoupto2023 <- alldemodata_upto2023[alldemodata_upto2023$site == "IA",]

annual_data_v2[821, "ID"] <- "24.13."   # update missing IDs




#fix missing quad from alldemo_upto2023

for (i in 1:nrow(all_data3)) {
  if (is.na(all_data3$quad[i])) {  # Only proceed if quad is NA
    match_row <- alldemodata_2021$quad[alldemodata_2021$site_ID == all_data3$site_ID[i]]
    
    if (length(match_row) == 1) {  # Ensure there's exactly one match
      all_data3$quad[i] <- match_row
    } else if (length(match_row) == 0) {
      message(sprintf("No match found for site_ID: %s", all_data3$site_ID[i]))
    } else {
      stop(sprintf("Multiple matches found for site_ID: %s", all_data3$site_ID[i]))
    }
  }
}

all_data3[all_data3$site_ID=="CM_18.32.", "quad"] <- "2"   # update missing IDs

any(is.na(TBF_data_long1$site_ID_year))    # check if a column contains na


