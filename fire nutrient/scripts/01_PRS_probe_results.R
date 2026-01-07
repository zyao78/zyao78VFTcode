###########################################################################################
####   open the PRS probe data in xlsx format. Do some preliminary checks #################

rm(list = ls())


library(tidyverse)


#############################################

nutrient_data <- openxlsx::read.xlsx("F:/VFT/VFT_github/zyao78VFTcode/fire nutrient/raw_data/Nutrient Supply Rate Data Project_2605.xlsx", startRow=6,detectDates=TRUE)

