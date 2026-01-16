###########################################################################################
####   open the PRS probe data in xlsx format. Do some preliminary checks #################

rm(list = ls())


library(tidyverse)
library(vegan)
library(labdsv)
library(MASS)
library(ape)
library(dplyr)

#############################################

nutrient_data <- openxlsx::read.xlsx("F:/VFT/VFT_github/zyao78VFTcode/fire nutrient/raw_data/Nutrient Supply Rate Data Project_2605.xlsx", startRow=6,detectDates=TRUE)
nutrient_data <- nutrient_data[-1,]   

#############################################  PcoA
distance<-vegdist(community_sub, method="bray")

nutr_cols <- c("Total.N", "NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe", "Mn", "Cu")

comm <- nutrient_data %>%
  dplyr::select(Sample.ID, all_of(nutr_cols)) %>%
  mutate(across(all_of(nutr_cols), as.numeric)) %>%
  group_by(Sample.ID) %>%
  summarise(across(all_of(nutr_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  column_to_rownames("Sample.ID")

distance<-vegdist(comm, method="bray")
prin_coord<-pco(distance, k=3)   # three dimensions, 3 axes
ordiplot(prin_coord)
text(prin_coord$points, labels = row.names(comm), cex=1)