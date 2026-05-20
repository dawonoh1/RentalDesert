#Rental Deserts Working Paper 041924
#Third Script, Part 2: 3.2_ZoningFileSetup_NZLUD
#This code processes the National Zoning and Land Use Index Data file and merges it on to the tract interpolation by municipal GEOID
#Municipal GEOIDs correspond to either the place or county subdivision
#The resulting file contains 2022 tracts (and rental variables) and the NZLUD values: RentalDeserts_dataset2022_nzlud_041924.csv

#nzlud muni file downloaded from: https://github.com/mtmleczko/nzlud/blob/main/nzlud_muni.csv
#nzlud msa file downloaded from: https://github.com/mtmleczko/nzlud/blob/main/nzlud_msa.csv

#LOAD LIBRARIES-----
library(tidyverse)
library(data.table)

#SET FILE PATH-----
#file path to read nzlud and write resulting file
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"

#READ NZLUD FILE & MERGE TO TRACT CROSSWALK----
nzlud_muni <- fread(paste0(filepath_data, "nzlud_muni.csv")) %>% 
  setNames(paste0('nzlud_muni.', names(.))) %>% 
  mutate(nzlud_muni.GEOID = as.numeric(nzlud_muni.GEOID))

nzlud_msa <- fread(paste0(filepath_data, "nzlud_msa.csv")) %>% 
  setNames(paste0('nzlud_msa.', names(.))) %>% 
  mutate(nzlud_msa.cbsa10 = as.numeric(nzlud_msa.cbsa10))

tractcross <- fread(paste0(filepath_data, "tracts2022_places10_countysub10_cbsa10.csv")) %>% 
  select(-V1) %>% 
  mutate(GEOID2022=as.numeric(GEOID2022),
         muni_GEOID10 = as.numeric(muni_GEOID10),
         cbsa_GEOID10 = as.numeric(cbsa_GEOID10))

tract_nzlud <- tractcross %>% 
  left_join(nzlud_muni, by=c("muni_GEOID10"="nzlud_muni.GEOID")) %>% 
  left_join(nzlud_msa, by=c("cbsa_GEOID10"="nzlud_msa.cbsa10"))

rm(nzlud_msa, nzlud_muni, tractcross)

#MERGE ON TO RENTAL DESERTS DATASET-----
rd_geo <- fread(paste0(filepath_data, "RentalDeserts_dataset2022_041924.csv")) %>% 
  select(-V1) %>% 
  mutate(GEOID2022=as.numeric(GEOID2022))

rd_nzlud <- rd_geo %>% 
  left_join(tract_nzlud, by="GEOID2022")

rm(rd_geo, tract_nzlud)

write.csv(rd_nzlud, paste0(filepath_data, "RentalDeserts_dataset2022_nzlud_041924.csv"))
