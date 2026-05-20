#Rental Deserts Working Paper 041924
#Fourth Script: 4.2_Analysis_Zoning
#This code runs the simple models for zoning restrictions and rental share

#LOAD LIBRARIES-----
library(tidyverse)
library(data.table)
library(dplyr)

#SET FILE PATH-----
#file path to read nzlud and write resulting file
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"

#LOAD RD DATASET-----
rd_nzlud <- fread(paste0(filepath_data, "RentalDeserts_dataset2022_nzlud_041924.csv"))

#recode multifamily share variable: add up variables ("struc05_mf_5_92022", "struc06_mf_10_192022", "struc07_mf_20_492022","struc08_mf_502022") divided by "struc00_total2022"
rd_nzlud <- rd_nzlud %>%
  rowwise() %>%
  mutate(mf_sh2022 = (struc05_mf_5_92022 + struc06_mf_10_192022 + struc07_mf_20_492022 + struc08_mf_502022) / struc00_total2022) %>%
  ungroup()%>%
  mutate(mf_sh2022_p = mf_sh2022*100)

#FULL ZRI----
#first: for interpretation, see what 1 unit of the zri is relative to a standard deviation
#1 unit increase in zri for both muni or msa is equivalent of roughly 1 standard deviation
rd_nzlud %>% 
  filter(!is.na(nzlud_muni.zri_st)) %>% 
  group_by(muni_GEOID10) %>% 
  summarise(zri_muni = max(nzlud_muni.zri_st, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(sd_zri_muni = sd(zri_muni)) #1 standard deviation is 0.983

rd_nzlud %>% 
  filter(!is.na(nzlud_msa.zri_full_st)) %>% 
  group_by(cbsa_GEOID10) %>% 
  summarise(zri_msa = max(nzlud_msa.zri_full_st, na.rm=TRUE)) %>% 
  ungroup() %>% 
  summarise(sd_zri_msa = sd(zri_msa)) #1 standard deviation is 1.01

#relationship between zri and rental share - municipal level
summary(lm(mf_sh2022_p ~ nzlud_muni.zri_st, data=rd_nzlud))

#relationship between zri and rental share - metropolitan level
summary(lm(mf_sh2022_p ~ nzlud_msa.zri_full_st, data=rd_nzlud))

#NZLUD SUBINDICES----
#recode subindices to put them into interpretable units
rd_nzlud_muni_recodes <- rd_nzlud %>% 
  mutate(sindex1_2 = if_else(nzlud_muni.sindex1==2, 1, 0),
         sindex1_4 = if_else(nzlud_muni.sindex1==4, 1, 0),
         sindex1_6 = if_else(nzlud_muni.sindex1==6, 1, 0),
         sindex3_2 = if_else(nzlud_muni.sindex3==2, 1, 0),
         sindex3_3 = if_else(nzlud_muni.sindex3==3, 1, 0),
         sindex3_4 = if_else(nzlud_muni.sindex3==4, 1, 0),
         sindex6_2 = if_else(nzlud_muni.sindex6==2, 1, 0),
         sindex6_3 = if_else(nzlud_muni.sindex6==3, 1, 0),
         sindex6_4 = if_else(nzlud_muni.sindex6==4, 1, 0),
         sindex6_5 = if_else(nzlud_muni.sindex6==5, 1, 0),
         sindex9_sc = nzlud_muni.sindex9/10,
         mf_per_sc = nzlud_muni.mf_per*10)

#subindex 1 - explicit growth controls (reference cat:0, compared to 2, 4, or 6 growth control restrictions)
#note: no municipalities have 1, 3, or 5 growth control restrictions
summary(lm(mf_sh2022_p ~ sindex1_2 + sindex1_4 + sindex1_6, data=rd_nzlud_muni_recodes))

#subindex 3 - minimum lot size (recoded as categorical instead of ordinal)
summary(lm(mf_sh2022_p ~ sindex3_2 + sindex3_3 + sindex3_4, data=rd_nzlud_muni_recodes)) #minimum lot size

#subindex 6 - maximum permitted density (recoded as categorical instead of ordinal)
summary(lm(mf_sh2022_p ~ sindex6_2 + sindex6_3 + sindex6_4 + sindex6_5, data=rd_nzlud_muni_recodes)) #permitted density

#subindex 7 - inclusionary zoning
summary(lm(mf_sh2022_p ~ nzlud_muni.sindex7, data=rd_nzlud)) #inclusionary (binary)

#subindex 8 - ADU ordinance
summary(lm(mf_sh2022_p ~ nzlud_muni.sindex8, data=rd_nzlud)) #ADU (binary)

#subindex 9 - maximum height index - scaled (increase in 10s of feet)
summary(lm(mf_sh2022_p ~ sindex9_sc, data=rd_nzlud_muni_recodes)) #maximum height (tens of ft increase)

#subindex 10 - minimum required parking
summary(lm(mf_sh2022_p ~ nzlud_muni.sindex10, data=rd_nzlud)) #parking index (values from 1-10)

#subindex 11 - permitted multifamily by right - scaled (0 to 1 = 10% increase in zones)
summary(lm(mf_sh2022_p ~ mf_per_sc, data=rd_nzlud_muni_recodes)) #scaled version of mf permitting (0 to 1 = 10% increase in zones)

#FULL composite MZI----

#create composite index by grouping the following subindices: subindex 1 - explicit growth controls (reference cat:0, compared to 2, 4, or 6 growth control restrictions)
#note: no municipalities have 1, 3, or 5 growth control restrictions + #subindex 6 - maximum permitted density (recoded as categorical instead of ordinal) + 
#subindex 9 - maximum height index - scaled (increase in 10s of feet) + #subindex 11 - permitted multifamily by right - scaled (0 to 1 = 10% increase in zones)
summary(lm(mf_sh2022_p ~ sindex1_2 + sindex1_4 + sindex1_6 + sindex6_2 + sindex6_3 + sindex6_4 + sindex6_5 + sindex9_sc + mf_per_sc, data=rd_nzlud_muni_recodes))

#find cutpoint for binary: lowest is 0 and 1 for all 3 categories, run that variable against multifamily share and see what is the significance
#then take two bottom categories and make them zero, and the two up ones as 1, run that variable against multifamily share and see what is the significance 
rd_nzlud_muni_recodes2 <- rd_nzlud %>% 
  mutate(sindex1_2_4 = if_else(nzlud_muni.sindex1 == 2 | nzlud_muni.sindex1 == 4, 1, 0),
    sindex1_6 = if_else(nzlud_muni.sindex1 == 6, 1, 0),
    sindex6_2_3 = if_else(nzlud_muni.sindex6 == 2 | nzlud_muni.sindex6 == 3, 1, 0),
    sindex6_4 = if_else(nzlud_muni.sindex6 == 4, 1, 0),
    sindex6_5 = if_else(nzlud_muni.sindex6 == 5, 1, 0),
    sindex9_sc = nzlud_muni.sindex9 / 10,
    mf_per_sc = nzlud_muni.mf_per * 10)

summary(lm(mf_sh2022_p ~ sindex1_2_4 + sindex1_6 + sindex6_2_3 + sindex6_4 + sindex6_5 + sindex9_sc + mf_per_sc, data=rd_nzlud_muni_recodes2))

#create MZI composite index, assuming equal weights

rd_nzlud_muni_recodes2 <- rd_nzlud_muni_recodes2 %>%
  mutate(mzi = (sindex1_2_4 + sindex1_6 + sindex6_2_3 + sindex6_4 + sindex6_5 + sindex9_sc + mf_per_sc) / 7)

#Summary Statistics to understand the composite index
summary(rd_nzlud_muni_recodes2$mzi)

#third: run linear regression to understand impact of composite index on multifamily share
summary(lm(mf_sh2022_p ~ mzi, data = rd_nzlud_muni_recodes2))
