#Rental Deserts Working Paper 041924
#Fourth Script: 4.1_Analysis_Descriptives
#This code runs the descriptive statistics on extent and characteristics of rental deserts
#do it as 20-40-60-80 share cutoffs instead of classic rental deserts and other types of markets

#LOAD LIBRARIES-----
library(tidyverse)
library(data.table)

#SET FILE PATH-----
#file path to read nzlud and write resulting file
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"

#LOAD RD DATASET-----
rd <- fread(paste0(filepath_data, "RentalDeserts_dataset2022_041924.csv"))

#EXTENT AND CHARACTERISTICS OF RENTAL DESERTS----

##basic rental deserts descriptives
#average rental share across neighborhoods
rd %>% 
  summarise(avg_rentalsh= mean(rentalsh2022, na.rm=TRUE))

#total number of housing units
rd %>% 
  summarise(tot_hsg = sum(tot_hsg_units2022, na.rm=TRUE))

#number and share of all neighborhoods that are rental deserts
rd %>% 
  filter(!is.na(rental_des2022)) %>% 
  mutate(neighborhood=1) %>% 
  group_by(rental_des2022) %>% 
  summarise(tot=sum(neighborhood)) %>% 
  mutate(sh=tot/sum(tot)*100)

##rental deserts by city/suburb
#overrepresentation in suburbs - overall distribution
rd %>% 
  filter(!is.na(rental_des2022)) %>% 
  mutate(nbrhd_type = case_when(city==1 ~ "1 Urban",
                                suburb==1 ~ "2 Suburban",
                                micro==1 ~ "3 Micropolitan",
                                rural==1 ~ "4 Rural"),
         neighborhood=1) %>% 
  group_by(nbrhd_type) %>% 
  summarise(tot = sum(neighborhood)) %>% 
  mutate(sh = tot/sum(tot)*100)

#overrepresentation in suburbs - distribution of rental deserts
rd %>% 
  filter(!is.na(rental_des2022)) %>% 
  mutate(nbrhd_type = case_when(city==1 ~ "1 Urban",
                                suburb==1 ~ "2 Suburban",
                                micro==1 ~ "3 Micropolitan",
                                rural==1 ~ "4 Rural")) %>% 
  group_by(nbrhd_type) %>% 
  summarise(tot = sum(rental_des2022)) %>% 
  mutate(sh = tot/sum(tot)*100)

##housing types by type of neighborhood
#share single-family homes
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_sfsh = mean(struc11_shsf2022, na.rm=TRUE))

#share mf 5+
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental"),
         sh_mf_5pl = struc13_sh5_192022 + struc14_sh20_492022 + struc15_sh50pl2022) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_sh_mf_5pl = mean(sh_mf_5pl, na.rm=TRUE))

#median rents
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_rent = mean(mdnrent2022, na.rm=TRUE))


#rent categories
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_sh_600 = mean(rntcat31_sh_6002022, na.rm=TRUE),
            avg_sh_600_999 = mean(rntcat32_sh_600_9992022, na.rm=TRUE),
            avg__600 = mean(rntcat26_6002022, na.rm=TRUE),
            avg_600_999 = mean(rntcat27_600_9992022, na.rm=TRUE))

##income characteristics of rental deserts
#average median incomes
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_income = mean(mdnhhinc2022, na.rm=TRUE))

#income cats
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental"),
         lower_incomesh = inccat_lower_income/inccat00_tot2022*100,
         middle_incomesh = inccat_middle_income/inccat00_tot2022*100,
         higher_incomesh = inccat_higher_income/inccat00_tot2022*100) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_lish = mean(lower_incomesh, na.rm=TRUE),
            avg_mish = mean(middle_incomesh, na.rm=TRUE),
            avg_hish = mean(higher_incomesh, na.rm=TRUE))
  
##racial characteristics of rental deserts
#share of pop by race by rental share
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_sh_poc = mean(race10_shpoc2022, na.rm=TRUE),
            avg_sh_white = mean(race07_shwhite2022, na.rm=TRUE),
            avg_sh_black = mean(race08_shblack2022, na.rm=TRUE),
            avg_sh_hisp = mean(race09_shhisp2022, na.rm=TRUE))

#share of pop that is people of color by rental share
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_sh_poc = mean(race10_shpoc2022, na.rm=TRUE))

#FIGURES-----
#Figure 1
#overrepresentation in suburbs - overall distribution
rd %>% 
  filter(!is.na(rental_des2022)) %>% 
  mutate(nbrhd_type = case_when(city==1 ~ "1 Urban",
                                suburb==1 ~ "2 Suburban",
                                micro==1 ~ "3 Micropolitan",
                                rural==1 ~ "4 Rural"),
         neighborhood=1) %>% 
  group_by(nbrhd_type) %>% 
  summarise(tot = sum(neighborhood)) %>% 
  mutate(sh = tot/sum(tot)*100)

#overrepresentation in suburbs - distribution of rental deserts
rd %>% 
  filter(!is.na(rental_des2022)) %>% 
  mutate(nbrhd_type = case_when(city==1 ~ "1 Urban",
                                suburb==1 ~ "2 Suburban",
                                micro==1 ~ "3 Micropolitan",
                                rural==1 ~ "4 Rural")) %>% 
  group_by(nbrhd_type) %>% 
  summarise(tot = sum(rental_des2022)) %>% 
  mutate(sh = tot/sum(tot)*100)

#Figure 2
#share by structure type
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_sfsh = mean(struc11_shsf2022, na.rm=TRUE),
            avg_2_4 = mean(struc12_sh2_42022, na.rm=TRUE),
            avg_5_19 = mean(struc13_sh5_192022, na.rm=TRUE),
            avg_20_49 = mean(struc14_sh20_492022, na.rm=TRUE),
            avg_50pl = mean(struc15_sh50pl2022, na.rm=TRUE),
            manuf = mean(struc16_shmanuf2022, na.rm=TRUE))

#Figure 3
#share by income
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_li=mean((inccat_lower_income/inccat00_tot2022*100), na.rm=TRUE),
            avg_mi=mean((inccat_middle_income/inccat00_tot2022*100), na.rm=TRUE),
            avg_hi=mean((inccat_higher_income/inccat00_tot2022*100), na.rm=TRUE))

#Figure 4
#share by race
rd %>% 
  filter(!is.na(rentalsh2022)) %>% 
  mutate(rd_nbrhd_cat = case_when(rentalsh2022<20 ~ "1 RentalDesert",
                                  rentalsh2022>=20 & rentalsh2022<80 ~ "2 MixedTenure",
                                  rentalsh2022>=80 ~ "3 HighRental")) %>% 
  group_by(rd_nbrhd_cat) %>% 
  summarise(avg_poc = mean(race10_shpoc2022, na.rm=TRUE),
            avg_black = mean(race08_shblack2022, na.rm=TRUE),
            avg_hisp = mean(race09_shhisp2022, na.rm=TRUE),
            avg_asian = mean((race04_asianpop2022/race00_totalpop2022*100), na.rm=TRUE),
            avg_white=mean(race07_shwhite2022, na.rm=TRUE))


