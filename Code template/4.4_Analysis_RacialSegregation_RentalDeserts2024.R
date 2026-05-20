#Rental Deserts Working Paper 041924
#Fourth Script: 4.4_Analysis_RacialSegregation
#This code calculates housing, race, and income divergence indices and runs correlations

#LOAD LIBRARIES-----
library(tidyverse)
library(data.table)
library(tigris)
library(rsegregation)
library(ggplot2)

#SET FILE PATH-----
#file path to read nzlud and write resulting file
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"

#LOAD RD DATASET-----
rd <- fread(paste0(filepath_data, "RentalDeserts_dataset2022_041924.csv"))

#CALCULATE DIVERGENCE INDEX VALUES - HOUSING-----
rd <- rd %>%
  rowwise() %>%
  mutate(mf_sh2022 = (struc05_mf_5_92022 + struc06_mf_10_192022 + struc07_mf_20_492022 + struc08_mf_502022) / struc00_total2022) %>%
  ungroup()%>%
  mutate(mf_sh2022_p = mf_sh2022*100)

metdiv_struc <- rd %>%
  filter(cbsa_top100==1) %>% 
  group_by(GEOID_CBSA, CBSAname) %>%
  mutate(multifamily = mf_sh2022,
         singlefamily = 1 - multifamily) %>%
  filter(!is.na(multifamily) & !is.na(singlefamily)) %>% 
  summarize(divergenceval_struc = divergence(multifamily, singlefamily, population=struc00_total2022, summed = T))

#CALCULATE DIVERGENCE INDEX VALUES - RACE-----
metdiv_race <- rd %>%
  filter(cbsa_top100==1) %>% 
  group_by(GEOID_CBSA, CBSAname) %>%
  mutate(white = race01_whitepop2022/race00_totalpop2022,
         black = race02_blackpop2022/race00_totalpop2022,
         hisp = race06_hisppop2022/race00_totalpop2022,
         oth = (race00_totalpop2022 - race01_whitepop2022 - race02_blackpop2022 - race06_hisppop2022)/race00_totalpop2022) %>%
  filter(!is.na(white) & !is.na(black) & !is.na(hisp) & !is.na(oth)) %>% 
  summarize(divergenceval_race = divergence(white, black, hisp, oth, population=race00_totalpop2022, summed = T))

#merge metro housing divergence and racial divergence values
metdiv_struc_race <- metdiv_struc %>% 
  left_join(metdiv_race, by=c("GEOID_CBSA", "CBSAname")) %>% 
  ungroup() %>% 
  mutate(struc_quart = ntile(divergenceval_struc, 4), 
         race_quart = ntile(divergenceval_race, 4),
         seg_type = case_when(struc_quart>=3 & race_quart>=3 ~ "1 High Housing Structure, High Racial Segregation",
                              struc_quart<=2 & race_quart<=2 ~ "4 Low Housing Structure, Low Racial Segregation",
                              struc_quart>=3 & race_quart<=2 ~ "2 High Housing Structure, Low Racial Segregation",
                              struc_quart<=2 & race_quart>=3 ~ "3 Low Housing Structure, High Racial Segregation"))

#correlation between housing and race divergence values
cor(metdiv_struc_race$divergenceval_struc, metdiv_struc_race$divergenceval_race)


#MAP HOUSING AND RACE DIVERGENCE QUADRANTS-----
statemap <- states(cb=TRUE) %>% 
  shift_geometry(position = "below") %>% 
  filter(as.numeric(GEOID)<=56)
cbsamap <- core_based_statistical_areas(cb = TRUE, year=2021) #uses tigris to pull place shapefile
cbsamap <- shift_geometry(
  cbsamap,
  position = "below"
)

cbsamap_div <- cbsamap %>% 
  mutate(GEOID_CBSA=as.integer(CBSAFP)) %>% 
  right_join(metdiv_struc_race, by="GEOID_CBSA") 

#render map        
ggplot() +
  geom_sf(data=statemap, fill="gray95") +
  geom_sf(data=cbsamap_div, aes(fill=`seg_type`), color="black", lwd=0) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("High Housing Structure, \nHigh Racial Segregation", "High Housing Structure, \nLow Racial Segregation", 
                             "Low Housing Structure, \nHigh Racial Segregation", "Low Housing Structure, \nLow Racial Segregation"), 
                    values = colorRampPalette(brewer.pal(n=9, "BuPu"))(9)[c(8, 6, 4, 2)]) +
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(0.1, .5), 
        legend.spacing.y = unit(0.75, 'cm'))+
  guides(fill=guide_legend(ncol=1, byrow=TRUE)) +
  labs(fill='Segregation Type')

#CALCULATE DIVERGENCE INDEX VALUES - INCOME-----
metdiv_inc <- rd %>%
  filter(cbsa_top100==1) %>% 
  group_by(GEOID_CBSA, CBSAname) %>%
  mutate(li = inccat_lower_income/inccat00_tot2022,
         mi = inccat_middle_income/inccat00_tot2022,
         hi = inccat_higher_income/inccat00_tot2022) %>%
  filter(!is.na(li) & !is.na(mi) & !is.na(hi)) %>% 
  summarize(divergenceval_inc = divergence(li, mi, hi, population=inccat00_tot2022, summed = T))

metdiv_struc_inc <- metdiv_struc %>% 
  left_join(metdiv_inc, by=c("GEOID_CBSA", "CBSAname")) %>% 
  ungroup() %>% 
  mutate(struc_quart = ntile(divergenceval_struc, 4), 
         inc_quart = ntile(divergenceval_inc, 4),
         seg_type = case_when(struc_quart>=3 & inc_quart>=3 ~ "1 High Housing Structure, High Income Segregation",
                              struc_quart<=2 & inc_quart<=2 ~ "4 Low Housing Structure, Low Income Segregation",
                              struc_quart>=3 & inc_quart<=2 ~ "2 High Housing Structure, Low Income Segregation",
                              struc_quart<=2 & inc_quart>=3 ~ "3 Low Housing Structure, High Income Segregation"))

cor(metdiv_struc_inc$divergenceval_struc, metdiv_struc_inc$divergenceval_inc)

#MAP HOUSING AND INCOME DIVERGENCE QUADRANTS-----
cbsamap_div <- cbsamap %>% 
  mutate(GEOID_CBSA=as.integer(CBSAFP)) %>% 
  right_join(metdiv_struc_inc, by="GEOID_CBSA") 

#render map        
ggplot() +
  geom_sf(data=statemap, fill="gray95") +
  geom_sf(data=cbsamap_div, aes(fill=`seg_type`), color="black", lwd=0) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("High Housing Structure, \nHigh Income Segregation", "High Housing Structure, \nLow Income Segregation", 
                             "Low Housing Structure, \nHigh Income Segregation", "Low Housing Structure, \nLow Income Segregation"), 
                    values = colorRampPalette(brewer.pal(n=9, "BuPu"))(9)[c(8, 6, 4, 2)]) +
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(0.1, .5), 
        legend.spacing.y = unit(0.75, 'cm'))+
  guides(fill=guide_legend(ncol=1, byrow=TRUE)) +
  labs(fill='Segregation Type')

#PLACES THAT OVERLAP ON ALL THREE MEASURES----
metdiv_all <- metdiv_struc %>% 
  left_join(metdiv_race, by=c("GEOID_CBSA", "CBSAname")) %>% 
  left_join(metdiv_inc, by = c("GEOID_CBSA", "CBSAname")) %>% 
  ungroup() %>%  
  mutate(racequart = ntile(divergenceval_race, 4), 
         incquart = ntile(divergenceval_inc, 4), 
         strucquart = ntile(divergenceval_struc, 4))

#top half of all three measures
metdiv_all %>% 
  filter(racequart>=3 & incquart>=3 & strucquart>=3) %>% 
  select(CBSAname) %>% 
  print(n=25)

#top quartile of all three measures
metdiv_all %>% 
  filter(racequart==4 & incquart==4 & strucquart==4) %>% 
  select(CBSAname)

#bottom half of all three measures
metdiv_all %>% 
  filter(racequart<=2 & incquart<=2 & strucquart<=2) %>% 
  select(CBSAname) %>% 
  print(n=25)

#top quartile of all three measures
metdiv_all %>% 
  filter(racequart==1 & incquart==1 & strucquart==1) %>% 
  select(CBSAname)

#CREATE 2 SCATTER PLOTS
#race/structure scatterplot

corr_di_struc_race <- lm( divergenceval_struc ~ divergenceval_race, data = metdiv_struc_race)
r_squared <- summary(corr_di_struc_race)$r.squared

ggplot(metdiv_struc_race, aes(x = divergenceval_struc, y = divergenceval_race)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Housing Divergence Index Value",
       y = "Race Divergence Index Value") +
  annotate("text", x = Inf, y = Inf, label = paste("R2 = ", round(r_squared, 2)), 
           hjust = 1.1, vjust = 1.1, size = 5, colour = "red", fontface = "bold") +
  theme_minimal()

#income/structure scatterplot

corr_di_struc_inc <- lm( divergenceval_struc ~ divergenceval_inc, data = metdiv_struc_inc)
r_squared <- summary(corr_di_struc_inc)$r.squared

ggplot(metdiv_struc_inc, aes(x = divergenceval_struc, y = divergenceval_inc)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(x = "Housing Divergence Index Value",
       y = "Income Divergence Index Value") +
  annotate("text", x = Inf, y = Inf, label = paste("R2 = ", round(r_squared, 2)), 
           hjust = 1.1, vjust = 1.1, size = 5, colour = "red", fontface = "bold") +
  theme_minimal()
