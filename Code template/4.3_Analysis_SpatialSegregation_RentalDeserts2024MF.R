#Rental Deserts Working Paper 041924
#Fourth Script: 4.3_Analysis_SpatialSegregation
#This code calculates multifamily/single family divergence indices 
#and produces the maps for the paper

#LOAD LIBRARIES-----
# Install first Rsegregation from Arthur Gailes GitHub
install.packages("devtools")
install.packages("httpuv")
library(devtools)
install.packages("remotes")
remotes::install_github("arthurgailes/rsegregation")

install.packages("RColorBrewer")
library(tidyverse)
library(data.table)
library(tigris)
library(sf)
library(rsegregation)
library(RColorBrewer)
library(ggplot2)

#SET FILE PATH-----
#file path to read nzlud and write resulting file
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"

#LOAD RD DATASET-----
rd <- fread(paste0(filepath_data, "RentalDeserts_dataset2022_041924.csv"))

#MULTIFAMILY SH BY METRO-----
rd <- rd %>%
  rowwise() %>%
  mutate(mf_sh2022 = (struc05_mf_5_92022 + struc06_mf_10_192022 + struc07_mf_20_492022 + struc08_mf_502022) / struc00_total2022) %>%
  ungroup()%>%
  mutate(mf_sh2022_p = mf_sh2022*100)

metro_mf_sh <- rd %>% 
  filter(cbsa_top100==1) %>% 
  group_by(GEOID_CBSA, CBSAname) %>% 
  summarise(tracts = n(),
            mf = sum(mf_sh2022, na.rm=TRUE),
            tot_hsg_struc = sum(struc00_total2022, na.rm=TRUE),
            occrnt = sum(rentocc00_tot2022, na.rm=TRUE),
            tothh = sum(occ2022, na.rm=TRUE)) %>%
  mutate(mf_sh = mf/tracts * 100,
         rntshp = occrnt/tothh * 100) %>% 
  select(GEOID_CBSA, CBSAname, mf_sh, rntshp)

#correlation between multifamily share and rentership share
cor(metro_mf_sh$mf_sh, metro_mf_sh$rntshp)
min(metro_mf_sh$mf_sh)

#CREATE MAP OF MULTIFAMILY SHARE-----
statemap <- states(cb=TRUE) %>% 
  shift_geometry(position = "below") %>% 
  filter(as.numeric(GEOID)<=56)
cbsamap <- core_based_statistical_areas(cb = TRUE, year=2021) #uses tigris to pull place shapefile
cbsamap <- shift_geometry(
  cbsamap,
  position = "below"
) #puts shapefile in format for mapping with alaska and hawaii

cbsamap_multifamsh <- cbsamap %>% 
  mutate(GEOID_CBSA=as.integer(CBSAFP)) %>% 
  right_join(metro_mf_sh, by="GEOID_CBSA") %>% 
  mutate("Multifamily Share" = case_when(mf_sh <15 ~ "1 Less than 15%",
                                            mf_sh >=15 & mf_sh<30 ~ "2 15-29.9%",
                                            mf_sh >=30 ~ "3 30% or More"))

#render map        
ggplot() +
  geom_sf(data=statemap, fill="gray95") +
  geom_sf(data=cbsamap_multifamsh, aes(fill=`Multifamily Share`), color="black", lwd=0) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("Less than 15", "15-29.9", "30-49.9"), 
                    values = colorRampPalette(brewer.pal(n=9, "BuPu"))(9)[c(3, 5, 8)]) + #colorRampPalette allows for specifying which shades you want to pull from a colorbrewer palette - default had bottom color too light
  # scale_fill_brewer(palette = "Greens", labels=c("Less than 2 percent", "2–3.9 percent", "4–6.9 percent", "7–9.9 percent", "10 percent or More", "No Data")) + #sets the fill color scheme for the map, title for the legend, and labels for the legend items
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(0.09, .5), 
        legend.spacing.y = unit(0.25, 'cm'))+
  guides(fill=guide_legend(ncol=1, byrow=TRUE)) +
  labs(fill='Multifamily Share \n(Percent)')

#CALCULATE DIVERGENCE INDEX VALUES-----
metdiv <- rd %>%
  filter(cbsa_top100==1) %>% 
  group_by(GEOID_CBSA, CBSAname) %>%
  mutate(multifamily = mf_sh2022,
         singlefamily = 1 - multifamily) %>%
  filter(!is.na(multifamily) & !is.na(singlefamily)) %>% 
  summarize(divergenceval = divergence(multifamily, singlefamily, population=struc00_total2022, summed = T))

trdiv <- rd %>%
  filter(cbsa_top100==1) %>% 
  group_by(GEOID_CBSA, CBSAname) %>%
  mutate(multifamily = mf_sh2022,
         singlefamily = 1 - multifamily) %>%
  filter(!is.na(multifamily) & !is.na(singlefamily)) %>% 
  mutate(tract_divergenceval = divergence(multifamily, singlefamily, population=struc00_total2022, summed = F)) %>% 
  select(GEOID2022, GEOID_CBSA, CBSAname, multifamily, singlefamily, rental_hu2022, struc00_total2022, tract_divergenceval)

#MAP METRO DIVERGENCE VALUES-----
cbsamap_div <- cbsamap %>% 
  mutate(GEOID_CBSA=as.integer(CBSAFP)) %>% 
  right_join(metdiv, by="GEOID_CBSA") %>% 
  mutate("Divergence Index" = as.factor(ntile(divergenceval, 4)))

#render map        
ggplot() +
  geom_sf(data=statemap, fill="gray95") +
  geom_sf(data=cbsamap_div, aes(fill=`Divergence Index`), color="black", lwd=0) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("Least Divergent", "Second Quartile", "Third Quartile", "Most Divergent"), 
                    values = colorRampPalette(brewer.pal(n=9, "BuPu"))(9)[c(2, 4, 6, 8)]) +
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(0.07, .5), 
        legend.spacing.y = unit(0.25, 'cm'))+
  guides(fill=guide_legend(ncol=1, byrow=TRUE)) +
  labs(fill='Divergence Index \nValue (Quartile)')

#MAP FOR TRACT DIVERGENCE IN NY METRO----
#subset tract divergence dataset to ny metro, calculate divergence quartiles,
#and identify if tract is divergent bc it is higher multifamily or higher single-family
ny <- trdiv %>% 
  filter(CBSAname == "New York-Newark-Jersey City, NY-NJ-PA") %>% 
  arrange(-tract_divergenceval) %>% 
  mutate(div_quartile = ntile(tract_divergenceval, 4),
         GEOID = as.numeric(GEOID2022)) %>% 
  group_by(CBSAname) %>% 
  mutate(metrosh_mf = sum(multifamily, na.rm=TRUE)/sum(struc00_total2022, na.rm=TRUE),
         metrosh_sf = (sum(struc00_total2022, na.rm=TRUE)-sum(multifamily, na.rm=TRUE))/sum(struc00_total2022, na.rm=TRUE)) %>% 
  ungroup() %>% 
  rename(tractsh_mf = multifamily,
         tractsh_sf = singlefamily) %>% 
  mutate(highmf_sh = if_else(tractsh_mf>metrosh_mf, 1, 0), #make flag if tract share of multifamily is higher than metro share
         highsf_sh = if_else(tractsh_sf>metrosh_sf, 1, 0), #same but for high single family
         div_type = if_else(highmf_sh==1, "1 Multifamily", "2 Single Family")) %>% 
  select(GEOID, GEOID_CBSA, CBSAname, tract_divergenceval, div_quartile, highmf_sh, div_type)

#subset cbsa shapefile to ny metro
cbsamap_ny <- cbsamap %>% 
  filter(NAME== "New York-Newark-Jersey City, NY-NJ-PA")

#pull tract shapefile and join data to it
nymet_states <- c("NY", "NJ", "PA")
tracts_ny <- rbind_tigris( #download tract tiger/line files for all states
  lapply(nymet_states, function(x) {
    tracts(x, cb = TRUE, refresh=TRUE, year =2022) #use refresh=TRUE to redownload files
  })
)
tracts_ny <- tracts_ny %>% 
  shift_geometry(position="below") %>% 
  mutate(GEOID=as.numeric(GEOID)) %>% 
  right_join(ny, by="GEOID") %>% 
  mutate(`Neighborhood Type` = case_when(highmf_sh==1 & div_quartile==4 ~ "1 Most Divergent, High Multifamily",
                                         highmf_sh==0 & div_quartile==4 ~ "2 Most Divergent, High Single Family",
                                         highmf_sh==1 & div_quartile<4 ~ "3 Less Divergent, High Multifamily",
                                         highmf_sh==0 & div_quartile<4 ~ "4 Less Divergent, High Single Family",
                                         is.na(highmf_sh) | is.na(div_quartile) ~ "5 NA",
  ))


#this is the actual plot code         
ggplot() +
  geom_sf(data=cbsamap_ny, aes(fill="gray97"), color=NA) +
  geom_sf(data=tracts_ny, aes(fill=`Neighborhood Type`), color=NA, lwd=0) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("Most Divergent, \nHigh Multifamily", "Most Divergent, \nHigh Single Family", "Less Divergent, \nHigh Multifamily", "Less Divergent, \nHigh Single Family", "NA"), 
                    values = c("skyblue4", "mediumpurple4",  "lightskyblue1", "lavender", "grey")) +
  # scale_fill_brewer(palette = "Greens", labels=c("Less than 2 percent", "2–3.9 percent", "4–6.9 percent", "7–9.9 percent", "10 percent or More", "No Data")) + #sets the fill color scheme for the map, title for the legend, and labels for the legend items
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(0.084, .2), 
        legend.spacing.y = unit(0.25, 'cm')) +
  guides(fill=guide_legend(ncol=2, byrow=TRUE)) +
  labs(fill='Neighborhood Divergence Type') +
  coord_sf(crs=2263)

#MAP FOR TRACT DIVERGENCE IN AUSTIN METRO----
#subset tract divergence dataset to austin metro, calculate divergence quartiles,
#and identify if tract is divergent bc it is higher multifamily or higher single family
austin <- trdiv %>% 
  filter(CBSAname == "Austin-Round Rock-Georgetown, TX") %>% 
  arrange(-tract_divergenceval) %>% 
  mutate(div_quartile = ntile(tract_divergenceval, 4),
         GEOID = as.numeric(GEOID2022)) %>% 
  group_by(CBSAname) %>% 
  mutate(metrosh_mf = sum(multifamily, na.rm=TRUE)/sum(struc00_total2022, na.rm=TRUE),
         metrosh_sf = (sum(struc00_total2022, na.rm=TRUE)-sum(multifamily, na.rm=TRUE))/sum(struc00_total2022, na.rm=TRUE)) %>% 
  ungroup() %>% 
  rename(tractsh_mf = multifamily,
         tractsh_sf = singlefamily) %>% 
  mutate(highmf_sh = if_else(tractsh_mf>metrosh_mf, 1, 0), #make flag if tract share of multifamily is higher than metro share
         highsf_sh = if_else(tractsh_sf>metrosh_sf, 1, 0), #same but for high single family
         div_type = if_else(highmf_sh==1, "1 Multifamily", "2 Single Family")) %>% 
  select(GEOID, GEOID_CBSA, CBSAname, tract_divergenceval, div_quartile, highmf_sh, div_type)

#subset cbsa shapefile to Austin metro
cbsamap_austin <- cbsamap %>% 
  filter(NAME== "Austin-Round Rock-Georgetown, TX")

#pull tract shapefile and join data to it
austinmet_states <- c("TX")
tracts_austin <- rbind_tigris( #download tract tiger/line files for all states
  lapply(austinmet_states, function(x) {
    tracts(x, cb = TRUE, refresh=TRUE, year =2022) #use refresh=TRUE to redownload files
  })
)
tracts_austin <- tracts_austin %>% 
  shift_geometry(position="below") %>% 
  mutate(GEOID=as.numeric(GEOID)) %>% 
  right_join(austin, by="GEOID") %>% 
  mutate(`Neighborhood Type` = case_when(highmf_sh==1 & div_quartile==4 ~ "1 Most Divergent, High Multifamily",
                                         highmf_sh==0 & div_quartile==4 ~ "2 Most Divergent, High Single Family",
                                         highmf_sh==1 & div_quartile<4 ~ "3 Less Divergent, High Multifamily",
                                         highmf_sh==0 & div_quartile<4 ~ "4 Less Divergent, High Single Family",
                                         is.na(highmf_sh) | is.na(div_quartile) ~ "5 NA",
  ))


#this is the actual plot code         
ggplot() +
  geom_sf(data=cbsamap_austin, aes(fill="gray48"), color=NA) +
  geom_sf(data=tracts_austin, aes(fill=`Neighborhood Type`), color=NA, lwd=0) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("Most Divergent, \nHigh Multifamily", "Most Divergent, \nHigh Single Family", "Less Divergent, \nHigh Multifamily", "Less Divergent, \nHigh Single Family", "NA"), 
                    values = c("skyblue4", "mediumpurple4",  "lightskyblue1", "lavender", "grey")) +
  # scale_fill_brewer(palette = "Greens", labels=c("Less than 2 percent", "2–3.9 percent", "4–6.9 percent", "7–9.9 percent", "10 percent or More", "No Data")) + #sets the fill color scheme for the map, title for the legend, and labels for the legend items
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(-0.10, 0.7), 
        legend.spacing.y = unit(0.25, 'cm')) +
  guides(fill=guide_legend(ncol=2, byrow=TRUE)) +
  labs(fill='Neighborhood Divergence Type') +
  coord_sf(crs=6587)

#MAP FOR TRACT DIVERGENCE IN ATLANTA METRO----
#subset tract divergence dataset to atl metro, calculate divergence quartiles,
#and identify if tract is divergent bc it is higher rental or higher homeownership
atl <- trdiv %>% 
  filter(CBSAname == "Atlanta-Sandy Springs-Alpharetta, GA") %>% 
  arrange(-tract_divergenceval) %>% 
  mutate(div_quartile = ntile(tract_divergenceval, 4),
         GEOID = as.numeric(GEOID2022)) %>% 
  group_by(CBSAname) %>% 
  mutate(metrosh_mf = sum(multifamily, na.rm=TRUE)/sum(struc00_total2022, na.rm=TRUE),
         metrosh_sf = (sum(struc00_total2022, na.rm=TRUE)-sum(multifamily, na.rm=TRUE))/sum(struc00_total2022, na.rm=TRUE)) %>% 
  ungroup() %>% 
  rename(tractsh_mf = multifamily,
         tractsh_sf = singlefamily) %>% 
  mutate(highmf_sh = if_else(tractsh_mf>metrosh_mf, 1, 0), #make flag if tract share of multifamily is higher than metro share
         highsf_sh = if_else(tractsh_sf>metrosh_sf, 1, 0), #same but for high single family
         div_type = if_else(highmf_sh==1, "1 Multifamily", "2 Single Family")) %>% 
  select(GEOID, GEOID_CBSA, CBSAname, tract_divergenceval, div_quartile, highmf_sh, div_type)

#subset cbsa shapefile to Austin metro
cbsamap_atl <- cbsamap %>% 
  filter(NAME== "Atlanta-Sandy Springs-Alpharetta, GA")

#pull tract shapefile and join data to it
atlmet_states <- c("GA")
tracts_atl <- rbind_tigris( #download tract tiger/line files for all states
  lapply(atlmet_states, function(x) {
    tracts(x, cb = TRUE, refresh=TRUE, year =2022) #use refresh=TRUE to redownload files
  })
)
tracts_atl <- tracts_atl %>% 
  shift_geometry(position="below") %>% 
  mutate(GEOID=as.numeric(GEOID)) %>% 
  right_join(atl, by="GEOID") %>% 
  mutate(`Neighborhood Type` = case_when(highmf_sh==1 & div_quartile==4 ~ "1 Most Divergent, High Multifamily",
                                         highmf_sh==0 & div_quartile==4 ~ "2 Most Divergent, High Single Family",
                                         highmf_sh==1 & div_quartile<4 ~ "3 Less Divergent, High Multifamily",
                                         highmf_sh==0 & div_quartile<4 ~ "4 Less Divergent, High Single Family",
                                         is.na(highmf_sh) | is.na(div_quartile) ~ "5 NA",
  ))


#this is the actual plot code         
ggplot() +
  geom_sf(data=cbsamap_atl, aes(fill="gray48"), color=NA) +
  geom_sf(data=tracts_atl, aes(fill=`Neighborhood Type`), color=NA, lwd=0.005) + #simplify renders boundaries in simpler format, fill is variable you want to display, lwd removes the borders
  scale_fill_manual(labels=c("Most Divergent, \nHigh Multifamily", "Most Divergent, \nHigh Single Family", "Less Divergent, \nHigh Multifamily", "Less Divergent, \nHigh Single Family", "NA"), 
                    values = c("skyblue4", "mediumpurple4",  "lightskyblue1", "lavender", "grey")) +
  # scale_fill_brewer(palette = "Greens", labels=c("Less than 2 percent", "2–3.9 percent", "4–6.9 percent", "7–9.9 percent", "10 percent or More", "No Data")) + #sets the fill color scheme for the map, title for the legend, and labels for the legend items
  theme_void() + 
  theme(legend.title=element_text(size=10), 
        legend.text=element_text(size=9), 
        legend.position = c(-0.024, .7), 
        legend.spacing.y = unit(0.25, 'cm')) +
  guides(fill=guide_legend(ncol=2, byrow=TRUE)) +
  labs(fill='Neighborhood Divergence Type') +
  coord_sf(crs=2240) 


#could add city boundaries to maps
# try <- rd %>% filter(city==1 & CBSAname=="Atlanta-Sandy Springs-Alpharetta, GA") %>% mutate(GEOID=as.numeric(GEOID2022))
#try_atl <- tracts_atl %>% shift_geometry(position="below") %>% 
#     mutate(GEOID=as.numeric(GEOID)) %>% 
#     right_join(try, by="GEOID") 
#atl_city <- st_union(try_atl$geometry)
#then add to end of ggplot command:
#geom_sf(data=atl_city, fill=NA, color="black", lwd=1)



