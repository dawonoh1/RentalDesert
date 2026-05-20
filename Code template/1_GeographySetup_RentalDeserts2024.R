#Rental Deserts Working Paper 041824
#first script:0_GeographySetUp
#This code creates the geographic crosswalk for the rental deserts dataset
#The resulting file gives the tract, the cbsa it is in, and the urban/suburban classification: tracts_cbsas_suburbs_2022.csv

#LOAD LIBRARIES------
install.packages(c("sf", "dplyr", "tigris"))
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("tigris", repos = "https://cloud.r-project.org/")
library(tigris)
library(tidyverse)
library(tidycensus)
library(sf)
census_api_key("d07d4957bdb1c055a6f146a3f98f1dbd59f037c9", install = TRUE)

#UPDATE SETTINGS AND FILE PATH------
#file path
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/" 
#tigris settings
  options(tigris_use_cache = TRUE) #saves files so it won't re-download ever time
  tigris_cache_dir("C:\\Users\\ohdaw\\OneDrive - Harvard University\\cRA_rentaldesert\\Updating tract geographies\\tigris cache\\") #set the cache directory
  readRenviron('~/.Renviron')
  Sys.getenv('TIGRIS_CACHE_DIR') #check cache directory if needed


#DOWNLOAD TRACT, PLACE, CBSA FILES FROM CENSUS------
sts <- unique(fips_codes$state)[1:51] #get fips id's for states

tracts <- rbind_tigris( #download tract tiger/line files for all states
  lapply(sts, function(x) {
    tracts(x, cb = TRUE, refresh=TRUE, year =2022) #use refresh=TRUE to redownload files
  })
)

places <- rbind_tigris( #download place tiger/line files for all states
  lapply(sts, function(x) {
    places(x, cb = TRUE, refresh=TRUE, year=2022) 
  })
)

cbsas <- core_based_statistical_areas(cb = TRUE, refresh=TRUE, year=2021) #download all cbsa tiger/line files - 2021 is most recent vintage


#CATEGORIZE PLACES AS CITY/SUBURBAN-----
place_cent <- st_centroid(places) #create centroid layer for places

places_cbsas <- st_join( #join place centroids to cbsa polygons
  place_cent,
  cbsas,
  join = st_within,
  suffix = c("place", "cbsa"),
  left = TRUE
) 

places_cbsas <- places_cbsas %>%  
  mutate(metname = sub("\\,.*", "", NAMEcbsa),
         metname = gsub("--", "-", metname), #format metro names to remove duplicate characters
         metname = recode(metname, "Nashville-Davidson-Murfreesboro-Franklin" = "Nashville/Davidson-Murfreesboro-Franklin", #format place names in cbsa titles to match how they appear in place names
                          "Scranton-Wilkes-Barre" = "Scranton-Wilkes Barre",
                          "Winston-Salem" = "Winston Salem",
                          "Augusta-Richmond County" = "Augusta/Richmond",
                          "Macon-Bibb County" = "Macon/Bibb",
                          "Lexington-Fayette" = "Lexington/Fayette",
                          "Athens-Clarke County" = "Athens/Clarke",
                          "Louisville/Jefferson County" = "Louisville"),
         NAMEplace = recode(NAMEplace, "Wilkes-Barre" = "Wilkes Barre", #format place names in place titles to match cbsa place names
                            "Winston-Salem" = "Winston Salem",
                            "Louisville/Jefferson County metro government (balance)" = "Louisville",
                            "Nashville-Davidson metropolitan government (balance)" = "Nashville/Davidson",
                            "Indianapolis city (balance)" = "Indianapolis",
                            "Augusta-Richmond County consolidated government (balance)" = "Augusta/Richmond",
                            "Macon-Bibb County" = "Macon/Bibb",
                            "Lexington-Fayette" = "Lexington/Fayette", 
                            "Athens-Clarke County unified government (balance)" = "Athens/Clarke")
  ) %>% 
  separate_wider_delim(metname, "-", #parse cbsa names into component places creating new variables
                       names = c("metname1", "metname2", "metname3"),
                       too_few = "align_start") %>% 
  filter(grepl("Metro", NAMELSADcbsa)) %>% #keep metros (drop micros)
  mutate(named1 = if_else(NAMEplace == metname1, 1, 0), #indicate if the place name matches the first named place in the cbsa
         named2 = if_else(NAMEplace == metname2, 1, 0),
         named3 = if_else(NAMEplace == metname3, 1, 0),
         named1 = case_when(NAMELSADplace == "Washington town" ~ 0, #clean up a few odd places with repeat names to make sure they aren't captured as a named place
                            NAMEplace == "Kansas City" & STUSPS == "KS" ~ 0,
                            NAMEplace=="Texarkana" & STUSPS=="AR" ~ 0,
                            TRUE~as.numeric(as.character(named1)))) #otherwise, keep it as a named place if above conditions are met
# #check that all msas have at least 1 named city and no more than 1 named city 
# (Louisville is in there twice to account for county balance though)
# check<- places_cbsas %>%
# group_by(NAMEcbsa) %>% summarize(tot = sum(named1, na.rm=TRUE))

#pull place populations to identify ones that meet the threshold for being considered a city
v22 <- load_variables(2022, "acs1", cache = TRUE)
placepop <- get_acs(geography = "place", 
                    variables = "B01003_001",
                    year=2022) %>% 
  select(-moe, -variable) %>% 
  rename(pop = estimate)

#merge place pop onto place/cbsa dataset
pl_cbsa_pop <- places_cbsas %>% 
  left_join(placepop, by=join_by(GEOIDplace == GEOID))

#define city/suburb
#city = first named in metro area or named city with >100000 pop
pl_cbsa_pop <- pl_cbsa_pop %>% 
  mutate(popcity = if_else(pop > 100000, 1, 0), #meets population threshold for a city
         namedcity = if_else(named1==1|named2==1|named3==1, 1, 0), #is a named city in the cbsa
         city = if_else((popcity==1 & namedcity==1) | named1 == 1, 1, 0),
         suburb = if_else(city==1, 0, 1)) %>% 
  select(GEOIDplace, NAMELSADplace, STUSPS, city, suburb)


#then join tracts on to places and dataset just created to assign tracts city/suburb status
tract_cent <- st_centroid(tracts) #create centroid layer for tracts

tracts_places <- st_join( #join places to tract centroids
  tract_cent,
  places,
  join = st_within,
  suffix = c("tract", "place"),
  left = TRUE
) 

tract_sub <- tracts_places %>% #merge on city/suburb classification
  left_join(pl_cbsa_pop, by="GEOIDplace") %>% 
  replace_na(list(city = 0, suburb = 0)) %>% 
  select(STATEFPtract:GEOIDtract, city:geometry)

#CATEGORIZE TRACTS AS METRO/MICRO/RURAL BASED ON CBSA-----
tracts_cbsas <- st_join( #spatial join tract data to cbsas to categorize micro/rural
  tract_sub,
  cbsas,
  join = st_within,
  suffix = c("tract", "cbsa"),
  left = TRUE
)

#pull and add cbsa pop to get top100
cbsapop <- get_acs(geography = "cbsa", 
                   variables = "B01003_001",
                   year=2022) %>% 
  select(-moe, -variable) %>% 
  rename(cbsapop = estimate) %>% 
  arrange(-cbsapop) %>% 
  filter(!grepl(", PR", NAME)) %>% #remove Puerto Rico metros
  mutate(cbsa_rank = 1:nrow(.), 
         cbsa_top100 = if_else(cbsa_rank<=100, 1, 0)) %>% 
  rename(CBSA_name = NAME)

tracts_cbsas <- tracts_cbsas %>% #join cbsapop rank to tract dataset
  left_join(cbsapop, by="GEOID")

#classify as metro, micro, rural based on metro names
tractdf <- tracts_cbsas %>% 
  mutate(metro = if_else(grepl("Metro", NAMELSAD), 1, 0),
         micro = if_else(grepl("Micro", NAMELSAD), 1, 0),
         rural = if_else(is.na(NAMELSAD) & micro==0, 1, 0),
         suburb = if_else(metro==1 & city==0, 1, 0))

#check numbers are reasonable
tractdf %>% 
  summarise(across(city:suburb, sum))

tractdf_final <- tractdf %>% 
  select(STATEFPtract, COUNTYFP, TRACTCE, GEOIDtract, 
         CBSAFP, GEOID, NAME, NAMELSAD, cbsa_top100, cbsa_rank,
         metro, city, suburb, micro, rural) %>% 
  rename(GEOID_CBSA = GEOID)


rm(list=setdiff(ls(), c("tractdf_final", "filepath_data"))) #clear environment except for final df

st_geometry(tractdf_final) <- NULL #change to normal dataframe
tractdf_final <- as_tibble(tractdf_final) #and then to tibble for dplyr manipulation

write.csv(tractdf_final, paste0(filepath_data, "tracts_cbsas_suburbs_2022.csv"))
