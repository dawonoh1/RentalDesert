m nh#Rental Deserts Working Paper 041924
#Third Script, Part 1: 3.1_ZoningFileSetup_TractInterpolation
#This code does spatial interpolation between 2022 tracts and 2010 places/county subdivisions
#For the purpose of merging on the National Zoning and Land Use Index Data
#The resulting file contains 2022 tracts and the 2010 place or county subdivision & cbsa in which they are located: tracts2022_places10_countysub10_cbsa10.csv

#states that use county subdivisions instead of places:
# Connecticut 09, Maine 23, Massachusetts 25, Michigan 26, 
# Minnesota 27, New Hampshire 33, New Jersey 34, New York 36, 
# Pennsylvania 42, Rhode Island 44, Vermont 50, and Wisconsin 55

#LOAD LIBRARIES-----
library(httr)
library(sf)
library(tidyverse)
library(tigris)

#UPDATE FILE PATH------
#file path to write resulting file - also need to update file path on line 32 for location to initially download place files
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"
filepath_placefiles <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/TIGERFiles/PlaceFiles/"

#CREATE STATE FIPS-----
state <- as.character(1:56) #state fips 1:56
state <- str_pad(state, 2, pad = "0") #add 0 before FIPS with 1 digit
state <- state[! state %in% c("03", "07", "14", "43", "52")] #remove FIPS codes that don't correspond to actual state

#PROCESS PLACE FILES-----
#download 2010 place tiger/line files from census website (not currently supported in tigris)
for (st in state) {
  data_url <- paste0("https://www2.census.gov/geo/tiger/TIGER2010/PLACE/2010/tl_2010_", st, "_place10.zip")
  zip_name <- basename(data_url)
  out_file <- file.path("/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Downloads/", zip_name)
  GET(data_url, write_disk(out_file, overwrite = TRUE))
  unzip(out_file, exdir = filepath_placefiles)
  print(paste0("download worked for ", st))
  
  df<- st_read(paste0(filepath_placefiles, "tl_2010_", st, "_place10.shp"))
  print(paste0("read file for ", st))
  
  dfname<-paste0("shp", st)
  assign(dfname, df)
  print(paste0("unique sf object created for ", st))
  rm(df)
}

shps <- lapply(ls(pattern="^shp"), function(x) get(x))
all_places_shps <- bind_rows(shps) #binds all shape files into one file

rm(list=setdiff(ls(), c("state", "all_places_shps", "filepath_data", "filepath_places"))) #clear individual shapefiles from environment

#PROCESS COUNTY SUBDIVISION FILES-----
all_countysubs_shp <- rbind_tigris(
  lapply(state, function(x) {
    county_subdivisions(x, cb = TRUE, year =2010) #use refresh=TRUE to redownload files
  })
)

#PROCESS CBSA FILES-----
all_cbsas_shp<-core_based_statistical_areas(cb = TRUE, refresh=TRUE, year=2010)

#PROCESS TRACT FILES-----
tracts <- rbind_tigris( #download tract tiger/line files for all states
  lapply(state, function(x) {
    tracts(x, cb = TRUE, refresh=TRUE, year =2022) #use refresh=TRUE to redownload files
  })
)

#JOIN TRACTS TO PLACES AND COUNTY SUBDIVISIONS-----
tract_cent <- st_centroid(tracts) #convert tracts to centroids

tracts_places <- st_join(
  tract_cent,
  all_places_shps,
  join = st_within,
  suffix = c("tract22", "place10"),
  left = TRUE
)

tracts_places_final <- data.frame(tracts_places) %>% 
  select(GEOID, STATEFP10, PLACEFP10, GEOID10, NAMELSAD10) %>% 
  mutate(place = if_else(!is.na(NAMELSAD10), 1, 0)) %>% 
  rename(place_GEOID10 = GEOID10,
         place_NAMESAD10 = NAMELSAD10)

tracts_cosubs <- st_join(
  tract_cent,
  all_countysubs_shp,
  join = st_within,
  suffix = c("tract22", "cosub10"),
  left = TRUE
)

tracts_cosubs_final <- data.frame(tracts_cosubs) %>% 
  select(GEOID, STATE, COUSUB, NAMEcosub10) %>% 
  mutate(countysub = if_else(!is.na(NAMEcosub10), 1, 0),
         cousub_GEOID = paste0(STATE, COUSUB))

tracts_full <- tracts_places_final %>% 
  left_join(tracts_cosubs_final, by="GEOID") %>% 
  select(GEOID, STATEFP10, STATE, place_GEOID10, place_NAMESAD10, cousub_GEOID, 
         NAMEcosub10, place, countysub) %>% 
  mutate(muni_GEOID10 = if_else(STATE %in% c("09", "23", "25",
                                             "26", "27", "33",
                                             "34", "36", "42",
                                             "44", "50", "55"), 
                                cousub_GEOID, place_GEOID10),
         muni_type_flag = if_else(STATE %in% c("09", "23", "25",
                                               "26", "27", "33",
                                               "34", "36", "42",
                                               "44", "50", "55"), 
                                  "county subdivision", "place"),
         GEOID= as.numeric(GEOID))

tracts_full_simple <- tracts_full %>% 
  select(GEOID, muni_GEOID10, muni_type_flag) %>% 
  rename(GEOID2022=GEOID) %>% 
  mutate(muni_type_flag = if_else(is.na(muni_GEOID10), NA_character_, muni_type_flag))

#JOIN TRACTS TO CBSAs-----
tracts_cbsas <- st_join(
  tract_cent,
  all_cbsas_shp,
  join = st_within,
  suffix = c("tract22", "cbsa10"),
  left = TRUE
)

tracts_cbsas_final <- data.frame(tracts_cbsas) %>% 
  select(GEOID, CBSA, NAMEcbsa10) %>% 
  rename(GEOID2022=GEOID,
         cbsa_GEOID10 = CBSA,
         cbsa_NAME10 = NAMEcbsa10) %>% 
  mutate(GEOID2022 = as.numeric(GEOID2022))

#ADD CBSA TO TRACT FILE-----
tracts_muni_cbsa <- tracts_full_simple %>% 
  left_join(tracts_cbsas_final, by="GEOID2022")

write.csv(tracts_muni_cbsa, paste0(filepath_data, "tracts2022_places10_countysub10_cbsa10.csv"))

rm(list=setdiff(ls(), "tracts_muni_cbsa")) #clear environment except for final df
