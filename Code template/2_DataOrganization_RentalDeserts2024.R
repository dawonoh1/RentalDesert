#Rental Deserts Working Paper 041924
#second script:2_DataOrganization
#This code creates the rental deserts dataset and merges on the geographic variables from first code script 
#The resulting file gives all census variables of interest and tract geographies: RentalDeserts_dataset2022_041924.csv

#LOAD LIBRARIES-----
library(tidyverse)
library(tidycensus)
library(data.table)

#UPDATE SETTINGS AND FILE PATH------
#file path to write resulting file
filepath_data <- "/Users/ohdaw/OneDrive/Documents/R_Projects/rental_desert/Data template/"
Sys.getenv("CENSUS_API_KEY")

#UPDATE CENSUS YEAR AND VARIABLE KEY-----
v22 <- load_variables(2022, "acs5", cache = TRUE) #view available variables
view(v22)

us <- unique(fips_codes$state)[1:51] #extract list of unique state fips code
yr <- c(2022)

#HOUSING UNITS VARIABLES-----
v22[which(v22$concept=="Tenure"),] #view variables in the TENURE portion
v22[which(v22$concept=="Vacancy Status"),]
v22 %>% filter(grepl("B25004", name))

hsgvars<-function(x, y) {
  get_acs(geography = "tract",
          variables = c(vacant = "B25004_001", vacant_for_rent = "B25004_002", vacant_rent_unocc = "B25004_003", occ = "B25003_001",
                        ownocc = "B25003_002", rentocc = "B25003_003"),
          year = y,
          state = x) 
} #this function pulls all tracts from tidycensus - usually requires entering one state at a time
hsgvars_towide <- function(x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) %>% #change from long to wide
    mutate(rental_hu = rentocc + vacant_for_rent + vacant_rent_unocc,
           tot_hsg_units =  vacant + occ, 
           rentalsh = rental_hu / tot_hsg_units * 100,
           rental_des = ifelse(rentalsh < 20, 1, 0)) #rental desert if share is under 20%, otherwise assign to 0
} #this function reshapes from long to wide and creates rental desert vars

for (i in yr) {
  nam <- paste("alltracts_hsg", i , sep="")
  assign(nam, hsgvars(us, i))
} #this loop pulls data from all tracts and for defined years and creates new dataframe for each

for (i in yr) {
  nam2 <- paste("wide_alltracts_hsg", i , sep="")
  df<- get(paste0("alltracts_hsg", i))
  assign(nam2, hsgvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_hsg", i))
  names(df1) <- paste(names(df1), i, sep="")
  nam2 <- paste("wide_alltracts_hsg", i , sep="")
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i) #remove dataframes/objects from environment that are no longer needed

hsgvars <- wide_alltracts_hsg2022


sum(hsgvars$tot_hsg_units2022) #check total housing units
prop.table(table(hsgvars$rental_des2022))*100 #share of rental deserts

rm(wide_alltracts_hsg2022, alltracts_hsg2022)

str(hsgvars) #summary of vars in dataframe

#HOUSING UNITS VARIABLES - STRUCTURE TYPE OF RENTALS-----
hsgvars2<-function(x, y) {
  get_acs(geography = "tract",
          variables = c(rentocc00_tot = "B25032_013", rentocc01_sf_det = "B25032_014", rentocc02_sf_att = "B25032_015", 
                        rentocc03_mf_2 = "B25032_016", rentocc04_mf_3_4 = "B25032_017", rentocc05_mf_5_9 = "B25032_018", 
                        rentocc06_mf_10_19 = "B25032_019", rentocc07_mf_20_49 = "B25032_020", rentocc08_mf_50 = "B25032_021", 
                        rentocc09_mobilehome = "B25032_022", rentocc10_boat_rv = "B25032_023"
          ),
          year = y,
          state = x) 
} #this function pulls all tracts from tidycensus - usually requires entering one state at a time
hsgvars2_towide <- function(x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate)%>% #change from long to wide
    mutate(rentocc11_shsf = (rentocc01_sf_det + rentocc02_sf_att) / rentocc00_tot * 100,
           rentocc12_sh2_4 = (rentocc03_mf_2 + rentocc04_mf_3_4) / rentocc00_tot * 100,
           rentocc13_sh5_19 = (rentocc05_mf_5_9 + rentocc06_mf_10_19) / rentocc00_tot * 100,
           rentocc14_sh20_49 = (rentocc07_mf_20_49) / rentocc00_tot * 100,
           rentocc15_sh50pl = (rentocc08_mf_50) / rentocc00_tot * 100,
           rentocc16_shmanuf = (rentocc09_mobilehome + rentocc10_boat_rv) / rentocc00_tot * 100)
}

for (i in yr) {
  nam <- paste("alltracts_hsg2", i , sep="")
  assign(nam, hsgvars2(us, i))
} #this loop pulls data from all tracts and for defined years and creates new dataframe for each

for (i in yr) {
  nam2 <- paste("wide_alltracts_hsg2", i , sep="")
  df<- get(paste0("alltracts_hsg2", i))
  assign(nam2, hsgvars2_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_hsg2", i))
  names(df1) <- paste(names(df1), i, sep="")
  nam2 <- paste("wide_alltracts_hsg2", i , sep="")
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i) #remove dataframes/objects from environment that are no longer needed

hsgvars2 <- wide_alltracts_hsg22022


sum(hsgvars2$rentocc2022) #check total rental hhs

rm(wide_alltracts_hsg22022, alltracts_hsg22022)

str(hsgvars2) #summary of vars in dataframe

#HOUSING STRUCTURE TYPE VARIABLES-----
units<-v22[which(v22$concept=="UNITS IN STRUCTURE"),]

unitvars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(struc00_total = "B25024_001", struc01_sf_det = "B25024_002", struc02_sf_att = "B25024_003", 
                        struc03_mf_2 = "B25024_004", struc04_mf_3_4 = "B25024_005", struc05_mf_5_9 = "B25024_006", 
                        struc06_mf_10_19 = "B25024_007", struc07_mf_20_49 = "B25024_008", struc08_mf_50 = "B25024_009", 
                        struc09_mobilehome = "B25024_010", struc10_boat_rv = "B25024_011"),
          year = y,
          state = x)
}

unitvars_towide <- function (x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) %>% #change from long to wide
    mutate(struc11_shsf = (struc01_sf_det + struc02_sf_att) / struc00_total * 100,
           struc12_sh2_4 = (struc03_mf_2 + struc04_mf_3_4) / struc00_total * 100,
           struc13_sh5_19 = (struc05_mf_5_9 + struc06_mf_10_19) / struc00_total * 100,
           struc14_sh20_49 = (struc07_mf_20_49) / struc00_total * 100,
           struc15_sh50pl = (struc08_mf_50) / struc00_total * 100,
           struc16_shmanuf = (struc09_mobilehome + struc10_boat_rv) / struc00_total * 100)
}

for (i in yr) {
  nam <- paste("alltracts_unitvars", i , sep="")
  assign(nam, unitvars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_unitvars", i , sep="")
  df<- get(paste0("alltracts_unitvars", i))
  assign(nam2, unitvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_unitvars", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_unitvars", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

unitvars <- wide_alltracts_unitvars2022

rm(alltracts_unitvars2022, wide_alltracts_unitvars2022, units)

#MORTGAGE STATUS OF OWNERS VARIABLES-----
v22$mort<-grepl("B25081", v22$name) #pull variables with b25119 in name
mort<-v22[which(v22$mort=="TRUE"),] #select those where this is true
(mort$name) #check that variables are the ones intended

mortvars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(ownocc_total = "B25081_001", ownocc_wmort = "B25081_002", ownocc_womort = "B25081_009"),
          year = y,
          state = x)
}

mortvars_towide <- function (x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) #change from long to wide
}

for (i in yr) {
  nam <- paste("alltracts_mortvars", i , sep="")
  assign(nam, mortvars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_mortvars", i , sep="")
  df<- get(paste0("alltracts_mortvars", i))
  assign(nam2, mortvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_mortvars", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_mortvars", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i, mort)

mortvars <- wide_alltracts_mortvars2022

sum(mortvars$ownocc_total2022) #check total owner hhs
mortvars %>% #check that variables add to total
  mutate(check = ownocc_total2022 - ownocc_wmort2022 - ownocc_womort2022) %>% 
  filter(check!=0)

rm(alltracts_mortvars2022, wide_alltracts_mortvars2022)

#MEDIAN INCOME BY TENURE VARIABLES-----
v22$inctab<-grepl("B25119", v22$name) #pull variables with b25119 in name
inctab<-v22[which(v22$inctab=="TRUE"),] #select those where this is true
(inctab$name) #check that variables are the ones intended

incvars<-function(x, y) {
  get_acs(geography = "tract",
          variables = c(mdnhhinc = "B25119_001", mdnhhinc_own = "B25119_002", mdnhhinc_rent = "B25119_003"),
          year = y,
          state = x)
}

incvars_towide<- function(x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) 
}

for (i in yr) {
  nam <- paste("alltracts_inc", i , sep="")
  assign(nam, incvars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_inc", i , sep="")
  df<- get(paste0("alltracts_inc", i))
  assign(nam2, incvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_inc", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_inc", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

incvars <- wide_alltracts_inc2022

rm(alltracts_inc2022, wide_alltracts_inc2022, inctab)

#INCOME CATEOGORY VARIABLES-----
v22$inccat<-grepl("B19001_", v22$name) #pull variables with b25119 in name
inccat<-v22[which(v22$inccat=="TRUE"),] #select those where this is true
(inccat$name) #check that variables are the ones intended
(inccat$concept)

incvars2<-function(x, y) {
  get_acs(geography = "tract",
          variables = c(inccat00_tot = "B19001_001", inccat01_10000 = "B19001_002", inccat02_10000_14999 = "B19001_003",
                        inccat03_15000_19999 = "B19001_004", inccat04_20000_24999 = "B19001_005", inccat05_25000_29999 = "B19001_006",
                        inccat06_30000_34999 = "B19001_007", inccat07_35000_39999 = "B19001_008", inccat08_40000_44999 = "B19001_009", 
                        inccat09_45000_49999 = "B19001_010", inccat10_50000_59999 = "B19001_011", inccat11_60000_74999 = "B19001_012", 
                        inccat12_75000_99999 = "B19001_013", inccat13_100000_124999 = "B19001_014", inccat14_125000_149999 = "B19001_015", 
                        inccat15_150000_199999 = "B19001_016", inccat16_200000 = "B19001_017"),
          year = y,
          state = x)
}

incvars2_towide<- function(x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) 
}

for (i in yr) {
  nam <- paste("alltracts_inc2", i , sep="")
  assign(nam, incvars2(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_inc2", i , sep="")
  df<- get(paste0("alltracts_inc2", i))
  assign(nam2, incvars2_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_inc2", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_inc2", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

incvars2 <- wide_alltracts_inc22022

incvars2 %>%  #check that variables sum to total
  rowwise() %>%
  mutate(SUM_RQ = sum(c_across(inccat01_100002022:inccat16_2000002022)),
         diff = inccat00_tot2022 - SUM_RQ) %>%
  filter(diff!=0)

rm(alltracts_inc22022, wide_alltracts_inc22022, inccat)

incvars2<-incvars2 %>%  #create lower-income, middle-income, and higher-income categories
  rowwise() %>%
  mutate(inccat_lower_income = sum(c_across(inccat01_100002022:inccat05_25000_299992022)),
         inccat_middle_income = sum(c_across(inccat06_30000_349992022:inccat11_60000_749992022 )),
         inccat_higher_income = sum(c_across(inccat12_75000_999992022:inccat16_2000002022 )))

incvars2 %>% #check that categories add up to total hhs
  mutate(check = inccat00_tot2022 - inccat_lower_income - inccat_middle_income - inccat_higher_income) %>% 
  filter(check!=0)


#RACE/ETHNICITY POPULATION VARIABLES-----
v22$racetab<-grepl("B03002", v22$name) #
racetab<-v22[which(v22$racetab=="TRUE"),]
(racetab$name)
(racetab$concept)

racevars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(race00_totalpop = "B03002_001", race01_whitepop = "B03002_003", race02_blackpop = "B03002_004", 
                        race03_am_indianpop = "B03002_005", race04_asianpop = "B03002_006", 
                        race05_native_hipop = "B03002_007", race06_hisppop = "B03002_012"),
          year = y,
          state = x)
}

racevars_towide <- function (x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) %>% #change from long to wide
    mutate(race07_shwhite = race01_whitepop / race00_totalpop * 100,
           race08_shblack = race02_blackpop / race00_totalpop * 100,
           race09_shhisp = race06_hisppop / race00_totalpop * 100,
           race10_shpoc = (race00_totalpop - race01_whitepop) / race00_totalpop * 100)
}


for (i in yr) {
  nam <- paste("alltracts_race", i , sep="")
  assign(nam, racevars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_race", i , sep="")
  df<- get(paste0("alltracts_race", i))
  assign(nam2, racevars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_race", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_race", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

racevars <- wide_alltracts_race2022

rm(alltracts_race2022, wide_alltracts_race2022, racetab)

#AGE POPULATION VARIABLES-----
v22$agetab<-grepl("B01001", v22$name) #
agetab<-v22[which(v22$agetab=="TRUE"),]
(agetab$name)

agevars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(age00_total = "B01001_001",
                        age01_male_5 = "B01001_003",
                        age02_male_5_9 = "B01001_004",
                        age03_male_10_14 = "B01001_005",
                        age04_male_15_17  = "B01001_006",
                        age05_male_18_19 = "B01001_007",
                        age06_male_20 = "B01001_008",
                        age07_male_21 = "B01001_009",
                        age08_male_22_24 = "B01001_010",
                        age09_male_25_29 = "B01001_011",
                        age10_male_30_34  = "B01001_012",
                        age11_male_35_39 = "B01001_013",
                        age12_male_40_44 = "B01001_014",
                        age13_male_45_49 = "B01001_015",
                        age14_male_50_54 = "B01001_016",
                        age15_male_55_59 = "B01001_017",
                        age16_male_60_61 = "B01001_018",
                        age17_male_62_64 = "B01001_019",
                        age18_male_65_66 = "B01001_020",
                        age19_male_67_69 = "B01001_021",
                        age20_male_70_74 = "B01001_022",
                        age21_male_75_79 = "B01001_023",
                        age22_male_80_84 = "B01001_024",
                        age23_male_85pl = "B01001_025",
                        age24_female_5 = "B01001_027",
                        age25_female_5_9 = "B01001_028",
                        age26_female_10_14 = "B01001_029",
                        age27_female_15_17  = "B01001_030",
                        age28_female_18_19 = "B01001_031",
                        age29_female_20 = "B01001_032",
                        age30_female_21 = "B01001_033",
                        age31_female_22_24 = "B01001_034",
                        age32_female_25_29 = "B01001_035",
                        age33_female_30_34  = "B01001_036",
                        age34_female_35_39 = "B01001_037",
                        age35_female_40_44 = "B01001_038",
                        age36_female_45_49 = "B01001_039",
                        age37_female_50_54 = "B01001_040",
                        age38_female_55_59 = "B01001_041",
                        age39_female_60_61 = "B01001_042",
                        age40_female_62_64 = "B01001_043",
                        age41_female_65_66 = "B01001_044",
                        age42_female_67_69 = "B01001_045",
                        age43_female_70_74 = "B01001_046",
                        age44_female_75_79 = "B01001_047",
                        age45_female_80_84 = "B01001_048",
                        age46_female_85pl = "B01001_049"),
          year = y,
          state = x)
}

agevars_towide<- function(x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) 
}

for (i in yr) {
  nam <- paste("alltracts_agevars", i , sep="")
  assign(nam, agevars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_age", i , sep="")
  df<- get(paste0("alltracts_agevars", i))
  assign(nam2, agevars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_age", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_age", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

agevars <- wide_alltracts_age2022

rm(alltracts_agevars2022, wide_alltracts_age2022, agetab)


#HOME VALUES-----
val<-v22[which(v22$concept=="MEDIAN VALUE (DOLLARS)"),]

valvars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(mdnhomeval = "B25077_001"),
          year = y,
          state = x)
}

valvars_towide <- function (x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) #change from long to wide
}

for (i in yr) {
  nam <- paste("alltracts_valvars", i , sep="")
  assign(nam, valvars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_valvars", i , sep="")
  df<- get(paste0("alltracts_valvars", i))
  assign(nam2, valvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_valvars", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_valvars", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

valvars <- wide_alltracts_valvars2022

rm(alltracts_valvars2022, wide_alltracts_valvars2022, val)

#MEDIAN MONTHLY OWNER COSTS
v22$own<-grepl("B25088", v22$name) #pull variables with b25119 in name
own<-v22[which(v22$own=="TRUE"),] #select those where this is true
(own$name) #check that variables are the ones intended

ownvars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(mdnown_smoc = "B25088_001", mdnown_wmort_smoc = "B25088_002", mdnown_womort_smoc = "B25088_003"),
          year = y,
          state = x)
}

ownvars_towide <- function (x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) #change from long to wide
}

for (i in yr) {
  nam <- paste("alltracts_ownvars", i , sep="")
  assign(nam, ownvars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_ownvars", i , sep="")
  df<- get(paste0("alltracts_ownvars", i))
  assign(nam2, ownvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_ownvars", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_ownvars", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

ownvars <- wide_alltracts_ownvars2022

rm(alltracts_ownvars2022, wide_alltracts_ownvars2022, own)

#MEDIAN RENT-----
rnt<-v22[which(v22$concept=="MEDIAN GROSS RENT (DOLLARS)"),]

rntvars <- function(x, y) {
  get_acs(geography = "tract",
          variables = c(mdnrent = "B25064_001"),
          year = y,
          state = x)
}

rntvars_towide <- function (x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) #change from long to wide
}

for (i in yr) {
  nam <- paste("alltracts_rntvars", i , sep="")
  assign(nam, rntvars(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_rntvars", i , sep="")
  df<- get(paste0("alltracts_rntvars", i))
  assign(nam2, rntvars_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_rntvars", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_rntvars", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

rentvars <- wide_alltracts_rntvars2022

rm(alltracts_rntvars2022, wide_alltracts_rntvars2022, rnt)

#RENT CATEGORY VARIABLES-----
v22$rntcat<-grepl("B25063", v22$name) #pull variables with b25119 in name
rntcat<-v22[which(v22$rntcat=="TRUE"),] #select those where this is true
(rntcat$name) #check that variables are the ones intended

rntvars2<-function(x, y) {
  get_acs(geography = "tract",
          variables = c(rntcat00_rntocctot = "B25063_001", rntcat00_tot_cashrnt = "B25063_002", rntcat01_100 = "B25063_003",
                        rntcat02_100_149 = "B25063_004", rntcat03_150_199 = "B25063_005", rntcat04_200_249 = "B25063_006",
                        rntcat05_250_299 = "B25063_007", rntcat06_300_349 = "B25063_008", rntcat07_350_399 = "B25063_009", 
                        rntcat08_400_449 = "B25063_010", rntcat09_450_499 = "B25063_011", rntcat10_500_549 = "B25063_012", 
                        rntcat11_550_599 = "B25063_013", rntcat12_600_649 = "B25063_014", rntcat13_650_699 = "B25063_015", 
                        rntcat14_700_749 = "B25063_016", rntcat15_750_799 = "B25063_017", rntcat16_800_899 = "B25063_018",
                        rntcat17_900_999 = "B25063_019", rntcat18_1000_1249 = "B25063_020", rntcat19_1250_1499 = "B25063_021",
                        rntcat20_1500_1999 = "B25063_022", rntcat21_2000_2499 = "B25063_023", rntcat22_2500_2999 = "B25063_024",
                        rntcat23_3000_3499 = "B25063_025", rntcat24_3500 = "B25063_026", rntcat25_nocashrnt = "B25063_027"),
          year = y,
          state = x)
}

rntvars2_towide<- function(x) {
  x %>%
    select(-moe) %>% #remove moe
    spread(variable, estimate) %>% 
    rowwise() %>% 
    mutate(rntcat26_600 = sum(c_across(rntcat01_100:rntcat11_550_599)),
           rntcat27_600_999 = sum(c_across(rntcat12_600_649:rntcat17_900_999)),
           rntcat28_1000_1499 = sum(c_across(rntcat18_1000_1249:rntcat19_1250_1499)),
           rntcat29_1500_1999 = sum(c_across(rntcat20_1500_1999)),
           rntcat30_2000 = sum(c_across(rntcat21_2000_2499:rntcat24_3500))) %>% 
    ungroup() %>% 
    mutate(rntcat31_sh_600 = rntcat26_600/rntcat00_tot_cashrnt*100,
           rntcat32_sh_600_999 = rntcat27_600_999/rntcat00_tot_cashrnt*100,
           rntcat33_sh_1000_1499 = rntcat28_1000_1499/rntcat00_tot_cashrnt*100,
           rntcat34_sh_1500_1999 = rntcat29_1500_1999/rntcat00_tot_cashrnt*100,
           rntcat35_2000 = rntcat30_2000/rntcat00_tot_cashrnt*100)
}

for (i in yr) {
  nam <- paste("alltracts_rnt2", i , sep="")
  assign(nam, rntvars2(us, i))
} #pull data

for (i in yr) {
  nam2 <- paste("wide_alltracts_rnt2", i , sep="")
  df<- get(paste0("alltracts_rnt2", i))
  assign(nam2, rntvars2_towide(df))
} # this loop reshapes dataframes from long to wide

for (i in yr) {
  df1<-get(paste0("wide_alltracts_rnt2", i))
  names(df1) <- paste0(names(df1), i)
  nam2 <- paste0("wide_alltracts_rnt2", i)
  assign(nam2, df1)
} #this loop adds survey year to the variable names
rm(nam, nam2, df, df1, i)

rentvars2 <- wide_alltracts_rnt22022

rm(alltracts_rnt22022, wide_alltracts_rnt22022, rntcat)

#MERGE VARIABLES INTO ONE DF-----
#remove name2022 variable from all dfs
list2  <- map(list(hsgvars, mortvars,
                   unitvars, hsgvars2,
                   incvars, incvars2, 
                   valvars, ownvars, 
                   rentvars, rentvars2,
                   racevars, agevars),
              select, -NAME2022)


#join datasets together
full <- 
  reduce(list2, 
         left_join, by = "GEOID2022")

full <- full %>% 
  mutate(GEOID2022=as.numeric(GEOID2022)) #convert to numeric for joining to geography file

#ADD GEOGRAPHY VARIABLES-----
#geography file created in 1_GeographySetup script
tractdf_final<-fread(paste0(filepath_data, "tracts_cbsas_suburbs_2022.csv")) %>% 
  mutate(GEOID2022 = as.numeric(GEOIDtract))

full_geo <- full %>% 
  left_join(tractdf_final, by="GEOID2022") %>%  #check observations is same as full
  select(-V1) %>% 
  rename(CBSAname = NAME,
         CBSAname_full = NAMELSAD)

rm(list=setdiff(ls(), c("full_geo", "filepath_data"))) #clear environment except for final df
dim(full_geo)
write.csv(full_geo, paste0(filepath_data, "RentalDeserts_dataset2022_041924.csv"))
#final dataset for analysis
