##### Clean UPP Project Data ###################################################

#####   HPD measures the potential for speculation by comparing a property’s 
##### capitalization rate (defined as a property’s net operating income divided 
##### by its sales price) to the median capitalization rate of similar buildings
##### sold in the same borough. HPD places properties sold with capitalization
##### rates below their borough’s median on the Speculation Watch List. 

#### Cap Rates Compared To Quarterly Borough Averages #####

##### See https://codelibrary.amlegal.com/codes/newyorkcity/latest/NYCrules/0-0-0-109975

##### I will compare more surgical applications of a relative cap-rate approach 
##### and compare the efficacy of a relative cap-rate appraoch to a paired-sales 
##### approach a la the Case-Shiller Index (Shiller 1991)

##### For now, I'm using just 2018Q2-2021Q2 sales to have uniform + & - years  
##### from placement on QTL/SWL. Therefore, will focus on dep vars in 2016-2023.

################################################################################

### Set Up

# Set WD
setwd("/Volumes/BigBrady/UPP_PASSPORT")

# Load Packages
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(stringr)
library(arsenal)
library(tigris)
library(sf)

# Set Time Frame 

years <- 2016:2022
years_bounded <- 2016:2023
dates_bounded <- as.Date(paste0(years_bounded,"-01-01"))
sale_qtrs <- seq(from = 201850, to = 202250, by = 25)/100

### Import Data

# Import Shiller (1991) Speculation Typology For All HCR BBLs
CS <- fread("./ProcessedData/CS_QTL.csv")

# Import bunten et al. (2023) Gentrification Typology @ NYC Zip 
GTzips <- fread("/Volumes/BigBrady/THESIS_PASSPORT/Harrasment_Gentrification_PASSPORT/My_Processed_Data/NYCMETRO_gent_zips_BUNTEN23.csv")

# Import Ding et al. (2016) Gentrification Typology @ NYC Tracts 
GTtracts <- fread("/Volumes/BigBrady/THESIS_PASSPORT/Harrasment_Gentrification_PASSPORT/My_Processed_Data/nyc_gent_tracts_DING16ACS10_19FULL.csv")

# Import 2022 Queried Lists
Elig22 <- fread("./RawData/22/swl_eligible_geocoded.csv") %>% clean_names()
QTL22 <- fread("./RawData/22/qual_transactions.csv") %>% clean_names()
SWL22 <- fread("./RawData/22/Speculation_Watch_List.csv") %>% clean_names()

# Import 2022 Raw Data
#DOB <- fread("./RawData/22/wwp_table.csv") %>% clean_names()
HMCV <- fread("./RawData/24/HMC_V_2007-2023.csv") %>% clean_names()
RSU <- fread("RawData/24/dhcr_rsu_final.csv") %>% clean_names()
Alt1 <- fread("RawData/24/dcpdelta_alt1_prmt_yr_count.csv") %>% clean_names()
Demo <- fread("RawData/24/dcpdelta_demo_prmt_yr_county.csv") %>% clean_names()
MYevicts <- fread("/Volumes/BigBrady/UPP_PASSPORT/RawData/24/marshal_evictions_all.csv")
MYhc <- fread("/Volumes/BigBrady/UPP_PASSPORT/RawData/24/Housing_Litigations_20241106.csv")

### Define Some Useful Values

# Get NYC 2010 Tracts
NYCtracts <- tracts(
  state = "36",
  county = c("005","047","081","061","085"),
  year = 2010)

bbox <- st_bbox(NYCtracts)

# Create Vectors Of BBLs per group
Elig22BBLs <- Elig22 %>%
  distinct() %>%
  filter(bbl != 0, !is.na(bbl)) %>%
  pull(bbl)

QTL22BBLs <- QTL22 %>%
  distinct() %>%
  pull(bbl)

SWL22BBLs <- SWL22 %>%
  distinct() %>%
  pull(bbl)

Group <- c("Eligible","QTL","SWL")
Total <- c(length(Elig22BBLs), length(QTL22BBLs), length(SWL22BBLs))
ALL22BBLsDF <- tibble(Group, Total) 

## Create Statuses Base DF
status <- Elig22 %>%
  mutate(isQTL = if_else(bbl %in% QTL22BBLs,1,0)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0))

### Data Cleaning 

# Clean CS 
CS_cln <- CS %>%
  rename(sale_year= sale1_year)
  
# Clean Gentrification Zips Typology
GTzipscln <- GTzips %>%
  select(zipcode = zip_code, gent_elig, gent_year) %>%
  distinct()

# Clean Gentrification Tracts Typology
GTtractscln <- GTtracts %>%
  select(geoid10 = GEOID, fips, eligible_to_gentrify, gentry_status)

## Get Coordinates & Tracts from Eligible BBL Population List
Elig22sel <- Elig22 %>%
  select(bbl, long = longitude,lat = latitude) %>%
  mutate(across(2:3, ~as.numeric(.x)))

Elig22coors <- st_as_sf(Elig22sel, coords = c("long", "lat"), crs = st_crs(NYCtracts))

Elig22tracts <- st_join(Elig22coors, NYCtracts) %>%
  clean_names() %>%
  select(bbl, geoid10)

# Get Price, Cap Rates, Sale Date, Borough, & Zip From QTL 
QTL22_sel <- QTL22 %>%
  select(bbl, boro, zipcode = postcode, ct10 = census_tract, nta, price, cap_rate, cap_rate_boro_mean = borough_cap_rate, sale_date = deed_date) %>%
  mutate(sale_date = parse_date_time(sale_date, "mdy")) %>%
  mutate(sale_qtr = yearqtr(sale_date)) %>%
  mutate(sale_year = year(sale_date)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0))

### Final Data Cleaning & Save CSV's
  
# Create MYSWLboro from QTL22_sel 
MySWLboro <- QTL22_sel %>%
  group_by(boro, sale_qtr) %>%
  mutate(cap_rate_boroqtr_50th = quantile(cap_rate, probs = 0.50)) %>%
  mutate(cap_rate_boroqtr_25th = quantile(cap_rate, probs = 0.25)) %>% 
  mutate(cap_rate_boroqtr_20th = quantile(cap_rate, probs = 0.20)) %>%
  mutate(cap_rate_boroqtr_10th = quantile(cap_rate, probs = 0.10)) %>%
  ungroup() %>%
  mutate(isBot10b = if_else(cap_rate < cap_rate_boroqtr_10th,1,0)) %>%
  mutate(isBot20b = if_else(cap_rate < cap_rate_boroqtr_20th,1,0)) %>%
  mutate(isBot25b = if_else(cap_rate < cap_rate_boroqtr_25th,1,0)) %>%
  mutate(isBot50b = if_else(cap_rate < cap_rate_boroqtr_50th,1,0)) %>%
  select(bbl,cap_rate, cap_rate_boroqtr_50th, cap_rate_boroqtr_25th,cap_rate_boroqtr_20th, cap_rate_boroqtr_10th, isBot10b, isBot20b, isBot25b, isBot50b) %>%
  filter(!is.na(bbl), bbl != 0) %>%
  mutate(isBot25b = if_else(is.na(isBot25b),0,isBot25b)) %>%
  write_csv("./ProcessedData/MySWLboro.csv")

MySWLboroBBLs25 <- MySWLboro %>%
  filter(isBot25b == 1) %>%
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)

MySWLboroBBLs20 <- MySWLboro %>%
  filter(isBot20b == 1) %>%
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)

MySWLboroBBLs10 <- MySWLboro %>%
  filter(isBot10b == 1) %>%
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)
  
# Create MySWLnta from QTL22_sel 
MySWLnta <- QTL22_sel %>%
  group_by(nta, sale_qtr) %>%
  mutate(cap_rate_ntaqtr_mean = mean(cap_rate)) %>%
  mutate(cap_rate_ntaqtr_50th = quantile(cap_rate, probs = 0.50)) %>%
  mutate(cap_rate_ntaqtr_25th = quantile(cap_rate, probs = 0.25)) %>% 
  mutate(cap_rate_ntaqtr_20th = quantile(cap_rate, probs = 0.20)) %>%
  mutate(cap_rate_ntaqtr_10th = quantile(cap_rate, probs = 0.10)) %>%
  ungroup() %>%
  mutate(isSwlNTA = if_else(cap_rate < cap_rate_ntaqtr_mean,1,0)) %>%
  mutate(isBot10n = if_else(cap_rate < cap_rate_ntaqtr_10th,1,0)) %>%
  mutate(isBot20n = if_else(cap_rate < cap_rate_ntaqtr_20th,1,0)) %>%
  mutate(isBot25n = if_else(cap_rate < cap_rate_ntaqtr_25th,1,0)) %>%
  mutate(isBot50n = if_else(cap_rate <= cap_rate_ntaqtr_50th,1,0)) %>%
  filter(nta != "", !is.na(nta)) %>%
  select(bbl, boro, nta, cap_rate, cap_rate_ntaqtr_mean, cap_rate_ntaqtr_50th, cap_rate_ntaqtr_25th,cap_rate_ntaqtr_20th, cap_rate_ntaqtr_10th, isSwlNTA, isBot10n, isBot20n, isBot25n, isBot50n) %>%
  filter(!is.na(bbl), bbl != 0) %>%
  mutate(isBot50n = if_else(is.na(isBot50n),0,isBot50n)) %>%
  write_csv("./ProcessedData/MySWLnta.csv")

MySWLntaBBLs50 <- MySWLnta %>%
  filter(isBot50n == 1) %>% #changing to median of NTA
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)

MySWLntaBBLs25 <- MySWLnta %>%
  filter(isBot25n == 1) %>% #changing to median of NTA
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)

MySWLntaBBLs20 <- MySWLnta %>%
  filter(isBot20n == 1) %>% #changing to median of NTA
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)

MySWLntaBBLs10 <- MySWLnta %>%
  filter(isBot10n == 1) %>% #changing to median of NTA
  filter(bbl != 0, !is.na(bbl)) %>%
  distinct() %>%
  pull(bbl)

### Clean & save final tidy dataframes

# Create blank df for all years and bbls 

nbbls <- length(QTL22BBLs)
nyears <- length(years_bounded)

qtl_bbls_long <- rep(QTL22BBLs, each = nyears)
bound_years_long <- rep(years_bounded, times = nbbls)

df_fin <- bind_cols(qtl_bbls_long, bound_years_long)
colnames(df_fin) <- c("bbl","year")

df_fin_finYEARS <- df_fin %>%
  mutate(isElig = if_else(bbl %in% Elig22BBLs,1,0)) %>%
  mutate(isQTL = if_else(bbl %in% QTL22BBLs,1,0)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0)) %>%
  mutate(isBot50nta = if_else(bbl %in% MySWLntaBBLs50,1,0)) %>%
  mutate(isBot25boro = if_else(bbl %in% MySWLboroBBLs25,1,0)) %>%
  mutate(isBot20boro = if_else(bbl %in% MySWLboroBBLs20,1,0)) %>%
  mutate(isBot10boro = if_else(bbl %in% MySWLboroBBLs10,1,0)) %>%
  mutate(isBot25nta = if_else(bbl %in% MySWLntaBBLs25,1,0)) %>%
  mutate(isBot20nta = if_else(bbl %in% MySWLntaBBLs20,1,0)) %>%
  mutate(isBot10nta =if_else(bbl %in% MySWLntaBBLs10,1,0)) 
  

# Clean & Join RSU Data
RSU_SWL <- RSU %>%
  filter(!is.na(bbl), bbl != 0) %>%
  select(bbl, x2016 = rsuc2016,x2017= rsuc2017,x2018= rsuc2018,x2019= rsuc2019,x2020= rsuc2020,x2021= rsuc2021,x2022= rsuc2022) %>%
  pivot_longer(!bbl, names_to = "year", values_to = "rsu") %>%
  mutate(year = str_remove(year, "x")) %>%
  mutate(isElig = if_else(bbl %in% Elig22BBLs,1,0)) %>%
  mutate(isQTL = if_else(bbl %in% QTL22BBLs,1,0)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0)) %>%
  filter(!(isElig == 1 & isQTL == 0 & isSWL == 1)) %>%
  filter(isElig == 1) %>%
  filter(isQTL == 1) %>%
  #mutate(isBot25nta = if_else(bbl %in% MySWLntaBBLs,1,0)) %>%
  #mutate(isBot25boro = if_else(bbl %in% MySWLboroBBLs,1,0)) %>%
  #mutate(isBot25boro = if_else(is.na(isBot25boro),0,isBot25boro),
  #       isBot25nta = if_else(is.na(isBot25nta),0,isBot25nta)) %>%
  select(bbl, year, rsu) %>%
  mutate(year = as.numeric(year))
  
# Clean  HPD Data All 
HPDv_all_SWL <- HMCV %>% #base of bbls is not QTL here
  select(bbl, inspection_date, class) %>%
  mutate(inspection_date = as.Date(parse_date_time(inspection_date, "mdy"))) %>%
  filter(inspection_date >= min(dates_bounded), inspection_date < max(dates_bounded)) %>%
  mutate(v_year = year(inspection_date)) %>%
  filter(bbl %in% Elig22BBLs) %>%
  mutate(isElig = 1) %>%
  mutate(isQTL = if_else(bbl %in% QTL22BBLs,1,0)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0)) %>%
  filter(isQTL == 1) %>%
  count(bbl, v_year, isSWL, sort = TRUE) %>%
  rename(HPDv_all = n) %>%
  select(bbl, year = v_year, HPDv_all) %>%
  mutate(year = as.numeric(year))

# Clean HPD Data Class C
HPDv_C_SWL <- HMCV %>%
  select(bbl, inspection_date, class) %>%
  mutate(inspection_date = as.Date(parse_date_time(inspection_date, "mdy"))) %>%
  filter(inspection_date >= min(dates_bounded), inspection_date < max(dates_bounded)) %>%
  mutate(v_year = year(inspection_date)) %>%
  filter(bbl %in% Elig22BBLs) %>%
  mutate(isElig = 1) %>%
  mutate(isQTL = if_else(bbl %in% QTL22BBLs,1,0)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0)) %>%
  filter(class == "C") %>%
  filter(isQTL == 1) %>%
  count(bbl, v_year, isSWL, sort = TRUE) %>%
  rename(HPDv_C = n) %>%
  select(bbl, year = v_year, HPDv_C) %>%
  mutate(year = as.numeric(year))

# Clean Evictions Data
MYevicts_cln <- MYevicts %>%
  clean_names() %>%
  select(bbl, boro = borough, nta, bin, executeddate) %>%
  filter(executeddate >= min(dates_bounded), executeddate < max(dates_bounded)) %>%
  mutate(year = data.table::year(as.Date(executeddate))) %>%
  filter(bbl %in% Elig22BBLs) %>%
  mutate(isQTL = if_else(bbl %in% QTL22BBLs,1,0)) %>%
  mutate(isSWL = if_else(bbl %in% SWL22BBLs,1,0)) %>%
  filter(isQTL == 1) %>%
  count(bbl, year, isSWL, sort = TRUE)  %>%
  rename(evicts = n) %>%
  select(bbl, year, evicts) %>%
  mutate(year = as.numeric(year))

# Clean Housing Court Data
MYhc_cln <- MYhc %>%  # Only 3 in QTL....
  clean_names() %>%
  mutate(finding_date = str_sub(finding_date,1,10)) %>%
  mutate(finding_date = str_replace_all(finding_date,"/","-")) %>%
  filter(nchar(finding_date) > 1) %>%
  mutate(finding_date = parse_date_time(finding_date, orders = "mdy")) %>%
  filter(finding_date >= min(dates_bounded), finding_date < max(dates_bounded)) %>%
  mutate(year = data.table::year(as.Date(finding_date))) %>%
  filter(finding_of_harassment != "No Harassment", finding_of_harassment != "") %>%
  mutate(th_found = 1) %>%
  select(bbl, year, th_found) %>%
  distinct() %>%
  filter(bbl %in% QTL22BBLs) %>%
  mutate(year = as.numeric(year))

# Clean Demo Data
Demo_cln <- Demo %>%
  select(bbl, year = permit_year, demo_filed, demos = count_dm_permits) %>%
  mutate(year = as.numeric(year)) %>%
  filter(bbl %in% QTL22BBLs)

# Clean Alt1 Data
Alt1_cln <- Alt1 %>%
  select(bbl, year = permit_year, alt1_filed, alt1s = count_dm_permits) %>%
  mutate(year = as.numeric(year))  %>%
  filter(bbl %in% QTL22BBLs)
  
# QTL transaction dates
QTL22_sel_sel <- QTL22_sel %>%
  select(bbl, boro, nta, sale_date, sale_qtr, sale_year) %>%
  mutate(sale_year = as.numeric(sale_year)) %>%
  distinct()
  
# JOIN EVERYTHING TOGETHER & SAVE!
df_fin_fin_fin <- df_fin_finYEARS %>%
  left_join(QTL22_sel_sel, by = c("bbl"), relationship = "many-to-many") %>%
  left_join(RSU_SWL, by = c("bbl","year")) %>%
  left_join(HPDv_all_SWL, by = c("bbl","year")) %>%
  left_join(HPDv_C_SWL, by = c("bbl","year")) %>%
  left_join(MYevicts_cln, by = c("bbl","year")) %>%
  left_join(MYhc_cln, by = c("bbl","year")) %>%
  left_join(Demo_cln, by = c("bbl","year")) %>%
  left_join(Alt1_cln, by = c("bbl","year")) %>%
  write_csv("./ProcessedData/infdfSWL.csv")

# Create blank Df of eligible bbls 

nbblsElig <- length(Elig22BBLs)
nyears <- length(years_bounded)

elig_bbls_long <- rep(Elig22BBLs, each = nyears)
bound_years_long <- rep(years_bounded, times = nbblsElig)

df_finElig <- bind_cols(elig_bbls_long, bound_years_long)
colnames(df_finElig) <- c("bbl","year")

df_finElig_fin <- df_finElig %>%
  left_join(CS_cln, by = c("bbl"),  relationship = "many-to-many") %>%
  filter(!is.na(pair_id)) %>%
  left_join(RSU_SWL, by = c("bbl","year")) %>%
  left_join(HPDv_all_SWL, by = c("bbl","year")) %>%
  left_join(HPDv_C_SWL, by = c("bbl","year")) %>%
  left_join(MYevicts_cln, by = c("bbl","year")) %>%
  left_join(MYhc_cln, by = c("bbl","year")) %>%
  left_join(Demo_cln, by = c("bbl","year")) %>%
  left_join(Alt1_cln, by = c("bbl","year")) %>%
  write_csv("./ProcessedData/infdfCS.csv")

