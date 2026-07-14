# load libraries
library(tigris)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(readxl)
library(here)

# load FIPS codes
data("fips_codes")

state_codes <- fips_codes %>% 
  distinct(state, .keep_all = TRUE) %>%  
  select(-c(county,county_code))

#####
# 1 # Congressional District metrics: Political ideology and state-level policies
#####

# load DW-Dominate
dw <- read.csv("data/HSall_members.csv")

dw_12_24 <- dw %>% 
  left_join(state_codes, join_by(state_abbrev == state)) %>% 
  filter(
    chamber  != 'President',
    congress %in% c(112,113,114,115,116,117,118)) %>% 
  mutate(period = case_when(
    congress %in% c(112,113) ~ '2012-2015',
    congress %in% c(114,115) ~ '2016-2019',
    congress %in% c(116,117) ~ '2020-2023',
    congress %in% c(118) ~ '2024',
    TRUE ~ NA_character_),
    district_code = as.character(str_pad(district_code, width = "2", pad = "0")),
    GEOID = paste0(state_code,district_code),
    party = case_when(
      party_code == '100' ~ 'D',
      party_code == '200' ~ 'R')) %>% 
  select(period,GEOID,state_code,district_code,congress,state_abbrev,chamber,party,bioname,nominate_dim1)

tab <- dw_12_24 %>% 
  group_by(period) %>% 
  summarise(
    mean_dw = round(mean(nominate_dim1 ,na.rm = T),2),
    median_dw = round(median(nominate_dim1 ,na.rm = T),2),
    sd_dw = round(sd(nominate_dim1 ,na.rm = T),2),
    min_dw = round(min(nominate_dim1 ,na.rm = T),2),
    max_dw = round(max(nominate_dim1 ,na.rm = T),2),
    iqr_dw = round(IQR(nominate_dim1 ,na.rm = T),2)) 

tab_house_only <- dw_12_24 %>% 
  filter(chamber == "House") %>% 
  group_by(period) %>% 
  summarise(
    mean_dw = round(mean(nominate_dim1 ,na.rm = T),2),
    median_dw = round(median(nominate_dim1 ,na.rm = T),2),
    sd_dw = round(sd(nominate_dim1 ,na.rm = T),2),
    min_dw = round(min(nominate_dim1 ,na.rm = T),2),
    max_dw = round(max(nominate_dim1 ,na.rm = T),2),
    iqr_dw = round(IQR(nominate_dim1 ,na.rm = T),2)) 

# load state liberalism



#####
# 2 # County-level covariates
##### 

# most counties are atoms, so pull the county-level dataset for the whole US ##
dc2020_county<- get_decennial(geography = "county",
                              variables = variables_dict$var,year = 2020,sumfile = "pl")

# pivot to a wide format for renaming
dc2020_county <- dc2020_county %>%
  pivot_wider(names_from = variable, values_from = value)

rename_vars <- setNames(variables_dict$var, variables_dict$shortname)
dc2020_county <- dc2020_county %>% rename(!!rename_vars)

# process racial composition data
dc2020_county <- dc2020_county %>%
  mutate(
    pct_white = 100*race_white_alone / pop_total,
    pct_black = 100*race_black_alone / pop_total,
    pct_aian = 100*race_am_indian_ak_native_alone / pop_total,
    pct_asian = 100*race_asian_alone / pop_total,
    pct_hispanic = 100*hispanic_count / hispanic_denom) %>%
  dplyr::select(geoid=GEOID,pop_total,pct_white,pct_black,pct_aian,pct_asian,pct_hispanic)

dc2020_county<-data.frame(dc2020_county)

racial_comp_summary_tab <- dc2020_county %>% 
  summarise(
    # mean_pct_white = round(mean(pct_white ,na.rm = T),2),
    # median_pct_white = round(median(pct_white ,na.rm = T),2),
    # # sd_pct_white = round(sd(nominate_dim1 ,na.rm = T),2),
    # min_pct_white= round(min(pct_white ,na.rm = T),2),
    # max_pct_white = round(max(pct_white ,na.rm = T),2),
    # iqr_pct_white = round(IQR(pct_white ,na.rm = T),2),
    
    mean_pct_black = round(mean(pct_black ,na.rm = T),2),
    median_pct_black = round(median(pct_black ,na.rm = T),2),
    sd_pct_black = round(sd(pct_black ,na.rm = T),2),
    min_pct_black= round(min(pct_black ,na.rm = T),2),
    max_pct_black = round(max(pct_black ,na.rm = T),2),
    iqr_pct_black = round(IQR(pct_black ,na.rm = T),2),
    
    mean_pct_aian = round(mean(pct_aian ,na.rm = T),2),
    median_pct_aian = round(median(pct_aian ,na.rm = T),2),
    sd_pct_aian = round(sd(pct_aian ,na.rm = T),2),
    min_pct_aian = round(min(pct_aian ,na.rm = T),2),
    max_pct_aian = round(max(pct_aian ,na.rm = T),2),
    iqr_pct_aian = round(IQR(pct_aian ,na.rm = T),2),
    
    mean_pct_asian = round(mean(pct_asian ,na.rm = T),2),
    median_pct_asian = round(median(pct_asian ,na.rm = T),2),
    sd_pct_asian = round(sd(pct_asian ,na.rm = T),2),
    min_pct_asian = round(min(pct_asian ,na.rm = T),2),
    max_pct_asian = round(max(pct_asian ,na.rm = T),2),
    iqr_pct_asian = round(IQR(pct_asian ,na.rm = T),2),
    
    mean_pct_hispanic = round(mean(pct_hispanic ,na.rm = T),2),
    median_pct_hispanic= round(median(pct_hispanic ,na.rm = T),2),
    sd_pct_hispanic = round(sd(pct_hispanic ,na.rm = T),2),
    min_pct_hispanic = round(min(pct_hispanic ,na.rm = T),2),
    max_pct_hispanic = round(max(pct_hispanic ,na.rm = T),2),
    iqr_pct_hispanic = round(IQR(pct_hispanic ,na.rm = T),2)
    ) 

#####
# 3 # County-level mortality
##### 

