# load libraries
library(tigris)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(readxl)
library(here)

# set working directory
analysis_dir <- here::here("2026_05_19_var_distributions")

# load FIPS codes
data("fips_codes")

state_codes <- fips_codes %>% 
  distinct(state, .keep_all = TRUE) %>%  
  select(-c(county,county_code))


# load DW-Dominate
dw <- read.csv("data/HSall_members.csv")

dw_12_24 <- dw %>% 
  left_join(state_codes, join_by(state_abbrev == state)) %>% 
  filter(
    # remove non-contiguous states & territories 
    !state_abbrev %in% c('AK','HI','DC','PR','GU','VI','AS','MP'), 
    chamber  != 'President',
    congress %in% c(112,113,114,115,116,117,118)) %>% 
  mutate(period = case_when(
    congress %in% c(112,113) ~ '2012-2015',
    congress %in% c(114,115) ~ '2016-2019',
    congress %in% c(116,117,118) ~ '2020-2024',
    # congress %in% c(118) ~ '2024',
    TRUE ~ NA_character_),
    district_code = as.character(str_pad(district_code, width = "2", pad = "0")),
    GEOID = paste0(state_code,district_code),
    party = case_when(
      party_code == '100' ~ 'D',
      party_code == '200' ~ 'R')) %>% 
  select(period,GEOID,state_code,district_code,congress,state_abbrev,state_name,chamber,party,bioname,nominate_dim1)



# load state policy liberalism index
liberalism_index <- read.csv('data/state_policy_ideology_2026-05-06.csv')

liberalism_12_24 <- liberalism_index %>% 
  filter(
    # remove non-contiguous states & territories 
    !state_abb %in% c('AK','HI','DC','PR','GU','VI','AS','MP'), 
    #filter to 2012-2024
    year %in% 2012:2024) %>% 
  mutate(period = case_when(
    year %in% 2012:2015 ~ '2012-2015',
    year %in% 2016:2019 ~ '2016-2019',
    year %in% 2010:2024 ~ '2020-2024',
    # year == 2024) ~ '2024',
    TRUE ~ NA_character_)) %>% 
  group_by(period,state_abb,state_name) %>% 
  summarise(
    state_liberalism_index = round(mean(median ,na.rm = T),2)) 


# create cd-level dataset for all political metrics
# cd_12_24 <- dw_12_24 %>% 
#   left_join(liberalism_12_24, by = c('period'='period','state_abbrev'= 'state_abb'),
#             relationship = 'many-to-many') %>%  # apply state-level policy measure to all congressional districts
#   select(period,congress,state_name.x,state_abbrev,GEOID,state_code,district_code,
#          chamber,party,nominate_dim1,state_liberalism_index) %>% 
#   janitor::clean_names() %>% 
#   rename(state_name = state_name_x)

# aggregate measures across period
house_dw_12_24 <- dw_12_24 %>% 
  filter(chamber == "House") %>% 
  group_by(period) %>% 
  summarise(
    mean_house_dw = round(mean(nominate_dim1 ,na.rm = T),2),
    median_house_dw = round(median(nominate_dim1 ,na.rm = T),2),
    sd_house_dw = round(sd(nominate_dim1 ,na.rm = T),2),
    min_house_dw = round(min(nominate_dim1 ,na.rm = T),2),
    max_house_dw = round(max(nominate_dim1 ,na.rm = T),2),
    iqr_house_dw = round(IQR(nominate_dim1 ,na.rm = T),2)) %>% 
  mutate(mean_sd = paste0(mean_house_dw, " (",sd_house_dw,")"),
         median = median_house_dw,
         min_max_iqr = paste0(min_house_dw, "\n",max_house_dw,"\n",iqr_house_dw)) %>% 
  select(period,mean_sd,median,min_max_iqr) %>% 
  pivot_wider(names_from = period, values_from = c(mean_sd,median,min_max_iqr)) %>% 
  mutate(var = "DW-Nominate: House Only")

senate_dw_12_24 <- dw_12_24 %>% 
  filter(chamber == "Senate") %>% 
  group_by(period) %>% 
  summarise(
    mean_senate_dw = round(mean(nominate_dim1 ,na.rm = T),2),
    median_senate_dw = round(median(nominate_dim1 ,na.rm = T),2),
    sd_senate_dw = round(sd(nominate_dim1 ,na.rm = T),2),
    min_senate_dw = round(min(nominate_dim1 ,na.rm = T),2),
    max_senate_dw = round(max(nominate_dim1 ,na.rm = T),2),
    iqr_senate_dw = round(IQR(nominate_dim1 ,na.rm = T),2)) %>% 
  mutate(mean_sd = paste0(mean_senate_dw, " (",sd_senate_dw,")"),
         median = median_senate_dw,
         min_max_iqr = paste0(min_senate_dw, "\n",max_senate_dw,"\n",iqr_senate_dw)) %>% 
  select(period,mean_sd,median,min_max_iqr) %>% 
  pivot_wider(names_from = period, values_from = c(mean_sd,median,min_max_iqr)) %>% 
  mutate(var = "DW-Nominate: Senate Only")

liberalism_12_24 <- liberalism_index %>% 
  filter(
    # remove non-contiguous states & territories 
    !state_abb %in% c('AK','HI','DC','PR','GU','VI','AS','MP'), 
    #filter to 2012-2024
    year %in% 2012:2024) %>% 
  mutate(period = case_when(
    year %in% 2012:2015 ~ '2012-2015',
    year %in% 2016:2019 ~ '2016-2019',
    year %in% 2010:2024 ~ '2020-2024',
    # year == 2024) ~ '2024',
    TRUE ~ NA_character_)) %>% 
  group_by(period) %>% 
  summarise(
    mean_state_liberalism_index = round(mean(median ,na.rm = T),2),
    median_state_liberalism_index = round(median(median ,na.rm = T),2),
    sd_state_liberalism_index = round(sd(median ,na.rm = T),2),
    min_state_liberalism_index = round(min(median ,na.rm = T),2),
    max_state_liberalism_index = round(max(median ,na.rm = T),2),
   iqr_state_liberalism_index = round(IQR(median ,na.rm = T),2)) %>% 
  mutate(mean_sd = paste0(mean_state_liberalism_index, " (",sd_state_liberalism_index,")"),
         median = median_state_liberalism_index,
         min_max_iqr = paste0(min_state_liberalism_index, "\n",max_state_liberalism_index,"\n",iqr_state_liberalism_index)) %>% 
  select(period,mean_sd,median,min_max_iqr) %>% 
  pivot_wider(names_from = period, values_from = c(mean_sd,median,min_max_iqr)) %>% 
  mutate(var = "State Policy Liberalism Index")


tab1 <- bind_rows(house_dw_12_24,senate_dw_12_24,liberalism_12_24) %>% 
  select(var,
         `mean_sd_2012-2015`,`median_2012-2015`,`min_max_iqr_2012-2015`,
         `mean_sd_2016-2019`,`median_2016-2019`,`min_max_iqr_2016-2019`,    
         `mean_sd_2020-2024`,`median_2020-2024`, `min_max_iqr_2020-2024`                      
         )

write.csv(tab1,paste0(analysis_dir,"/cd_metrics_by_period.csv"))

# count congrssional districts
cd_num <- dw_12_24 %>% 
  group_by(period) %>% 
  distinct(GEOID, .keep_all = T) %>% 
  summarise(n = n())

# load congressional district boundaries from tigris
# https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
# https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2024.html#list-tab-1883739534
cd12 <- congressional_districts(year = 2012) #112
cd13 <- congressional_districts(year = 2013) #113
cd14 <- congressional_districts(year = 2014) #114
cd15 <- congressional_districts(year = 2015) #114
cd16 <- congressional_districts(year = 2016) #115
cd17 <- congressional_districts(year = 2017) #115
cd18 <- congressional_districts(year = 2018) #116
cd19 <- congressional_districts(year = 2019) #116
cd20 <- congressional_districts(year = 2020) #116
cd21 <- congressional_districts(year = 2021) #116
cd22 <- congressional_districts(year = 2022) #118
cd23 <- congressional_districts(year = 2023) #118
cd24 <- congressional_districts(year = 2024) #119

cd12