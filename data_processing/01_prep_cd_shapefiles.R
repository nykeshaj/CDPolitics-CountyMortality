#####
# 0 # setup
#####

## load libraries
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(s2)
library(USAboundaries)
library(sp)
# library(rgeos)
library(raster)
library(dplyr)
library(data.table)
library(spdep)
library(tibble)
library(tidyr)
library(stringr)
library(magrittr)

library(readxl)
library(here)

# set working directory
analysis_dir <- here::here("data_processing")


#####
# 1 # load congressional district boundaries from tigris
#####

# load FIPS codes
data("fips_codes")

state_codes <- fips_codes %>%
  distinct(state, .keep_all = TRUE) %>%
  dplyr::select(-c(county,county_code))

# load congressional district boundaries from tigris
options(tigris_use_cache = TRUE)

cd113 <- tigris::congressional_districts(year = 2013) #113th congress TIGER/Line Shapefile
# remove undefined & non-contiguous delegate districts
cd113 <- cd113 %>% filter(!GEOID %in% c("09ZZ","17ZZ","26ZZ","6098","6698","6998","7898"))

cd115 <- tigris::congressional_districts(year = 2016) #115th congress TIGER/Line Shapefile
# remove undefined & non-contiguous delegate districts
cd115 <- cd115 %>% filter(!GEOID %in% c("09ZZ","17ZZ","26ZZ","6098","6698","6998","7898"))

cd118 <- tigris::congressional_districts(year = 2022) #118th congress
cd118 <- cd118 %>%
  # remove undefined & non-contiguous delegate districts
  filter(!GEOID20 %in% c("09ZZ","17ZZ","26ZZ","6098","6698","6998","7898")) %>%
  mutate(GEOID20 = ifelse(GEOID20 == "3001","3000",GEOID20))


#####
# 2 # pull CD-level census data
#####

# Pull CD-level covariates of interest (poverty & racial composition)
# Note: The 2010 & 2020 decennial census do not have income data so we will use
# ACS 5-year estimates for poverty

# function to get cd-level poverty
get_cd_poverty <- function(yr) {

  ## grab the poverty data from ACS (this code is modified from Christian, hence the tidy syntax)
  variables_dict <-
    tibble::tribble(
      ~var,          ~shortname,      ~desc,
      "B23024_001",  'poverty_denominator', "20-64 year old total population for poverty estimates",
      "B23024_002",  'poverty_count',    "20-64 year old estimate of income below poverty level",
    )

  acs_data <- get_acs(
    geography = 'congressional district',
    #geometry = TRUE,
    year = yr,
    variables = variables_dict$var)

  # pivot to a wide format for renaming, dropping the margin of error data
  acs_data <- acs_data %>%
    dplyr::select(-moe) %>%
    pivot_wider(names_from = variable, values_from = estimate)

  rename_vars <- setNames(variables_dict$var, variables_dict$shortname)
  acs_data <- acs_data %>% rename(!!rename_vars)

  # calculate percent poverty
  acs_data <- acs_data %>%
    mutate(poverty_pct = 100*poverty_count  / poverty_denominator) %>%
    dplyr::select(GEOID,poverty_count,poverty_denominator,poverty_pct)

}

cd13_pov <- get_cd_poverty(2013)
cd16_pov <- get_cd_poverty(2016)
cd22_pov <- get_cd_poverty(2022)

# racial composition from the 2010 & 2020 census respectively

# list 2010 decennial census racial composition variables
race_vars_2010 <- c(
  pop_total                      = "P003001",
  race_white_alone               = "P003002",
  race_black_alone               = "P003003",
  race_am_indian_ak_native_alone = "P003004",
  race_asian_alone               = "P003005",
  race_native_hawaiian_opi_alone = "P003006",
  hispanic_denom                 = "P004001",
  hispanic_count                 = "P004003"
)

# list 2020 decennial census racial composition variables
race_vars_2020 <- c(
  pop_total                      = "P1_001N",
  race_white_alone               = "P1_003N",
  race_black_alone               = "P1_004N",
  race_am_indian_ak_native_alone = "P1_005N",
  race_asian_alone               = "P1_006N",
  race_native_hawaiian_opi_alone = "P1_007N",
  hispanic_denom                 = "P2_001N",
  hispanic_count                 = "P2_002N"
)

get_cd_racial_comp <- function(race_vars,yr,sumfile_nm){

  cd_race <- get_decennial(
    geography = "congressional district",
    variables = race_vars,
    year = yr,
    sumfile = sumfile_nm) %>%
    dplyr::select(GEOID, NAME, variable, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    mutate(
      pct_white      = 100*race_white_alone / pop_total,
      pct_black      = 100*race_black_alone / pop_total,
      pct_aian       = 100*race_am_indian_ak_native_alone / pop_total,
      pct_asian      = 100*race_asian_alone / pop_total,
      pct_nhpi       = 100*race_native_hawaiian_opi_alone / pop_total,
      pct_hispanic   = 100*hispanic_count / hispanic_denom,
    )
}

cd_2010_race <- get_cd_racial_comp(
  race_vars = race_vars_2010,
  yr = 2010,
  sumfile_nm = "sf1"
)

cd_2020_race <- get_cd_racial_comp(
  race_vars = race_vars_2020,
  yr = 2020,
  sumfile_nm = "pl"
)

#####
# 3 # load political metrics data
#####

# load DW-Dominate
# Source: https://voteview.com/data
dw <- read.csv("data/dw_nominate/HSall_members.csv")
dw_12_24 <- dw %>%
  left_join(state_codes, join_by(state_abbrev == state)) %>%
  filter(
    # remove non-contiguous states & territories
    !state_abbrev %in% c('AK','HI','DC','PR','GU','VI','AS','MP'),
    chamber  != 'President',
    congress %in% c(112,113,114,115,116,117,118)) %>%
  mutate(
    period = case_when(
      congress %in% c(112,113) ~ '2012-2015',
      congress %in% c(114,115) ~ '2016-2019',
      congress %in% c(116,117,118) ~ '2020-2024',
      # congress %in% c(118) ~ '2024',
      TRUE ~ NA_character_),
    # recode single district states from district_code == 00 to district_code == 01
    # Delaware, Wyoming, North Dakota, South Dakota, Vermont & Montana
    district_code = as.character(str_pad(district_code, width = "2", pad = "0")),
    district_code = ifelse(district_code == "01" & state_abbrev %in% c("DE","WY","ND","SD","VT","MT"),"00",district_code),
    GEOID = paste0(state_code,district_code),
    party = case_when(
      party_code == '100' ~ 'D',
      party_code == '200' ~ 'R')) %>%
  dplyr::select(period,GEOID,state_code,district_code,congress,state_abbrev,state_name,chamber,party,bioname,nominate_dim1)

# load state policy liberalism index
# source: https://dynamicdemocracy.shinyapps.io/ShinyApp/
liberalism_index <- read.csv('data/state_policy_ideology/state_policy_ideology_2026-05-06.csv')
liberalism_12_24 <- liberalism_index %>%
  filter(
    # remove non-contiguous states & territories
    !state_abb %in% c('AK','HI','DC','PR','GU','VI','AS','MP'),
    #filter to 2012-2024
    year %in% 2012:2024) %>%
  mutate(
    period = case_when(
      year %in% 2012:2015 ~ '2012-2015',
      year %in% 2016:2019 ~ '2016-2019',
      year %in% 2010:2024 ~ '2020-2024',
      # year == 2024) ~ '2024',
      TRUE ~ NA_character_)) %>%
  group_by(period,state_abb,state_name) %>%
  summarise(state_liberalism_index = round(mean(median ,na.rm = T),2))

state_lib_12_15 <- liberalism_12_24 %>%  filter(period == "2012-2015")
state_lib_16_19 <- liberalism_12_24 %>%  filter(period == "2016-2019")
state_lib_20_24 <- liberalism_12_24 %>%  filter(period == "2020-2024")


#####
# 4 # generate congressional district-level datasets for each period
#####

## function to get:
## -- mean DW Nominate for House of representatives
## -- mean DW Nominate for Senate
## -- mean state liberalism index
## -- join to most recent cd geography for specified time
make_cd_dataset_by_yr <- function(yrs,state_lib,cd_shapefile){

  # obtain mean DW-Nominate for House of Representative measures for each cd across time period
  house_dw <- dw_12_24 %>%
    filter(period == yrs) %>%
    filter(chamber == "House") %>%
    group_by(GEOID,period) %>%
    summarise(mean_house_dw = round(mean(nominate_dim1 ,na.rm = T),2)) %>%
    mutate(state_code = str_sub(GEOID, 1, 2))  %>%
    left_join(state_codes, by ="state_code")

  # obtain mean DW-Nominate for Senate measures for each state across time period & apply to each cd
  senate_dw <- dw_12_24 %>%
    filter(period == yrs) %>%
    filter(chamber == "Senate") %>%
    group_by(GEOID,period) %>%
    summarise(mean_senate_dw = round(mean(nominate_dim1 ,na.rm = T),2)) %>%
    mutate(state_code = str_sub(GEOID, 1, 2))  %>%
    left_join(state_codes, by="state_code")

  # join DW-Nominate for HOR & Senate w/ state liberalism index
  cd_metrics <- house_dw %>%
    left_join(senate_dw, by = c("period","state","state_name","state_code"))%>%
    # apply state liberalism index score to each cd
    left_join(state_lib, by = c("state" = "state_abb", "period","state_name")) %>%
    rename(GEOID=GEOID.x) %>%
    dplyr::select(period,GEOID,state,state_name,mean_house_dw,
                  mean_senate_dw,state_liberalism_index)


    # join cd_metrics to congressional district boundaries
    if("GEOID20" %in% names(cd_shapefile)){
    cd_metrics <- cd_metrics %>%
      left_join(cd_shapefile, by=c("GEOID" = "GEOID20"))
    } else {
    cd_metrics <- cd_metrics %>%
      left_join(cd_shapefile, by="GEOID")
    }

  cd_metrics <- cd_metrics %>%
    # remove non-contiguous states, territories & DC
    filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

  return(cd_metrics)
}

# 2012 - 2015 (112th & 113th congress) -----------------------------------------
# There was no related geometry found in the 113th congress for N = 12 districts.
# Illinois, Iowa, Louisiana, Massachusetts, Michigan, Missouri, New Jersey & Pennsylvania each
# lost 1 seat. New York & Ohio respectively lost 2 seats.
# These were removed for analysis.

# Since we decided to use the 2010 decennial estimates (111th congress) for the racial composition, there
# are n = 12 districts that do not have census data.These were also removed from analysis
cd12_15 <- make_cd_dataset_by_yr(
  yrs = "2012-2015",
  state_lib = state_lib_12_15,
  cd_shapefile = cd113
  )

# join census data
cd12_15 <- cd12_15 %>%
  left_join(cd13_pov, by = "GEOID") %>%
  left_join(cd_2010_race, by = "GEOID") %>%
  filter(! is.na(NAMELSAD)) %>% # remove districts w. missing 113th congress shapefile
  filter(! is.na(NAME)) %>% # remove districts w. missing census data
  dplyr::select(
    period,GEOID,state_abb=state,state_name,congress_113=NAMELSAD,mean_house_dw,
    mean_senate_dw,state_liberalism_index,poverty_count,poverty_denominator,poverty_pct,
    pop_total,race_white_alone,race_black_alone,race_am_indian_ak_native_alone,
    race_asian_alone,race_native_hawaiian_opi_alone,hispanic_denom,hispanic_count,
    pct_white,pct_black,pct_aian,pct_asian,pct_nhpi,pct_hispanic,geometry
  )

# 2016 - 2019 (114th & 115th congress) -----------------------------------------
cd16_19 <- make_cd_dataset_by_yr(
  yrs = "2016-2019",
  state_lib = state_lib_16_19,
  cd_shapefile = cd115
)

# join census data
cd16_19 <- cd16_19 %>%
  left_join(cd16_pov, by = "GEOID") %>%
  left_join(cd_2020_race, by = "GEOID") %>%
  dplyr::select(
    period,GEOID,state_abb=state,state_name,congress_115=NAMELSAD,NAME,mean_house_dw,
    mean_senate_dw,state_liberalism_index,poverty_count,poverty_denominator,poverty_pct,
    pop_total,race_white_alone,race_black_alone,race_am_indian_ak_native_alone,
    race_asian_alone,race_native_hawaiian_opi_alone,hispanic_denom,hispanic_count,
    pct_white,pct_black,pct_aian,pct_asian,pct_nhpi,pct_hispanic,geometry
  )


# 2020 - 2024 (116th, 117th & 118th congress) ----------------------------------
# There was no related geometry found in the 118th congress for N = 7 districts.
# California, Illinois, Michigan, New York & Ohio, Pennsylvania & West Virginia each
# lost 1 seat. These were removed for analysis.
cd20_24 <- make_cd_dataset_by_yr(
  yrs = "2020-2024",
  state_lib = state_lib_20_24,
  cd_shapefile = cd118
)

# join census data
cd20_24 <- cd20_24 %>%
  left_join(cd22_pov, by = "GEOID") %>%
  left_join(cd_2020_race, by = "GEOID") %>%
  filter(! is.na(NAMELSAD20)) %>% # remove districts w. missing 113th congress shapefile
  dplyr::select(
    period,GEOID,state_abb=state,state_name,congress_118=NAMELSAD20,mean_house_dw,
    mean_senate_dw,state_liberalism_index,poverty_count,poverty_denominator,poverty_pct,
    pop_total,race_white_alone,race_black_alone,race_am_indian_ak_native_alone,
    race_asian_alone,race_native_hawaiian_opi_alone,hispanic_denom,hispanic_count,
    pct_white,pct_black,pct_aian,pct_asian,pct_nhpi,pct_hispanic,geometry
  )



# save datasets --------------------------------------------------------------
saveRDS(cd12_15, here("data","analytic_datasets","filtered_to_period","cd12_15.rds"))
saveRDS(cd16_19, here("data","analytic_datasets","filtered_to_period","cd16_19.rds"))
saveRDS(cd20_24, here("data","analytic_datasets","filtered_to_period","cd20_24.rds"))

cd <- bind_rows(cd12_15,cd16_19,cd20_24)

write_xlsx(cd, here("data","analytic_datasets","cd_political_metrics_2012-2024.xlsx"))

