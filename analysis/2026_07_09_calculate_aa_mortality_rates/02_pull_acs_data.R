# get county-level population estimates, racialized composition & poverty from ACS 5-year estimates

# load libraries 
library(magrittr)
library(tidycensus)
library(stringr)
library(here)
library(writexl)

# function to get ABSMs at county level 
get_absms_by_year <- function(yr) {
  
  # load FIPS codes
  data("fips_codes")
  
  state_codes <- fips_codes %>% 
    distinct(state, .keep_all = TRUE) %>%  
    select(-c(county,county_code))
  
  # Create a data dictionary for ABSMS. The first column indicates the total
  # variable code, the second the variable name, and the third the description.
  absms_dictionary <- tibble::tribble(
    ~var, ~varname, ~description,
    # total population
    "B01001_001",  "total_popsize", "total population estimate",
    
    # racial composition
    'B01003_001',  "race_ethnicity_total", "race_ethnicity_total",
    
    # poverty
    "B05010_002",  'in_poverty',    "population with household income < poverty line",
    "B05010_001",  'total_pop_for_poverty_estimates',  "total population for poverty estimates",
    
    # crowded housing
    "B25014_005",  'owner_occupied_crowding1', 'owner occupied, 1 to 1.5 per room',
    "B25014_006",  'owner_occupied_crowding2', 'owner occupied, 1.51 to 2 per room',
    "B25014_007",  'owner_occupied_crowding3', 'owner occupied, 2.01 or more per room',
    "B25014_011",  'renter_occupied_crowding1', 'owner occupied, 1 to 1.5 per room',
    "B25014_012",  'renter_occupied_crowding2', 'owner occupied, 1.51 to 2 per room',
    "B25014_013",  'renter_occupied_crowding3', 'owner occupied, 2.01 or more per room',
    "B25014_001",  'crowding_total',            'total for crowding (occupants per room)',
    
    "B01001I_001",  'total_hispanic',           'total hispanic population estimate',
    "B01001B_001",  'total_black',              'total black, hispanic or non-hispanic estimate',
    "B01001H_001",  'total_white_nh',           'total white, non-hispanic population estimate',
    "B01001D_001",  'total_asian',               'total_asian population estimate',
    "B01001C_001",  'total_aian',               'total_aian population estimate'
  )
  
  absms <- tidycensus::get_acs(
    geography = 'county',
    year = yr,
    # state = state,
    variables = absms_dictionary$var, #Get the variables indicated in the data dictionary.
    geometry = FALSE # We already have the geometry so we don't need that.
  )
  
  # pivot wider so that each row corresponds to a tract
  absms %<>% dplyr::select(-moe) %>%
    tidyr::pivot_wider(names_from = variable, values_from = estimate)
  # Change the new column names to reflect variables names from the dictionary
  rename_vars <- setNames(absms_dictionary$var, absms_dictionary$varname)
  absms <- absms %>% rename(!!rename_vars)
  
  absms %<>%
    mutate(
      pct_poverty = 100*in_poverty / total_pop_for_poverty_estimates,
      pct_black = 100*total_black / total_popsize,
      pct_hispanic = 100*total_hispanic / total_popsize,
      pct_white_nh = 100*total_white_nh / total_popsize,
      pct_asian = 100*total_asian/ total_popsize,
      pct_aian = 100*total_aian/ total_popsize,
      year = yr
    ) %>%
    rename(geoid=GEOID) %>% 
    dplyr::select(year,geoid,pct_poverty,pct_black,pct_hispanic,pct_white_nh,pct_asian,pct_aian)
  
  return(absms)
}

# function to get county-level population estimates
get_acs_pop_by_year <- function(yr){
  # specify variables
  age_variables <- paste0("B01001_0", stringr::str_pad(1:49, 2, side='left', pad=0))
  
  # pull population data
  popsizes <- tidycensus::get_acs(
    geography = 'county',
    year = yr,
    state = state.abb,
    variables = age_variables
  )
  
  # separate out total population estimate
  total_population <- popsizes %>% filter(variable == 'B01001_001')
  total_population %<>% rename(total_population = estimate)
  total_population %<>% select(-c(variable, moe))
  
  # clean variables
  variables <- load_variables(yr, "acs5", cache = TRUE)
  
  # get specific variable definitions
  variables %<>% filter(name %in% age_variables)
  
  # parse estimate labels
  variables %<>% tidyr::separate(label, into = c('estimate_label', 'total_label', 'sex_gender', 'age_group'), sep = '!!')
  variables %<>% select(-estimate_label, -total_label, -concept)
  variables$sex_gender %<>% stringr::str_remove_all(":")
  
  # join in the variable labels
  popsizes %<>% left_join(variables, by = c('variable' = 'name'))
  
  # aggregate sex/gender
  popsizes %<>% group_by(GEOID, NAME, age_group) %>%
    summarize(
      estimate = sum(estimate),
      moe = tidycensus::moe_sum(estimate, moe))
  
  # assign age bands
  popsizes %<>% mutate(
    age_group = case_when(
      age_group == 'Under 5 years' ~ 'Under 5',
      age_group %in% c('5 to 9 years', '10 to 14 years') ~ '5-24',
      age_group %in% c('15 to 17 years', '18 and 19 years', '20 years', '21 years', '22 to 24 years') ~ '5-24',
      age_group %in% c('25 to 29 years', '30 to 34 years') ~ '25-44',
      age_group %in% c('35 to 39 years', '40 to 44 years') ~ '25-44',
      age_group %in% c('45 to 49 years', '50 to 54 years') ~ '45-64',
      age_group %in% c('55 to 59 years', '60 and 61 years', '62 to 64 years') ~ '45-64',
      age_group %in% c('65 and 66 years', '67 to 69 years', '70 to 74 years') ~ '65-74',
      age_group %in% c('75 to 79 years', '80 to 84 years') ~ '75+',
      age_group == '85 years and over' ~ '75+'
    ))
  
  # sum up age groups within 10-year age bands
  popsizes %<>% group_by(GEOID, age_group) %>%
    summarize(estimate = sum(estimate))
  
  # make the age groups a factor with ordered levels
  popsizes$age_group %<>% factor(levels = c('Under 5','5-24', '25-44', '45-64', '65-74', '75+'))
  
  ## remove NAs (these are total population*2)
  popsizes<-subset(popsizes,!is.na(age_group))
  
  ## put in wide format by age group
  popsizes<-popsizes%>%
    pivot_wider(names_from = age_group, values_from = estimate)%>%
    rename(geoid=GEOID,age_under5=`Under 5`,age5_24=`5-24`,age25_44=`25-44`,age45_64=`45-64`,age65_74=`65-74`,age75plus=`75+`)
  
  return(popsizes)
}

# Make county-level datasets for 2015, 2019 & 2024 ----------------------------
# get county-level data for 2015 
covars_15 <- get_county_absms_by_year(2015)  # 2011-2015 5-year ACS
pop_15 <- get_acs_pop_data(2015)
boundaries_15 <- tigris::counties(year = 2015) # load county boundaries

county_15 <- covars_15 %>% 
  full_join(pop_2015, by = "geoid") %>%
  full_join(boundaries_15, by=c("geoid"="GEOID")) %>% 
    left_join(state_codes, by = c("STATEFP" = "state_code")) %>% 
  # remove non-contiguous states & territories
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))


# get county-level data for 2019 
covars_19 <- get_county_absms_by_year(2019)  # 2015-2019 5-year ACS
pop_19 <- get_acs_pop_data(2019)
boundaries_19 <- tigris::counties(year = 2019) # load county boundaries

county_19 <- covars_19 %>% 
  full_join(pop_19, by = "geoid") %>%
  full_join(boundaries_19, by=c("geoid"="GEOID")) %>% 
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>% 
  # remove non-contiguous states & territories
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))


# get county-level data for 2021 
# NOTE: In 2022 CT switched from counties to county-equivalent planning districts, 
# but the mortality data was collected at the county level and there is no crosswalk 
# for the county/county-equivalent that I'm aware of. Since we need the shapefile, 
# I think it makes sense to use the most recent time period where the counties exist

# covars_24 <- get_county_absms_by_year(2024)  # 2020-2024 5-year ACS
# pop_24 <- get_acs_pop_data(2024)
# county_24 <- covars_24 %>% 
#   full_join(pop_24, by = "geoid") %>%
#   full_join(boundaries_24, by=c("geoid"="GEOID")) %>% 
#   left_join(state_codes, by = c("STATEFP" = "state_code")) %>% 
#   # remove non-contiguous states & territories
#   filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))
covars_21 <- get_county_absms_by_year(2021)  # 2017-2021 5-year ACS
pop_21 <- get_acs_pop_data(2021)
boundaries_21 <- tigris::counties(year = 2021) # load county boundaries
county_21 <- covars_21 %>%
  full_join(pop_21, by = "geoid") %>%
  full_join(boundaries_21, by=c("geoid"="GEOID")) %>%
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))


# save data 
saveRDS(county_15,here("data","analytic_datasets","filtered_to_period","county_data_15.rds"))
saveRDS(county_19,here("data","analytic_datasets","filtered_to_period","county_data_19.rds"))
# saveRDS(county_24,here("data","analytic_datasets","filtered_to_period","county_data_24.rds"))
saveRDS(county_21,here("data","analytic_datasets","filtered_to_period","county_data_21.rds"))


cnty <- bind_rows(county_15,county_19,county_21) #county_24
# write_xlsx(cnty, here("data","analytic_datasets","county_acs_estimates_2012-2024.xlsx"))
write_xlsx(cnty, here("data","analytic_datasets","county_acs_estimates_2012-2021.xlsx"))
