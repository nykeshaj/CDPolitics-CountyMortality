# get county-level race & poverty covariates

# load libraries 
library(magrittr)
library(tidycensus)
library(stringr)

# Function to get ABSMs at county level ---------------------------------------
get_county_absms_by_year <- function(year) {
  
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
    year = year,
    geography = 'county',
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
      pct_aian = 100*total_aian/ total_popsize
      
    ) %>%
    dplyr::select(GEOID,pct_poverty,pct_black,pct_hispanic,pct_white_nh,pct_asian,pct_aian)
  
  return(absms)
}

# execute function for 5-year county-level ACS estimates
cnty_covars_15 <- get_county_absms_by_year(2015)  # 2011-2015 5 year ACS
cnty_covars_15 %<>% mutate(year = '2015')

cnty_covars_19 <- get_county_absms_by_year(2019)  # 2015-2019 5 year ACS
cnty_covars_19 %<>% mutate(year = '2019')

cnty_covars_24 <- get_county_absms_by_year(2024) # 2020-2024 5 year ACS
cnty_covars_24 %<>% mutate(year = '2024') 

# clean & save data ---------------------------------------
# load FIPS codes
data("fips_codes")

state_codes <- fips_codes %>% 
  distinct(state, .keep_all = TRUE) %>%  
  select(-c(county,county_code))

# bind data for all years
cnty_covars_15_19_24 <- bind_rows(
  cnty_covars_15,
  cnty_covars_19,
  cnty_covars_24) %>% 
  mutate(
    state_fips = str_sub(GEOID, 1, 2),
    county_fips = str_sub(GEOID, 3, 5)
  )  %>% 
  left_join(state_codes, join_by(state_fips == state_code)) %>% 
  # remove non-contiguous states & territories
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

saveRDS(cnty_covars_15_19_24,paste0(analysis_dir,"/clean_data/cnty_covars.rds"))

# generate table 1 averages -------------------------------

library(dplyr)
library(tidyr)
library(purrr)

get_var_summary <- function(var) {
  
  var_tab <- cnty_covars_15_19_24 %>% 
    group_by(year) %>% 
    summarise(
      mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      median = round(median(.data[[var]], na.rm = TRUE), 2),
      sd = round(sd(.data[[var]], na.rm = TRUE), 2),
      min = round(min(.data[[var]], na.rm = TRUE), 2),
      max = round(max(.data[[var]], na.rm = TRUE), 2),
      iqr = round(IQR(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    ) %>% 
    mutate(
      mean_sd = paste0(mean, " (", sd, ")"),
      min_max_iqr = paste0(min, "\n", max, "\n", iqr),
      var = var
    ) %>% 
    select(var, year, mean_sd, median, min_max_iqr) %>% 
    pivot_wider(
      names_from = year,
      values_from = c(mean_sd, median, min_max_iqr)
    )
  
  return(var_tab)
}

vars <- c(
  'pct_poverty',
  'pct_white_nh',
  'pct_black',
  'pct_hispanic',
  'pct_asian',
  'pct_aian'
)

cnty_vars_tab1 <- map_df(vars, ~get_var_summary(.)) 
cnty_vars_tab1 %<>% select(
  var,
  mean_sd_2015,median_2015,min_max_iqr_2015,
  mean_sd_2019,median_2019,min_max_iqr_2019,
  mean_sd_2024, median_2024,min_max_iqr_2024
)

write.csv(cnty_vars_tab1,paste0(analysis_dir,"/cnty_covar_by_period.csv"))


# count congrssional districts
cnty_num <- cnty_covars_15_19_24 %>% 
  group_by(year) %>% 
  summarise(n = n())
