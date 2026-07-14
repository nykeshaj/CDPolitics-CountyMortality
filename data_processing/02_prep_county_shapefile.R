#####
# 0 # setup
#####
# List of required packages
required_packages <- c(
  "tidyverse",
  "tidycensus",
  "tigris",
  "sf",
  "s2",
  "USAboundaries",
  "sp",
  "raster",
  "dplyr",
  "data.table",
  "spdep",
  "tibble",
  "tidyr",
  "stringr",
  "magrittr",
  "readxl",
  "here"
)

# Install any missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

## load libraries
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(s2)
# library(USAboundaries)
library(sp)
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
# 1 # load county boundaries from tigris
#####
boundaries_15 <- tigris::counties(year = 2015) # load county boundaries
boundaries_19 <- tigris::counties(year = 2019) # load county boundaries
boundaries_24 <- tigris::counties(year = 2024) # load county boundaries


#####
# 2 # load & aggregate mortality data to the county-level
#####

# read in restricted-use vital statistics data stored on secure drive
mort2012_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2012_extract.RDS")
mort2013_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2013_extract.RDS")
mort2014_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2014_extract.RDS")
mort2015_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2015_extract.RDS")
mort2016_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2016_extract.RDS")
mort2017_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2017_extract.RDS")
mort2018_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2018_extract.RDS")
mort2019_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2019_extract.RDS")
mort2020_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2020_extract.RDS")
mort2021_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2021_extract.RDS")
mort2022_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2022_extract.RDS")
mort2023_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2023_extract.RDS")
mort2024_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2024_extract.RDS")


# function to create age categories and translate ICD codes for underlying
# causes into mortality categories
aggregate_mortality_data_by_county <- function(mort_extract,yr){

  mort_df <- mort_extract |>
    mutate(
      age_cat = factor(case_when(
        # age27 %in% c("01","02") ~ "<1",
        age27 %in% c("01","02","03","04","05","06") ~"<5",
        age27 %in% c("07","08") ~ "5-14",
        age27 %in% c("09","10") ~ "15-24",
        age27 %in% c("11","12") ~"25-34",
        age27 %in% c("13","14") ~ "35-44",
        age27 %in% c("15","16") ~ "45-54",
        age27 %in% c("17","18") ~ "55-64",
        age27 %in% c("19","20") ~ "65-74",
        age27 %in% c("21","22") ~ "75-84",
        age27 %in% c("23","24","25","26") ~ "85+",
        age27 == "27" ~ "age_not_stated"
      ),
      levels = c("<5","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75-84","85+","age_not_stated")),
      # Fix ICD code to include decimal place
      icd10 = str_replace(uc, "^([A-Z]\\d{2})(\\d+)$", "\\1.\\2"),
      icd_letter = str_extract(icd10,"^[A-Za-z]+"), # extract ICD letter
      icd_number = as.numeric(str_extract(icd10,"[0-9].*$")), # extract ICD number
      # Create indicators for the type of mortality of interest
      icd_cat = case_when(
        icd_letter == "I" & icd_number < 79 ~ "all_cvd", # ICD I00-I78
        icd_letter == "C" & icd_number < 98  ~ "all_cancer", # ICD C00-C97
        TRUE ~ "other"
      ),
      cvd_type = case_when(
        icd_letter == "I" & icd_number < 10 ~ "heart_disease", # ICD I00-I09
        icd_letter == "I" & icd_number >= 11 & icd_number < 12 ~ "heart_disease", # ICD I11,I13
        icd_letter == "I" & icd_number >= 13 & icd_number < 14 ~ "heart_disease", # ICD I11,I13
        icd_letter == "I" & icd_number >= 20 & icd_number < 52  ~ "heart_disease", # ICD I20-I51
        icd_letter == "I" & icd_number >= 60 & icd_number < 70  ~ "stroke", # ICD C60-C69
        TRUE ~ NA_character_
      ),
      cancer_type = case_when(
        icd_letter == "C" & icd_number >= 33 & icd_number < 35 ~ "lung_cancer", # ICD C33-34
        icd_letter == "C" & icd_number >= 18 & icd_number < 22  ~ "colorectal_cancer", # ICD C18-21
        icd_letter == "C" & icd_number >= 50 & icd_number < 51 & sex == "F" ~ "breast_cancer", # ICD C50
        icd_letter == "C" & icd_number >= 61 & icd_number < 62 & sex == "M" ~ "prostate_cancer", # ICD C61
        TRUE ~ NA_character_),

      influenza = case_when(
        icd_letter == "J" & icd_number >= 9 & icd_number < 12 ~ "influenza"), # ICD J09-11

      diabetes = case_when(
        icd_letter == "E" & icd_number >= 8 & icd_number < 14 ~ "diabetes"), # ICD E08-13

      premature = ifelse(age_cat %in% c("<5","5-14","15-24","25-34","35-44","45-54","55-64"),
                         "premature",NA_character_),

      child = ifelse(age_cat == "<5","child",NA_character_),
      state_code = str_extract(GEOID, "^\\d{2}"))

  ## aggregate N deaths to the county-level by age category ------------------
  # premature mortality
  premature_mortality <- mort_df |>
    filter(premature == "premature") |>
    group_by(GEOID,age_cat) |>
    summarise(under_65 = sum(count, na.rm = T))

  # child mortality
  child_mortality <- mort_df |>
    filter(child == "child") |>
    group_by(GEOID,age_cat) |>
    summarise(under_5 = sum(count, na.rm = T))

  # influenza
  flu_mortality <- mort_df |>
    filter(influenza == "influenza") |>
    group_by(GEOID,age_cat) |>
    summarise(influenza = sum(count, na.rm = T))

  # all cancer deaths
  all_cancer_deaths <- mort_df |>
    filter(icd_cat == "all_cancer") |>
    group_by(GEOID,age_cat) |>
    summarise(all_cancer = sum(count, na.rm = T))

  # cause-specific cancer
  specific_cancer_deaths <- mort_df |>
    filter(!is.na(cancer_type)) |>
    group_by(GEOID,age_cat,cancer_type) |>
    summarise(n_deaths = sum(count, na.rm = T)) |>
    pivot_wider(names_from = cancer_type, values_from = n_deaths)

  # all CVD deaths
  all_cvd_deaths <- mort_df |>
    filter(icd_cat == "all_cvd") |>
    group_by(GEOID,age_cat) |>
    summarise(cvd = sum(count, na.rm = T))

  # create single mortality df per year -----------
  county_mort <- premature_mortality |>
    full_join(child_mortality,by = c("GEOID","age_cat")) |>
    full_join(flu_mortality,by = c("GEOID","age_cat")) |>
    full_join(all_cancer_deaths,by = c("GEOID","age_cat")) |>
    full_join(specific_cancer_deaths,by = c("GEOID","age_cat")) |>
    full_join(all_cvd_deaths,by = c("GEOID","age_cat")) |>
    mutate(year = yr) |>
    dplyr::select(year,GEOID,age_cat,under_5,under_65,influenza,all_cancer,breast_cancer,
                  colorectal_cancer,lung_cancer,cvd)

}

# list the mortality datasets of interest for analysis
extract_list <- list(
  mort2012_extract,mort2013_extract,mort2014_extract,mort2015_extract,
  mort2016_extract,mort2017_extract,mort2018_extract,mort2019_extract,
  mort2020_extract,mort2021_extract,mort2022_extract,mort2023_extract,
  mort2024_extract
)

# list the corresponding years
yrs <- list('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021','2022','2023','2024')

mort_df_list <- purrr::map2(extract_list,yrs,aggregate_mortality_data_by_county)
mort_df <- do.call(rbind,mort_df_list)


# aggregate by presidential election years (2012-2015, 2016-2019 & 2020-2024)
mort_df_12_15 <- mort_df %>%
  filter(year %in% c("2012","2013","2014","2015")) %>%
  # remove deaths where age was not stated
  filter(! age_cat == "age_not_stated") %>%
  group_by(GEOID,age_cat) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    period = "2012-2015",
    state_fips = str_sub(GEOID, 1, 2),
    county_fips = str_sub(GEOID, 3, 5)) %>%
  left_join(state_codes, join_by(state_fips == state_code)) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

mort_df_16_19 <- mort_df %>%
  filter(year %in% c("2016","2017","2018","2019")) |>
  # remove deaths where age was not stated
  filter(! age_cat == "age_not_stated") %>%
  group_by(GEOID,age_cat) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    period = "2016-2019",
    state_fips = str_sub(GEOID, 1, 2),
    county_fips = str_sub(GEOID, 3, 5)) %>%
  left_join(state_codes, join_by(state_fips == state_code)) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

mort_df_20_24 <- mort_df %>%
  filter(year %in% c("2020","2021","2022","2023","2024")) |>
  # remove deaths where age was not stated
  filter(! age_cat == "age_not_stated") %>%
  group_by(GEOID,age_cat) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>%
  mutate(
    period = "2020-2024",
    state_fips = str_sub(GEOID, 1, 2),
    county_fips = str_sub(GEOID, 3, 5)) %>%
  left_join(state_codes, join_by(state_fips == state_code)) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))


#####
# 3 # load acs data & calculate severe housing cost burden for each county
#####

# Note: Pre-calculated Severe Housing Cost Burden	from County Health Rankings
# only available 2019-2024.Since the County Health Ranking people state they use
# the 5-year estimates to compute Severe Housing Cost Burden, we can likely just use data from
# ACS 5-Yr estimates: 2015, 2019 and 2024 respectively.

# https://www.countyhealthrankings.org/health-data/community-conditions/physical-environment/housing-and-transportation/severe-housing-cost-burden?year=2025

# function to get county-level severe housing cost burden
get_cnty_housing_burden <- function(yr) {

  shcb_variables_dict <-
  tibble::tribble(
    ~var,          ~shortname,      ~desc,
    "B25074_001", 'total_rent',     "total households (table total)",
    "B25074_009", 'hh_income_lt10k_rent50p', "household income < $10,000 and gross rent ≥ 50% income",
    "B25074_018", 'hh_income_10_19k_rent50p', "household income $10,000-19,999 and gross rent ≥ 50% income",
    "B25074_027", 'hh_income_20_34k_rent50p', "household income $20,000-34,999 and gross rent ≥ 50% income",
    "B25074_036", 'hh_income_35_49k_rent50p', "household income $35,000-49,999 and gross rent ≥ 50% income",
    "B25074_045", 'hh_income_50_74k_rent50p', "household income $50,000-74,999 and gross rent ≥ 50% income",
    "B25074_054", 'hh_income_75_99k_rent50p', "household income $75,000-99,999 and gross rent ≥ 50% income",
    "B25074_063", 'hh_income_gt100k_rent50p', "household income ≥ $100,000 and gross rent ≥ 50% income",

    "B25095_001", 'total_own',              "total households (table total)",
    "B25095_009", 'hh_income_lt10k_own50p', "household income < $10,000 and owner costs ≥ 50% income",
    "B25095_018", 'hh_income_10_19k_own50p', "household income $10,000-19,999 and owner costs ≥ 50% income",
    "B25095_027", 'hh_income_20_34k_own50p', "household income $20,000-34,999 and owner costs ≥ 50% income",
    "B25095_036", 'hh_income_35_49k_own50p', "household income $35,000-49,999 and owner costs ≥ 50% income",
    "B25095_045", 'hh_income_50_74k_own50p', "household income $50,000-74,999 and owner costs ≥ 50% income",
    "B25095_054", 'hh_income_75_99k_own50p', "household income $75,000-99,999 and owner costs ≥ 50% income",
    "B25095_063", 'hh_income_100_149_own50p', "household income ≥ $100,000-149,000 and owner costs ≥ 50% income",
    "B25095_072", 'hh_income_gt150k_own50p', "household income ≥ $150,000 and owner costs ≥ 50% income",
  )

schb_data <- get_acs(
  geography = 'county',
  year = yr,
  survey = "acs5",
  variables = shcb_variables_dict$var)

# pivot to a wide format for renaming, dropping the margin of error data
schb_data <- schb_data %>% dplyr::select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate)

rename_vars <- setNames(shcb_variables_dict$var, shcb_variables_dict$shortname)
schb_data <- schb_data %>% rename(!!rename_vars)
# calculate percent severe housing cost burden

schb_data <- schb_data %>%
  group_by(GEOID,NAME) %>%
  mutate(severe_housing_burden_denom = total_rent+total_own,
         severe_housing_burden_count = hh_income_lt10k_rent50p + hh_income_10_19k_rent50p +
           hh_income_20_34k_rent50p + hh_income_35_49k_rent50p + hh_income_50_74k_rent50p +
           hh_income_75_99k_rent50p + hh_income_gt100k_rent50p + hh_income_lt10k_own50p +
           hh_income_10_19k_own50p + hh_income_20_34k_own50p + hh_income_35_49k_own50p +
           hh_income_50_74k_own50p + hh_income_75_99k_own50p + hh_income_100_149_own50p +
           hh_income_gt150k_own50p,
         severe_housing_burden_pct = 100*severe_housing_burden_count/severe_housing_burden_denom
  ) %>%
  dplyr::select(GEOID,NAME,severe_housing_burden_count,severe_housing_burden_denom,severe_housing_burden_pct)
}

housing_burden15 <- get_cnty_housing_burden(2015)
housing_burden19 <- get_cnty_housing_burden(2019)
housing_burden24 <- get_cnty_housing_burden(2024)

#####
# 4 # pull in decennial census data for population
#####

# 2010 Decennial Census ---------------------------
vars2010 <- load_variables(2010, "sf1", cache = TRUE) # Load all available variables for the 2010 SF1

# All variables from P012 (Sex by Age)
p012_2010_vars <- vars2010 %>%
  filter(concept == "SEX BY AGE") %>%
  filter(stringr::str_detect(name,"^P012"))

p012_2010_vars_dict <-
  tibble::tribble(
    ~var,          ~shortname,      ~desc,
    "P012001", 'total_population',     "total population",
    "P012002", 'total_male', "total male population",
    "P012003", 'male_under5', "male population under 5 yrs",
    "P012004", 'male5_9', "male population aged 5-9 yrs",
    "P012005", 'male10_14', "male population aged 10-14 yrs",
    "P012006", 'male15_17', "male population aged 15-17 yrs",
    "P012007", 'male18_19', "male population aged 18-19 yrs",
    "P012008", 'male20', "male population aged 20 yrs",
    "P012009", 'male21', "male population aged 21 yrs",
    "P012010", 'male22_24', "male population aged 22-24 yrs",
    "P012011", 'male25_29', "male population aged 25-29 yrs",
    "P012012", 'male30_34', "male population aged 30-34 yrs",
    "P012013", 'male35_39', "male population aged 35-39 yrs",
    "P012014", 'male40_44', "male population aged 40-44 yrs",
    "P012015", 'male45_49', "male population aged 45-49 yrs",
    "P012016", 'male50_54', "male population aged 50-54 yrs",
    "P012017", 'male55_59', "male population aged 55-59 yrs",
    "P012018", 'male60_61', "male population aged 60-61 yrs",
    "P012019", 'male62_64', "male population aged 62-64 yrs",
    "P012020", 'male65_66', "male population aged 65-66 yrs",
    "P012021", 'male67_69', "male population aged 67-69 yrs",
    "P012022", 'male70_74', "male population aged 70-74 yrs",
    "P012023", 'male75_79', "male population aged 75-79 yrs",
    "P012024", 'male80_84', "male population aged 80-84 yrs",
    "P012025", 'male_over85', "male population aged 85+ yrs",

    "P012026", 'total_female', "total female population",
    "P012027", 'female_under5', "female population under 5 yrs",
    "P012028", 'female5_9', "female population aged 5-9 yrs",
    "P012029", 'female10_14', "female population aged 10-14 yrs",
    "P012030", 'female15_17', "female population aged 15-17 yrs",
    "P012031", 'female18_19', "female population aged 18-19 yrs",
    "P012032", 'female20', "female population aged 20 yrs",
    "P012033", 'female21', "female population aged 21 yrs",
    "P012034", 'female22_24', "female population aged 22-24 yrs",
    "P012035", 'female25_29', "female population aged 25-29 yrs",
    "P012036", 'female30_34', "female population aged 30-34 yrs",
    "P012037", 'female35_39', "female population aged 35-39 yrs",
    "P012038", 'female40_44', "female population aged 40-44 yrs",
    "P012039", 'female45_49', "female population aged 45-49 yrs",
    "P012040", 'female50_54', "female population aged 50-54 yrs",
    "P012041", 'female55_59', "female population aged 55-59 yrs",
    "P012042", 'female60_61', "female population aged 60-61 yrs",
    "P012043", 'female62_64', "female population aged 62-64 yrs",
    "P012044", 'female65_66', "female population aged 65-66 yrs",
    "P012045", 'female67_69', "female population aged 67-69 yrs",
    "P012046", 'female70_74', "female population aged 70-74 yrs",
    "P012047", 'female75_79', "female population aged 75-79 yrs",
    "P012048", 'female80_84', "female population aged 80-84 yrs",
    "P012049", 'female_over85', "female population aged 85+ yrs",
  )

county_sex_age_2010 <- get_decennial(
  geography = "county",
  year = 2010,
  sumfile = "sf1",
  variables = p012_2010_vars$name
)

county_sex_age_2010 <- county_sex_age_2010 %>%
  pivot_wider(names_from = variable, values_from = value)

rename_vars <- setNames(p012_2010_vars_dict$var, p012_2010_vars_dict$shortname)
county_sex_age_2010 <- county_sex_age_2010 %>%
  rename(!!rename_vars)

county_sex_age_2010 <- county_sex_age_2010 %>%
  mutate(
    under5_pop = male_under5 + female_under5,
    under65_pop = male_under5 + male5_9 + male10_14 + male15_17 + male18_19 +  male20 +  male21 +
      male22_24 + male25_29 + male30_34 + male35_39 + male40_44 + male45_49 + male50_54 +
      male55_59 + male60_61 + male62_64 +
      female_under5 + female5_9 + female10_14 + female15_17 + female18_19 + female20 +
      female21 + female22_24 +  female25_29  + female30_34 + female35_39  + female40_44 +
     female45_49 + female50_54 + female55_59 + female60_61 + female62_64
    ) %>%
  dplyr::select(GEOID,NAME,total_pop=total_population, female_pop=total_female,under5_pop,under65_pop)

# 2020 Decenniel Census ---------------------------
vars2020 <- load_variables(2020, "dhc", cache = TRUE) # Load all available variables for the 2010 SF1

# All variables from P12 (Sex by Age)
p12_2020_vars <- vars2020 |>
  dplyr::filter(stringr::str_detect(name, "^P12_"))

p12_2020_vars_dict <-
  tibble::tribble(
    ~var,          ~shortname,      ~desc,
    "P12_001N", 'total_population',     "total population",
    "P12_002N", 'total_male', "total male population",
    "P12_003N", 'male_under5', "male population under 5 yrs",
    "P12_004N", 'male5_9', "male population aged 5-9 yrs",
    "P12_005N", 'male10_14', "male population aged 10-14 yrs",
    "P12_006N", 'male15_17', "male population aged 15-17 yrs",
    "P12_007N", 'male18_19', "male population aged 18-19 yrs",
    "P12_008N", 'male20', "male population aged 20 yrs",
    "P12_009N", 'male21', "male population aged 21 yrs",
    "P12_010N", 'male22_24', "male population aged 22-24 yrs",
    "P12_011N", 'male25_29', "male population aged 25-29 yrs",
    "P12_012N", 'male30_34', "male population aged 30-34 yrs",
    "P12_013N", 'male35_39', "male population aged 35-39 yrs",
    "P12_014N", 'male40_44', "male population aged 40-44 yrs",
    "P12_015N", 'male45_49', "male population aged 45-49 yrs",
    "P12_016N", 'male50_54', "male population aged 50-54 yrs",
    "P12_017N", 'male55_59', "male population aged 55-59 yrs",
    "P12_018N", 'male60_61', "male population aged 60-61 yrs",
    "P12_019N", 'male62_64', "male population aged 62-64 yrs",
    "P12_020N", 'male65_66', "male population aged 65-66 yrs",
    "P12_021N", 'male67_69', "male population aged 67-69 yrs",
    "P12_022N", 'male70_74', "male population aged 70-74 yrs",
    "P12_023N", 'male75_79', "male population aged 75-79 yrs",
    "P12_024N", 'male80_84', "male population aged 80-84 yrs",
    "P12_025N", 'male_over85', "male population aged 85+ yrs",

    "P12_026N", 'total_female', "total female population",
    "P12_027N", 'female_under5', "female population under 5 yrs",
    "P12_028N", 'female5_9', "female population aged 5-9 yrs",
    "P12_029N", 'female10_14', "female population aged 10-14 yrs",
    "P12_030N", 'female15_17', "female population aged 15-17 yrs",
    "P12_031N", 'female18_19', "female population aged 18-19 yrs",
    "P12_032N", 'female20', "female population aged 20 yrs",
    "P12_033N", 'female21', "female population aged 21 yrs",
    "P12_034N", 'female22_24', "female population aged 22-24 yrs",
    "P12_035N", 'female25_29', "female population aged 25-29 yrs",
    "P12_036N", 'female30_34', "female population aged 30-34 yrs",
    "P12_037N", 'female35_39', "female population aged 35-39 yrs",
    "P12_038N", 'female40_44', "female population aged 40-44 yrs",
    "P12_039N", 'female45_49', "female population aged 45-49 yrs",
    "P12_040N", 'female50_54', "female population aged 50-54 yrs",
    "P12_041N", 'female55_59', "female population aged 55-59 yrs",
    "P12_042N", 'female60_61', "female population aged 60-61 yrs",
    "P12_043N", 'female62_64', "female population aged 62-64 yrs",
    "P12_044N", 'female65_66', "female population aged 65-66 yrs",
    "P12_045N", 'female67_69', "female population aged 67-69 yrs",
    "P12_046N", 'female70_74', "female population aged 70-74 yrs",
    "P12_047N", 'female75_79', "female population aged 75-79 yrs",
    "P12_048N", 'female80_84', "female population aged 80-84 yrs",
    "P12_049N", 'female_over85', "female population aged 85+ yrs",
  )

county_sex_age_2020 <- get_decennial(
  geography = "county",
  year = 2020,
  sumfile = "dhc",
  variables = p12_2020_vars$name
)

county_sex_age_2020 <- county_sex_age_2020 %>%
  pivot_wider(names_from = variable, values_from = value)

rename_vars <- setNames(p12_2020_vars_dict$var, p12_2020_vars_dict$shortname)
county_sex_age_2020 <- county_sex_age_2020 %>%
  rename(!!rename_vars)

county_sex_age_2020 <- county_sex_age_2020 %>%
  mutate(
    under5_pop = male_under5 + female_under5,
    under65_pop = male_under5 + male5_9 + male10_14 + male15_17 + male18_19 +  male20 +  male21 +
      male22_24 + male25_29 + male30_34 + male35_39 + male40_44 + male45_49 + male50_54 +
      male55_59 + male60_61 + male62_64 +
      female_under5 + female5_9 + female10_14 + female15_17 + female18_19 + female20 +
      female21 + female22_24 +  female25_29  + female30_34 + female35_39  + female40_44 +
      female45_49 + female50_54 + female55_59 + female60_61 + female62_64
  ) %>%
  dplyr::select(GEOID,NAME,total_pop=total_population, female_pop=total_female,under5_pop,under65_pop)


#####
# 4 # create analytic shapefiles
#####

mort_df_12_15 <- mort_df_12_15 %>%
  left_join(housing_burden15, by = "GEOID") %>%
  left_join(county_sex_age_2010, by = "GEOID") %>%
  dplyr::select(
    period,GEOID,state_abbr,state_name,county_name=NAME.x,age_cat,under5_deaths=under_5,
    under65_deaths=under_65,influenza_deaths=influenza,all_cancer_deaths=all_cancer,
    breast_cancer_deaths=breast_cancer,colorectal_cancer_deaths=colorectal_cancer,
    lung_cancer_deaths=lung_cancer,cvd_deaths=cvd,severe_housing_burden_pct,
    severe_housing_burden_count,severe_housing_burden_denom,total_pop,female_pop,
    under5_pop, under65_pop
  ) %>%
  # append
  left_join(boundaries_15, by = "GEOID")


mort_df_16_19 <- mort_df_16_19 %>%
  left_join(housing_burden19, by = "GEOID") %>%
  left_join(county_sex_age_2010, by = "GEOID")%>%
  dplyr::select(
    period,GEOID,state_abbr,state_name,county_name=NAME.x,age_cat,under5_deaths=under_5,
    under65_deaths=under_65,influenza_deaths=influenza,all_cancer_deaths=all_cancer,
    breast_cancer_deaths=breast_cancer,colorectal_cancer_deaths=colorectal_cancer,
    lung_cancer_deaths=lung_cancer,cvd_deaths=cvd,severe_housing_burden_pct,
    severe_housing_burden_count,severe_housing_burden_denom,total_pop,female_pop,
    under5_pop, under65_pop
  ) %>%
  # append
  left_join(boundaries_19, by = "GEOID")


mort_df_20_24 <- mort_df_20_24 %>%
  left_join(housing_burden24, by = "GEOID") %>%
  left_join(county_sex_age_2020, by = "GEOID")%>%
  dplyr::select(
    period,GEOID,state_abbr,state_name,county_name=NAME.x,age_cat,under5_deaths=under_5,
    under65_deaths=under_65,influenza_deaths=influenza,all_cancer_deaths=all_cancer,
    breast_cancer_deaths=breast_cancer,colorectal_cancer_deaths=colorectal_cancer,
    lung_cancer_deaths=lung_cancer,cvd_deaths=cvd,severe_housing_burden_pct,
    severe_housing_burden_count,severe_housing_burden_denom,total_pop,female_pop,
    under5_pop, under65_pop
  ) %>%
  # append
  left_join(boundaries_24, by = "GEOID")

# -------------

saveRDS(mort_df_12_15,"S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_12_15.rds")
saveRDS(mort_df_16_19,"S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_16_19.rds")
saveRDS(mort_df_20_24,"S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_20_24.rds")
