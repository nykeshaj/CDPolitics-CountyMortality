# load libraries
library(tidyverse)
library(stringr)

# set working directory
analysis_dir <- here::here("2026_05_19_var_distributions")

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
mort2022_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2020_extract.RDS")
mort2023_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2020_extract.RDS")
mort2024_extract <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/mort2020_extract.RDS")

# list the mortality datasets of interest for analysis
extract_list <- list(
  mort2012_extract,mort2013_extract,mort2014_extract,mort2015_extract,
  mort2016_extract,mort2017_extract,mort2018_extract,mort2019_extract,
  mort2020_extract,mort2021_extract,mort2022_extract,mort2023_extract,
  mort2024_extract
)

# list the corresponding years
yrs <- list('2012','2013','2014','2015','2016','2017','2018','2019','2020','2021','2022','2023','2024')

# create function to create age categories and translate ICD codes for underlying 
# causes into mortality categories
pull_mortality_data <- function(mort_extract,yr){
  
## clean nvss data ----------------------------------
mort_df <- mort_extract |>
  mutate(
    count = as.numeric(count),
    # create age categories to match ACS
    age_cat = factor(case_when(
      age27 %in% c("01","02","03","04","05","06") ~ "<5",
      age27 %in% c("07","08","09","10") ~"5-24",
      # age27 == "07" ~ "5-9",
      # age27 == "08" ~ "10-14",
      # age27 == "09" ~ "15-19",
      # age27 == "10" ~ "20-24",
      age27 %in% c("11","12","13","14") ~"25-44",
      # age27 == "11" ~ "25-29",
      # age27 == "12" ~ "30-34",
      # age27 %in% c("13","14") ~ "35-44",
      age27 %in% c("15","16","17","18") ~ "45-64",
      # age27 %in% c("15","16") ~ "45-54",
      # age27 %in% c("17","18") ~ "55-64",
      age27 %in% c("19","20") ~ "65-74",
      age27 %in% c("21","22","23","24","25","26") ~ "75+",
      # age27 %in% c("21","22") ~ "75-84",
      # age27 %in% c("23","24","25","26") ~ "85+"
      age27 == "27" ~ "age_not_stated"
    ),
    # levels = c("<5","5-9","10-14","15-19","20-24","25-29",
    #            "30-34","35-44","45-54","55-64","65-74","75-84","85+")),
    levels = c("<5","5-24","25-44","45-64","65-74","75+","age_not_stated")),
    # Fix ICD code to include decimal place
    icd10 = str_replace(uc, "^([A-Z]\\d{2})(\\d+)$", "\\1.\\2"),
    icd_letter = str_extract(icd10,"^[A-Za-z]+"), # extract ICD letter
    icd_number = as.numeric(str_extract(icd10,"[0-9].*$")), # extract ICD number
    # Create indicators for the type of mortality of interest
    icd_cat = case_when(
      icd_letter == "I" & icd_number < 79 ~ "all_cvd", # ICD I00-I78
      icd_letter == "C" & icd_number < 98  ~ "all_cancer", # ICD C00-C97
      # icd_letter == "B" & icd_number >= 20 & icd_number < 25 ~ "hiv_aids", # ICD B20-24
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
    
    premature = ifelse(age_cat %in% c("<5","5-24","25-44","45-64"),
                       "premature",NA_character_),
    
    child = ifelse(age_cat == "<5","child",NA_character_),
    state_code = str_extract(GEOID, "^\\d{2}"))

## aggregate N deaths to county-level ------------------
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
  select(year,GEOID,age_cat,under_5,under_65,influenza,all_cancer,breast_cancer,
         colorectal_cancer,lung_cancer,cvd)
}

mort_df_list <- purrr::map2(extract_list,yrs,pull_mortality_data) 

mort_df <- do.call(rbind,mort_df_list)

# aggregate by presidential election yearss (2012-2015, 2016-2019 & 2020-2024)
mort_df_12_15 <- mort_df %>% 
  filter(year %in% c("2012","2013","2014","2015")) |>
  group_by(GEOID,age_cat) |>
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>% 
  mutate(period = "2012-2015")


mort_df_16_19 <- mort_df %>% 
  filter(year %in% c("2016","2017","2018","2019")) |>
  group_by(GEOID,age_cat) |>
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>% 
  mutate(period = "2016-2019")


mort_df_20_24 <- mort_df %>% 
  filter(year %in% c("2020","2021","2022","2023","2024")) |>
  group_by(GEOID,age_cat) |>
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop") %>% 
  mutate(period = "2020-2024")


deaths_by_county <- bind_rows(mort_df_12_15,mort_df_16_19,mort_df_20_24)

saveRDS(deaths_by_county,"S:SHDH/nethery_spatial_misalignment/clean_data/cnty_deaths_12_24.rds")
deaths_by_county <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/cnty_deaths_12_24.rds")


# ---------

aggregate_deaths_12_15 <- mort_df_12_15 %>% 
  group_by(age_cat) %>% 
  summarise(
    total_deaths_under5 = sum(under_5, na.rm = T),
    total_deaths_under65 = sum(under_65, na.rm = T), 
    total_flu_deaths = sum(influenza, na.rm = T),
    total_cancer_deaths = sum(all_cancer, na.rm = T),
    total_breast_cancer_deaths = sum(breast_cancer, na.rm = T), 
    total_colorectal_cancer_deaths = sum(colorectal_cancer, na.rm = T),
    total_cvd_deaths = sum(cvd, na.rm = T)
) %>%
  filter(! age_cat == "age_not_stated")

aggregate_deaths_16_19 <- mort_df_16_19 %>% 
  group_by(age_cat) %>% 
  summarise(
    total_deaths_under5 = sum(under_5, na.rm = T),
    total_deaths_under65 = sum(under_65, na.rm = T), 
    total_flu_deaths = sum(influenza, na.rm = T),
    total_cancer_deaths = sum(all_cancer, na.rm = T),
    total_breast_cancer_deaths = sum(breast_cancer, na.rm = T), 
    total_colorectal_cancer_deaths = sum(colorectal_cancer, na.rm = T),
    total_cvd_deaths = sum(cvd, na.rm = T)
  ) %>%
  filter(! age_cat == "age_not_stated")


aggregate_deaths_20_24 <- mort_df_20_24 %>% 
  group_by(age_cat) %>% 
  summarise(
    total_deaths_under5 = sum(under_5, na.rm = T),
    total_deaths_under65 = sum(under_65, na.rm = T), 
    total_flu_deaths = sum(influenza, na.rm = T),
    total_cancer_deaths = sum(all_cancer, na.rm = T),
    total_breast_cancer_deaths = sum(breast_cancer, na.rm = T), 
    total_colorectal_cancer_deaths = sum(colorectal_cancer, na.rm = T),
    total_cvd_deaths = sum(cvd, na.rm = T)
  ) %>%
  filter(! age_cat == "age_not_stated")

# load ACS 5 year pop estimates
analysis_dir <- here::here("2026_05_19_var_distributions")

# 2015
pop_2015 <- readRDS(paste0(analysis_dir,"/clean_data/county_acs_pop_2015"))
agg_pop_2015 <- pop_2015 %>% 
  summarise(across(c(age_under5,age5_24,age25_44,age45_64,age65_74,age75plus), ~sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(!geometry,names_to = "age" ,values_to = "popsize_estimate_15") 

aggregate_deaths_12_15 <- bind_cols(aggregate_deaths_12_15,agg_pop_2015) 
aggregate_deaths_12_15 %<>% select(-c(geometry,age)) 

saveRDS(aggregate_deaths_12_15, paste0(analysis_dir,"/clean_data/agg_totals_12_15.rds"))

# 2019
pop_2019 <- readRDS(paste0(analysis_dir,"/clean_data/county_acs_pop_2019"))
agg_pop_2019 <- pop_2019 %>% 
  summarise(across(c(age_under5,age5_24,age25_44,age45_64,age65_74,age75plus), ~sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(!geometry,names_to = "age" ,values_to = "popsize_estimate_19")

aggregate_deaths_16_19 <- bind_cols(aggregate_deaths_16_19,agg_pop_2019) 
aggregate_deaths_16_19 %<>% select(-c(geometry,age))

saveRDS(aggregate_deaths_16_19, paste0(analysis_dir,"/clean_data/agg_totals_16_19.rds"))

# 2024
pop_2024 <- readRDS(paste0(analysis_dir,"/clean_data/county_acs_pop_2024"))
agg_pop_2024 <- pop_2024 %>% 
  summarise(across(c(age_under5,age5_24,age25_44,age45_64,age65_74,age75plus), ~sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(!geometry,names_to = "age" ,values_to = "popsize_estimate_24")

aggregate_deaths_20_24 <- bind_cols(aggregate_deaths_20_24,agg_pop_2024) 
aggregate_deaths_20_24 %<>% select(-c(geometry,age))

saveRDS(aggregate_deaths_20_24, paste0(analysis_dir,"/clean_data/agg_totals_20_24.rds"))


# calculate age adjusted rates for total population for table 1
aggregate_deaths_12_15 <- readRDS(paste0(analysis_dir,"/clean_data/agg_totals_12_15.rds"))
aggregate_deaths_16_19 <- readRDS(paste0(analysis_dir,"/clean_data/agg_totals_16_19.rds"))
aggregate_deaths_20_24 <- readRDS(paste0(analysis_dir,"/clean_data/agg_totals_20_24.rds"))

rates_12_15 <- aggregate_deaths_12_15 %>% 
  mutate(
    under5_rate = 100000*total_deaths_under5/(popsize_estimate_15*4),
    under65_rate = 100000*total_deaths_under65/(popsize_estimate_15*4),
    flu_rate = 100000*total_flu_deaths/(popsize_estimate_15*4),
    cancer_rate = 100000*total_cancer_deaths/(popsize_estimate_15*4),
    breast_cancer_rate = 100000*total_breast_cancer_deaths/(popsize_estimate_15*4),
    colorectal_cancer_rate = 100000*total_colorectal_cancer_deaths/(popsize_estimate_15*4),
    cvd_rate = 100000*total_cvd_deaths/(popsize_estimate_15*4)
  )
    
         