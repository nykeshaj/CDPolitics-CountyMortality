# load libraries
library(tidyverse)
library(here)
library(stringr)
library(magrittr)

# load aggregated population estimates & convert from wide to long
acs15 <- load_acs_pop_estimates(2015)
acs15 %<>% mutate(
  popsize_4yr = estimate.total*4,
  female_popsize_4yr =estimate.female*4
)
# pop_2015 <- readRDS(here("data","analytic_datasets","filtered_to_period","county_data_15.rds"))
# pop_2015_long <- pop_2015 %>%
#   dplyr::select(geoid,state,state_name,age_under5,age5_24,age25_44,age45_64,age65_74,age75plus,geometry) %>%
#   pivot_longer(!c(geoid,state,state_name,geometry),names_to = "age_cat" ,values_to = "popsize") %>%
#   mutate(
#     popsize_4yr = popsize*4,
#     age_cat = case_when(
#       age_cat == "age_under5" ~ "<5",
#       age_cat == "age5_24" ~ "5-24",
#       age_cat == "age25_44" ~ "25-44",
#       age_cat == "age45_64" ~ "45-64",
#       age_cat == "age65_74" ~ "65-74",
#       age_cat == "age75plus" ~ "75+"
#     )
#   )
#
# pop_2019 <-readRDS(here("data","analytic_datasets","filtered_to_period","county_data_19.rds"))
# pop_2019_long <- pop_2019 %>%
#   dplyr::select(geoid,state,state_name,age_under5,age5_24,age25_44,age45_64,age65_74,age75plus,geometry) %>%
#   pivot_longer(!c(geoid,state,state_name,geometry),names_to = "age_cat" ,values_to = "popsize") %>%
#   mutate(
#     popsize_4yr = popsize*4,
#     age_cat = case_when(
#       age_cat == "age_under5" ~ "<5",
#       age_cat == "age5_24" ~ "5-24",
#       age_cat == "age25_44" ~ "25-44",
#       age_cat == "age45_64" ~ "45-64",
#       age_cat == "age65_74" ~ "65-74",
#       age_cat == "age75plus" ~ "75+"
#     )
#   )

# pop_2024 <-readRDS(here("data","analytic_datasets","filtered_to_period","county_data_24.rds"))
# pop_2024_long <- pop_2024 %>%
#   dplyr::select(geoid,state,state_name,age_under5,age5_24,age25_44,age45_64,age65_74,age75plus,geometry) %>%
#   pivot_longer(!c(geoid,state,state_name,geometry),names_to = "age_cat" ,values_to = "popsize") %>%
#   mutate(
#     popsize_4yr = popsize*4,
#     age_cat = case_when(
#       age_cat == "age_under5" ~ "<5",
#       age_cat == "age5_24" ~ "5-24",
#       age_cat == "age25_44" ~ "25-44",
#       age_cat == "age45_64" ~ "45-64",
#       age_cat == "age65_74" ~ "65-74",
#       age_cat == "age75plus" ~ "75+"
#     )
#   )

pop_2021 <-readRDS(here("data","analytic_datasets","filtered_to_period","county_data_21.rds"))
pop_2021_long <- pop_2021 %>%
  dplyr::select(geoid,state,state_name,age_under5,age5_24,age25_44,age45_64,age65_74,age75plus,geometry) %>%
  pivot_longer(!c(geoid,state,state_name,geometry),names_to = "age_cat" ,values_to = "popsize") %>%
  mutate(
    popsize_4yr = popsize*4,
    age_cat = case_when(
      age_cat == "age_under5" ~ "<5",
      age_cat == "age5_24" ~ "5-24",
      age_cat == "age25_44" ~ "25-44",
      age_cat == "age45_64" ~ "45-64",
      age_cat == "age65_74" ~ "65-74",
      age_cat == "age75plus" ~ "75+"
    )
  )
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



#####
# 1 # load & aggregate mortality data to the county-level
#####

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
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

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
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

mort_df_20_24 <- mort_df %>%
  filter(year %in% c("2020","2021","2022","2023","2024")) |>
  mort_df_16_19 <- mort_df %>%
  filter(year %in% c("2016","2017","2018","2019")) |>
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
  filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

saveRDS(mort_df_12_15,"S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/aggregate_cnty_deaths_12_15.rds")
saveRDS(mort_df_16_19,"S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/aggregate_cnty_deaths_16_19.rds")
saveRDS(mort_df_20_24,"S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/aggregate_cnty_deaths_20_24.rds")


# calculate age adjusted mortality rates for specific cause of deaths using bridge

under5_pop<- acs15 %>%  filter(age_group == "<5")

deaths_under5 <- mort_df_12_15 %>%
  filter(age_cat == "<5") %>%
  left_join(under5_pop, by = c("GEOID","age_cat" = "age_group")) %>%
  left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
  mutate(rate = 100000*under_5/popsize_4yr,
         wtd_rate = rate*std_popsize_proportion) %>%
  group_by(GEOID) %>%
  summarise(aa_rate = sum(wtd_rate, na.rm = T))



flu_deaths <- mort_df_12_15 %>%
  # join to bridge
  left_join(acs15,  by = c("GEOID","age_cat" = "age_group")) %>%
  left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
  mutate(rate = 100000*influenza/popsize_4yr,
         wtd_rate = rate*std_popsize_proportion) %>%
  group_by(GEOID) %>%
  summarise(aa_rate = sum(wtd_rate, na.rm = T))


  flu_deaths <- mort_df_12_15 %>%
    # join to bridge
    left_join(acs15,  by = c("GEOID","age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*influenza/popsize_4yr,
           wtd_rate = rate*std_popsize_proportion) %>%
    group_by(GEOID) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T))

  summary(flu_deaths$aa_rate)
calculate_mortality_rate_w_bridge <- function(mort_df,
                                              total_bridge,
                                              yr
){

  # load the standard population weights
  std_popsizes_aggregated <- load_std_popsizes()

  # child mortality

  under5_pop<- acs15 %>%  filter(age_group == "<5")
  deaths_under5 <- mort_df %>%
    # filter to cause of death of interest
    filter(child == "child") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(child = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    left_join(under5_pop, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*child/popsize_4yr,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "child",
           year = yr)

  # premature mortality
  deaths_under65 <- mort_df %>%
    # filter to cause of death of interest
    filter(premature == "premature") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(premature = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*premature/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "premature",
           year = yr)

  # flu
  flu_deaths <- mort_df %>%
    # filter to cause of death of interest
    filter(influenza == "influenza") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(influenza = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*influenza/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T))  %>%
    mutate(mortality_rate = "influenza",
           year = yr)

  # any cancer
  cancer_deaths <- mort_df %>%
    # filter to cause of death of interest
    filter(icd_cat == "all_cancer") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(all_cancer = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*all_cancer/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "any_cancer",
           year = yr)

  # breast
  breast_cancer_deaths <- mort_df %>%
    # filter to cause of death of interest
    filter(cancer_type == "breast_cancer") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(breast_cancer = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to relevant ACS pop estimates
    # left_join(us_female_pop, by = c("age_cat" = "age_group")) %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*breast_cancer/female_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "breast_cancer",
           year = yr)

  # colorectal
  colorectal_cancer_deaths <- mort_df %>%
    # filter to cause of death of interest
    filter(cancer_type == "colorectal_cancer") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(colorectal_cancer = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*colorectal_cancer/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "colorectal_cancer",
           year = yr)


  # lung
  lung_cancer_deaths <- mort_df %>%
    # filter to cause of death of interest
    filter(cancer_type == "lung_cancer") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(lung_cancer = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*lung_cancer/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "lung_cancer",
           year = yr)

  # any cvd
  cvd_deaths <- mort_df %>%
    # filter to cause of death of interest
    filter(icd_cat == "all_cvd") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(cvd = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*cvd/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "cvd",
           year = yr)


  # heart disease
  heart_disease <- mort_df %>%
    # filter to cause of death of interest
    filter(cvd_type == "heart_disease") %>%
    left_join(state_codes, by = "state_code") %>%
    # remove non-contiguous states & territories
    # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
    group_by(age_cat)  %>%
    summarise(heart_disease = sum(count, na.rm = T)) %>%
    filter(age_cat != "age_not_stated") %>%
    # join to bridge
    left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(rate = 100000*heart_disease/total_pop,
           wtd_rate = rate*std_popsize_proportion) %>%
    summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
    mutate(mortality_rate = "heart_disease",
           year = yr)


  aa_rates <- bind_rows(
    deaths_under5,
    deaths_under65,
    flu_deaths,
    cancer_deaths,
    breast_cancer_deaths,
    colorectal_cancer_deaths,
    lung_cancer_deaths,
    cvd_deaths,
    heart_disease
  )

  return(aa_rates)
}




std_popsizes_aggregated <- load_std_popsizes()


#####
# 2 # calculate age-adjusted mortality rates for each county
#####

under5_pop<- acs15 %>%  filter(age_group == "<5")
under65_pop<- acs15 %>%  filter(age_group %in% c('<5','5-14','15-24', '25-34','35-44','45-54','55-64'))

# load helper function to calculate age adjusted rate
source(here::here("data_processing","helpers.R"))

mort_df_12_15 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/aggregate_cnty_deaths_12_15.rds")
mort_df_16_19 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/aggregate_cnty_deaths_16_19.rds")
mort_df_20_24 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/aggregate_cnty_deaths_20_24.rds")

std_popsizes_aggregated <- load_std_popsizes()
mort_rates_12_15 <- mort_df_12_15 %>%
  left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
  mutate(
    ageAdj_under5_rate = under5_rate*std_popsize_proportion,
    ageAdj_under65_rate = under65_rate*std_popsize_proportion,
    ageAdj_flu_rate = flu_rate*std_popsize_proportion,
    ageAdj_cancer_rate = cancer_rate*std_popsize_proportion,
    ageAdj_breast_cancer_rate = breast_cancer_rate*std_popsize_proportion,
    ageAdj_colorectal_cancer_rate = colorectal_cancer_rate*std_popsize_proportion,
    ageAdj_lung_cancer_rate = lung_cancer_rate*std_popsize_proportion,
    ageAdj_cvd_rate = cvd_rate*std_popsize_proportion
  ) %>%
  group_by(period,GEOID,state,state_name,state_fips,county_fips) %>%
  summarise(across(c(ageAdj_under5_rate,ageAdj_under65_rate,ageAdj_flu_rate,ageAdj_cancer_rate,
                     ageAdj_breast_cancer_rate,ageAdj_colorectal_cancer_rate,ageAdj_lung_cancer_rate,
                     ageAdj_cvd_rate),
                   ~sum(.x, na.rm = TRUE)))

mort_rates_16_19 <- mort_df_16_19 %>%
  left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
  mutate(
    ageAdj_under5_rate = under5_rate*std_popsize_proportion,
    ageAdj_under65_rate = under65_rate*std_popsize_proportion,
    ageAdj_flu_rate = flu_rate*std_popsize_proportion,
    ageAdj_cancer_rate = cancer_rate*std_popsize_proportion,
    ageAdj_breast_cancer_rate = breast_cancer_rate*std_popsize_proportion,
    ageAdj_colorectal_cancer_rate = colorectal_cancer_rate*std_popsize_proportion,
    ageAdj_lung_cancer_rate = lung_cancer_rate*std_popsize_proportion,
    ageAdj_cvd_rate = cvd_rate*std_popsize_proportion
  ) %>%
  group_by(period,GEOID,state,state_name,state_fips,county_fips) %>%
  summarise(across(c(ageAdj_under5_rate,ageAdj_under65_rate,ageAdj_flu_rate,ageAdj_cancer_rate,
                     ageAdj_breast_cancer_rate,ageAdj_colorectal_cancer_rate,ageAdj_lung_cancer_rate,
                     ageAdj_cvd_rate),
                   ~sum(.x, na.rm = TRUE)))

mort_rates_20_24 <- mort_df_20_24 %>%
  left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
  mutate(
    ageAdj_under5_rate = under5_rate*std_popsize_proportion,
    ageAdj_under65_rate = under65_rate*std_popsize_proportion,
    ageAdj_flu_rate = flu_rate*std_popsize_proportion,
    ageAdj_cancer_rate = cancer_rate*std_popsize_proportion,
    ageAdj_breast_cancer_rate = breast_cancer_rate*std_popsize_proportion,
    ageAdj_colorectal_cancer_rate = colorectal_cancer_rate*std_popsize_proportion,
    ageAdj_lung_cancer_rate = lung_cancer_rate*std_popsize_proportion,
    ageAdj_cvd_rate = cvd_rate*std_popsize_proportion
  ) %>%
  group_by(period,GEOID,state,state_name,state_fips,county_fips) %>%
  summarise(across(c(ageAdj_under5_rate,ageAdj_under65_rate,ageAdj_flu_rate,ageAdj_cancer_rate,
                     ageAdj_breast_cancer_rate,ageAdj_colorectal_cancer_rate,ageAdj_lung_cancer_rate,
                     ageAdj_cvd_rate),
                   ~sum(.x, na.rm = TRUE)))


saveRDS(mort_rates_12_15, "S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/ageAdj_cnty_mort_12_15.rds")
saveRDS(mort_rates_16_19, "S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/ageAdj_cnty_mort_16_19.rds")
saveRDS(mort_rates_20_24, "S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets/ageAdj_cnty_mort_20_24.rds")


