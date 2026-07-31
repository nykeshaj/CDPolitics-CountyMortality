# load libraries
library(tidyverse)
library(tigris)
library(tidycensus)
library(dplyr)

# load FIPS codes
data("fips_codes")

county_codes <- fips_codes %>%
  mutate(geoid = paste0(state_code,county_code))

# https://www.cdc.gov/nchs/hus/sources-definitions/age-adjustment.htm
## function from christian to get population standard rates ##
load_std_popsizes <- function(){
  std_popsizes <- tibble::tribble(
    ~age_group,    ~std_popsize,
    "00 years",    13818,
    "01-04 years", 55317,
    "05-09 years", 72533,
    "10-14 years", 73032,
    "15-19 years", 72169,
    "20-24 years", 66478,
    "25-29 years", 64529,
    "30-34 years", 71044,
    "35-39 years", 80762,
    "40-44 years", 81851,
    "45-49 years", 72118,
    "50-54 years", 62716,
    "55-59 years", 48454,
    "60-64 years", 38793,
    "65-69 years", 34264,
    "70-74 years", 31773,
    "75-79 years", 26999,
    "80-84 years", 17842,
    "85+ years", 15508
  )

  std_popsizes_aggregated <- std_popsizes %>%
    mutate(
      age_group = case_when(
        age_group == "00 years"    ~ "<5",
        age_group == "01-04 years" ~ "<5",
        age_group == "05-09 years" ~ "5-14",
        age_group == "10-14 years" ~ "5-14",
        age_group == "15-19 years" ~ "15-24",
        age_group == "20-24 years" ~ "15-24",
        age_group == "25-29 years" ~ "25-34",
        age_group == "30-34 years" ~ "25-34",
        age_group == "35-39 years" ~ "35-44",
        age_group == "40-44 years" ~ "35-44",
        age_group == "45-49 years" ~ "45-54",
        age_group == "50-54 years" ~ "45-54",
        age_group == "55-59 years" ~ "55-64",
        age_group == "60-64 years" ~ "55-64",
        age_group == "65-69 years" ~ "65-74",
        age_group == "70-74 years" ~ "65-74",
        age_group == "75-79 years" ~ "75-84",
        age_group == "80-84 years" ~ "75-84",
        age_group == "85+ years"   ~ "85+")) %>%
    group_by(age_group) %>%
    summarize(std_popsize = sum(std_popsize)) %>%
    ungroup()

  std_popsizes_aggregated %<>% mutate(std_popsize_proportion = std_popsize / sum(std_popsize))
}

# calculate_mortality_rate_w_bridge <- function(mort_df,
#                                               total_bridge,
#                                               yr
# ){
#
#   # load the standard population weights
#   std_popsizes_aggregated <- load_std_popsizes()
#
#   # child mortality
#   deaths_under5 <- mort_df %>%
#     # filter to cause of death of interest
#     filter(child == "child") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(child = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     left_join(under5_pop, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*child/popsize_4yr,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "child",
#            year = yr)
#
#   # premature mortality
#   deaths_under65 <- mort_df %>%
#     # filter to cause of death of interest
#     filter(premature == "premature") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(premature = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*premature/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "premature",
#            year = yr)
#
#   # flu
#   flu_deaths <- mort_df %>%
#     # filter to cause of death of interest
#     filter(influenza == "influenza") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(influenza = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*influenza/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T))  %>%
#     mutate(mortality_rate = "influenza",
#            year = yr)
#
#   # any cancer
#   cancer_deaths <- mort_df %>%
#     # filter to cause of death of interest
#     filter(icd_cat == "all_cancer") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(all_cancer = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*all_cancer/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "any_cancer",
#            year = yr)
#
#   # breast
#   breast_cancer_deaths <- mort_df %>%
#     # filter to cause of death of interest
#     filter(cancer_type == "breast_cancer") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(breast_cancer = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to relevant ACS pop estimates
#     # left_join(us_female_pop, by = c("age_cat" = "age_group")) %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*breast_cancer/female_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "breast_cancer",
#            year = yr)
#
#   # colorectal
#   colorectal_cancer_deaths <- mort_df %>%
#     # filter to cause of death of interest
#     filter(cancer_type == "colorectal_cancer") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(colorectal_cancer = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*colorectal_cancer/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "colorectal_cancer",
#            year = yr)
#
#
#   # lung
#   lung_cancer_deaths <- mort_df %>%
#     # filter to cause of death of interest
#     filter(cancer_type == "lung_cancer") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(lung_cancer = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*lung_cancer/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "lung_cancer",
#            year = yr)
#
#   # any cvd
#   cvd_deaths <- mort_df %>%
#     # filter to cause of death of interest
#     filter(icd_cat == "all_cvd") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(cvd = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*cvd/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "cvd",
#            year = yr)
#
#
#   # heart disease
#   heart_disease <- mort_df %>%
#     # filter to cause of death of interest
#     filter(cvd_type == "heart_disease") %>%
#     left_join(state_codes, by = "state_code") %>%
#     # remove non-contiguous states & territories
#     # filter(!state %in% c('AK','HI','DC','PR','GU','VI','AS','MP')) %>%
#     group_by(age_cat)  %>%
#     summarise(heart_disease = sum(count, na.rm = T)) %>%
#     filter(age_cat != "age_not_stated") %>%
#     # join to bridge
#     left_join(total_bridge, by = c("age_cat" = "age_group")) %>%
#     left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
#     mutate(rate = 100000*heart_disease/total_pop,
#            wtd_rate = rate*std_popsize_proportion) %>%
#     summarise(aa_rate = sum(wtd_rate, na.rm = T)) %>%
#     mutate(mortality_rate = "heart_disease",
#            year = yr)
#
#
#   aa_rates <- bind_rows(
#     deaths_under5,
#     deaths_under65,
#     flu_deaths,
#     cancer_deaths,
#     breast_cancer_deaths,
#     colorectal_cancer_deaths,
#     lung_cancer_deaths,
#     cvd_deaths,
#     heart_disease
#   )
#
#   return(aa_rates)
# }
