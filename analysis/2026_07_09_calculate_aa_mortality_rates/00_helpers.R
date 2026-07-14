# load libraries
library(tidyverse)
library(tigris)
library(tidycensus)
library(dplyr)

# load FIPS codes
data("fips_codes")

state_codes <- fips_codes %>%
  distinct(state, .keep_all = TRUE) %>%
  select(-c(county,county_code))


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

