# load libraries
library(tidyverse)
library(here)
library(stringr)
library(magrittr)

# set analysis directory
analysis_dir <- here::here("analysis","2026_07_14_table1_var_distribution/")

# load aggregated county mortality
mort_df_12_15 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_12_15.rds")
mort_df_16_19 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_16_19.rds")
mort_df_20_24 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_20_24.rds")

# calculate age adjusted 4-Year mortality rates
calculate_ageAdjusted_4YrMortRate <- function(mort_df,yrs){

  # load the standard population weights
  std_popsizes_aggregated <- load_std_popsizes()

  # we don't need to age standardize child mortality;
  # report the crude number of deaths under 5/total under 5 pop
  # child mortality
  under5_mort_rate <- mort_df%>%
    filter(age_cat == "<5") %>%
    distinct(GEOID,.keep_all = T) %>%
    mutate(under5_4YrRate = 100000*under5_deaths/under5_pop) #%>%
    # dplyr::select(GEOID,under5_deaths,under5_pop,under5_4YrRate)

  # load the standard population weights
  std_popsizes_aggregated <- load_std_popsizes()

  # calculate all mortality rates that require age adjustment
  aa_mort_rates <- mort_df %>%
    left_join(std_popsizes_aggregated, by = c("age_cat" = "age_group")) %>%
    mutate(

      # premmature mortality
      under65_4YrRate = 100000*under65_deaths/under65_pop,
      under65_wtd4YrRate = under65_4YrRate*std_popsize_proportion,

      # influenza
      flu_4YrRate = 100000*influenza_deaths/total_pop,
      flu_wtd4YrRate = flu_4YrRate*std_popsize_proportion,

      # cancer
      cancer_4YrRate = 100000*all_cancer_deaths/total_pop,
      cancer_wtd4YrRate = cancer_4YrRate*std_popsize_proportion,

      # breast cancer
      breast_cancer_4YrRate = 100000*breast_cancer_deaths/female_pop,
      breast_cancer_wtd4YrRate = breast_cancer_4YrRate*std_popsize_proportion,

      # colorectal cancer
      colorectal_cancer_4YrRate = 100000*colorectal_cancer_deaths/total_pop,
      colorectal_cancer_wtd4YrRate = colorectal_cancer_4YrRate*std_popsize_proportion,

      # lung
      lung_cancer_4YrRate = 100000*lung_cancer_deaths/total_pop,
      lung_cancer_wtd4YrRate = lung_cancer_4YrRate*std_popsize_proportion,

      # diseases of the heart
      heart_disease_4YrRate = 100000*heart_disease_deaths/total_pop,
      heart_disease_wtd4YrRate = heart_disease_4YrRate*std_popsize_proportion
      ) %>%
    group_by(GEOID) %>%
    # sum the weighted rates in each age group to get the age adjusted rate for each county
    summarise(
      aa_under65_4YrRate = sum(under65_wtd4YrRate, na.rm = T),
      aa_flu_4YrRate = sum(flu_wtd4YrRate, na.rm = T),
      aa_cancer_4YrRate = sum(cancer_wtd4YrRate, na.rm = T),
      aa_breast_cancer_4YrRate = sum(breast_cancer_wtd4YrRate, na.rm = T),
      aa_colorectal_cancer_4YrRate = sum(colorectal_cancer_wtd4YrRate, na.rm = T),
      aa_lung_cancer_4YrRate = sum(lung_cancer_wtd4YrRate, na.rm = T),
      aa_heart_disease_4YrRate = sum(heart_disease_wtd4YrRate, na.rm = T)
      )

  aa_mort_rates <- aa_mort_rates %>%
    left_join(under5_mort_rate, by = "GEOID") %>%
    left_join(county_codes, by = c("GEOID"="geoid")) %>%
    mutate(period = yrs) %>%
    dplyr::select(period,GEOID,state,county,under5_4YrRate,aa_under65_4YrRate,aa_flu_4YrRate,
                  aa_cancer_4YrRate,aa_breast_cancer_4YrRate,aa_colorectal_cancer_4YrRate,
                  aa_lung_cancer_4YrRate,aa_heart_disease_4YrRate)

  return(aa_mort_rates)
}


aa_rates_12_15 <- calculate_ageAdjusted_4YrMortRate(mort_df_12_15,"2012-2015")
aa_rates_16_19 <- calculate_ageAdjusted_4YrMortRate(mort_df_16_19,"2016-2019")
aa_rates_20_24 <- calculate_ageAdjusted_4YrMortRate(mort_df_20_24,"2020-2024")


saveRDS(aa_rates_12_15,paste0(analysis_dir,"/data/aa_rates_12_15.rds"))
saveRDS(aa_rates_16_19,paste0(analysis_dir,"/data/aa_rates_16_19.rds"))
saveRDS(aa_rates_20_24,paste0(analysis_dir,"/data/aa_rates_20_24.rds"))
