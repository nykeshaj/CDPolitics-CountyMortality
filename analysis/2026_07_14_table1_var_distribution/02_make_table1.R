# Purpose: Report distribution of congressional district-level political metrics & county-level
# poverty, racialized composition & mortality rates

# load libraries
library(tidyverse)
library(here)
library(writexl)

# set analytic directory
analysis_dir <- here::here("analysis","2026_07_14_table1_var_distribution/")


# Congressional District ------------------------------------------------------
cd12_15 <- readRDS(here("data","analytic_datasets","filtered_to_period","cd12_15.rds"))
cd16_19 <- readRDS(here("data","analytic_datasets","filtered_to_period","cd16_19.rds"))
cd20_24 <- readRDS(here("data","analytic_datasets","filtered_to_period","cd20_24.rds"))

# function to obtain summary statistics for cd measures
get_cd_stats_by_yr <- function(cd_df,cd_measure){

  cd_measure_name <- sym(cd_measure)

  cd_summary_stats <- cd_df %>%
    group_by(period) %>%
    summarise (
      mean = round(mean(!!cd_measure_name,na.rm = T),2),
      median = round(median(!!cd_measure_name ,na.rm = T),2),
      sd = round(sd(!!cd_measure_name ,na.rm = T),2),
      min = round(min(!!cd_measure_name ,na.rm = T),2),
      max = round(max(!!cd_measure_name ,na.rm = T),2),
      iqr = round(IQR(!!cd_measure_name ,na.rm = T),2)
    ) %>%
    mutate(
      political_metric = cd_measure,
      mean_sd = paste0(mean, " (",sd,")"),
      median = as.character(median),
      min_max_iqr = paste0(min, "\n",max,"\n",iqr)
    )

  return(cd_summary_stats)
}

# congressional district measure summary statistics: 2012-2015
cd_summary_12_15 <- purrr::map_df(
  .x = c("mean_house_dw","mean_senate_dw","state_liberalism_index",
         "poverty_pct","pct_white","pct_black","pct_aian","pct_asian","pct_nhpi",
         "pct_hispanic"),
  .f = ~get_cd_stats_by_yr(cd_df = cd12_15, cd_measure = .x))

# congressional district measure summary statistics: 2016-2019
cd_summary_16_19 <- purrr::map_df(
  .x = c("mean_house_dw","mean_senate_dw","state_liberalism_index",
         "poverty_pct","pct_white","pct_black","pct_aian","pct_asian","pct_nhpi",
         "pct_hispanic"),
  .f = ~get_cd_stats_by_yr(cd_df = cd16_19, cd_measure = .x))

# congressional district measure summary statistics: 2020-2024
cd_summary_20_24 <- purrr::map_df(
  .x = c("mean_house_dw","mean_senate_dw","state_liberalism_index",
         "poverty_pct","pct_white","pct_black","pct_aian","pct_asian","pct_nhpi",
         "pct_hispanic"),
  .f = ~get_cd_stats_by_yr(cd_df = cd20_24, cd_measure = .x))

# bind across all time periods
cd_summary <- bind_rows(
  cd_summary_12_15,
  cd_summary_16_19,
  cd_summary_20_24
) %>%
  pivot_wider(id_cols = political_metric, names_from = period ,values_from = c(mean_sd,median,min_max_iqr)) %>%
  dplyr::select(
    political_metric,
    `mean_sd_2012-2015`,`median_2012-2015`,`min_max_iqr_2012-2015`,
    `mean_sd_2016-2019`,`median_2016-2019`,`min_max_iqr_2016-2019`,
    `mean_sd_2020-2024`,`median_2020-2024`,`min_max_iqr_2020-2024`
  )

# write.csv(cd_summary,"us_aggregate_cd_measures_by_period_tbl.csv")

# County  ----------------------------------------------------------

# load cnty shapefiles that already include the severe housing cost variable
mort_df_12_15 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_12_15.rds")
mort_df_16_19 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_16_19.rds")
mort_df_20_24 <- readRDS("S:SHDH/nethery_spatial_misalignment/clean_data/analytic_datasets_cnty/aggregate_cnty_deaths_20_24.rds")

# list the county-level covariates of interest
covars_of_interest <- c("severe_housing_burden_pct")

# function to get the summary statistics for the covariate of interest
get_covar_stats_by_yr <- function(df,covar,yrs){

  covar_name <- sym(covar)

  covar_summary_stats <- df %>%
    summarise (
      mean = round(mean(!!covar_name,na.rm = T),2),
      median = round(median(!!covar_name ,na.rm = T),2),
      sd = round(sd(!!covar_name ,na.rm = T),2),
      min = round(min(!!covar_name ,na.rm = T),2),
      max = round(max(!!covar_name ,na.rm = T),2),
      iqr = round(IQR(!!covar_name ,na.rm = T),2)
    ) %>%
    mutate(
      period = yrs,
      covariate = covar,
      mean_sd = paste0(mean, " (",sd,")"),
      median = as.character(median),
      min_max_iqr = paste0(min, "\n",max,"\n",iqr)
    )

  return(covar_summary_stats)
}


# covariate summary statistics: 2012-2015
covar_summary_12_15 <- purrr::map_df(
  .x = covars_of_interest,
  .f = ~get_covar_stats_by_yr(df = mort_df_12_15, covar = .x, yrs = "2012-2015"))

# covariate summary statistics: 2016-2019
covar_summary_16_19 <- purrr::map_df(
  .x = covars_of_interest,
  .f = ~get_covar_stats_by_yr(df = mort_df_16_19, covar = .x, yrs = "2016-2019"))

# covariate summary statistics: 2020-2024
covar_summary_20_24 <- purrr::map_df(
  .x = covars_of_interest,
  .f = ~get_covar_stats_by_yr(df = mort_df_20_24, covar = .x, yrs = "2020-2024"))

# bind across all time periods
cnty_covar_summary <- bind_rows(
  covar_summary_12_15,
  covar_summary_16_19,
  covar_summary_20_24
) %>%
  pivot_wider(id_cols = covariate, names_from = period ,values_from = c(mean_sd,median,min_max_iqr)) %>%
  dplyr::select(
    covariate,
    `mean_sd_2012-2015`,`median_2012-2015`,`min_max_iqr_2012-2015`,
    `mean_sd_2016-2019`,`median_2016-2019`,`min_max_iqr_2016-2019`,
    `mean_sd_2020-2024`,`median_2020-2024`,`min_max_iqr_2020-2024`
  )

write.csv(covar_summary,"us_aggregate_covar_measures_by_period_tbl.csv")


# County mortality -----------------------------------------------------------

# load age adjusted 4-year county-level mortality rates
aa_rates_12_15 <- readRDS(paste0(analysis_dir,"/data/aa_rates_12_15.rds"))
aa_rates_16_19 <- readRDS(paste0(analysis_dir,"/data/aa_rates_16_19.rds"))
aa_rates_20_24 <- readRDS(paste0(analysis_dir,"/data/aa_rates_20_24.rds"))

mort_rates_of_interest <- c(
  "under5_4YrRate",
  "aa_under65_4YrRate",
  "aa_flu_4YrRate",
  "aa_cancer_4YrRate",
  "aa_breast_cancer_4YrRate",
  "aa_colorectal_cancer_4YrRate",
  "aa_lung_cancer_4YrRate",
  "aa_heart_disease_4YrRate"
)

# function to obtain summary statistics for mortality rates
get_mortality_stats_by_yr <- function(mort_rate_df,rate){

  rate_name <- sym(rate)

  mortality_summary_stats <- mort_rate_df %>%
    group_by(period) %>%
    summarise (
      mean = round(mean(!!rate_name,na.rm = T),2),
      median = round(median(!!rate_name ,na.rm = T),2),
      sd = round(sd(!!rate_name ,na.rm = T),2),
      min = round(min(!!rate_name ,na.rm = T),2),
      max = round(max(!!rate_name ,na.rm = T),2),
      iqr = round(IQR(!!rate_name ,na.rm = T),2)
      ) %>%
    mutate(
      mortality_rate = rate,
      mean_sd = paste0(mean, " (",sd,")"),
      median = as.character(median),
      min_max_iqr = paste0(min, "\n",max,"\n",iqr)
    )

  return(mortality_summary_stats)
}

mortality_summary_12_15 <- purrr::map_df(
  .x = mort_rates_of_interest,
  .f = ~get_mortality_stats_by_yr(mort_rate_df = aa_rates_12_15, rate = .x))

mortality_summary_16_19 <- purrr::map_df(
  .x = mort_rates_of_interest,
  .f = ~get_mortality_stats_by_yr(mort_rate_df = aa_rates_16_19, rate = .x))

mortality_summary_20_24 <- purrr::map_df(
  .x = mort_rates_of_interest,
  .f = ~get_mortality_stats_by_yr(mort_rate_df = aa_rates_20_24, rate = .x))

# write.csv(mortality_summary_20_24,"temp_file.csv")

mortality_summary <- bind_rows(
  mortality_summary_12_15,
  mortality_summary_16_19,
  mortality_summary_20_24
) %>%
  pivot_wider(id_cols = mortality_rate, names_from = period ,values_from = c(mean_sd,median,min_max_iqr)) %>%
  dplyr::select(
    mortality_rate,
    `mean_sd_2012-2015`,`median_2012-2015`,`min_max_iqr_2012-2015`,
    `mean_sd_2016-2019`,`median_2016-2019`,`min_max_iqr_2016-2019`,
    `mean_sd_2020-2024`,`median_2020-2024`,`min_max_iqr_2020-2024`
  )

# write.csv(mortality_summary,"us_ageAdj_mortality_stats_by_period_tbl.csv")


# stack everything together to create table 1
tbl1 <- bind_rows(
    cd_summary,
    cnty_covar_summary,
    mortality_summary) %>%
  mutate(
    political_metric = ifelse(is.na(political_metric),covariate,political_metric),
    political_metric = ifelse(is.na(political_metric),mortality_rate,political_metric)
  ) %>%
  rename(var = political_metric) %>%
  dplyr::select(-c(covariate,mortality_rate))

write_xlsx(tbl1,paste0(analysis_dir,"table_shells/variable_summary_stats.xlsx"))
