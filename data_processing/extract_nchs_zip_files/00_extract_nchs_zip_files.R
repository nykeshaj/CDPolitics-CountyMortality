# Title: Extract, Clean, & Export NCHS Restricted-Use Mortality Data
# Date: Nov 7, 2024

# NOTE 1: This workflow is desgined to work in tandem with 7-Zip, so please make sure
# that it is downloaded to your computer (you can download at https://www.7-zip.org/)

# NOTE 2: This workflow also assumes your NCHS password is stored in the .Renviron
# Recall that you can use usethis::edit_r_environ() to add your password to the .Renviron

# 1: setup ----------------------------------------------------------------
## install & load relevant packages 
library(usethis)
library(tidyverse)
library(here)
library(tidycensus)

## define relevant folderpaths
read.data_folderpath <- 'S://SHDH/nethery_spatial_misalignment/raw_data' # this is where we will access the raw data from
save.data_folderpath <- 'S://SHDH/nethery_spatial_misalignment/clean_data' # this is where we will save cleaned datasets

## list the zipped folders you will be accessing data from
zipped_folders <- list.files(path='S://SHDH/nethery_spatial_misalignment/raw_data', pattern='.zip', all.files=TRUE, full.names=FALSE)

# 2: extract, clean & export data -----------------------------------------
for (zipped_folder in zipped_folders){

# Step 1: Define the zip file path 
zip_file_path <- paste0(read.data_folderpath,"/",zipped_folder)

# Step 2: Extract USAllCnty.txt file 

## List all files in the zipped folder
file_list_raw <- system(
  paste0('"C:/Program Files/7-Zip/7z.exe" l -p', Sys.getenv('NCHS_PW'), ' "', zip_file_path, '"'), 
  intern = TRUE
)

## Extract the name of the file that ends with "USAllCnty.txt"
if(!zip_file_path %in% c("MulT2024.USPS.AllCnty.zip")){
  
file_to_extract <- str_extract(
   string = file_list_raw[str_detect(file_list_raw,"USAllCnty\\.txt$|[Uu][Ss]\\.AllCnty\\.txt$|US\\.AllCounty\\.txt$")], 
   pattern = "\\S+\\.txt$")

} else if(zip_file_path %in% c("MulT2024.USPS.AllCnty.zip")){
  
  file_to_extract <- "Mort2024.US.AllCounty_r20251208"
}

print(file_to_extract)

## Extract the actual file to the temporary directory
temp_dir <- tempdir()  # temporary directory for extracted files

system(
  paste0('"C:/Program Files/7-Zip/7z.exe" e -p', Sys.getenv('NCHS_PW'),
         ' -o"', temp_dir, '" "', zip_file_path, '" "', file_to_extract, '"'),
  intern = TRUE
)

print(file_to_extract)
# Step 3: Execute JTC data cleaning to create aggregated data by year with
# necessary variables only (GEOID,race,ethnicity,sex/gender,age_cat,cause of death)

# # For 2003 - 2022 ------------------------------------------------
if(! zipped_folder %in% c("MULT2012.USPSAllCnty.zip","MULT2013.USPSAllCnty.zip","MULT2014.USPSAllCnty.zip",
                        "MULT2015.USPSAllCnty.zip","MULT2016.USPSAllCnty.zip","MULT2017.USPSAllCnty.zip",
                        "MULT2018.USPSAllCnty.zip","MULT2019.USPSAllCnty.zip","MULT2020.AllCnty.zip",
                        "MULT2021.AllCnty.zip" )){

mort_df <- read_fwf(file.path(temp_dir, basename(file_to_extract)),
                    fwf_cols(res = c(19, 19),
                             res_status = c(20, 20),
                             yearofdeath = c(102,105),
                             res_state_FIP = c(29,30),
                             res_statecountry_FIP = c(33,34),
                             res_county_FIP = c(35,37),
                             DateofDeath_month = c(65,66),
                             sex = c(69,69),
                             age27 = c(77, 78),
                             age12 = c(79,80),
                             uc = c(146, 149),
                             cause358 = c(150, 152),
                             cause113 = c(154, 156),
                             race5 = c(450,450), # no race data for 2021
                             hisp_origin = c(484, 486),
                             race_eth = c(488,488),
                             race40 = c(489,490))) |>
  mutate(race = case_when(
    race_eth %in% c(1,2,3,4,5) ~ "Hispanic",
    race_eth == 6 ~ "White Non-Hispanic",
    race_eth == 7 ~ "Black Non-Hispanic",
    race_eth == 8 & race5 == 3 ~ "AIAN Non-Hispanic",
    race_eth == 8 & race5 %in% c(4, 5) ~ "API Non-Hispanic", # Asian/Native Hawaiian or Other Pacific
    race_eth == 8 & race5 == 6 ~ "More than one race Non-Hispanic",
    TRUE ~ NA)) |>
  filter(res_status < 4) |>
  left_join(tidycensus::fips_codes, by = c("res_state_FIP" = "state",
                                           "res_county_FIP" = "county_code")) |>
  mutate(GEOID = paste0(state_code , res_county_FIP)) |>
  group_by(yearofdeath, GEOID, sex, race, age27, uc) |>
  summarise(count = n()) |>
  ungroup()

  } else {

  # For 2022 and later ------------------------------------------------
  mort_df <- read_fwf(file.path(temp_dir, basename(file_to_extract)),
                      fwf_cols(res = c(19, 19),
                               res_status = c(20, 20),
                               yearofdeath = c(102,105),
                               res_state_FIP = c(29,30),
                               res_statecountry_FIP = c(33,34),
                               res_county_FIP = c(35,37),
                               DateofDeath_month = c(65,66),
                               sex = c(69,69),
                               age27 = c(77, 78),
                               age12 = c(79,80),
                               uc = c(146, 149),
                               cause358 = c(150, 152),
                               cause113 = c(154, 156),
                               race6 = c(450,450), # race recode from 2022 and later
                               hisp_origin = c(484, 486),
                               race_eth = c(488,488), # hispanic origin/race recode from 2022 and later
                               race40 = c(489,490))) |>
    mutate(race = case_when(
      race_eth %in% c(1,2,3,4,5,6,7) ~ "Hispanic",
      race_eth == 8 ~ "White Non-Hispanic",
      race_eth == 9 ~ "Black Non-Hispanic",
      race_eth == 10 & race6 == 3 ~ "AIAN Non-Hispanic",
      race_eth == 11 & race6 == 4 ~ "API Non-Hispanic", # Asian
      race_eth == 12 & race6 == 5 ~ "API Non-Hispanic", # NHPI
      race_eth == 13 & race6 == 6 ~ "More than one race Non-Hispanic",
      TRUE ~ NA)) |>
    filter(res_status < 4) |>
    left_join(tidycensus::fips_codes, by = c("res_state_FIP" = "state",
                                             "res_county_FIP" = "county_code")) |>
    mutate(GEOID = paste0(state_code , res_county_FIP)) |>
    group_by(yearofdeath, GEOID, sex, race, age27, uc) |>
    summarise(count = n()) |>
    ungroup()

}

# save clean data
saveRDS(mort_df, file=paste0(save.data_folderpath,'/mort',str_extract(zipped_folder,"(?<=MULT)\\d{4}"),'_extract.RDS'))


# clear temporary directory 
unlink(temp_dir, recursive = TRUE)

}
