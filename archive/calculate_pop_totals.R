# install.packages("pak")
# library(pak)
# pak::pkg_install("ropensci/USAboundaries")

library(tidycensus)
library(ggplot2)
library(USAboundaries)
library(sf)
library(tibble)
library(tidyr)
library(dplyr)
library(stringr)
library(magrittr)

get_acs_pop_data <- function(yr){
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

## merge with the other county data ##
# county<-merge(acs_data,popsizes,by='geoid')
county <- popsizes

## merge with county shapefile ##
county_shp<-us_counties()
county_shp<-subset(county_shp,!(state_abbr%in% c('AK','HI','DC','PR','GU','VI','AS','MP')))
county_shp<-county_shp[,c('geoid','aland','geometry')]

county<-merge(county_shp,county,by='geoid')
}

pop_2015 <- get_acs_pop_data(2015)
pop_2019 <- get_acs_pop_data(2019)
pop_2024 <- get_acs_pop_data(2024)

saveRDS(pop_2015,paste0(analysis_dir,"/clean_data/county_acs_pop_2015"))
saveRDS(pop_2019,paste0(analysis_dir,"/clean_data/county_acs_pop_2019"))
saveRDS(pop_2024,paste0(analysis_dir,"/clean_data/county_acs_pop_2024"))

## add in the county-level covid 19 death data (this comes from Lancet paper, before re-allocation to CDs) ##
# covid<-readRDS('county_deaths_imputed.rds')
# 
# covid<-subset(covid,age_group=='all_ages' & period=="apr21_to_mar22")
# 
# county<-merge(county,covid[,c('county_fips','deaths')],by.x='geoid',by.y='county_fips')
# 
# ## export data
# save(county,file='county_data.RData')