load_acs_pop_estimates<- function(yr){
  
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
  variables <- tidycensus::load_variables(yr, "acs5", cache = TRUE)
  
  # get specific variable definitions
  variables %<>% filter(name %in% age_variables)
  
  # parse estimate labels
  variables %<>% tidyr::separate(label, into = c('estimate_label', 'total_label', 'sex_gender', 'age_group'), sep = '!!')
  variables %<>% select(-estimate_label, -total_label, -concept)
  variables$sex_gender %<>% stringr::str_remove_all(":")
  
  # join in the variable labels
  popsizes %<>% left_join(variables, by = c('variable' = 'name'))
  female_popsizes <- popsizes %>% filter(sex_gender == "Female") # create female only for the breast cancer denom
  
  # aggregate sex/gender for total
  total_popsizes <- popsizes %<>% 
    group_by(GEOID, NAME, age_group) %>%
    summarize(
      estimate = sum(estimate),
      moe = tidycensus::moe_sum(estimate, moe))
  
  female_popsizes <- female_popsizes %<>% 
    group_by(GEOID, NAME, age_group) %>%
    summarize(
      estimate = sum(estimate),
      moe = tidycensus::moe_sum(estimate, moe))
  
  # assign age bands
  total_popsizes %<>% mutate(
    age_group = case_when(
      age_group == 'Under 5 years' ~ '<5',
      age_group %in% c('5 to 9 years', '10 to 14 years') ~ '5-14',
      age_group %in% c('15 to 17 years', '18 and 19 years', '20 years', '21 years', '22 to 24 years') ~ '15-24',
      age_group %in% c('25 to 29 years', '30 to 34 years') ~ '25-34',
      age_group %in% c('35 to 39 years', '40 to 44 years') ~ '35-44',
      age_group %in% c('45 to 49 years', '50 to 54 years') ~ '45-54',
      age_group %in% c('55 to 59 years', '60 and 61 years', '62 to 64 years') ~ '55-64',
      age_group %in% c('65 and 66 years', '67 to 69 years', '70 to 74 years') ~ '65-74',
      age_group %in% c('75 to 79 years', '80 to 84 years') ~ '75-84',
      age_group == '85 years and over' ~ '85+'
    ))
  
  female_popsizes %<>% mutate(
    age_group = case_when(
      age_group == 'Under 5 years' ~ '<5',
      age_group %in% c('5 to 9 years', '10 to 14 years') ~ '5-14',
      age_group %in% c('15 to 17 years', '18 and 19 years', '20 years', '21 years', '22 to 24 years') ~ '15-24',
      age_group %in% c('25 to 29 years', '30 to 34 years') ~ '25-34',
      age_group %in% c('35 to 39 years', '40 to 44 years') ~ '35-44',
      age_group %in% c('45 to 49 years', '50 to 54 years') ~ '45-54',
      age_group %in% c('55 to 59 years', '60 and 61 years', '62 to 64 years') ~ '55-64',
      age_group %in% c('65 and 66 years', '67 to 69 years', '70 to 74 years') ~ '65-74',
      age_group %in% c('75 to 79 years', '80 to 84 years') ~ '75-84',
      age_group == '85 years and over' ~ '85+'
    ))
  
  # sum up age groups within 10-year age bands
  total_popsizes %<>% group_by(GEOID, age_group) %>%
    summarize(estimate = sum(estimate))
  
  female_popsizes %<>% group_by(GEOID, age_group) %>%
    summarize(estimate = sum(estimate))
  
  # make the age groups a factor with ordered levels
  total_popsizes$age_group %<>% factor(levels = c('<5','5-14','15-24', '25-34','35-44','45-54','55-64','65-74','75-84','85+'))
  female_popsizes$age_group %<>% factor(levels = c('<5','5-14','15-24', '25-34','35-44','45-54','55-64','65-74','75-84','85+'))
  
  ## remove NAs (these are total population*2)
  total_popsizes<-subset(total_popsizes,!is.na(age_group))
  female_popsizes<-subset(female_popsizes,!is.na(age_group))
  
  total_popsizes <- total_popsizes %>% left_join(female_popsizes, by = c("GEOID","age_group"), suffix = c(".total",".female"))
  
}
