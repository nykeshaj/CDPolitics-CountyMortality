library(tidycensus)
library(dplyr)
library(data.table)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)
library(parallel)


# 1. Demarcate 2016, 2020, and 2024 ACS 5 Year variables of interest ------

vars.dict <- tribble(
  ~var,     ~shortname,    ~desc,
  "B03002_001", "pop_race_denom", "total population in the race table",
  "B03002_003", "white_nh_pop",   "non hispanic white population",
  "B03002_004", "pop_black_nh",   "non hispanic black population",
  "B03002_014", "pop_black_h",    "hispanic black population",
  "B03002_006", "pop_asian_nh",   "non hispanic asian population",
  "B03002_016", "pop_asian_h",    "hispanic asian population",
  "B03002_005", "pop_naan_nh",    "non-hispanic native american or alaskan antive population",
  "B03002_015", "pop_naan_h",     "hispanic native american or alaskan native population",
  "B03002_012", "hisp_pop",       "hispanic population",
  "C17002_001", "pop_pov_denom",  "population for whom porverty status is determined",
  "C17002_002", "pop_icr_und_pt_5", "population with a ratio of income in the past 12 months to poverty of less than 1/2",
  "C17002_003", "pop_icr_btwn_.5_.99", "population with a ratio of income in the past 12 months to poverty between .5 and .99",
)

# 2. Get FIPS codes for all counties in 2016, 2020, and 2024. -----

# we are only interested in the continental United States,
# not Alaska, Hawaii, D.C., or territories.
excluded.fips <- c("02", "15", "60", "61", "72", "11",
                   "66", "67", "71", "72", "73", "74",
                   "75", "76", "77", "78", "79")

geoid.2016 <- tigris::counties(year=2016) |>
  sf::st_drop_geometry() |>
  filter(! STATEFP %in% excluded.fips) |>
  mutate(GEOID=str_c(STATEFP, COUNTYFP)) |>
  select(GEOID)

geoid.2020 <- tigris::counties(year=2020) |>
  sf::st_drop_geometry() |>
  filter(! STATEFP %in% excluded.fips) |>
  mutate(GEOID=str_c(STATEFP, COUNTYFP)) |>
  select(GEOID)

geoid.2024 <- tigris::counties(year=2024) |>
  sf::st_drop_geometry() |>
  filter(! STATEFP %in% excluded.fips) |>
  mutate(GEOID=str_c(STATEFP, COUNTYFP)) |>
  select(GEOID)

# 3. Make helper functions for pulling data -----

pull_county_bg_data <- function(county.geoid, year) {
  if (!year %in% c(2016, 2020, 2024)) {
    stop("year should be 2016, 2020, or 2024")
  }

  state.fips = substr(county.geoid, 1, 2)
  county.fips = substr(county.geoid, 3, 5)

  # Use a try catch on the off chance our data throws
  # an error
  bg.data <- NULL
  tryCatch(
    {
        bg.data <- tidycensus::get_acs(
        geography = 'cbg',
        state = state.fips,
        county = county.fips,
        variables = vars.dict$var,
        year=year,
        geometry=FALSE
      )
    },
    error = function(e) {
      print(e)
      print(county.geoid)
    }
  )

  return(bg.data)
}

clean_county_bg_data <- function(bg.data) {
  rename.vars <- stats::setNames(vars.dict$var, vars.dict$shortname)


  bg.data <- bg.data |>
    select(GEOID, variable, estimate) |>
    pivot_wider(
      names_from=variable,
      values_from=estimate,
      id_cols=c("GEOID")
    ) |>
    rename(
      all_of(rename.vars)
    ) |>
    mutate(
      black_pop = pop_black_nh + pop_black_h,
      asian_pop = pop_asian_nh + pop_asian_h,
      naan_pop  = pop_naan_nh  + pop_naan_h ,
      pov_pop   = pop_icr_und_pt_5 + pop_icr_btwn_.5_.99
    ) |>
    select(
      GEOID,
      pop_race_denom,
      pop_pov_denom,
      white_nh_pop,
      black_pop,
      asian_pop,
      pov_pop
    )

  return(bg.data)
}

pull_and_clean_county <- function(county.geoid, year) {
  bg.data <- pull_county_bg_data(county.geoid, year)

  bg.data <- clean_county_bg_data(bg.data)

  return(bg.data)
}

handle_batch <- function(county.geoids, year) {
  purrr::map(county.geoids, ~ pull_and_clean_county(.x, year)) |>
    reduce(rbind)
}

# 4. Prepare clusters for parallel requesting -----

## Create equal sized batches for parallel processing
## and separate out stragglers

## TODO: Figure out if these ACTUALLY have to be the same size or not.

cuts <- cut(1:nrow(geoid.2016), # 1:3140(==nrow(geoid.2016)), divides by 4
            breaks=4,
            labels=FALSE)

geoid.2016.batches <- unname(split(geoid.2016$GEOID, cuts))

geoid.2020.batches <- unname(split(geoid.2020$GEOID[1:3140], cuts))
geoid.2020.additional <- geoid.2020$GEOID[3141] # one straggler

geoid.2024.batches <- unname(split(geoid.2024$GEOID[1:3140], cuts))
geoid.2024.additional <- geoid.2024$GEOID[3141:3142] # two stragglers

# Create cluster and populate it with the relevant things

cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(tidycensus)
  library(dplyr)
  library(data.table)
  library(tibble)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(parallel)
  library(magrittr)
})
clusterExport(cl,
              c(
                "vars.dict",
                "pull_county_bg_data",
                "clean_county_bg_data",
                "pull_and_clean_county"
              ))

# 5. Grab 2016 batches in parallel --------------

bg.data.2016 <- parLapply(cl,
                          geoid.2016.batches,
                          handle_batch,
                          year=2016) |>
  reduce(rbind)

# Save our work
saveRds(bg.data.2016, '../block_data/acs_data/bg2016.rds')

# Save RAM
rm(bg.data.2016)
rm(geoid.2016.batches)
rm(geoid.2016)

# 6. Grab 2020 batches in parallel

bg.data.2020 <- parLapply(cl,
                          geoid.2020.batches,
                          handle_batch,
                          year=2020) |>
  reduce(rbind)

# Add the additional value
bg.data.2020 <- c(
    bg.data.2020,
    pull_and_clean_county(geoid.2020.additional, 2020)
  ) |>
  reduce(rbind)

# Save our work
saveRds(bg.data.2020, '../block_data/acs_data/bg2020.rds')

# Save RAM
rm(bg.data.2020)
rm(geoid.2020.batches)
rm(geoid.2020)

### 7. Grab 2024 batches in parallel -----------

bg.data.2024 <- parLapply(cl,
                          geoid.2024.batches,
                          handle_batch,
                          year=2024) |>
  reduce(rbind)

# Add the additional value

bg.data.2024 <- c(
    bg.data.2024,
    handle_batch(geoid.2024.additional, 2024)
  ) |>
  reduce(rbind)

# Save our work
saveRds(bg.data.2024, '../block_data/acs_data/bg2024.rds')

# 8. Stop the clusters ------------

stopCluster(cl)

