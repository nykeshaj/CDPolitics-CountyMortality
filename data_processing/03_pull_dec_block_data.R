library(sf)
library(tidycensus)
library(dplyr)
library(data.table)
library(tibble)
library(tidyr)
library(stringr)
library(magrittr)
library(purrr)
library(parallel)

# 1. Load 2010 & 2020 census block variables of interest -----------------------

# 2010 Decennial Census ---------------------------
vars2010 <- load_variables(2010, "sf1", cache = TRUE) # Load all available variables for the 2010 SF1

# All variables from P012 (Sex by Age)
p012_2010_vars <- vars2010 %>%
  filter(concept == "SEX BY AGE") %>%
  filter(stringr::str_detect(name,"^P012"))

p012_2010_vars_dict <-
  tibble::tribble(
    ~var,          ~shortname,      ~desc,
    "P012001", 'total_population',     "total population",
    "P012002", 'total_male', "total male population",
    "P012003", 'male_under5', "male population under 5 yrs",
    "P012004", 'male5_9', "male population aged 5-9 yrs",
    "P012005", 'male10_14', "male population aged 10-14 yrs",
    "P012006", 'male15_17', "male population aged 15-17 yrs",
    "P012007", 'male18_19', "male population aged 18-19 yrs",
    "P012008", 'male20', "male population aged 20 yrs",
    "P012009", 'male21', "male population aged 21 yrs",
    "P012010", 'male22_24', "male population aged 22-24 yrs",
    "P012011", 'male25_29', "male population aged 25-29 yrs",
    "P012012", 'male30_34', "male population aged 30-34 yrs",
    "P012013", 'male35_39', "male population aged 35-39 yrs",
    "P012014", 'male40_44', "male population aged 40-44 yrs",
    "P012015", 'male45_49', "male population aged 45-49 yrs",
    "P012016", 'male50_54', "male population aged 50-54 yrs",
    "P012017", 'male55_59', "male population aged 55-59 yrs",
    "P012018", 'male60_61', "male population aged 60-61 yrs",
    "P012019", 'male62_64', "male population aged 62-64 yrs",
    "P012020", 'male65_66', "male population aged 65-66 yrs",
    "P012021", 'male67_69', "male population aged 67-69 yrs",
    "P012022", 'male70_74', "male population aged 70-74 yrs",
    "P012023", 'male75_79', "male population aged 75-79 yrs",
    "P012024", 'male80_84', "male population aged 80-84 yrs",
    "P012025", 'male_over85', "male population aged 85+ yrs",

    "P012026", 'total_female', "total female population",
    "P012027", 'female_under5', "female population under 5 yrs",
    "P012028", 'female5_9', "female population aged 5-9 yrs",
    "P012029", 'female10_14', "female population aged 10-14 yrs",
    "P012030", 'female15_17', "female population aged 15-17 yrs",
    "P012031", 'female18_19', "female population aged 18-19 yrs",
    "P012032", 'female20', "female population aged 20 yrs",
    "P012033", 'female21', "female population aged 21 yrs",
    "P012034", 'female22_24', "female population aged 22-24 yrs",
    "P012035", 'female25_29', "female population aged 25-29 yrs",
    "P012036", 'female30_34', "female population aged 30-34 yrs",
    "P012037", 'female35_39', "female population aged 35-39 yrs",
    "P012038", 'female40_44', "female population aged 40-44 yrs",
    "P012039", 'female45_49', "female population aged 45-49 yrs",
    "P012040", 'female50_54', "female population aged 50-54 yrs",
    "P012041", 'female55_59', "female population aged 55-59 yrs",
    "P012042", 'female60_61', "female population aged 60-61 yrs",
    "P012043", 'female62_64', "female population aged 62-64 yrs",
    "P012044", 'female65_66', "female population aged 65-66 yrs",
    "P012045", 'female67_69', "female population aged 67-69 yrs",
    "P012046", 'female70_74', "female population aged 70-74 yrs",
    "P012047", 'female75_79', "female population aged 75-79 yrs",
    "P012048", 'female80_84', "female population aged 80-84 yrs",
    "P012049", 'female_over85', "female population aged 85+ yrs"
  )

# 2020 Decenniel Census ---------------------------
vars2020 <- load_variables(2020, "dhc", cache = TRUE) # Load all available variables for the 2010 SF1

# All variables from P12 (Sex by Age)
p12_2020_vars <- vars2020 |>
  dplyr::filter(stringr::str_detect(name, "^P12_"))

p12_2020_vars_dict <-
  tibble::tribble(
    ~var,          ~shortname,      ~desc,
    "P12_001N", 'total_population',     "total population",
    "P12_002N", 'total_male', "total male population",
    "P12_003N", 'male_under5', "male population under 5 yrs",
    "P12_004N", 'male5_9', "male population aged 5-9 yrs",
    "P12_005N", 'male10_14', "male population aged 10-14 yrs",
    "P12_006N", 'male15_17', "male population aged 15-17 yrs",
    "P12_007N", 'male18_19', "male population aged 18-19 yrs",
    "P12_008N", 'male20', "male population aged 20 yrs",
    "P12_009N", 'male21', "male population aged 21 yrs",
    "P12_010N", 'male22_24', "male population aged 22-24 yrs",
    "P12_011N", 'male25_29', "male population aged 25-29 yrs",
    "P12_012N", 'male30_34', "male population aged 30-34 yrs",
    "P12_013N", 'male35_39', "male population aged 35-39 yrs",
    "P12_014N", 'male40_44', "male population aged 40-44 yrs",
    "P12_015N", 'male45_49', "male population aged 45-49 yrs",
    "P12_016N", 'male50_54', "male population aged 50-54 yrs",
    "P12_017N", 'male55_59', "male population aged 55-59 yrs",
    "P12_018N", 'male60_61', "male population aged 60-61 yrs",
    "P12_019N", 'male62_64', "male population aged 62-64 yrs",
    "P12_020N", 'male65_66', "male population aged 65-66 yrs",
    "P12_021N", 'male67_69', "male population aged 67-69 yrs",
    "P12_022N", 'male70_74', "male population aged 70-74 yrs",
    "P12_023N", 'male75_79', "male population aged 75-79 yrs",
    "P12_024N", 'male80_84', "male population aged 80-84 yrs",
    "P12_025N", 'male_over85', "male population aged 85+ yrs",

    "P12_026N", 'total_female', "total female population",
    "P12_027N", 'female_under5', "female population under 5 yrs",
    "P12_028N", 'female5_9', "female population aged 5-9 yrs",
    "P12_029N", 'female10_14', "female population aged 10-14 yrs",
    "P12_030N", 'female15_17', "female population aged 15-17 yrs",
    "P12_031N", 'female18_19', "female population aged 18-19 yrs",
    "P12_032N", 'female20', "female population aged 20 yrs",
    "P12_033N", 'female21', "female population aged 21 yrs",
    "P12_034N", 'female22_24', "female population aged 22-24 yrs",
    "P12_035N", 'female25_29', "female population aged 25-29 yrs",
    "P12_036N", 'female30_34', "female population aged 30-34 yrs",
    "P12_037N", 'female35_39', "female population aged 35-39 yrs",
    "P12_038N", 'female40_44', "female population aged 40-44 yrs",
    "P12_039N", 'female45_49', "female population aged 45-49 yrs",
    "P12_040N", 'female50_54', "female population aged 50-54 yrs",
    "P12_041N", 'female55_59', "female population aged 55-59 yrs",
    "P12_042N", 'female60_61', "female population aged 60-61 yrs",
    "P12_043N", 'female62_64', "female population aged 62-64 yrs",
    "P12_044N", 'female65_66', "female population aged 65-66 yrs",
    "P12_045N", 'female67_69', "female population aged 67-69 yrs",
    "P12_046N", 'female70_74', "female population aged 70-74 yrs",
    "P12_047N", 'female75_79', "female population aged 75-79 yrs",
    "P12_048N", 'female80_84', "female population aged 80-84 yrs",
    "P12_049N", 'female_over85', "female population aged 85+ yrs"
  )


# 2. Create some helper functions for actually pulling data -----

# function to pull decennial block data by county
pull_decennial_block_data <- function(st.abb, county.fips, yr) {

  if (yr == 2020) {
    vars <- p12_2020_vars_dict$var
    sumfile <- "dhc"

  } else if (yr == 2010) {
    vars <- p012_2010_vars_dict$var
    sumfile <- "sf1"

  } else {
    stop("yr input must be either 2010 or 2020")
  }

  tryCatch(
    {
      dc_blocks <- NULL
      dc_blocks <- tidycensus::get_decennial(
        geography = "block",
        state = st.abb,
        county = county.fips,
        variables = vars,
        year = yr,
        sumfile = sumfile,
        geometry = FALSE
      )
    },
    error = function(e) {
      print(e)
    }

  )

  return(dc_blocks)
}

pull_state_block_data <- function(st.abb, yr) {

  if (!yr %in% c(2010, 2020)) {
    stop("yr must be either 2010 or 2020")
  }

  if (yr == 2020) {
    variable_dict <- p12_2020_vars_dict
  } else {
    variable_dict <- p012_2010_vars_dict
  }

  # Obtain counties valid for the requested Census year
  county_codes <- tigris::counties(
    state = st.abb,
    year = yr,
    cb = TRUE,
    class = "sf"
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      county_code = COUNTYFP,
      county = NAME
    ) %>%
    dplyr::distinct()

  # Pull each county; skip only individual failures
  county_results <- purrr::map2(
    county_codes$county_code,
    county_codes$county,
    \(county_fips, county_name) {

      message(
        "Pulling ", yr, " blocks for ",
        county_name, ", ", st.abb
      )

      tryCatch(
        {
          pull_decennial_block_data(
            st.abb = st.abb,
            county.fips = county_fips,
            yr = yr
          )
        },
        error = function(e) {
          warning(
            "Skipping ", county_name,
            " (", st.abb, "-", county_fips, "): ",
            conditionMessage(e),
            call. = FALSE
          )

          NULL
        }
      )
    }
  )

  names(county_results) <- county_codes$county

  county_results <- purrr::compact(county_results)

  if (length(county_results) == 0) {
    warning(
      "No block data were returned for ", st.abb,
      " in ", yr,
      call. = FALSE
    )

    return(NULL)
  }

  county_results <- dplyr::bind_rows(
    county_results,
    .id = "county_name"
  )

  county_results <- county_results %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = value
    )

  rename_vars <- stats::setNames(
    variable_dict$var,
    variable_dict$shortname
  )

  county_results <- county_results %>%
    dplyr::rename(
      dplyr::all_of(rename_vars)
    ) %>%
    dplyr::mutate(
      under5_pop =
        male_under5 +
        female_under5,

      under65_pop = rowSums(
        dplyr::pick(
          male_under5:male62_64,
          female_under5:female62_64
        ),
        na.rm = FALSE
      )
    ) %>%
    dplyr::select(
      GEOID,
      NAME,
      total_pop = total_population,
      female_pop = total_female,
      under5_pop,
      under65_pop
    )

  county_results
}

pull_batch_of_states <- function(lst, yr) {
  purrr::map(lst, ~pull_state_block_data(st.abb=.x, yr)) |>
    purrr::reduce(rbind)
}

# 3. Batch states to setup for parallel compute -------

# state batches: four workers so four batches
st_batch1 <- c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN")
st_batch2 <- c("IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT")
st_batch3 <- c("NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA")
st_batch4 <- c("RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

state_batches <- list(st_batch1, st_batch2, st_batch3, st_batch4)

# 4. Setup cluster --------------

### Grab data from 2010
# set up cluster
cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(sf)
  library(tidycensus)
  library(dplyr)
  library(data.table)
  library(tibble)
  library(tidyr)
  library(stringr)
  library(magrittr)
  library(purrr)
})

clusterExport(cl, c("p012_2010_vars_dict", "p12_2020_vars_dict",
                    "pull_decennial_block_data",
                    "pull_state_block_data"))


# 5. Run tidycensus in parallel --------------

# 2010
all_blocks_2010 <- parLapply(cl, state_batches,
                             pull_batch_of_states, yr=2010) |>
  reduce(rbind)
saveRDS(all_blocks_2010, '../block_data/decennial_only/block2010.rds')

rm(all_blocks_2010) # save memory, these are big!

# 2020
all_blocks_2020 <- parLapply(cl, state_batches,
                             pull_batch_of_states, yr=2020) |>
  reduce(rbind)
saveRDS(all_blocks_2020, '../block_data/decennial_only/block2020.rds')

