library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(parallel)

### 1. Create helper functions, constants

create_atoms <- function(state, county.year, cd.year) {

  # Get shapes
  cnty.shp <- tigris::counties(state=state, year=county.year) |>
    select(GEOID.CTY = GEOID,
           NAMELSAD.CTY = NAMELSAD)

  cd.shp <- NA
  if (cd.year == 2022) {
    cd.shp <- tigris::congressional_districts(state=state, year=cd.year) |>
      select(GEOID.CD = GEOID20,
             NAMELSAD.CD = NAMELSAD20)
  } else {
    cd.shp <- tigris::congressional_districts(state=state, year=cd.year) |>
      select(GEOID.CD = GEOID,
             NAMELSAD.CD = NAMELSAD)
  }

  st_make_valid(st_intersection(cnty.shp, cd.shp))
}

create_atoms_batch <- function(states, county.year, cd.year) {
  map(states, ~ create_atoms(.x, county.year, cd.year)) |>
    reduce(rbind)
}

# state batches: four workers so four batches
state_batches <- list(
  c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN"),
  c("IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT"),
  c("NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA"),
  c("RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
)

### 2. Setup a cluster to do this in parallel

cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
  library(stringr)
  library(purrr)
})

clusterExport(cl, c("create_atoms", "create_atoms_batch"))

### 3. Run parallel across all four batches, once per time period set.

# 2013 CD / 2015 counties:
atoms15 <- parLapply(cl,
                     state_batches,
                     create_atoms_batch,
                     county.year = 2015,
                     cd.year = 2013) |>
  reduce(rbind)
saveRDS(atoms15, here('data', 'atom_datasets', 'atoms15.rds'))
rm(atoms15)

# 2016 CD / 2019 counties:
atoms19 <- parLapply(cl,
                     state_batches,
                     create_atoms_batch,
                     county.year = 2019,
                     cd.year = 2016) |>
  reduce(rbind)
saveRDS(atoms19, here('data', 'atom_datasets', 'atoms19.rds'))
rm(atoms19)

# 2022 CD / 2024 counties:
atoms24 <- parLapply(cl,
                     state_batches,
                     create_atoms_batch,
                     county.year = 2024,
                     cd.year = 2022) |>
  reduce(rbind)
saveRDS(atoms24, here('data', 'atom_datasets', 'atoms24.rds'))
rm(atoms24)

### 4. Stop cluster

stopCluster(cl)
