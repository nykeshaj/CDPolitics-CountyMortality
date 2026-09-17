library(sf)
library(tigris)
library(dplyr)
library(purrr)
library(here)
library(parallel)

### 1. Define a concerning number of helper functions --------------------------


## Functions which get centroids

block_centroids <- function(county.geoid, blocks.year) {

  state <- substr(county.geoid, 1, 2)
  county <- substr(county.geoid, 3, 5)

  if (blocks.year >= 2020) {
    centroids <- blocks(state=state, county=county, year=blocks.year) |>
      st_centroid() |>
      select(GEOID20)
  } else {
    centroids <- blocks(state=state, county=county, year=blocks.year) |>
      st_centroid() |>
      select(GEOID10)
  }

  centroids
}

bg_centroids <- function(county.geoid, bg.year) {

  state <- substr(county.geoid, 1, 2)
  county <- substr(county.geoid, 3, 5)

  if (bg.year >= 2020) {
    centroids <- block_groups(state=state, county=county, year=bg.year) |>
      st_centroid() |>
      select(GEOID20)
  } else {
    centroids <- block_groups(state=state, county=county, year=bg.year) |>
      st_centroid() |>
      select(GEOID10)
  }

  centroids
}


## Functions which assign blocks, block groups to atoms

assign_blocks_to_atoms <- function(county.geoid, blocks.year, atoms) {
  blocks <- block_centroids(county.geoid, blocks.year)
  rel.atoms <- filter(atoms, GEOID.CTY==county.geoid)

  assigned <- blocks |>
    st_join(
      rel.atoms,
      join=st_within
    ) |>
    st_drop_geometry()

  assigned
}

assign_bg_to_atoms <- function(county.geoid, bg.year, atoms) {
  bgs <- bg_centroids(county.geoid, bg.year)
  rel.atoms <- filter(atoms, GEOID.CTY==county.geoid)

  assigned <- blocks |>
    st_join(
      rel.atoms,
      join=st_within
    ) |>
    st_drop_geometry()

  assigned
}


## Functions which create crosswalks

create_blocks_cw <- function(states, blocks.year, atoms) {

  # create the block:atom crosswalk table for a single state and year
  create_block_atom_cw_state <- function(state, blocks.year, atoms) {

    # get the full list of counties in the state for that year
    counties.fips <- (tigris::counties(state=state, year=blocks.year) |>
                        st_drop_geometry())$GEOID

    cw <- counties.fips |>
      map(~ assign_blocks_to_atoms(.x, blocks.year, atoms)) |>
      reduce(rbind)

    cw
  }

  cw <- states |>
    map(~ create_block_atom_cw_state(state=.x, blocks.year, atoms)) |>
    reduce(rbind)

  cw
}

create_bg_cw <- function(states, bg.year, atoms) {

  # create the block:atom crosswalk table for a single state and year
  create_bg_atom_cw_state <- function(state, bg.year, atoms) {

    # get the full list of counties in the state for that year
    counties.fips <- (tigris::counties(state=state, year=bg.year) |>
                        st_drop_geometry())$GEOID

    cw <- counties.fips |>
      map(~ assign_bg_to_atoms(.x, bg.year, atoms)) |>
      reduce(rbind)

    cw
  }

  cw <- states |>
    map(~ create_bg_atom_cw_state(state=.x, bg.year, atoms)) |>
    reduce(rbind)

  cw
}


### 2. Setup for parallel compute ----------------------------------------------

state_batches <- list(
  c("AL","AZ","AR","CA","CO","CT","DE","FL","GA","ID","IL","IN"),
  c("IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT"),
  c("NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA"),
  c("RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
)

cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tigris)
})

clusterExport(cl,
              c("block_centroids",
              "bg_centroids",
              "assign_blocks_to_atoms",
              "assign_bg_to_atoms")
              )


### 3. Compute crosswalks for all sensible block:atom pairs --------------------


## 2010 blocks to atoms15

save_loc <- here('data', 'crosswalks', 'blocks10_atoms15.rds')
if (! file.exists(save_loc)) {
  atoms15 <- readRDS(here('data','atom_datasets', 'atoms15.rds'))
  blocks10_atoms15_cw <- parLapply(cl,
                                   state_batches,
                                   create_blocks_cw,
                                   blocks.year=2010,
                                   atoms=atoms15) |>
    reduce(rbind)

  saveRDS(blocks10_atoms15_cw, save_loc)
  rm(atoms15)
  rm(blocks10_atoms15_cw)
}


## 2010 blocks to atoms19

save_loc <- here('data', 'crosswalks', 'blocks10_atoms19.rds')
if (! file.exists(save_loc)) {
  atoms19 <- readRDS(here('data','atom_datasets','atoms19.rds'))
  blocks10_atoms19_cw <- parLapply(cl,
                                   state_batches,
                                   create_blocks_cw,
                                   blocks.year=2010,
                                   atoms=atoms19) |>
    reduce(rbind)

  saveRDS(blocks10_atoms19_cw, save_loc)
  rm(blocks10_atoms19_cw)
  rm(atoms19)
}


## 2020 blocks to atoms19

save_loc <- here('data', 'crosswalks', 'blocks20_atoms19.rds')
if (!file.exists(save_loc)) {
  atoms19 <- readRDS(here('data','atom_datasets','atoms19.rds'))
  blocks20_atoms19_cw <- parLapply(cl,
                                   state_batches,
                                   create_blocks_cw,
                                   blocks.year=2020,
                                   atoms=atoms19) |>
    reduce(rbind)

  saveRDS(blocks20_atoms19_cw, save_loc)
  rm(atoms19)
  rm(blocks20_atoms19_cw)
}

## 2020 blocks to atoms24
save_loc <- here('data', 'crosswalks', 'blocks20_atoms24.rds')
if (!file.exists(save_loc))  {
  atoms24 <- readRDS('data/atom_datasets/atoms24.rds')
  blocks20_atoms19_cw <- parLapply(cl,
                                   state_batches,
                                   create_blocks_cw,
                                   blocks.year=2024,
                                   atoms=atoms24) |>
    reduce(rbind)


  saveRDS(blocks20_atoms24_cw, save_loc)
  rm(atoms24)
  rm(blocks20_atoms24_cw)
}

### 5. Compute crosswalks for all sensible blockgroup:atom pairs ---------------


## 2016 bgs to atoms15

save_loc <- here('data', 'crosswalks', 'bgs16_atoms15.rds')
if (!file.exists(save_loc))  {
  atoms15 <- readRDS(here('data', 'atom_datasets', 'atoms15.rds'))
  bgs16_atoms15_cw <- parLapply(cl,
                                state_batches,
                                create_bg_cw,
                                bg.year=2016,
                                atoms=atoms15) |>
    reduce(rbind)

  saveRDS(bgs16_atoms15_cw, save_loc)
  rm(bgs16_atoms15_cw)
  rm(atoms15)
}


## 2020 bgs to atoms19

save_loc <- here('data', 'crosswalks', 'bgs20_atoms19.rds')
if (!file.exists(save_loc)) {
  atoms19 <- readRDS(here('data', 'atom_datasets', 'atoms19.rds'))
  bgs20_atoms19_cw <- parLapply(cl,
                                state_batches,
                                create_bg_cw,
                                bg.year=2020,
                                atoms=atoms19) |>
    reduce(rbind)

  saveRDS(bgs20_atoms19_cw, save_loc)
  rm(bgs20_atoms19_cw)
  rm(atoms19)
}


## 2024 bgs to atoms24

save_loc <- here('data', 'crosswalks', 'bgs24_atoms24.rds')
if (!file.exists(save_loc)) {
  atoms24 <- readRDS(here('data', 'atom_datasets', 'atoms24.rds'))
  bgs24_atoms24_cw <- parLapply(cl,
                                state_batches,
                                create_bg_cw,
                                bg.year=2024,
                                atoms=atoms24) |>
    reduce(rbind)

  saveRDS(bgs24_atoms24_cw, save_loc)
  rm(bgs24_atoms24_cw)
  rm(atoms24)
}


### 5. Stop the cluster --------------------------------------------------------

stopCluster(cl)
