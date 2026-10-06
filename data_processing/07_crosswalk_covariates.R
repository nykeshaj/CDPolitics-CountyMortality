library(dplyr)
library(purrr)
library(magrittr)
library(stringr)
library(here)

###################################
##### 1. Create 2015 atoms dataset.
###################################

# blocks
blocks10_atoms15 <- here('data', 'crosswalks', 'blocks10_atoms15.rds') |>
  readRDS() |>
  mutate(GEOID.ATOM = str_c(GEOID.CTY, '.', GEOID.CD)) |>
  select(GEOID.ATOM, GEOID=GEOID10)


blocks2010 <- readRDS(
  here('data', 'block_data', 'decennial_only', 'block2010.rds')
  )

atoms2015 <- blocks2010 |>
  merge(blocks10_atoms15, by='GEOID') |>
  select(-GEOID, -NAME) |>
  group_by(GEOID.ATOM) |>
  summarize(total_pop = sum(total_pop),
            female_pop = sum(female_pop),
            under5_pop = sum(under5_pop),
            under65_pop = sum(under65_pop))

rm(blocks10_atoms15)
rm(blocks2010)

# block groups
bg16_15_cw <- readRDS(
    here('data', 'crosswalks', 'bgs16_atoms15.rds')
  ) |>
  mutate(GEOID.ATOM = str_c(GEOID.CTY, '.', GEOID.CD)) |>
  select(GEOID.ATOM, GEOID)

bgs2016 <- readRDS(
  here('data', 'block_data', 'acs_data', 'bg2016.rds')
)


atoms2015.bgs <- bgs2016 |>
  merge(bg16_15_cw, by='GEOID') |>
  select(-GEOID) |>
  group_by(GEOID.ATOM) |>
  summarize(pop_race_denom = sum(pop_race_denom),
            pop_pov_denom = sum(pop_pov_denom),
            white_nh_pop = sum(white_nh_pop),
            black_pop = sum(black_pop),
            asian_pop = sum(asian_pop),
            pov_pop = sum(pov_pop),
            naan_pop = sum(naan_pop),
            hisp_pop = sum(hisp_pop),)
rm(bgs2016)
rm(bg16_15_cw)

atoms2015 <- merge(atoms2015, atoms2015.bgs, by='GEOID.ATOM')
rm(atoms2015.bgs)

saveRDS(atoms2015, here('data', 'atom_datasets', 'covariates', 'atoms15.rds'))
rm(atoms2015)

###################################
#### 2. Create 2019 atoms dataset (use 2010 blocks)
###################################


# blocks
blocks10_atoms19 <- here('data', 'crosswalks', 'blocks10_atoms19.rds') |>
  readRDS() |>
  mutate(GEOID.ATOM = str_c(GEOID.CTY, '.', GEOID.CD)) |>
  select(GEOID.ATOM, GEOID=GEOID10)


blocks2010 <- readRDS(
  here('data', 'block_data', 'decennial_only', 'block2010.rds')
)

atoms2019 <- blocks2010 |>
  merge(blocks10_atoms19, by='GEOID') |>
  select(-GEOID, -NAME) |>
  group_by(GEOID.ATOM) |>
  summarize(total_pop = sum(total_pop),
            female_pop = sum(female_pop),
            under5_pop = sum(under5_pop),
            under65_pop = sum(under65_pop))

rm(blocks10_atoms19)
rm(blocks2010)

# block groups
bg20_19_cw <- readRDS(
  here('data', 'crosswalks', 'bgs20_atoms19.rds')
) |>
  mutate(GEOID.ATOM = str_c(GEOID.CTY, '.', GEOID.CD)) |>
  select(GEOID.ATOM, GEOID)

bgs2020 <- readRDS(
  here('data', 'block_data', 'acs_data', 'bg2020.rds')
)


atoms2019.bgs <- bgs2020 |>
  merge(bg20_19_cw, by='GEOID') |>
  select(-GEOID) |>
  group_by(GEOID.ATOM) |>
  summarize(pop_race_denom = sum(pop_race_denom),
            pop_pov_denom = sum(pop_pov_denom),
            white_nh_pop = sum(white_nh_pop),
            black_pop = sum(black_pop),
            asian_pop = sum(asian_pop),
            naan_pop = sum(naan_pop),
            hisp_pop = sum(hisp_pop),
            pov_pop = sum(pov_pop))
rm(bgs2020)
rm(bg20_19_cw)

atoms2019 <- merge(atoms2019, atoms2019.bgs, by='GEOID.ATOM')
rm(atoms2019.bgs)

saveRDS(atoms2019, here('data', 'atom_datasets', 'covariates', 'atoms19.rds'))
rm(atoms2019)


###################################
#### 3. Create 2024 atoms dataset
###################################



# blocks
blocks20_atoms24 <- here('data', 'crosswalks', 'blocks20_atoms24.rds') |>
  readRDS() |>
  mutate(GEOID.ATOM = str_c(GEOID.CTY, '.', GEOID.CD)) |>
  select(GEOID.ATOM, GEOID=GEOID20)


blocks2020 <- readRDS(
  here('data', 'block_data', 'decennial_only', 'block2020.rds')
)

atoms2024 <- blocks2020 |>
  merge(blocks20_atoms24, by='GEOID') |>
  select(-GEOID, -NAME) |>
  group_by(GEOID.ATOM) |>
  summarize(total_pop = sum(total_pop),
            female_pop = sum(female_pop),
            under5_pop = sum(under5_pop),
            under65_pop = sum(under65_pop))

rm(blocks20_atoms24)
rm(blocks2020)

# block groups
bg24_24_cw <- readRDS(
  here('data', 'crosswalks', 'bgs24_atoms24.rds')
) |>
  mutate(GEOID.ATOM = str_c(GEOID.CTY, '.', GEOID.CD)) |>
  select(GEOID.ATOM, GEOID)

bgs2024 <- readRDS(
  here('data', 'block_data', 'acs_data', 'bg2024.rds')
)


atoms2024.bgs <- bgs2024 |>
  merge(bg24_24_cw, by='GEOID') |>
  select(-GEOID) |>
  group_by(GEOID.ATOM) |>
  summarize(pop_race_denom = sum(pop_race_denom),
            pop_pov_denom = sum(pop_pov_denom),
            white_nh_pop = sum(white_nh_pop),
            black_pop = sum(black_pop),
            asian_pop = sum(asian_pop),
            naan_pop = sum(naan_pop),
            hisp_pop = sum(hisp_pop),
            pov_pop = sum(pov_pop))
rm(bgs2024)
rm(bg24_24_cw)

atoms2024 <- merge(atoms2024, atoms2024.bgs, by='GEOID.ATOM')
rm(atoms2024.bgs)

saveRDS(atoms2024, here('data', 'atom_datasets', 'covariates', 'atoms24.rds'))
rm(atoms2024)
