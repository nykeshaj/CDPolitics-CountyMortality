## get county and CD intersections ##
library(sf)
library(s2)
library(USAboundaries)
library(sp)
# library(rgeos)
library(tidycensus)
library(raster)
library(dplyr)
library(data.table)
library(spdep)
library(tibble)
library(tidyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(here)


###########################################################
## CREATE ATOMS BY INTERSECTING CD AND COUNTY SHAPEFILES ##
###########################################################

# load county boundaries
cnty_15 <- tigris::counties(year = 2015) %>%
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

cnty_19 <- tigris::counties(year = 2019) %>%
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

cnty_24 <- tigris::counties(year = 2024) %>%
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

# load CD boundaries
cd113 <- tigris::congressional_districts(year = 2013) #113th congress TIGER/Line Shapefile
# remove undefined & non-contiguous delegate districts
cd113 <- cd113 %>%
  filter(!GEOID %in% c("09ZZ","17ZZ","26ZZ","6098","6698","6998","7898")) %>%
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

cd115 <- tigris::congressional_districts(year = 2016) #115th congress TIGER/Line Shapefile
# remove undefined & non-contiguous delegate districts
cd115 <- cd115 %>% filter(!GEOID %in% c("09ZZ","17ZZ","26ZZ","6098","6698","6998","7898"))  %>%
  left_join(state_codes, by = c("STATEFP" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))

cd118 <- tigris::congressional_districts(year = 2022) #118th congress
cd118 <- cd118 %>%
  # remove undefined & non-contiguous delegate districts
  filter(!GEOID20 %in% c("09ZZ","17ZZ","26ZZ","6098","6698","6998","7898")) %>%
  mutate(GEOID20 = ifelse(GEOID20 == "3001","3000",GEOID20))  %>%
  left_join(state_codes, by = c("STATEFP20" = "state_code")) %>%
  # remove non-contiguous states & territories
  filter(!state_abbr %in% c('AK','HI','DC','PR','GU','VI','AS','MP'))


# function to create atoms
create_atoms <- function(cnty_shpfile,cd_shpfile){

# plot intersecting atoms
gridy<-cnty_shpfile %>%
  rename(ID=GEOID) %>%
  ## ID variables should be numeric
  mutate(ID=as.numeric(ID))

gridx<-cd_shpfile %>%
  rename(ID=GEOID) %>%
  ## ID variables should be numeric
  mutate(ID=as.numeric(ID))

ggplot() +
  geom_sf(data= gridy, fill=NA, color="orange", linewidth=1.2) +
  geom_sf(data= gridx, fill=NA, color="blue", linetype='dashed', linewidth=0.5) +
  theme_void()

atoms <- raster::intersect(as(gridy, 'Spatial'), as(gridx, 'Spatial'))
atoms <- sf::st_as_sf(atoms)
atoms <- atoms %>%
  rename(ID_y = ID_1,
         ID_x = ID_2)
}


atoms15 <- create_atoms(cnty_15,cd113)
atoms19 <- create_atoms(cnty_19,cd115)
atoms24 <- create_atoms(cnty_24,cd118)


# save
saveRDS(atoms15,here('data','atom_datasets','atoms15.rds'))
saveRDS(atoms19,here('data','atom_datasets','atoms19.rds'))
saveRDS(atoms24,here('data','atom_datasets','atoms24.rds'))


#########################################
## GET NUMBER OF ATOMS FOR EACH COUNTY ##
#########################################

atoms15 <- readRDS(here('data','atom_datasets','atoms15.rds'))
atoms19 <- readRDS(here('data','atom_datasets','atoms19.rds'))
atoms24 <- readRDS(here('data','atom_datasets','atoms24.rds'))

## order the county and atom datasets so that the counties that are atoms are first ##
## to do this, first count the number of times each county's geoid appears in the atom dataset (how many atoms it's split into) ##
countyXatom15<-data.frame(table(atoms15$ID_y))
names(countyXatom15)<-c('geoid','num_atoms')


countyXatom19<-data.frame(table(atoms19$ID_y))
names(countyXatom19)<-c('geoid','num_atoms')


countyXatom24<-data.frame(table(atoms24$ID_y))
names(countyXatom24)<-c('geoid','num_atoms')
