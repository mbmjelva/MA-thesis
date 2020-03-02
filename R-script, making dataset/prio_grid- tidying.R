
############################
### Prio/Grid - tidying  ###
############################

library(tidyverse)
library(countrycode)

setwd("/Users/mathildemjelva/Library/Mobile Documents/com~apple~CloudDocs/Mastergrad/Masteroppgave/R-master/")

popul_countr <- read_csv("./Data/PRIO-GRID/PRIO-GRID Yearly Variables for 1989-2014 - 2020-02-01.csv")

## Info om population fra 2010, distance to urban center og land. Andre ting på prio som er interessant?
urban <- read_csv("./Data/PRIO-GRID/PRIO-GRID Static Variables - 2020-02-01.csv")

pop_urban <- left_join(popul_countr, urban)

add_years <- 2015:2020
last_year <- filter(pop_urban, year == 2014)

for(year in add_years){
  pg_year <- last_year
  pg_year$year <- year
  
  pop_urban <- bind_rows(pop_urban, pg_year)
}


saveRDS(pop_urban, "./Egne datasett/priogrid.rds")






