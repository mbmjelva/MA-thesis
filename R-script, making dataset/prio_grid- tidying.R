
############################
### Prio/Grid - tidying  ###
############################

library(tidyverse)
library(zoo)

#setwd("/Users/mathildemjelva/Library/Mobile Documents/com~apple~CloudDocs/Mastergrad/Masteroppgave/R-master/")

popul_countr <- read_csv("./Data/PRIO-GRID/PRIO-GRID Yearly Variables for 1989-2014 - 2020-03-06 (1).csv")
urban <- read_csv("./Data/PRIO-GRID/PRIO-GRID Static Variables - 2020-02-01.csv")

pop_urban <- left_join(popul_countr, urban)

## agri_ih and irrig_sum are only given values every fifth year. Must fill in missing.

# The order of the years arent always correct, so need to order it to fill inn missing values correctly
pop_urban <- pop_urban %>% group_by(gid) %>% arrange(gid, year) 

#' Values only given every fifth year. Filled missing values upwards so that the value from 1995 is given to the cell from 1995-1999, 
#' the value for 2000 for 2000-2005 etc. No value is added after 2005, so the value for 2005 is given for the subsequent years
pop_urban_filled <- pop_urban %>% group_by(gid) %>% mutate(irrig_sum = na.locf(irrig_sum, na.rm=FALSE),
                                                           agri_ih = na.locf(agri_ih, na.rm=FALSE))


#' The above function only fills in the values for the subsequent years. The below function gives the value for 
#' the previous year. The cells in 1989 are given the value from 1990.
pop_filled_more <- pop_urban_filled %>% fill(irrig_sum, .direction = "up")
pop_filled_more <- pop_urban_filled %>% fill(agri_ih, .direction = "up")


# Fix yearly variable
add_years <- 2015:2020
last_year <- filter(pop_urban, year == 2014)

for(year in add_years){
  pg_year <- last_year
  pg_year$year <- year
  
  pop_urban <- bind_rows(pop_urban, pg_year)
}


saveRDS(pop_urban, "./Egne datasett/priogrid.rds")






