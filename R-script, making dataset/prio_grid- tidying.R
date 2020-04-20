
############################
### Prio/Grid - tidying  ###
############################

library(tidyverse)
library(zoo)

yearly <- read_csv("./Data/PRIO-GRID/Total - PRIO-GRID Yearly Variables for 1989-2014 - 2020-04-08 (3).csv")
static <- read_csv("./Data/PRIO-GRID/PRIO-GRID Static Variables - 2020-02-01.csv")

priogrid <- left_join(static, cont)


# Fill in missing values for agri_ih and irrig_sum ------------------------
## agri_ih and irrig_sum are only given values every tenth and fifth year

# The order of the years are not always correct, so need to order it to fill inn missing values correctly
priogrid <- priogrid %>% group_by(gid) %>% arrange(gid, year) 

#' Filled missing values upwards so that the value from 1995 is given to the cell from 1995-1999, 
#' the value for 2000 for 2000-2005 etc. No value is added after 2005, so the value for 2005 is given for the subsequent years
priogrid <- priogrid %>% group_by(gid) %>% mutate(irrig_sum = na.locf(irrig_sum, na.rm = FALSE), 
                                                  agri_ih = na.locf(agri_ih, na.rm = FALSE))

summary(priogrid)
#' The above function only fills in the values for the subsequent years. The below function gives the value for 
#' the previous year. The cells in 1989 are given the value from 1990.
priogrid <- priogrid %>% fill(irrig_sum, .direction = "up")
priogrid <- priogrid %>% fill(agri_ih, .direction = "up")


# Fill in values from 2015 to 2018. All values are static except for ttime_mean. This one 
add_years <- 2015:2018
last_year <- filter(priogrid, year == 2014)

for(year in add_years){
  pg_year <- last_year
  pg_year$year <- year
  
  priogrid <- bind_rows(priogrid, pg_year)
}

saveRDS(priogrid, "./Egne datasett/priogrid.rds")




