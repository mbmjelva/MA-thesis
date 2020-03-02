
############################
### Worldbank - tidying  ###
############################

library(tidyverse)
library(countrycode)
library(wbstats)


wb <- wb(indicator = c("SL.AGR.EMPL.ZS", "SL.UEM.TOTL.NE.ZS"), startdate = 1989, enddate = 2019, country = "countries_only", return_wide = T)


wb_clean <- wb %>% mutate(year = date, empl_agr = SL.AGR.EMPL.ZS, unempl_tot = SL.UEM.TOTL.NE.ZS)


wb_clean_2 <- wb_clean %>% select(country, year, empl_agr, unempl_tot)


saveRDS(wb_clean_2, "./Egne datasett/wbi.rds")
