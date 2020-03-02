
#######################
### V-DEM - tidying ###
#######################


library(tidyverse)

vdem <- read_csv("./Data/Country_Year_V-Dem_Core_CSV_v9/Country_Year_V-Dem_Full+others_CSV_v9/Country_Year_V-Dem_Full+others_CSV_v9/V-Dem-CY-Full+Others-v9.csv")

vdem <- vdem %>% dplyr::select(country_name, country_id, year, v2x_libdem, e_migdppc) %>% filter(year > 1988) %>% 
  mutate(country = country_name, libdem = v2x_libdem, gdp = e_migdppc)

vdem <- vdem %>% select(country, year, libdem, gdp)


saveRDS(vdem, file = "./Egne datasett/vdem.rds")

