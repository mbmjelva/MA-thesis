
## SPEI - tidying ##

# This script gives the code for making the SPEI and temperature-variables for the analysis

library(SPEI)
library(tidyverse)
library(lubridate)


cru <- readRDS("./Data/CRU/cru_spei.rds")
mirca <- readRDS("./Egne datasett/mirca_growseas_tibble.rds")

cru$spei3 <- if_else(is.infinite(cru$spei3), 0, cru$spei3)


# Get year and month information. Change month to numbers for both datasets so that mergable
cru <- cru %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(year>1988)


mirca$month <- sapply(mirca$names, function(x) grep(paste("(?i)",x,sep=""),month.abb))

# Merge the datasets so that the growing season weight is in the cru-dataset
cru <- left_join(cru, mirca, by = c("lon" = "x", "lat" = "y", "month"))

# Aggregate to years, weighted mean by the growing season
cru_w <- cru %>%
  dplyr::select(lon, lat, year, temp, spei3, gs_weight) %>% 
  group_by(year, lon, lat) %>% 
  summarise(spei3 = weighted.mean(spei3, gs_weight, na.rm = T), temp = weighted.mean(temp, gs_weight, na.rm = T)) %>%
  ungroup(year, lon, lat) 

# Not weighted. Make two so that can compare missing before and after weighting
cru2 <- cru %>%
  dplyr::select(lon, lat, year, temp, spei3, gs_weight) %>% 
  group_by(year, lon, lat) %>% 
  summarise(spei3 = mean(spei3, na.rm = T), temp = mean(temp, na.rm = T)) %>%
  ungroup(year, lon, lat) 


saveRDS(cru_w,"./Egne datasett/cru_w.rds")
saveRDS(cru2,"./Egne datasett/cru.rds")




