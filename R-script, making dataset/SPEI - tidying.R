
## SPEI - tidying ##


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
cru2 <- left_join(cru, mirca, by = c("lon" = "x", "lat" = "y", "month"))

# Aggregate to years, weighted mean by the growing season
cru2 <- cru2 %>%
  dplyr::select(lon, lat, year, temp, spei3, gs_weight) %>% 
  group_by(year, lon, lat) %>% 
  summarise(spei3 = weighted.mean(spei3, gs_weight, na.rm = T), temp = weighted.mean(temp, gs_weight, na.rm = T))

saveRDS(cru2,"./Egne datasett/cru.rds")

