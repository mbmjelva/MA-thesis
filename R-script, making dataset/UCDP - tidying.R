
###################################
### UCDP_GED - dataset, tidying ###
###################################

library(tidyverse)

### UCDP-GED armed conflict dataset ###
ucdp_ged <- read_csv("./Data/UCDP-GED/ged191.csv")

ucdp <- ucdp_ged %>% filter(type_of_violence == 1) %>%
  group_by(priogrid_gid, year) %>%
  summarize(events = n(),
            best = sum(best, na.rm = T))

saveRDS(ucdp, file = "./Egne datasett/ucdp_ged.rds")
