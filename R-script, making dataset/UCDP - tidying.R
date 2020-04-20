
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

ucdp2 <- ucdp_ged %>% filter(type_of_violence == 2) %>%
  group_by(priogrid_gid, year) %>%
  summarize(non_state_conflict = n())

ucdp3 <- full_join(ucdp, ucdp2, by = c("priogrid_gid", "year")) # Full join so that keep the non_state_conflict values

saveRDS(ucdp3, file = "./Egne datasett/ucdp_ged.rds")
