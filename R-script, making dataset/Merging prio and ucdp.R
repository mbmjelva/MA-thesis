

#### Merging priogrid and ucdp ####

# Merge PRIO-GRID and UCDP first prior to merging with the others, in order to make the conflict-variable

library(tidyverse)

ucdp_ged <- read_rds("./Egne datasett/ucdp_ged.rds")
priogrid <- read_rds("./Egne datasett/priogrid.rds")

## Merge UCDP and priogrid first on gid value and year
prio_ucdp <- left_join(priogrid, ucdp_ged, by = c("gid" = "priogrid_gid", "year" = "year"))

# Make the conflict-variable
# Set NA to 0 on events and make dummy variabel depicting whether there is conflict or not (there was no NA in the UCDP-ged before merging)
prio_ucdp$events <- if_else(is.na(prio_ucdp$events), 0L, prio_ucdp$events)
prio_ucdp$best <- if_else(is.na(prio_ucdp$best), 0, prio_ucdp$best)
prio_ucdp$conflict <- if_else(prio_ucdp$events > 0L, 1L, prio_ucdp$events)

prio_ucdp$non_state_conflict <- if_else(is.na(prio_ucdp$non_state_conflict), 0L, prio_ucdp$non_state_conflict)
prio_ucdp$non_state_conflict<- if_else(prio_ucdp$non_state_conflict > 0L, 1L, prio_ucdp$non_state_conflict)

saveRDS(prio_ucdp, "./Egne datasett/prio_ucdp_merged.rds")

