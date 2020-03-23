

#### Merging priogrid and ucdp ####

library(tidyverse)

ucdp_ged <- read_rds("./Egne datasett/ucdp_ged.rds")
priogrid <- read_rds("./Egne datasett/priogrid.rds")


## Merger ucdp og priogrid først på gid-verdi og year
prio_ucdp <- left_join(priogrid, ucdp_ged, by = c("gid" = "priogrid_gid", "year" = "year"))


# Setter NA til 0 på events og lager dummyvariabel for om det var en konflikt eller ikke (conflict)
prio_ucdp$events <- if_else(is.na(prio_ucdp$events), 0L, prio_ucdp$events)
prio_ucdp$best <- if_else(is.na(prio_ucdp$best), 0, prio_ucdp$best)
prio_ucdp$conflict <- if_else(prio_ucdp$events > 0L, 1L, prio_ucdp$events)


saveRDS(prio_ucdp, "./Egne datasett/prio_ucdp_merged.rds")




