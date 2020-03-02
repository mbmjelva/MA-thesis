
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



k <- ucdp_ged %>% select(year, active_year, type_of_violence, best) %>% filter(type_of_violence == 1)

table(ucdp_ged$active_year, useNA = "always")
table(k$active_year, useNA = "always")
table(k$best, useNA = "always")
# Det er veldig mange events der det ikke er noen døde en gang. Hvordan skjer det? 

table(ucdp$active_year == 1, ucdp$best>25, ucdp$year)

# Så ingen events som har færre enn 25 deaths, ikke på best, men per deaths_b, deaths_a og/eller deaths_civilians, er kodet til 1. 
# Videre kan et event ha 0 på deaths_civilians eller a/b, men ha over 25 døde i en av de andre kategoriene. 
#' Men, forklarer ikke hvorfor det er 135000 events som har færre enn 25 døde og er kodet til å være en del av active years
#' 
