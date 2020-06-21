
###############################
### KOF globalization index ###
###############################

library(haven)
library(tidyverse)

kof <- read_dta("./Data/KOF Globalization Index/KOFGI_2019_data.dta")


kof_clean <- kof %>% dplyr::select(country, year, KOFGI) %>% filter(year > 1988) %>% mutate(global_ind = KOFGI)
kof_clean_2 <- kof_clean %>% dplyr:: select(country, year, global_ind)

saveRDS(kof_clean_2, "./Egne datasett/kof.rds")

