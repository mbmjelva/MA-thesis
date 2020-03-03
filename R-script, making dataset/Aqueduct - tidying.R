
##########################
### Aqueduct - tidying ###
##########################

library(tidyverse)
library(countrycode)

# Codebook: https://github.com/wri/aqueduct30_data_download/blob/master/metadata.md#extra-columns-for-location-analyzer

aqueduct <- read_csv("Data/Aqueduct 3.0/Y2019M07D12_Aqueduct30_V01/baseline/annual/csv/y2019m07d11_aqueduct30_annual_v01.csv")
fin <- readRDS("Egne datasett/final_dataset.rds")

# Ikke info om lon/lat eller år. 

# Endrer landenavn
aq2 <- dplyr::mutate(aqueduct, gwno = countrycode(gid_0, "iso3c", "gwn"))
# Finner ikke: ALA, ATF, ASM, ATG, BES, BMU, CYM, ESH, FRO, GGY, GLP, GRD, GRL, GUF, GUM, HKG, IMN, JEY, KNA, MNP, MSR, MTQ, MYT, NCL, NRU, PLW, PRI, PSE, PYF, REU, SHN, SJM, SMR, SPM,
# SYC, TCA, TKL, XAD, XKO, XNC. Disse er ikke-selvstendige stater. (ALA er for eksempel Åland.)

aq2$gwno[aq2$gid_0 == "AND"] <- 232
aq2$gwno[aq2$gid_0 == "DMA"] <- 54
aq2$gwno[aq2$gid_0 == "FSM"] <- 987
aq2$gwno[aq2$gid_0 == "LIE"] <- 223
aq2$gwno[aq2$gid_0 == "MAC"] <- 343
aq2$gwno[aq2$gid_0 == "STP"] <- 403
aq2$gwno[aq2$gid_0 == "TON"] <- 972
aq2$gwno[aq2$gid_0 == "TUV"] <- 973
aq2$gwno[aq2$gid_0 == "WSM"] <- 990
aq2$gwno[aq2$gid_0 == "YEM"] <- 678

aq2 <- select(aq2, gwno, w_awr_def_tot_score, w_awr_def_tot_cat, w_awr_def_tot_label, bws_score, bws_cat, bws_label)

# Må merge på landenivå da det ikke finnes grid-id

finaq <- left_join(fin, aq2, by = "gwno")

# Error: std::bad_alloc
