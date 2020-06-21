#### Sub-sampling ####

# This script subsamples the larger dataset so that the number of observations is managable

library(tidyverse)
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112") # Must download from this github page to get the correct code


final <- read_rds("./Egne datasett/final_dataset.rds")


# Create subsample with lagged variables only -----------------------------
#' Keep the static variables as they are so that do not miss information from 1989 when lagged (gwno has been static since 2011)
#' Remove the lagged versions of the static variables. Also remove unemployment, as has a large amount of missing (almost all of Africa is missing)

final_lagged <- select(final, gid, lon, lat, year, gwno, continent, conflict, ttime_mean, starts_with("lag"), -lag_1_lon, -lag_1_lat, -lag_1_gwno, -lag_1_ttime_mean, -lag_1_unempl_tot, -unempl_tot)

# Remove all cases that have NA on the SPEI-variable, so that the sub-sample contains enough SPEI values
final_lagged <- final_lagged %>% filter(!is.na(lag_1_spei3))


# Cannot have more than 1,000,000 observations, conduct stratified sub-sampling so that improve the balance of the conflict-variable
# 75% of the observations are set to 0 and 25% to 1 
s_l <- stratified(final_lagged, "conflict", size = c(750000, 250000), replace = T)

table(s_l$conflict)/nrow(s_l)

saveRDS(s_l, "./Egne datasett/resampled_data_lagged.rds")

