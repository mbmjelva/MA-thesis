#### Sub-sampling ####


library(tidyverse)
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112") # Må laste ned fra github-siden for å få riktig funksjon


final <- read_rds("./Egne datasett/final_dataset.rds")


# Create subsample with lagged variables only -----------------------------

final_lagged <- select(final, starts_with("lag"), conflict)

# Remove all cases that have NA on the SPEI-variable, so that the sub-sample contains enough SPEI values
final_lagged <- final_lagged %>% filter(!is.na(lag_1_spei3))


# Ønsker 1,000,000 totalt, så da må 75% av disse være 0 og 25% være 1. 
s_l <- stratified(final_lagged, "conflict", size = c(750000, 250000), replace = T)

table(s_l$conflict)/nrow(s_l)

saveRDS(s_l, "./Egne datasett/resampled_data_lagged.rds")

