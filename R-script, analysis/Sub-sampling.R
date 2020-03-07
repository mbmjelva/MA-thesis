#### Sub-sampling ####


library(tidyverse)
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112") # Må laste ned fra github-siden for å få riktig funksjon


final <- read_rds("./Egne datasett/final_dataset.rds")


# Ønsker 1,000,000 totalt, så da må 75% av disse være 0 og 25% være 1. 
s <- stratified(final, "conflict", size = c(750000, 250000), replace = T)

table(s$conflict)/nrow(s)

saveRDS(s, "./Egne datasett/resampled_data.rds")
