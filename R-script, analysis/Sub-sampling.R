#### Sub-sampling ####


library(tidyverse)

final <- read_rds("./Egne datasett/final_dataset.rds")

final <- select(final, -best, -events, -agri_ih)


library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112") # Må laste ned fra github-siden for å få riktig funksjon

# Ønsker 500,000 totalt, så da må 75% av disse være 0 og 25% være 1. 
500000*0.75 # 375000
500000-375000

s <- stratified(final, "conflict", size = c(375000, 125000), replace = T)

s

# Blir riktig
table(s$conflict)/nrow(s)


saveRDS(s, "./Egne datasett/resamled_data_25_vs_75.rds")
