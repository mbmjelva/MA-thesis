#### Making subsample of final dataset ####

library(tidyverse)

final <- read_rds("./Egne datasett/final_dataset.rds")
table(complete.cases(final))

# Remove best and events --------------------------------------------------

# Disse er høyt korrelert med AV, skal ikke være en del av predictor-variablene
final <- select(final, -best, -events)


# Subsample the dataset, so that managable to work with -------------------

### Alternative 1: Sørger for at både 0 og 1 fra conflict blir med, men fortsatt ca. 98% som har 0 på response.
# Stratified sampling with dplyr?
set.seed(235)
sample_final <- sample_n(final, 500000, weight = conflict) 

# The proportions are (almost) equal
table(final$conflict) / nrow(final)
table(sample_final$conflict) / nrow(sample_final)

saveRDS(sample_final, "./Egne datasett/sample_of_finaldata_for_rf.rds")



### Alternative 2: Gir mulighet til å replace, sørger for at det er like mange som har conflict = 0 og conflict = 1 i datasettet.
set.seed(25)
sample_final_prop <- final %>%
  group_by(conflict) %>%
  mutate(num_rows=n()) %>%
  sample_n(250000, weight=num_rows, replace = T) %>%
  ungroup

table(sample_final_prop$conflict)/nrow(sample_final_prop)

sample_final_prop <- select(sample_final_prop, -num_rows)

#' Usikker på hvilken metode som er best. Blir jo en massiv overrepresentasjon av de med conflict = 1 i datasettet.

