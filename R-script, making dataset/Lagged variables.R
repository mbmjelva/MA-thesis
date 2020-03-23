#### Lagging variables ####

library(tidyverse)
library(tidymodels)

final <- readRDS("./Egne datasett/final_dataset.rds")

final_lagged <- final %>%
  
  group_by(gid) %>%
  
  arrange(year) %>%
  
  recipe(conflict ~ .) %>% # Identifies the kind of model we have
  
  remove_role(gid, year, old_role = "predictor") %>% # Remove gid and year from being part of the predictors
  
  step_lag(all_predictors(), lag = 1) %>% # Lag all the predictors one year
  
  prep() %>% # Estimate the required parameters from a training set that can later be applied to the whole data set
  
  juice() # Returns variables from the processed training set (all the lagged variables to the dataset)

saveRDS(final_lagged, file = "./Egne datasett/final_lagged.rds")

