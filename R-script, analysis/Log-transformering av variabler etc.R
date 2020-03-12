### Analysing results ###

library(tidymodels)


load(file = "./R-script, analysis/Models/cf_neg.rds")


train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg.rds")


# Skewed fordeling, bør log-transofmeres. Før analysen?
hist(train_speineg$gdp)
hist(train_speineg$ttime_mean)
hist(train_speineg$bdist3)
hist(train_speineg$capdist)


# Lagge spei??
df %>% mutate(log_x = log(x+1)) %>%
  
  group_by(spatial_id) %>%
  
  arrange(time) %>%
  
  mutate(lag_x = lag(x))



# Fjerne conflict og spei3 fra der hvor man plotter (?)



log_vars <- c("gdp", "bdist3", "capdist", "ttime_mean")

names(train_speineg)

train_log <- train_speineg %>%
  
  select(gid, year, gwno, lon, lat, conflict, spei3_neg, temp, agri_ih, irrig_sum, bdist3, 
         capdist, ttime_mean, pop, empl_agr, unempl_tot, excluded, shdi, 
         libdem, global_ind, gdp) %>%
  
  recipe(.) %>%
  
  update_role(!!log_vars, new_role = "log_transform") %>%
  
  step_log(has_role("log_transform"), offset = 1) %>%
  
  prep() %>%
  
  juice()


?step_log

saveRDS(train_log, file = "./R-script, analysis/Models/train_speineg_logtransformed.rds")


train_log_lagged <- train_log %>%
  
  group_by(gid) %>%
  
  arrange(year) %>%
  
  recipe(conflict ~ .) %>%
  
  remove_role(gid, year, old_role = "predictor") %>%
  
  step_lag(all_predictors(), lag = 1) %>%
  
  prep() %>%
  
  juice()

saveRDS(train_log_lagged, file = "./R-script, analysis/Models/train_speineg_logtransformed_lagged.rds")

hist(train_log$gdp)
hist(train_log$ttime_mean)
hist(train_log$bdist3)
hist(train_log$capdist)


