
### Test of the overlap assumption ###


library(tidyverse)
library(grf)
library(finalfit)


# Fix the dataset ---------------------------------------------------------
sample_final <- readRDS("./Egne datasett/resampled_data_lagged.rds")

# Renomve prefix for all variables so that easier to work with (lagged conflict stays the same as starts only with lag_)
# Also remove:
# - The other conflict-variables, will be used as alternative outcome variables for robustness checks
# - The full SPEI variable, might be used for robustness check?
sample_final <- sample_final %>% rename_at(vars(starts_with("lag_1_")), 
                                           funs(str_replace(., "lag_1_", ""))) %>% 
  select(-best, -events, -spei3)


# Make the spei variables binary
sample_final <- sample_final %>% mutate(spei3_neg = ifelse(spei3_neg < (-1), 1, 0),
                                        spei3_pos = ifelse(spei3_pos > 1, 1, 0))

train_nona <- na.exclude(sample_final)


# Run seperate models with spei_neg and spei_pos as W. Must exclude the other variables to be able to run
train_speipos <- select(train_nona, -spei3_neg)
train_speineg <- select(train_nona, -spei3_pos)


# Test of the overlap assumption ------------------------------------------

# SPEI3 pos
X <- model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos"))])
W <- train_speipos$spei3_pos

propensity.forest_pos <- regression_forest(X, W)
saveRDS(propensity.forest_pos, "./R-script, analysis/Models/overlap_ass_pos.rds")

W.hat <- predict(propensity.forest_pos)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "SPEI3 positive")

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg$spei3_neg

propensity.forest_neg <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg, "./R-script, analysis/Models/overlap_ass_neg.rds")

W.hat <- predict(propensity.forest_neg)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "SPEI3 negative")



# Check if median values vary between groups of treatment variable --------
sample_final$spei3_neg_fact <- as.factor(sample_final$spei3_neg)
sample_final$spei3_pos_fact <- as.factor(sample_final$spei3_pos)

spei_dist_neg_binary <- sample_final %>% 
  summary_factorlist(dependent = "spei3_neg_fact", 
                     explanatory = c("temp", "agri_ih", "irrig_sum", "bdist3", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, cont = "median")

stargazer::stargazer(spei_dist_neg_binary, summary = F, title = "Median and SD, SPEI3 negative")


spei_dist_pos_binary <- sample_final %>% 
  summary_factorlist(dependent = "spei3_pos_fact", 
                     explanatory = c("temp", "agri_ih", "irrig_sum", "bdist3", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, cont = "median")

stargazer::stargazer(spei_dist_pos_binary, summary = F, title = "Median and SD, SPEI3 positive")



# Estimate difference between ATE and ATT ---------------------------------

att_neg <- average_treatment_effect(cf_neg_speidich, target.sample = "treated")
ate_neg <- average_treatment_effect(cf_neg_speidich)

att_pos <- average_treatment_effect(cf_pos_speidich, target.sample = "treated")
ate_pos <- average_treatment_effect(cf_pos_speidich)

df_at <- rbind(ate_neg, att_neg, ate_pos, att_pos)

stargazer::stargazer(df_at, summary = F)


# Test of overlap assumption with continuous SPEI3 ------------------------

sample_final <- readRDS("./Egne datasett/resampled_data_lagged.rds")

# Renomve prefix for all variables so that easier to work with (lagged conflict stays the same as starts only with lag_)
# Also remove:
# - The other conflict-variables, will be used as alternative outcome variables for robustness checks
# - The full SPEI variable, might be used for robustness check?
sample_final <- sample_final %>% rename_at(vars(starts_with("lag_1_")), 
                                           funs(str_replace(., "lag_1_", ""))) %>% 
  select(-best, -events, -spei3)


train_nona <- na.exclude(sample_final)


# Run seperate models with spei_neg and spei_pos as W. Must exclude the other variables to be able to run
train_speipos <- select(train_nona, -spei3_neg)
train_speineg <- select(train_nona, -spei3_pos)

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg$spei3_neg

propensity.forest_neg <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg, "./R-script, analysis/Models/overlap_ass_neg.rds")

W.hat <- predict(propensity.forest_neg)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "SPEI3 negative")


# SPEI3 pos
X <- model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos"))])
W <- train_speipos$spei3_pos

propensity.forest_pos <- regression_forest(X, W)
saveRDS(propensity.forest_pos, "./R-script, analysis/Models/overlap_ass_pos.rds")

W.hat <- predict(propensity.forest_pos)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "SPEI3 positive")


