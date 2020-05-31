
### Test of the overlap assumption for continent-specific analyses ###


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


# Africa ------------------------------------------------------------------

train_speineg_af <- train_speineg %>% filter(continent == "Africa") %>% select(-continent)
train_speipos_af <- train_speipos %>% filter(continent == "Africa") %>% select(-continent)

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg_af[, !(names(train_speineg_af) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg_af$spei3_neg

propensity.forest_neg <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg, "./R-script, analysis/Models/overlap_ass_neg_africa.rds")

W.hat <- predict(propensity.forest_neg)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "SPEI3 negative")


# SPEI3 pos
X <- model.matrix(~., data = train_speipos_af[, !(names(train_speipos_af) %in% c("conflict", "spei3_pos"))])
W <- train_speipos_af$spei3_pos

propensity.forest_pos <- regression_forest(X, W)
saveRDS(propensity.forest_pos, "./R-script, analysis/Models/overlap_ass_pos_africa.rds")

W.hat <- predict(propensity.forest_pos)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "SPEI3 positive")



# Asia --------------------------------------------------------------------

train_speineg_as <- train_speineg %>% filter(continent == "Asia") %>% select(-continent)
train_speipos_as <- train_speipos %>% filter(continent == "Asia") %>% select(-continent)

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg_as[, !(names(train_speineg_as) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg_as$spei3_neg

propensity.forest_neg_as <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg_as, "./R-script, analysis/Models/overlap_ass_neg_asia.rds")

W.hat <- predict(propensity.forest_neg_as)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Asia: SPEI3 negative")


# SPEI3 pos
X <- model.matrix(~., data = train_speipos_as[, !(names(train_speipos_as) %in% c("conflict", "spei3_pos"))])
W <- train_speipos_as$spei3_pos

propensity.forest_pos <- regression_forest(X, W)
saveRDS(propensity.forest_pos, "./R-script, analysis/Models/overlap_ass_pos_asia.rds")

W.hat <- predict(propensity.forest_pos)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Asia: SPEI3 positive")


# Americas ----------------------------------------------------------------

train_speineg_am <- train_speineg %>% filter(continent == "Americas") %>% select(-continent)
train_speipos_am <- train_speipos %>% filter(continent == "Americas") %>% select(-continent)

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg_am[, !(names(train_speineg_am) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg_am$spei3_neg

propensity.forest_neg_am <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg_am, "./R-script, analysis/Models/overlap_ass_neg_americas.rds")

W.hat <- predict(propensity.forest_neg_am)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Americas: SPEI3 negative")


# SPEI3 pos
X <- model.matrix(~., data = train_speipos_am[, !(names(train_speipos_am) %in% c("conflict", "spei3_pos"))])
W <- train_speipos_am$spei3_pos

propensity.forest_pos_am <- regression_forest(X, W)
saveRDS(propensity.forest_pos_am, "./R-script, analysis/Models/overlap_ass_pos_americas.rds")

W.hat <- predict(propensity.forest_pos_am)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Americas: SPEI3 positive")


# Europe ------------------------------------------------------------------

train_speineg_eu <- train_speineg %>% filter(continent == "Europe") %>% select(-continent)
train_speipos_eu <- train_speipos %>% filter(continent == "Europe") %>% select(-continent)

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg_eu[, !(names(train_speineg_eu) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg_eu$spei3_neg

propensity.forest_neg_eu <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg_eu, "./R-script, analysis/Models/overlap_ass_neg_europe.rds")

W.hat <- predict(propensity.forest_neg_eu)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Europe: SPEI3 negative")


# SPEI3 pos
X <- model.matrix(~., data = train_speipos_eu[, !(names(train_speipos_eu) %in% c("conflict", "spei3_pos"))])
W <- train_speipos_eu$spei3_pos

propensity.forest_pos_eu <- regression_forest(X, W)
saveRDS(propensity.forest_pos_eu, "./R-script, analysis/Models/overlap_ass_pos_europe.rds")

W.hat <- predict(propensity.forest_pos_eu)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Europe: SPEI3 positive")


# Oceania -----------------------------------------------------------------

train_speineg_oc <- train_speineg %>% filter(continent == "Oceania") %>% select(-continent)
train_speipos_oc <- train_speipos %>% filter(continent == "Oceania") %>% select(-continent)

# SPEI3 neg
X_neg <- model.matrix(~., data = train_speineg_oc[, !(names(train_speineg_oc) %in% c("conflict", "spei3_neg"))])
W_neg <- train_speineg_oc$spei3_neg

propensity.forest_neg_oc <- regression_forest(X_neg, W_neg)
saveRDS(propensity.forest_neg_oc, "./R-script, analysis/Models/overlap_ass_neg_oceania.rds")

W.hat <- predict(propensity.forest_neg_oc)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Oceania: SPEI3 negative")


# SPEI3 pos
X <- model.matrix(~., data = train_speipos_oc[, !(names(train_speipos_oc) %in% c("conflict", "spei3_pos"))])
W <- train_speipos_oc$spei3_pos

propensity.forest_pos_oc <- regression_forest(X, W)
saveRDS(propensity.forest_pos_oc, "./R-script, analysis/Models/overlap_ass_pos_oceania.rds")

W.hat <- predict(propensity.forest_pos_oc)$predictions
hist(W.hat, xlab = "propensity score", density = 25, breaks = 40, col = "#3C1518", main = "Oceania: SPEI3 positive")

