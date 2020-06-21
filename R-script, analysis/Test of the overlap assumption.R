
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
X <- model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos", "temp"))])
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


# Check if mean values vary between groups of treatment variable --------
sample_final$spei3_neg_fact <- as.factor(sample_final$spei3_neg)
sample_final$spei3_pos_fact <- as.factor(sample_final$spei3_pos)


spei_dist_neg_binary <- sample_final %>% 
  summary_factorlist(dependent = "spei3_neg_fact", 
                     explanatory = c("temp", "agri_ih", "irrig_sum", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, add_col_totals = T)

# Calculate difference between groups manually
spei_dist_neg_binary$diff <- c("", (20.8-17.4), (15.7-17.5), (8825.7-9395.8), (6.4-6.4), 
          (5.8-5.7), (109.6-108.1), (35.9-31), (0.5-0.4), 0.6-0.6, 0.4-0.4, 52.2-56.4, 8.7-8.7)

spei_dist_neg_binary$diff <- round(as.numeric(spei_dist_neg_binary$diff), 1)

stargazer::stargazer(spei_dist_neg_binary, summary = F, #type = "text",
                     title = "Mean and SD, SPEI3 negative", digits = 1)

spei_dist_pos_binary <- sample_final %>% 
  summary_factorlist(dependent = "spei3_pos_fact", 
                     explanatory = c("temp", "agri_ih", "irrig_sum", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, add_col_totals = T)

# Make function to calculate difference
diff_mean <- function(spei3_pos, var) {
  mean_1 <- mean(var[spei3_pos == 1], na.rm = T)
  mean_0 <- mean(var[spei3_pos == 0], na.rm = T)
  diff <-  round((mean_1 - mean_0),1)
  output <- list(diff = diff)
  return(output)
}

a <-  diff_mean(sample_final$spei3_pos, sample_final$temp)
b <- diff_mean(sample_final$spei3_pos, sample_final$agri_ih)
c <- diff_mean(sample_final$spei3_pos, sample_final$irrig_sum)
d <- diff_mean(sample_final$spei3_pos, sample_final$capdist)
e <- diff_mean(sample_final$spei3_pos, sample_final$ttime_mean)
f <- diff_mean(sample_final$spei3_pos, sample_final$pop)
g <- diff_mean(sample_final$spei3_pos, sample_final$empl_agr)
h <- diff_mean(sample_final$spei3_pos, sample_final$excluded)
i <- diff_mean(sample_final$spei3_pos, sample_final$shdi)
j <- diff_mean(sample_final$spei3_pos, sample_final$libdem)
k <- diff_mean(sample_final$spei3_pos, sample_final$global_ind)
l <- diff_mean(sample_final$spei3_pos, sample_final$gdp)

spei_dist_pos_binary$difference <- c("", a, b, c, d, e, f, g, h, i, j, k, l)

stargazer::stargazer(spei_dist_pos_binary, summary = F, #type = "text",
                     title = "Mean and SD, SPEI3 positive")


