# Generalized random forest - lagged model #


library(tidyverse)
library(grf)
library(caret)
library(rpart.plot)
library(ggpubr)

sample_final <- readRDS("./Egne datasett/resampled_data_lagged.rds")

# Renomve prefix for all variables so that easier to work with (lagged conflict stays the same as starts only with lag_)
sample_final <- sample_final %>% rename_at(vars(starts_with("lag_1_")), 
                                           funs(str_replace(., "lag_1_", "")))

# Remove:
# - The other conflict-variables, will be used as alternative outcome variables for robustness checks
# - The full SPEI variable, might be used for robustness check?
sample_final <- select(sample_final, -best, -events, -spei3)


# Reverse the SPEI neg variable so that high value means high dryness

sample_final$spei3_neg <- sample_final$spei3_neg * (-1)


# Create training and test data set
set.seed(125)

trainrows <- createDataPartition(sample_final$conflict, p = 0.7, list = F) # Fra caret-pakka
training <- sample_final[trainrows,]
test <- sample_final[-trainrows,]


# Exclude NA to be able to run model (must have same lenght for all variables)
train_nona <- na.exclude(training)
test_nona <- na.exclude(test)

# Run seperate models with spei_neg and spei_pos as W. Must exclude the other variables to be able to run
train_speipos <- select(train_nona, -spei3_neg)
train_speineg <- select(train_nona, -spei3_pos)

test_speipos <- select(test_nona, -spei3_neg)
test_speineg <- select(test_nona, -spei3_pos)

saveRDS(train_speipos, "./R-script, analysis/Models/train_speipos_lagged.rds")
saveRDS(train_speineg, "./R-script, analysis/Models/train_speineg_lagged.rds")

saveRDS(test_speipos, "./R-script, analysis/Models/test_speipos_lagged.rds")
saveRDS(test_speineg, "./R-script, analysis/Models/test_speineg_lagged.rds")

# Run models --------------------------------------------------------------

# Model with negative SPEI
cf_neg <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 
  W = train_speineg$spei3_neg,
  tune.parameters = "all", # Model tunes all tunable variables
  seed = 2865
)

# Save the model
save(cf_neg, file = "./R-script, analysis/Models/cf_neg_lagged.rds")


# Model with positive SPEI
cf_pos <- causal_forest(
  X = model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speipos$conflict) - 1, # convert outcome to 0 or 1
  W = train_speipos$spei3_pos,
  tune.parameters = "all",
  seed = 2865
)

save(cf_pos, file = "./R-script, analysis/Models/cf_pos_lagged.rds")

