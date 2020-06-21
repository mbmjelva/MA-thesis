# Generalized random forest - lagged model, SPEI binary #


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

# Make the spei variables binary
sample_final <- sample_final %>% mutate(spei3_neg = ifelse(spei3_neg < (-1), 1, 0),
                                        spei3_pos = ifelse(spei3_pos > 1, 1, 0))

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

saveRDS(train_speipos, "./R-script, analysis/Models/train_speipos_lagged_speidich.rds")
saveRDS(train_speineg, "./R-script, analysis/Models/train_speineg_lagged_speidich.rds")

saveRDS(test_speipos, "./R-script, analysis/Models/test_speipos_lagged_speidich.rds")
saveRDS(test_speineg, "./R-script, analysis/Models/test_speineg_lagged_speidich.rds")

# Run models --------------------------------------------------------------

# Model with negative SPEI
cf_neg_speidich <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speineg$spei3_neg,
  tune.parameters = "all", # Model tuens all tunable parameters that are set to NULL
  seed = 2865
)

# Save the model
save(cf_neg_speidich, file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")


# Model with positive SPEI
cf_pos_speidich <- causal_forest(
  X = model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speipos$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speipos$spei3_pos,
  tune.parameters = "all",
  seed = 2865
)

save(cf_pos_speidich, file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")


