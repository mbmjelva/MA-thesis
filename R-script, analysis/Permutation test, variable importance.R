### Permutation test ###

# Unsure if this is the correct way to do it. But in ONeill and Weeks they created a permutation test by permuting
# the dependent variable 1000 times and creating 1000 forests based on that, for then to use that information to create p-values.
# My dependent variable is binary, so I guess I can only permute it once (if I have understood permutation correctly. Reorder the values on the dependent variable)



library(tidyverse)
library(grf)
library(caret)
library(rpart.plot)
library(ggpubr)


train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged.rds")

train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged.rds")

# Reorder values on dependent variable

train_speineg$conflict <- factor(train_speineg$conflict, levels = c("1","0"))
test_speineg$conflict <- factor(test_speineg$conflict, levels = c("1","0"))
train_speipos$conflict <- factor(train_speipos$conflict, levels = c("1","0"))
test_speipos$conflict <- factor(test_speipos$conflict, levels = c("1","0"))


# Run the models again with permuted dependent variable
# Negative SPEI
cf_neg_permuted <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speineg$spei3_neg[!is.na(train_speineg$spei3_neg)], # Must be without NA
  tune.parameters = "all", # Model tuens all tunable variables 
  seed = 2865
)

save(cf_neg_permuted, file = "./R-script, analysis/Models/cf_neg_permuted.rds")

varimp_neg <- cf_neg_permuted %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg_permuted$X.orig)) %>% 
  arrange(desc(V1))


# Positive SPEI
cf_pos_permuted <- causal_forest(
  X = model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speipos$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speipos$spei3_pos[!is.na(train_speipos$spei3_pos)], # Must be without NA
  tune.parameters = "all", # Model tuens all tunable variables 
  seed = 2865
)

save(cf_neg_permuted, file = "./R-script, analysis/Models/cf_neg_permuted.rds")

varimp_neg <- cf_neg_permuted %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg_permuted$X.orig)) %>% 
  arrange(desc(V1))

