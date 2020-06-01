### Diverse testing av ATES med treated og overlap etc. ###

library(tidyverse)
library(grf)

# Load files
load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich_only_conflict.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich_only_conflict.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich_only_conflict.rds")
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich_only_conflict.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged_speidich_only_conflict.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged_speidich_only_conflict.rds")

# Calculate ATT -----------------------------------------------------------
att_neg <- average_treatment_effect(cf_neg_speidich, target.sample = "treated")
att_pos <- average_treatment_effect(cf_pos_speidich, target.sample = "treated")

# Calculate CATT SPEI neg ----------------------------------------------------------

test_speineg$lag_conflict <- as.numeric(as.character(test_speineg$lag_conflict))
train_speineg$lag_conflict <- as.numeric(as.character(train_speineg$lag_conflict))

test_speineg <- dplyr::select(test_speineg, -conflict, -spei3_neg, -lon, -lat, -gwno)# Must remove the cluster-variable and treatment and outcome
train_speineg <- dplyr::select(train_speineg, -conflict, -spei3_neg, -lon, -lat, -gwno)

catt_neg <- tibble()

vars <- names(test_speineg)

for(var in vars){
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0, 1, 0.25))
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_treatment_effect(cf_neg_speidich, subset = train_speineg[[var]] <= qtl, target.sample = "treated")
    } else if (qtl == qtl_l) {
      res <- average_treatment_effect(cf_neg_speidich, subset = train_speineg[[var]] <= qtl, target.sample = "treated")
    } else {
      res <- average_treatment_effect(cf_neg_speidich, subset = (train_speineg[[var]] <= qtl) & (train_speineg[[var]] > qtl_l), target.sample = "treated")
    }
    catt_neg <- bind_rows(catt_neg, tibble("variable" = var, "quantile" = qtl, "catt" = res[1], "catt_std" = res[2]))
  }
}


catt_neg$cattmin <- catt_neg$catt - (1.96*catt_neg$catt_std)
catt_neg$cattmax <- catt_neg$catt + (1.96*catt_neg$catt_std)

catt_neg$significant <- if_else(catt_neg$cattmin > 0 | catt_neg$cattmax < 0, "yes", "no")

save(catt_neg, file = "./R-script, analysis/Models/catt_neg_speidich_only_conflict.rds")



# Calculate CATT SPEI pos -----------------------------------------------------------
test_speipos$lag_conflict <- as.numeric(as.character(test_speipos$lag_conflict))
train_speipos$lag_conflict <- as.numeric(as.character(train_speipos$lag_conflict))

test_speipos <- dplyr::select(test_speipos, -conflict, -spei3_pos, -lon, -lat, -gwno)# Must remove the cluster-variable and treatment and outcome
train_speipos <- dplyr::select(train_speipos, -conflict, -spei3_pos, -lon, -lat, -gwno)

catt_pos <- tibble()

vars <- names(test_speipos)

for(var in vars){
  var_quantiles <- quantile(train_speipos[[var]], probs = seq(0.05, 0.95, 0.25))
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_treatment_effect(cf_pos_speidich, subset = train_speipos[[var]] <= qtl, target.sample = "treated")
    } else if (qtl == qtl_l) {
      res <- average_treatment_effect(cf_pos_speidich, subset = train_speipos[[var]] <= qtl, target.sample = "treated")
    } else {
      res <- average_treatment_effect(cf_pos_speidich, subset = (train_speipos[[var]] <= qtl) & (train_speipos[[var]] > qtl_l), target.sample = "treated")
    }
    catt_pos <- bind_rows(catt_pos, tibble("variable" = var, "quantile" = qtl, "catt" = res[1], "catt_std" = res[2]))
  }
}


catt_pos$cattmin <- catt_pos$catt - (1.96*catt_pos$catt_std)
catt_pos$cattmax <- catt_pos$catt + (1.96*catt_pos$catt_std)

catt_pos$significant <- if_else(catt_pos$cattmin > 0 | catt_pos$cattmax < 0, "yes", "no")

save(catt_pos, file = "./R-script, analysis/Models/catt_pos_speidich_only_conflict.rds")


