### Estimation of CATEs ###
# Lagged, spei binary

# Using averatge_treatment_effect


library(tidyverse)
library(grf)

load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged_speidich.rds")

# Tester med numerisk variabel
test_speineg$lag_conflict <- as.numeric(as.character(test_speineg$lag_conflict))
train_speineg$lag_conflict <- as.numeric(as.character(train_speineg$lag_conflict))

test_speineg <- dplyr::select(test_speineg, -conflict, -spei3_neg, -lon, -lat, -gwno)# Must remove the cluster-variable and treatment and outcome
train_speineg <- dplyr::select(train_speineg, -conflict, -spei3_neg, -lon, -lat, -gwno)

cates_neg <- tibble()

vars <- names(test_speineg)

for(var in vars){
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0, 1, 0.25))
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_treatment_effect(cf_neg_speidich, subset = train_speineg[[var]] <= qtl, target.sample = "overlap")
    } else if (qtl == qtl_l) {
      res <- average_treatment_effect(cf_neg_speidich, subset = train_speineg[[var]] <= qtl, target.sample = "overlap")
    } else {
      res <- average_treatment_effect(cf_neg_speidich, subset = (train_speineg[[var]] <= qtl) & (train_speineg[[var]] > qtl_l), target.sample = "overlap")
    }
    cates_neg <- bind_rows(cates_neg, tibble("variable" = var, "quantile" = qtl, "ape" = res[1], "ape_std" = res[2]))
  }
}


cates_neg$apemin <- cates_neg$ape - (1.96*cates_neg$ape_std)
cates_neg$apemax <- cates_neg$ape + (1.96*cates_neg$ape_std)

cates_neg$significant <- if_else(cates_neg$apemin > 0 | cates_neg$apemax < 0, "yes", "no")

save(cates_neg, file = "./R-script, analysis/Models/cates_neg_speidich_with_target.rds")



# SPEI pos ----------------------------------------------------------------


load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")

train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged_speidich.rds")

# Tester med numerisk variabel
test_speipos$lag_conflict <- as.numeric(as.character(test_speipos$lag_conflict))
train_speipos$lag_conflict <- as.numeric(as.character(train_speipos$lag_conflict))

test_speipos <- dplyr::select(test_speipos, -conflict, -spei3_pos, -lon, -lat, -gwno)# Must remove the cluster-variable and treatment and outcome
train_speipos <- dplyr::select(train_speipos, -conflict, -spei3_pos, -lon, -lat, -gwno)

cates_pos <- tibble()

vars <- names(test_speipos)

for(var in vars){
  var_quantiles <- quantile(train_speipos[[var]], probs = seq(0.05, 0.95, 0.25))
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_treatment_effect(cf_pos_speidich, subset = train_speipos[[var]] <= qtl, target.sample = "overlap")
    } else if (qtl == qtl_l) {
      res <- average_treatment_effect(cf_pos_speidich, subset = train_speipos[[var]] <= qtl, target.sample = "overlap")
    } else {
      res <- average_treatment_effect(cf_pos_speidich, subset = (train_speipos[[var]] <= qtl) & (train_speipos[[var]] > qtl_l), target.sample = "overlap")
    }
    cates_pos <- bind_rows(cates_pos, tibble("variable" = var, "quantile" = qtl, "ape" = res[1], "ape_std" = res[2]))
  }
}


cates_pos$apemin <- cates_pos$ape - (1.96*cates_pos$ape_std)
cates_pos$apemax <- cates_pos$ape + (1.96*cates_pos$ape_std)

cates_pos$significant <- if_else(cates_pos$apemin > 0 | cates_pos$apemax < 0, "yes", "no")

save(cates_pos, file = "./R-script, analysis/Models/cates_pos_speidich_with_target.rds")
