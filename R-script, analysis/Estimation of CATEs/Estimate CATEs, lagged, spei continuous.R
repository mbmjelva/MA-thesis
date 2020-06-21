### Estimation of CATEs ###

# SPEI continuous

# This script estimates the CATEs for different subgroups of the population with continuous treatment
# Prepares the data to be plottet for the plots of CATEs in the analysis and appendices


library(tidyverse)
library(grf)

load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged.rds")

# Make variable numeric
test_speineg$lag_conflict <- as.numeric(as.character(test_speineg$lag_conflict))
train_speineg$lag_conflict <- as.numeric(as.character(train_speineg$lag_conflict))

test_speineg <- dplyr::select(test_speineg, -conflict, -spei3_neg, -lon, -lat, -gwno)# Must remove the cluster-variable and treatment and outcome
train_speineg <- dplyr::select(train_speineg, -conflict, -spei3_neg, -lon, -lat, -gwno)

area_of_overlap <- cf_neg$W.hat > 0.1 & cf_neg$W.hat < 0.9

cates_neg <- tibble()

vars <- names(test_speineg)

for(var in vars){
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0.05, 0.95, 0.25), na.rm = T)
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_partial_effect(cf_neg, subset = (area_of_overlap & (train_speineg[[var]] <= qtl)))
    } else if (qtl == qtl_l) {
      res <- average_partial_effect(cf_neg, subset = (area_of_overlap & (train_speineg[[var]] <= qtl)))
    } else {
      res <- average_partial_effect(cf_neg, subset = (area_of_overlap & (train_speineg[[var]] <= qtl) & (train_speineg[[var]] > qtl_l)))
    }
    cates_neg <- bind_rows(cates_neg, tibble("variable" = var, "quantile" = qtl, "cate" = res[1], "cate_std" = res[2]))
  }
}


cates_neg$catemin <- cates_neg$cate - (1.96*cates_neg$cate_std)
cates_neg$catemax <- cates_neg$cate + (1.96*cates_neg$cate_std)

cates_neg$significant <- if_else(cates_neg$catemin > 0 | cates_neg$catemax < 0, "yes", "no")

save(cates_neg, file = "./R-script, analysis/Models/cates_neg_with_overlap, continuous.rds")



# SPEI pos ----------------------------------------------------------------

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")

train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged.rds")

# Tester med numerisk variabel
test_speipos$lag_conflict <- as.numeric(as.character(test_speipos$lag_conflict))
train_speipos$lag_conflict <- as.numeric(as.character(train_speipos$lag_conflict))

test_speipos <- dplyr::select(test_speipos, -conflict, -spei3_pos, -lon, -lat, -gwno)# Must remove the cluster-variable and treatment and outcome
train_speipos <- dplyr::select(train_speipos, -conflict, -spei3_pos, -lon, -lat, -gwno)

area_of_overlap_pos <- cf_pos$W.hat > 0.1 & cf_pos$W.hat < 0.9

cates_pos <- tibble()

vars <- names(test_speipos)

for(var in vars){
  var_quantiles <- quantile(train_speipos[[var]], probs = seq(0.05, 0.95, 0.25), na.rm = T)
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_partial_effect(cf_pos, subset = (area_of_overlap & (train_speipos[[var]] <= qtl)))
    } else if (qtl == qtl_l) {
      res <- average_partial_effect(cf_pos, subset = (area_of_overlap & (train_speipos[[var]] <= qtl)))
    } else {
      res <- average_partial_effect(cf_pos, subset = (area_of_overlap & (train_speipos[[var]] <= qtl) & (train_speipos[[var]] > qtl_l)))
    }
    cates_pos <- bind_rows(cates_pos, tibble("variable" = var, "quantile" = qtl, "cate" = res[1], "cate_std" = res[2]))
  }
}


cates_pos$catemin <- cates_pos$cate - (1.96*cates_pos$cate_std)
cates_pos$catemax <- cates_pos$cate + (1.96*cates_pos$cate_std)

cates_pos$significant <- if_else(cates_pos$catemin > 0 | cates_pos$catemax < 0, "yes", "no")

save(cates_pos, file = "./R-script, analysis/Models/cates_pos_overlap_continuous.rds")
