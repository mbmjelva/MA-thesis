## Best linear projection ##

library(tidyverse)
library(grf)
library(stargazer)

# Load relevant files
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich_only_conflict.rds")
load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich_only_conflict.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich_only_conflict.rds")
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich_only_conflict.rds")

# Estimate the best linear projection of the CATEs
blp_neg <- best_linear_projection(cf_neg_speidich, 
                                  A = train_speineg[, (names(train_speineg) %in% 
                                                         c("shdi", "agri_ih", "empl_agr", 
                                                           "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                           "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                           "temp"))])
blp_pos <- best_linear_projection(cf_pos_speidich, 
                                  A = train_speipos[, (names(train_speipos) %in% 
                                                                          c("shdi", "agri_ih", "empl_agr", 
                                                                            "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                            "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                            "temp"))])

stargazer(blp_neg, blp_pos, type = "text",
          column.labels = c("CF-neg", "CF-pos"),
          dep.var.caption = "")

# Check if effects change much if exclude some of the variables
blp_neg_selected <- best_linear_projection(cf_neg_speidich, 
                                           A = train_speineg[, (names(train_speineg) %in% 
                                                                  c("shdi", "agri_ih", 
                                                                    "capdist", "ttime_mean", 
                                                                    "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                    "temp"))])

blp_pos_selected <- best_linear_projection(cf_pos_speidich, 
                                  A = train_speipos[, (names(train_speipos) %in% 
                                                         c("shdi", "agri_ih", 
                                                           "capdist", "ttime_mean", 
                                                           "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                           "temp"))])

stargazer(blp_neg_selected, blp_pos_selected, blp_neg, blp_pos, type = "text",
          column.labels = c("CF-neg", "CF-pos"),
          dep.var.caption = "", column.sep.width = "-15pt")

# Not much change 


