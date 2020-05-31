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

blp_neg_cate <- best_linear_projection(cf_neg_speidich, 
                                       A = train_speineg[, (names(train_speineg) %in% 
                                                              c("shdi", "agri_ih", "empl_agr", 
                                                                "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                "temp"))])
blp_pos_cate <- best_linear_projection(cf_pos_speidich, 
                                       A = train_speipos[, (names(train_speipos) %in% 
                                                              c("shdi", "agri_ih", "empl_agr", 
                                                                "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                "temp"))])

blp_neg_catt <- best_linear_projection(cf_neg_speidich, 
                                  A = train_speineg[, (names(train_speineg) %in% 
                                                         c("shdi", "agri_ih", "empl_agr", 
                                                           "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                           "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                           "temp"))],
                                  subset = train_speineg$spei3_neg > 0)
blp_pos_catt <- best_linear_projection(cf_pos_speidich, 
                                  A = train_speipos[, (names(train_speipos) %in% 
                                                                          c("shdi", "agri_ih", "empl_agr", 
                                                                            "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                            "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                            "temp"))],
                                  subset = train_speipos$spei3_pos > 0)

stargazer(blp_neg_catt, blp_neg_cate, blp_pos_catt, blp_pos_cate, #type = "text",
          column.labels = c("CF-neg CATT", "CF-neg CATE", "CF-pos CATT", "CF-pos CATE"),
          dep.var.caption = "")


# Check if vary per continent ---------------------------------------------

# Africa
blp_neg_catt_af <- best_linear_projection(cf_neg_speidich, 
                                       A = train_speineg[, (names(train_speineg) %in% 
                                                              c("shdi", "agri_ih", "empl_agr", 
                                                                "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                "temp"))],
                                       subset = c(train_speineg$spei3_neg > 0 & train_speineg$continent == "Africa"))
blp_pos_catt_af <- best_linear_projection(cf_pos_speidich, 
                                       A = train_speipos[, (names(train_speipos) %in% 
                                                              c("shdi", "agri_ih", "empl_agr", 
                                                                "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                "temp"))],
                                       subset = c(train_speipos$spei3_pos > 0 & train_speipos$continent == "Africa")) 

# Asia
blp_neg_catt_as <- best_linear_projection(cf_neg_speidich, 
                                          A = train_speineg[, (names(train_speineg) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speineg$spei3_neg > 0 & train_speineg$continent == "Asia"))
blp_pos_catt_as <- best_linear_projection(cf_pos_speidich, 
                                          A = train_speipos[, (names(train_speipos) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speipos$spei3_pos > 0 & train_speipos$continent == "Asia")) 
# Americas
blp_neg_catt_am <- best_linear_projection(cf_neg_speidich, 
                                          A = train_speineg[, (names(train_speineg) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speineg$spei3_neg > 0 & train_speineg$continent == "Americas"))
blp_pos_catt_am <- best_linear_projection(cf_pos_speidich, 
                                          A = train_speipos[, (names(train_speipos) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speipos$spei3_pos > 0 & train_speipos$continent == "Americas")) 

# Europe
blp_neg_catt_eu <- best_linear_projection(cf_neg_speidich, 
                                          A = train_speineg[, (names(train_speineg) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speineg$spei3_neg > 0 & train_speineg$continent == "Europe"))
blp_pos_catt_eu <- best_linear_projection(cf_pos_speidich, 
                                          A = train_speipos[, (names(train_speipos) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speipos$spei3_pos > 0 & train_speipos$continent == "Europe")) 

# Oceania

blp_neg_catt_oc <- best_linear_projection(cf_neg_speidich, 
                                          A = train_speineg[, (names(train_speineg) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speineg$spei3_neg > 0 & train_speineg$continent == "Oceania"))

blp_pos_catt_oc <- best_linear_projection(cf_pos_speidich, 
                                          A = train_speipos[, (names(train_speipos) %in% 
                                                                 c("shdi", "agri_ih", "empl_agr", 
                                                                   "irrig_sum", "bdist3", "capdist", "ttime_mean", 
                                                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                                                   "temp"))],
                                          subset = c(train_speipos$spei3_pos > 0 & train_speipos$continent == "Oceania")) 

stargazer(blp_neg_catt_af, blp_neg_catt_as, blp_neg_catt_am, blp_neg_catt_eu, blp_neg_catt_oc, #type = "text",
          column.labels = c("Africa", "Asia", "America", "Europe", "Oceania"), dep.var.caption = "CATT",
          title = "Best linear projection of the CATT, negative SPEI3")

stargazer(blp_pos_catt_af, blp_pos_catt_as, blp_pos_catt_am, blp_pos_catt_eu, blp_pos_catt_oc, #type = "text",
          column.labels = c("Africa", "Asia", "America", "Europe", "Oceania"), dep.var.caption = "CATT",
          title = "Best linear projection of the CATT, positive SPEI3")

