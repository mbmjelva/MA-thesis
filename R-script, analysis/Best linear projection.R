## Best linear projection ##

library(tidyverse)
library(grf)
library(stargazer)
library(corrplot)

# Load relevant files
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")
load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich.rds")
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich.rds")


# Estimation of blp based on propensity scores ----------------------------
area_of_overlap <- cf_neg_speidich$W.hat > 0.1 & cf_neg_speidich$W.hat < 0.9 

X <- train_speineg[, (names(train_speineg) %in% 
                            c("shdi", "agri_ih", "empl_agr", 
                              "irrig_sum", "capdist", "ttime_mean", 
                              "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                              "temp", "libdem"))]

blp_overlap <- best_linear_projection(cf_neg_speidich, A = X, subset = which(area_of_overlap))

# CF-pos
area_of_overlap_pos <- cf_pos_speidich$W.hat > 0.1 & cf_pos_speidich$W.hat < 0.9 

X_pos <- train_speipos[, (names(train_speipos) %in% 
                        c("shdi", "agri_ih", "empl_agr", 
                          "irrig_sum", "capdist", "ttime_mean", 
                          "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                          "temp", "libdem"))]

blp_overlap_pos <- best_linear_projection(cf_pos_speidich, A = X_pos, subset = which(area_of_overlap_pos))


stargazer::stargazer(blp_overlap, blp_overlap_pos, # type = "text",
                     column.labels = c("CF-neg", "CF-pos"),
                     dep.var.caption = "")


## Analyse correlation plots (how much should the results depend on correlations here?)
corrplot(cor(X)) 
corrplot(cor(X_pos), title = "SPEI3 pos")

## Look at what happens when I exclude some of the variables with high correlations

## CF-neg
X_empl <- train_speineg[, (names(train_speineg) %in% 
                                 c("shdi", "agri_ih",
                                   "irrig_sum", "capdist", "ttime_mean", 
                                   "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                                   "temp", "libdem"))]

X_gdp <- train_speineg[, (names(train_speineg) %in% 
                                c("shdi", "agri_ih", "empl_agr",
                                  "irrig_sum", "capdist", "ttime_mean", 
                                  "pop", "excluded", "global_ind", "non_state_conflict",
                                  "temp", "libdem"))]

X_shdi <- train_speineg[, (names(train_speineg) %in% 
                                 c("agri_ih", "empl_agr",
                                   "irrig_sum", "capdist", "ttime_mean", 
                                   "pop", "excluded", "gdp", "global_ind", "non_state_conflict",
                                   "temp", "libdem"))]

X_global_ind <- train_speineg[, (names(train_speineg) %in% 
                                       c("shdi", "agri_ih", "empl_agr",
                                         "irrig_sum", "capdist", "ttime_mean", 
                                         "pop", "excluded", "gdp", "non_state_conflict",
                                         "temp", "libdem"))]

# Add all the models to one table
blp_overlap_empl <- best_linear_projection(cf_neg_speidich, A = X_empl, subset = which(area_of_overlap))
blp_overlap_gdp <- best_linear_projection(cf_neg_speidich, A = X_gdp, subset = which(area_of_overlap))
blp_overlap_shdi <- best_linear_projection(cf_neg_speidich, A = X_shdi, subset = which(area_of_overlap))
blp_overlap_global_ind <- best_linear_projection(cf_neg_speidich, A = X_global_ind, subset = which(area_of_overlap))


stargazer::stargazer(blp_overlap, blp_overlap_empl, blp_overlap_gdp,
                     blp_overlap_global_ind, blp_overlap_shdi, #type = "text",
                     column.labels = c("All", "Employment Agriculture", "GDP", "Globalization", "SHDI"),
                     dep.var.caption = "CF-neg")



## CF-pos
X_pos_empl <- train_speipos[, (names(train_speipos) %in% 
                            c("shdi", "agri_ih",
                              "irrig_sum", "capdist", "ttime_mean", 
                              "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                              "temp", "libdem"))]

X_pos_gdp <- train_speipos[, (names(train_speipos) %in% 
                            c("shdi", "agri_ih", "empl_agr",
                              "irrig_sum", "capdist", "ttime_mean", 
                              "pop", "excluded", "global_ind", "non_state_conflict",
                              "temp", "libdem"))]

X_pos_shdi <- train_speipos[, (names(train_speipos) %in% 
                                c("agri_ih", "empl_agr",
                                  "irrig_sum", "capdist", "ttime_mean", 
                                  "pop", "excluded", "gdp", "global_ind", "non_state_conflict",
                                  "temp", "libdem"))]

X_pos_global_ind <- train_speipos[, (names(train_speipos) %in% 
                                 c("shdi", "agri_ih", "empl_agr",
                                   "irrig_sum", "capdist", "ttime_mean", 
                                   "pop", "excluded", "gdp", "non_state_conflict",
                                   "temp", "libdem"))]

# Add all the models to one table
blp_overlap_pos_empl <- best_linear_projection(cf_pos_speidich, A = X_pos_empl, subset = which(area_of_overlap_pos))
blp_overlap_pos_gdp <- best_linear_projection(cf_pos_speidich, A = X_pos_gdp, subset = which(area_of_overlap_pos))
blp_overlap_pos_shdi <- best_linear_projection(cf_pos_speidich, A = X_pos_shdi, subset = which(area_of_overlap_pos))
blp_overlap_pos_global_ind <- best_linear_projection(cf_pos_speidich, A = X_pos_global_ind, subset = which(area_of_overlap_pos))


stargazer::stargazer(blp_overlap_pos, blp_overlap_pos_empl, blp_overlap_pos_gdp,
                     blp_overlap_pos_global_ind, blp_overlap_pos_shdi, #type = "text",
                     column.labels = c("All", "Employment Agriculture", "GDP", "Globalization", "SHDI"),
                     dep.var.caption = "CF-pos")





