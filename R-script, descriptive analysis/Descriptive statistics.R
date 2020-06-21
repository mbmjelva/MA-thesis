
#### Descriptive statistics ######

library(tidyverse)
library(stargazer)
library(ggpubr)
library(finalfit)
library(grf)
library(corrplot)
library(randomForest)
library(rpart)
library(rpart.plot)

final <- read_rds("./Egne datasett/final_dataset.rds")

# Make table of descriptive statistics
final_df <- final %>% select(-starts_with("lag_1"), -gid, -gwno, -lon, -lat, -year, -bdist3, -events, 
                             -best, -unempl_tot) %>% as.data.frame()# For å kunne bruke stargazer, kan ikke være tibble.

stargazer(final_df, #type = "text",
          title = "Descriptive statistics", 
          covariate.labels = c("Non-State Conflict", "SPEI3", "SPEI3 positive", "SPEI3 negative", 
                               "Temperature", "Agricultural area in cell (percentage)",
                                "Total are covered by irrigation in cell",
                                "Distance to capital (km)",
                                "Travel time to nearest\nurban center (min)",
                                "Population", "Employment in agriculture",
                                "Number of excluded groups",
                                "SHDI", "Liberal Democracy Index", "Globalization index",
                                "GDP"))


# Make correlation plots for operationalization of variables, Chap ------------------------

# Check correlation between variables that operationalize the same concept

# Dependency on rain-fed agriculture

# In order for the machine to calculate the correlation plots, need a lower number of observations.
# Since the train_datasets are picked randomly, I just chose one of these. The distribution should be the same as for the bigger dataset.
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich.rds")

dep_agriculture <- train_speipos[, (names(train_speipos) %in% c("agri_ih", "empl_agr", 
                                             "irrig_sum"))]
corrplot(cor(dep_agriculture))

good_governance <- train_speipos[, (names(train_speipos) %in% c("shdi", "gdp", "excluded"))]
corrplot(cor(good_governance))

urbanization <- train_speipos[, (names(train_speipos) %in% c("ttime_mean", "capdist"))]
corrplot(cor(urbanization))


## Plot CF tree ##
final <- final %>%
  select(-starts_with("lag"), -unempl_tot, -events, -best, -spei3, -spei3_neg, -spei3_pos)

# Example of decision tree - Chapter 4 ------------------------------------

tree <- rpart(conflict~., data=final)

rpart.plot(tree)



