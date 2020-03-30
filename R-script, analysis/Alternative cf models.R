### Alternative CF models ###


# SPEI3 neg, inverse ------------------------------------------------------
# sjekker om samme resultat

train_speineg$spei3_neg <- train_speineg$spei3_neg*(-1)

cf_neg_inverse <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speineg$spei3_neg[!is.na(train_speineg$spei3_neg)], # Must be without NA
  tune.parameters = "all", # Model tuens all tunable variables 
  seed = 2865
)

save(cf_neg_inverse, file = "./R-script, analysis/Models/cf_neg_inverse.rds")

# Variable importance
varimp_neg_inverse <- cf_neg_inverse %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg_inverse$X.orig)) %>% 
  arrange(desc(V1))


varimp_neg_wo_inv <- subset(varimp_neg_inverse, !variable %in% c("gid", "lon", "lat", "gwno"))

area_color <- c("#3C1518", "#B77659", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                "#3C1518", "#3C1518", "#B77659", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                "#3C1518", "#3C1518", "#B77659", "#B77659", "#3C1518", "#3C1518", "#3C1518", 
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#B77659")
#Colors: #B77659, #3C1518, #2D2A32, #842F43

ggplot(varimp_neg_wo_inv) + 
    geom_bar(aes(reorder(variable, V1), V1), stat = "identity") + # Reorder order the chategories depending on the values of a second variable (V1)
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(x = "", y = "Variable Importance", title = "SPEI3 negative") +
    coord_flip()



# Lagged SPEI3 values -----------------------------------------------------

library(tidyverse)
library(grf)
library(caret)
library(rpart.plot)
library(ggpubr)

sample_final <- readRDS("./Egne datasett/resampled_data_lagged.rds")

# Change to factor for later analysis
sample_final$conflict <- as.factor(sample_final$conflict)

# Remove:
# - The other conflict-variables and their lags, will be used as alternative outcome variables for robustness checks
# - The lagged geographical variables as they are not of substantial interest
# - The SPEI variables, will only use the lagged variables here
# - The full SPEI variable, might be used for robustness check?
sample_final <- select(sample_final, -best, -events, -lag_1_best, -lag_1_events, -lag_1_lon, -lag_1_lat, -lag_1_gwno, -lag_1_bdist3, 
                       -lag_1_capdist, -lag_1_ttime_mean, -lag_1_spei3, -spei3_neg, -spei3_pos, -spei3)

# Create training and test data set
set.seed(125)

trainrows <- createDataPartition(sample_final$conflict, p = 0.7, list = F) # Fra caret-pakka
training <- sample_final[trainrows,]
test <- sample_final[-trainrows,]


# Exclude NA to be able to run model (must have same lenght for all variables)
train_nona <- na.exclude(training)
test_nona <- na.exclude(test)

# Run seperate models with spei_neg and spei_pos as W. Must exclude the other variables to be able to run
train_speipos <- select(train_nona, -lag_1_spei3_neg)
train_speineg <- select(train_nona, -lag_1_spei3_pos)

test_speipos <- select(test_nona, -lag_1_spei3_neg)
test_speineg <- select(test_nona, -lag_1_spei3_pos)

# Run models --------------------------------------------------------------

# Model with negative SPEI
cf_neg_lag <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "lag_1_spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speineg$lag_1_spei3_neg[!is.na(train_speineg$lag_1_spei3_neg)], # Must be without NA
  tune.parameters = "all", # Model tuens all tunable variables 
  seed = 2865
)

# Save the model
save(cf_neg_lag, file = "./R-script, analysis/Models/cf_neg_lagged_w_speilag.rds")

# Variable importance
