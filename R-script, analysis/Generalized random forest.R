### GRF ###

library(tidyverse)
library(grf)
library(caret)
library(rpart.plot) 

sample_final <- readRDS("./Egne datasett/resampled_data.rds")

# Change to factor for later analysis
sample_final$conflict <- as.factor(sample_final$conflict)

# Remove the other conflict-variables. Here for robustness test (??)
sample_final <- select(sample_final, -best, -events)

# Create training and test data set
set.seed(125)

trainrows <- createDataPartition(sample_final$conflict, p = 0.7, list = F) # Fra caret-pakka
training <- sample_final[trainrows,]
test <- sample_final[-trainrows,]


# Exclude NA to be able to run model (must have same lenght for all variables)
train_nona <- na.exclude(training)
test_nona <- na.exclude(test)

# Run seperate models with spei_neg and spei_pos as W. Must exclude the other variables to be able to run
train_speipos <- select(train_nona, -spei3, -spei3_neg)
train_speineg <- select(train_nona, -spei3, -spei3_pos)

test_speipos <- select(test_nona, -spei3, -spei3_neg)
test_speineg <- select(test_nona, -spei3, -spei3_pos)

train_speifull <- select(train_nona, -spei3_pos, -spei3_neg)
test_speifull <- select(test_nona, -spei3_pos, -spei3_neg)



# Run models --------------------------------------------------------------


# Model with negative SPEI
cf_neg <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speineg$spei3_neg[!is.na(train_speineg$spei3_neg)], # Must be without NA
  num.trees = 1000,
  seed = 2865
)

# Save the model
save(cf_neg, file = "./R-script, analysis/Models/cf_neg.rds")

# Variable importance
varimp_neg <- cf_neg %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))


# Model with positive SPEI
cf_pos <- causal_forest(
  X = model.matrix(~., data = train_speipos[, !(names(train_speipos) %in% c("conflict", "spei3_pos"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speipos$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speipos$spei3_pos[!is.na(train_speipos$spei3_pos)], # Must be without NA
  num.trees = 1000,
  seed = 2865
)

save(cf_pos, file = "./R-script, analysis/Models/cf_pos.rds")

# Variable importance
varimp_pos <- cf_pos %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))


# Model with full-scale SPEI
cf_full <- causal_forest(
  X = model.matrix(~., data = train_speipos[, !(names(train_speifull) %in% c("conflict", "spei3"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speifull$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speifull$spei3[!is.na(train_speifull$spei3)], # Must be without NA
  num.trees = 1000,
  seed = 2865
)

save(cf_full, file = "./R-script, analysis/Models/cf_full.rds")

varimp_full <- cf_full %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))




# Plots of variable importance --------------------------------------------
# Fargevalg for analysen: #CE916D/#C78A4C, #98A982, #615E5D

load(file = "./R-script, analysis/Models/cf_neg.rds")
load(file = "./R-script, analysis/Models/cf_pos.rds")
load(file = "./R-script, analysis/Models/cf_full.rds")

area_color <- c("#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D",
                "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#CE916D", "#615E5D",
                "#615E5D", "#615E5D", "#615E5D", "#98A982", "#615E5D", "#615E5D")

(neg <- ggplot(varimp_neg) + 
   geom_bar(aes(reorder(variable, V1), V1), stat = "identity", fill = area_color) + # Reorder order the chategories depending on the values of a second variable (V1)
   theme_minimal() +
   scale_y_continuous(limits = c(0, 0.3)) +
   labs(x = "", y = "Variable Importance", title = "SPEI3 negative") +
   coord_flip())

area_color_pos <- c("#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D",
                "#615E5D", "#615E5D", "#615E5D", "#98A982", "#615E5D", "#615E5D", "#615E5D",
                "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#615E5D", "#CE916D")

(pos <- ggplot(varimp_pos) + 
  geom_bar(aes(reorder(variable, V1), V1), stat = "identity", fill = area_color_pos) + # Reorder order the chategories depending on the values of a second variable (V1)
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.3)) +
  labs(x = "", y = "Variable Importance", title = "SPEI3 positive") +
  coord_flip())


(full <- ggplot(varimp_full) + 
  geom_bar(aes(reorder(variable, V1), V1), stat = "identity", fill = "#615E5D") + # Reorder order the chategories depending on the values of a second variable (V1)
  theme_minimal() +
  labs(x = "", y = "Variable Importance", title = "SPEI3") +
  #scale_x_discrete() +
  coord_flip())


ggpubr::ggarrange(neg, pos)

ggsave("./Figurer/variance_importance_threegraphs.jpg")
