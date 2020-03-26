# Generalized random forest - lagged model #

# Nb, koden er ikke endret fra scriptet til generalized random forest, bortsett fra å bruke et annet datasett og endre navn på lagrede produkt. 
# Altså vil dette scriptet overkjøre det andre hvis man kjører samtidig.

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
# - The lagged SPEI variables, they will be used as alternative treatment effects for robustness checks
# - The full SPEI variable, might be used for robustness check?
sample_final <- select(sample_final, -best, -events, -lag_1_best, -lag_1_events, lag_1_lon, lag_1_lat, lag_1_gwno, lag_1_bdist3, lag_1_capdist, lag_1_ttime_mean,
                       -lag_1_spei3, -lag_1_spei3_neg, -lag_1_spei3_pos, -spei3)

# Create training and test data set
set.seed(125)

trainrows <- createDataPartition(sample_final$conflict, p = 0.7, list = F) # Fra caret-pakka
training <- sample_final[trainrows,]
test <- sample_final[-trainrows,]


# Exclude NA to be able to run model (must have same lenght for all variables)
train_nona <- na.exclude(training)
test_nona <- na.exclude(test)

# Run seperate models with spei_neg and spei_pos as W. Must exclude the other variables to be able to run
train_speipos <- select(train_nona, -spei3_neg)
train_speineg <- select(train_nona, -spei3_pos)

test_speipos <- select(test_nona, -spei3_neg)
test_speineg <- select(test_nona, -spei3_pos)

saveRDS(train_speipos, "./R-script, analysis/Models/train_speipos_lagged.rds")
saveRDS(train_speineg, "./R-script, analysis/Models/train_speineg_lagged.rds")

saveRDS(test_speipos, "./R-script, analysis/Models/test_speipos_lagged.rds")
saveRDS(test_speineg, "./R-script, analysis/Models/test_speineg_lagged.rds")

# Run models --------------------------------------------------------------

# Model with negative SPEI
cf_neg <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_speineg$spei3_neg[!is.na(train_speineg$spei3_neg)], # Must be without NA
  tune.parameters = "all", # Model tuens all tunable variables 
  seed = 2865
)

# Save the model
save(cf_neg, file = "./R-script, analysis/Models/cf_neg_lagged.rds")

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
  tune.parameters = "all",
  seed = 2865
)

save(cf_pos, file = "./R-script, analysis/Models/cf_pos_lagged.rds")

# Variable importance
varimp_pos <- cf_pos %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))


## Plotter
# SPEI_neg
# Remove variables that are not explanatory so that not included in the plot
varimp_neg_wo <- subset(varimp_neg, !variable %in% c("gid", "lon", "lat"))

area_color <- c("#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", 
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#2D2A32")
#Colors: #B77659, #3C1518, #2D2A32, #842F43

(spei_neg <- ggplot(varimp_neg_wo) + 
    geom_bar(aes(reorder(variable, V1), V1), stat = "identity", fill = "#3C1518") + # Reorder order the chategories depending on the values of a second variable (V1)
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(x = "", y = "Variable Importance", title = "SPEI3 negative") +
    coord_flip())

ggsave("./Figurer/speineg_varimp.png")

# SPEI_pos
# Remove variables that are not explanatory so that not included in the plot
varimp_pos_wo <- subset(varimp_pos, !variable %in% c("gid", "lon", "lat", "lag_1_lon", "lag_1_lat", "lag_1_spei3", "lag_1_spei3_pos", "lag_1_spei3_neg"))

(spei_pos <- ggplot(varimp_pos_wo) + 
  geom_bar(aes(reorder(variable, V1), V1), stat = "identity") + # Reorder order the chategories depending on the values of a second variable (V1)
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(x = "", y = "Variable Importance", title = "SPEI3 positive") +
  coord_flip())


ggpubr::ggarrange(spei_neg, spei_pos)

ggsave("./Figurer/variable_importence_lagged.png")
