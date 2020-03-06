### GRF ###

library(tidyverse)
library(grf)
library(caret)
library(rpart.plot) 

sample_final <- readRDS("./Egne datasett/resampled_data_enmill.rds")

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

cf_neg <- causal_forest(
  X = model.matrix(~., data = train_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), # exclude the outcome and treatment variables
  Y = as.numeric(train_speineg$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = train_nona$spei3[!is.na(train_speineg$spei3_neg)], # Must be without NA
  num.trees = 1000,
  seed = 2865
)

cf_neg


# Fra grf-github ----------------------------------------------------------

tau.hat.oob <- predict(cf_neg)
hist(tau.hat.oob$predictions)

# Estimate treatment effects for the test sample.


tau.hat <- predict(cf_neg, test_speineg)
plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlab = "x", ylab = "tau", type = "l")
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 2)

# Estimate the conditional average treatment effect on the full sample (CATE).
average_treatment_effect(cf_neg, target.sample = "all")

# Estimate the conditional average treatment effect on the treated sample (CATT).
# Here, we don't expect much difference between the CATE and the CATT, since
# treatment assignment was randomized.
average_treatment_effect(cf_neg, target.sample = "treated")


# Fra Mark ----------------------------------------------------------------
# Thank you Mark

# Variable importance
cf_neg %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))

preds <- predict(
  object = cf_neg, 
  newdata = model.matrix(~ ., data = test_speineg[, !(names(train_speineg) %in% c("conflict", "spei3_neg"))]), 
  estimate.variance = TRUE
)

# Mark gjorde noe annet her. Det funket ikke. Hvorfor? test_speineg$preds <- preds$predictions[, 1]
test_speineg$preds <- preds$predictions
test_speineg


p1 <- ggplot(test_speineg, aes(x = unempl_tot, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()
p1

p2 <- ggplot(test_speineg, aes(x = canvass_minutes, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p3 <- ggplot(test_speineg, aes(x = trans_therm_pre, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p4 <- ggplot(test_speineg, aes(x = sdo, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

cowplot::plot_grid(p1, p2, p3, p4)
