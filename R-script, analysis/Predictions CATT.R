## Predictions of CATTs ##


library(tidyverse)
library(grf)
library(stargazer)

# Load relevant files
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich_only_conflict.rds")
load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich_only_conflict.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich_only_conflict.rds")
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich_only_conflict.rds")

test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged_speidich_only_conflict.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged_speidich_only_conflict.rds")


# Make predictions of how the treatment effect would vary if all other variables are held constant

# Set all continuous variables to mean and all factors to median
pred1 <- with(train_speineg, data.frame(spei3_neg = 0:1,
                                        libdem = 0:1, 
                                        temp = mean(temp), 
                                        capdist = mean(capdist), 
                                        bdist3 = mean(bdist3), 
                                        ttime_mean = mean(ttime_mean), 
                                        global_ind = mean(global_ind), 
                                        pop = mean(pop),
                                        gdp = mean(gdp), 
                                        shdi = mean(shdi), 
                                        empl_agr = mean(empl_agr), 
                                        irrig_sum = mean(irrig_sum), 
                                        agri_ih = mean(agri_ih), 
                                        excluded = median(excluded), 
                                        lag_conflict = median(excluded), 
                                        non_state_conflict = median(non_state_conflict)))


pred_plot <- data.frame(predict(all, newdata = pred1, type = "risk", se = TRUE, reference = "sample"), 
                        pred1)

pred_plot$upper <- pred_plot$fit + 1.96 * pred_plot$se.fit
pred_plot$lower <- pred_plot$fit - 1.96 * pred_plot$se.fit
# Funker dårlig


# Prøver Marks metode
preds <- predict(
  object = cf_neg_speidich, 
  newdata = model.matrix(~ ., test_speineg[, !(names(test_speineg) %in% c("conflict", "spei3_neg"))]),
  estimate.variance = TRUE,
)

test_speineg$preds <- preds$predictions

test_speineg1 <- filter(test_speineg, spei3_neg > 0)

p1 <- ggplot(test_speineg1, aes(x = libdem, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p2 <- ggplot(test_speineg1, aes(x = bdist3, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p3 <- ggplot(test_speineg1, aes(x = temp, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p4 <- ggplot(test_speineg1, aes(x = shdi, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

?geom_smooth

## En tredje metode?

forest <- causal_forest(X, Y, W)
tau.hat <- predict(cf_neg_speidich)
Y.hat.0 <- cf_neg_speidich$Y.hat -  cf_neg_speidich$W.hat * tau.hat   # E[Y|X,W=0]
Y.hat.1 <- cf_neg_speidich$Y.hat + (1 -  cf_neg_speidich$W.hat) * tau.hat  # E[Y|X, W=1]

