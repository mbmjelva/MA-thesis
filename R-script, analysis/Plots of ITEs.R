### Plots of individual treatment effects ###

library(tidyverse)
library(grf)

# Negative SPEI
load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged.rds")


preds <- predict(
  object = cf_neg, 
  newdata = model.matrix(~., data = test_speineg[, !(names(test_speineg) %in% c("conflict", "spei3_neg"))]), 
  estimate.variance = TRUE
)

test_speineg$preds <- preds$predictions

ggplot(test_speineg, aes(spei3_neg, preds)) +
  geom_point(colour = "#3C1518", alpha = 0.5) +
  theme_bw() +
  labs(y = "Estimated treatment effect", x = "SPEI3 negative")

ggsave("./Figurer/speineg estimated treatment effect.png")


# Positive SPEI
load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")

train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged.rds")

preds_pos <- predict(
  object = cf_pos, 
  newdata = model.matrix(~., data = test_speipos[, !(names(test_speipos) %in% c("conflict", "spei3_pos"))]), 
  estimate.variance = TRUE
)

test_speipos$preds <- preds_pos$predictions


ggplot(test_speipos, aes(spei3_pos, preds)) +
  geom_point(colour = "#3C1518", alpha = 0.5) +
  theme_bw() +
  labs(y = "Estimated treatment effect", x = "SPEI3 positive")

ggsave("./Figurer/speipos estimated treatment effect.png")
