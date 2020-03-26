### Testing for heterogeneity in the models ###

library(grf)

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")


het_test <- test_calibration(cf_neg)
het_test_pos <- test_calibration(cf_pos)

stargazer::stargazer(het_test, het_test_pos)

tau.hat <- predict(cf_neg)$predictions
hist(tau.hat, density = 20, breaks = 40)

tau.hat_pos <- predict(cf_pos)$predictions
hist(tau.hat_pos, density = 20, breaks = 40)
