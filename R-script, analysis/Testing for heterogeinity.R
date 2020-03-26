### Testing for heterogeneity in the models ###

library(grf)
library(stargazer)

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

# Estimate the treatment heterogeneity
het_test <- test_calibration(cf_neg)
stargazer::stargazer(het_test)

tau.hat <- predict(cf_neg)$predictions
hist(tau.hat, density = 25, breaks = 40, col = "#B77659", xlab = "Estimated treatment effect", main = "Negative SPEI3")

het_test_pos <- test_calibration(cf_pos)
stargazer::stargazer(het_test, het_test_pos)


tau.hat_pos <- predict(cf_pos)$predictions
hist(tau.hat_pos, density = 20, breaks = 40)

# Sjekker ATE

# Estimate the average treatment effect. We perhaps have a moderate positive effect.
ate.hat = average_partial_effect(cf_neg)
print(paste("95% CI for ATE:", round(ate.hat["estimate"], 3), "+/-", round(1.96 * ate.hat["std.err"], 3)))
