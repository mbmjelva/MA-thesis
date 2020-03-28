### Testing for heterogeneity in the models ###

library(grf)
library(stargazer)

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

# Estimate the treatment heterogeneity
het_test <- test_calibration(cf_neg)
tau.hat <- predict(cf_neg)$predictions

het_test_pos <- test_calibration(cf_pos)
tau.hat_pos <- predict(cf_pos)$predictions

hist(tau.hat, density = 25, breaks = 40, col = "#B77659", xlab = "", main = "SPEI3 negative")
hist(tau.hat_pos, density = 20, breaks = 40,  col = "#B77659", xlab = "", main = "SPEI3 positive")



# Estimate the average treatment effect
ate.hat = average_partial_effect(cf_neg)
print(paste("95% CI for ATE:", round(ate.hat["estimate"], 3), "+/-", round(1.96 * ate.hat["std.err"], 3)))

# Estimate the average treatment effect
ate.hat_pos = average_partial_effect(cf_pos)
print(paste("95% CI for ATE:", round(ate.hat_pos["estimate"], 3), "+/-", round(1.96 * ate.hat_pos["std.err"], 3)))


# Add all info to one graph
stargazer::stargazer(het_test, het_test_pos,
                     column.labels = c("SPEI3 neg", "SPEI3 pos"),
                     dep.var.caption = "",
                     covariate.labels = c("Mean forest prediction", "Differential forest prediction"),
                     add.lines = list(c("Estimate", "0.123***", "-0.080***"),
                                      c("", "(0.008)", "(0.008)")))





split_frequencies(cf_neg) # Funker, men skjønner ikke helt hva som kommer ut
# https://grf-labs.github.io/policytree/articles/policytree.html#binary-treatment-effect-estimation-and-policy-learning


