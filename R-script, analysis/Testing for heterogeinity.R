### Testing for heterogeneity in the models ###

library(grf)
library(stargazer)


# SPEI continuous, lagged vars --------------------------------------------

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

# Estimate the treatment heterogeneity
het_test <- test_calibration(cf_neg)
tau.hat <- predict(cf_neg)$predictions

het_test_pos <- test_calibration(cf_pos)
tau.hat_pos <- predict(cf_pos)$predictions

hist(tau.hat, density = 25, breaks = 40, col = "#B77659", xlab = "Estimated treatment effects", main = "SPEI3 negative")
hist_speipos <- hist(tau.hat_pos, density = 20, breaks = 40,  col = "#B77659", xlab = "Estimated treatment effects", main = "SPEI3 positive")



# Estimate the average treatment effect
ate.hat <- average_partial_effect(cf_neg)
print(paste("95% CI for ATE:", round(ate.hat["estimate"], 3), "+/-", round(1.96 * ate.hat["std.err"], 3)))

# Estimate the average treatment effect
ate.hat_pos <- average_partial_effect(cf_pos)
print(paste("95% CI for ATE:", round(ate.hat_pos["estimate"], 3), "+/-", round(1.96 * ate.hat_pos["std.err"], 3)))


# Add all info to one graph
stargazer::stargazer(het_test, het_test_pos, type = "text",
                     column.labels = c("SPEI3 neg", "SPEI3 pos"),
                     dep.var.caption = "",
                     covariate.labels = c("Mean forest prediction", "Differential forest prediction"),
                     #add.lines = list(c("Estimated treatment effect", "0.123***", "-0.080***"),
                     #                 c("", "(0.008)", "(0.008)")),
                     out = "./Figurer/Differential forest prediction.html")



# SPEI dichotomous, laggede vars ------------------------------------------

load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")

# Estimate the treatment heterogeneity
het_test <- test_calibration(cf_neg)
tau.hat <- predict(cf_neg)$predictions

het_test_pos <- test_calibration(cf_pos_dich)
tau.hat_pos <- predict(cf_pos_dich)$predictions

hist(tau.hat, density = 25, breaks = 40, col = "#B77659", xlab = "Estimated treatment effects", main = "SPEI3 negative")
hist(tau.hat_pos, density = 20, breaks = 40,  col = "#B77659", xlab = "Estimated treatment effects", main = "SPEI3 positive")



# Estimate the average treatment effect
ate.hat <- average_partial_effect(cf_neg)
print(paste("95% CI for ATE:", round(ate.hat["estimate"], 3), "+/-", round(1.96 * ate.hat["std.err"], 3)))

# Estimate the average treatment effect
ate.hat_pos <- average_partial_effect(cf_pos_dich)
print(paste("95% CI for ATE:", round(ate.hat_pos["estimate"], 3), "+/-", round(1.96 * ate.hat_pos["std.err"], 3)))


# Add all info to one graph
stargazer::stargazer(het_test, het_test_pos, type = "text",
                     column.labels = c("SPEI3 neg", "SPEI3 pos"),
                     dep.var.caption = "",
                     covariate.labels = c("Mean forest prediction", "Differential forest prediction"),
                     #add.lines = list(c("Estimated treatment effect", "0.123***", "-0.080***"),
                      #                c("", "(0.008)", "(0.008)")),
                     out = "./Figurer/Differential forest prediction_speidich.html")



