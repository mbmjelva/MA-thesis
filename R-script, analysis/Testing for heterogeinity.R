### Testing for heterogeneity in the models ###

# This script makes the omnibus test of heterogeneity and the plots of ITEs showed in that section of the Analysis

library(grf)
library(stargazer)


# SPEI dichotomous, overlap------------------------------------------

load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")

# Estimate the treatment heterogeneity
area_of_overlap <- cf_neg_speidich$W.hat > 0.1 & cf_neg_speidich$W.hat < 0.9
area_of_overlap_pos <- cf_pos_speidich$W.hat > 0.1 & cf_pos_speidich$W.hat < 0.9

het_test <- test_calibration(cf_neg_speidich)
tau.hat <- predict(cf_neg_speidich)$predictions

het_test_pos <- test_calibration(cf_pos_speidich)
tau.hat_pos <- predict(cf_pos_speidich)$predictions

tau.hat_overlap <- tau.hat[which(area_of_overlap,)]
tau.hat_pos_overlap <- tau.hat_pos[which(area_of_overlap_pos,)]

hist(tau.hat_overlap, density = 20, breaks = 50, col = "#B77659", xlab = "Estimated treatment effects", main = "SPEI3 negative")
hist(tau.hat_pos_overlap, density = 20, breaks = 50,  col = "#B77659", xlab = "Estimated treatment effects", main = "SPEI3 positive")



# Estimate the average treatment effect
ate.hat <- average_partial_effect(cf_neg_speidich)
print(paste("95% CI for ATE:", round(ate.hat["estimate"], 3), "+/-", round(1.96 * ate.hat["std.err"], 3)))

# Estimate the average treatment effect
ate.hat_pos <- average_partial_effect(cf_pos_speidich)
print(paste("95% CI for ATE:", round(ate.hat_pos["estimate"], 3), "+/-", round(1.96 * ate.hat_pos["std.err"], 3)))


# Add all info to one graph
stargazer::stargazer(het_test, het_test_pos,
                     column.labels = c("SPEI3 neg", "SPEI3 pos"),
                     dep.var.caption = "",
                     covariate.labels = c("Mean forest prediction", "Differential forest prediction"),
                     add.lines = list(c("Average treatment effect", "-0.977***", "-0.732***"),
                                      c("", "(0.007)", "(0.005)")),
                     out = "./Figurer/Differential forest prediction_speidich.html")



