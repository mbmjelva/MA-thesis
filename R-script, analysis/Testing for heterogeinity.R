### Testing for heterogeinity in data ###

library(grf)

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")


het_test <- test_calibration(cf_neg)
het_test_pos <- test_calibration(cf_pos)

stargazer::stargazer(het_test, het_test_pos, type = "text")