### Validation of results ###

#' In this script I take a closer look at the heterogeneity within conditional variables
#' 

library(tidyverse)

load(file = "./R-script, analysis/Models/cates_neg_speidich_with_target.rds")
load(file = "./R-script, analysis/Models/cates_pos_speidich_with_target.rds")

# Confidence intervals of effect difference -------------------------------
# Calculate confidence intervals of effect heterogeneity for each variable (with some heterogeneity)

# PS: for å spare tid heter "upper" og "lower" det samme for alle utregningene. Disse må altså kjøres på nytt hvis man skal regne ut CI på nytt


# CF-neg ------------------------------------------------------------------

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged_speidich.rds")

# GDP
cates_neg %>% filter(variable == "gdp")

upper <- cates_neg %>% filter(variable == "gdp", quantile >= 10)
lower <- cates_neg %>% filter(variable == "gdp", quantile <= 6.92)

gdp_diff <-  upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)

# agri_ih
cates_neg %>% filter(variable == "agri_ih")

upper <- cates_neg %>% filter(variable == "agri_ih", quantile >= 35)
lower <- cates_neg %>% filter(variable == "agri_ih", quantile <= 0)

agri_diff <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)

# empl_agr
cates_neg %>% filter(variable == "empl_agr") # Check levels

upper <- cates_neg %>% filter(variable == "empl_agr", quantile >= 52.2) # Oppstår fort feil, best å taste inn manuelt
lower <- cates_neg %>% filter(variable == "empl_agr", quantile <= 2)

empl_diff <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)

# SHDI
cates_neg %>% filter(variable == "shdi") # Check levels

upper <- cates_neg %>% filter(variable == "shdi", quantile >= 0.8) # Oppstår fort feil, best å taste inn manuelt
lower <- cates_neg %>% filter(variable == "shdi", quantile <= 0.4)

shdi_diff <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)

# Population
cates_neg %>% filter(variable == "pop") # Check levels

upper <- cates_neg %>% filter(variable == "pop", quantile >= 100) # Oppstår fort feil, best å taste inn manuelt
lower <- cates_neg %>% filter(variable == "pop", quantile <= 1)

pop_diff <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)


# Excluded
cates_neg %>% filter(variable == "excluded") # Check levels

upper <- cates_neg %>% filter(variable == "excluded", quantile >= 1)

lower <- cates_neg %>% filter(variable == "excluded", quantile == 0)
lower <- lower[-c(2,3),] # Three of the quantiles have the value 0. Remove two of the rows so that easier to calculate CIs

excl_diff <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)

# CF-pos ------------------------------------------------------------------

# empl_agr
cates_pos %>% filter(variable == "empl_agr")

upper <- cates_pos %>% filter(variable == "empl_agr", quantile >= 52)
lower <- cates_pos %>% filter(variable == "empl_agr", quantile <= 2)

empl_diff_pos <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)

# irrig_sum
cates_pos %>% filter(variable == "irrig_sum") # Check levels

upper <- cates_pos %>% filter(variable == "irrig_sum", quantile >= 12570) # Oppstår fort feil, best å taste inn manuelt
lower <- cates_pos %>% filter(variable == "irrig_sum", quantile <= 0)

irrig_diff_pos <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)


# global_ind
cates_pos %>% filter(variable == "global_ind") # Check levels

upper <- cates_pos %>% filter(variable == "global_ind", quantile >= 71) # Oppstår fort feil, best å taste inn manuelt
lower <- cates_pos %>% filter(variable == "global_ind", quantile <= 32)

global_diff_pos <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)


# Population
cates_pos %>% filter(variable == "pop") # Check levels

upper <- cates_pos %>% filter(variable == "pop", quantile >= 100) # Oppstår fort feil, best å taste inn manuelt
lower <- cates_pos %>% filter(variable == "pop", quantile <= 1)

pop_diff_pos <- upper[["ape"]] - lower[["ape"]] +
  c(-1, 1) * qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2)


# Make table with the CIs -------------------------------------------------
# Combine all the CIs to one data frame

# CF-neg
tb <- rbind(agri_diff, empl_diff, excl_diff, gdp_diff, pop_diff, shdi_diff) %>% as.data.frame

colnames(tb) <- c("Lower CI", "Upper CI")
rownames(tb) <- c("Agricultural area in cell", "Employment in agriculture", "Excluded", "GDP", "Population density", "SHDI")

stargazer::stargazer(tb,
                     summary = F)

# CF-pos
tb_pos <- rbind(empl_diff_pos, global_diff_pos, pop_diff_pos, irrig_diff_pos) %>% as.data.frame

colnames(tb_pos) <- c("Lower CI", "Upper CI")
rownames(tb_pos) <- c("Employment in agriculture", "Globalization", "Population density", "Irrigation")

stargazer::stargazer(tb_pos, #type = "text",
                     summary = F)


