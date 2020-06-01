### Significance of heterogeneity ###

#' In this script I take a closer look at the heterogeneity within conditional variables
#' Calculate the difference in effects and the sd of that difference based on calculations found in this paper: https://arxiv.org/pdf/1902.07409.pdf

library(tidyverse)
library(grf)

load(file = "./R-script, analysis/Models/cates_neg_speidich_with_target.rds")
load(file = "./R-script, analysis/Models/cates_pos_speidich_with_target.rds")

# Load CATTs
load(file = "./R-script, analysis/Models/catt_neg_speidich_only_conflict.rds")
load(file = "./R-script, analysis/Models/catt_pos_speidich_only_conflict.rds")

# Make function to extract difference estimate and sd ---------------------

diff_effect <- function(catt_type, var) {
  upper <- catt_type %>% dplyr::filter(variable == var, quantile == max(quantile[catt_type$variable == var])) 
  lower <- catt_type %>% dplyr::filter(variable == var, quantile == min(quantile[catt_type$variable == var]))
  diff <-  round(upper[["catt"]] - lower[["catt"]],3)
  se <- round(qnorm(0.975) * sqrt(upper[["catt_std"]]^2 + lower[["catt_std"]]^2),3)
  t <- round(diff/se, 3)
  output <- list(diff = diff, se = se, t = t)
  return(output)
}


# CATTs -------------------------------------------------------------------

### CF-neg ### 
agri_neg <- diff_effect(catt_neg, "agri_ih")
bdist_neg <- diff_effect(catt_neg, "bdist3")
capdist_neg <- diff_effect(catt_neg, "capdist")
empl_neg <- diff_effect(catt_neg, "empl_agr")
excl_neg <- diff_effect(catt_neg, "excluded")
gdp_neg <- diff_effect(catt_neg, "gdp")
irrig_neg <- diff_effect(catt_neg, "irrig_sum")
lag_conf_neg <- diff_effect(catt_neg, "lag_conflict")
libdem_neg <- diff_effect(catt_neg, "libdem")
non_state_neg <- diff_effect(catt_neg, "non_state_conflict")
pop_neg <-  diff_effect(catt_neg, "pop")
shdi_neg <- diff_effect(catt_neg, "shdi")
temp_neg <- diff_effect(catt_neg, "temp")
ttime_neg <- diff_effect(catt_neg, "ttime_mean")

# The variables on factor level produce three or four estimates (if the estimated effect is the same for all or for several of the levels of the variables)
# Since the estimates are all the same, remove some of the items so that only one estimate in the final table
excl_neg$diff <- excl_neg$diff[-c(2,3)]
excl_neg$se <- excl_neg$se[-c(2,3)]
excl_neg$t <- excl_neg$t[-c(2,3)]

lag_conf_neg$diff <- lag_conf_neg$diff[-c(2:4)]
lag_conf_neg$se <- lag_conf_neg$se[-c(2:4)]
lag_conf_neg$t <- lag_conf_neg$t[-c(2:4)]

non_state_neg$diff <- non_state_neg$diff[-c(2:4)]
non_state_neg$se <- non_state_neg$se[-c(2:4)]
non_state_neg$t <- non_state_neg$t[-c(2:4)]

# Combine the estimates into one dataframe
df_neg <- rbind(agri_neg, bdist_neg, capdist_neg, empl_neg, excl_neg, gdp_neg, irrig_neg, lag_conf_neg,
                libdem_neg, non_state_neg, pop_neg, shdi_neg, temp_neg, ttime_neg) %>% as.data.frame()

# Add stars depending on t-values
df_neg$stars <- c("", "*", rep("", 8), rep("*", 4))


# Label the dataframe
colnames(df_neg) <- c("Estimated difference", "Standard error", "t-value", "p < 0.05")
rownames(df_neg) <- c("Agricultural area in cell", "Distance to border", "Distance to capital",
                      "Employment in agriculture", "Excluded", "GDP", "Irrigation", "Conflict lagged",
                      "liberal democracy", "Non-state conflict", "Population density", "SHDI",
                      "Temperature", "Time to urban center")

# Create table
stargazer::stargazer(df_neg, #type = "text",
                     summary = F, digits = 3)


### CF-pos ### 
agri_pos <- diff_effect(catt_pos, "agri_ih")
bdist_pos <- diff_effect(catt_pos, "bdist3")
capdist_pos <- diff_effect(catt_pos, "capdist")
empl_pos <- diff_effect(catt_pos, "empl_agr")
excl_pos <- diff_effect(catt_pos, "excluded")
gdp_pos <- diff_effect(catt_pos, "gdp")
irrig_pos <- diff_effect(catt_pos, "irrig_sum")
lag_conf_pos <- diff_effect(catt_pos, "lag_conflict")
libdem_pos <- diff_effect(catt_pos, "libdem")
non_state_pos <- diff_effect(catt_pos, "non_state_conflict")
pop_pos <-  diff_effect(catt_pos, "pop")
shdi_pos <- diff_effect(catt_pos, "shdi")
temp_pos <- diff_effect(catt_pos, "temp")
ttime_pos <- diff_effect(catt_pos, "ttime_mean")

# The variables on factor level produce three or four estimates (if the estimated effect is the same for all or for several of the levels of the variables)
# Since the estimates are all the same, remove some of the items so that only one estimate in the final table
excl_pos$diff <- excl_pos$diff[-c(2,3)]
excl_pos$se <- excl_pos$se[-c(2,3)]
excl_pos$t <- excl_pos$t[-c(2,3)]

lag_conf_pos$diff <- lag_conf_pos$diff[-c(2:4)]
lag_conf_pos$se <- lag_conf_pos$se[-c(2:4)]
lag_conf_pos$t <- lag_conf_pos$t[-c(2:4)]

non_state_pos$diff <- non_state_pos$diff[-c(2:4)]
non_state_pos$se <- non_state_pos$se[-c(2:4)]
non_state_pos$t <- non_state_pos$t[-c(2:4)]

# Combine the estimates into one dataframe
df_pos <- rbind(agri_pos, bdist_pos, capdist_pos, empl_pos, excl_pos, gdp_pos, irrig_pos, lag_conf_pos,
                libdem_pos, non_state_pos, pop_pos, shdi_pos, temp_pos, ttime_pos) %>% as.data.frame()

# Add stars depending on t-values
df_pos$stars <- c(rep("*", 7), "", "*", "", "*", "", "*", "*")


# Label the dataframe
colnames(df_pos) <- c("Estimated difference", "Standard error", "t-value", "p < 0.05")
rownames(df_pos) <- c("Agricultural area in cell", "Distance to border", "Distance to capital",
                      "Employment in agriculture", "Excluded", "GDP", "Irrigation", "Conflict lagged",
                      "liberal democracy", "Non-state conflict", "Population density", "SHDI",
                      "Temperature", "Time to urban center")

# Create table
stargazer::stargazer(df_pos,
                     summary = F)




# CATEs -------------------------------------------------------------------

# Make function to extract difference
diff_effect_cate <- function(cates, var) {
  upper <- cates %>% dplyr::filter(variable == var, quantile == max(quantile[cates$variable == var])) 
  lower <- cates %>% dplyr::filter(variable == var, quantile == min(quantile[cates$variable == var]))
  diff <-  round(upper[["ape"]] - lower[["ape"]],3)
  sd <- round(qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2),3)
  output <- list(diff = diff, sd = sd)
  return(output)
}

# CF-neg 

gdp_neg <- diff_effect_cate(cates_neg, "gdp")
agri_neg <- diff_effect(cates_neg, "agri_ih")
empl_neg <- diff_effect(cates_neg, "empl_agr")
excl_neg <- diff_effect(cates_neg, "excluded")
pop_neg <-  diff_effect(cates_neg, "pop")
shdi_neg <- diff_effect(cates_neg, "shdi")

df_neg <- rbind(gdp_neg, agri_neg, empl_neg, excl_neg, pop_neg, shdi_neg) %>% as.data.frame()

colnames(df_neg) <- c("Estimated difference", "Standard deviation")
rownames(df_neg) <- c("GDP", "Agricultural area in cell", "Employment in agriculture", 
                  "Excluded", "Population density", "SHDI")


stargazer::stargazer(df_neg,
                     summary = F)

# CF-pos 

empl_pos <- diff_effect(cates_pos, "empl_agr")
pop_pos <-  diff_effect(cates_pos, "pop")
global_pos <- diff_effect(cates_pos, "global_ind")
irrig_pos <- diff_effect(cates_pos, "irrig_sum")

df_pos <- rbind(empl_pos, pop_pos, global_pos, irrig_pos) %>% as.data.frame()

colnames(df_pos) <- c("Estimated difference", "Standard deviation")
rownames(df_pos) <- c("Employment in agriculture", "Population density", "Globalization", "Irrigation")

stargazer::stargazer(df_pos, summary = F)

