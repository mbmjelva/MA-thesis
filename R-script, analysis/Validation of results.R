### Validation of results ###

#' In this script I take a closer look at the heterogeneity within conditional variables
#' Calculate the difference in effects and the sd of that difference based on calculations found in this paper: https://arxiv.org/pdf/1902.07409.pdf

library(tidyverse)

load(file = "./R-script, analysis/Models/cates_neg_speidich_with_target.rds")
load(file = "./R-script, analysis/Models/cates_pos_speidich_with_target.rds")


# Make function to extract difference estimate and sd ---------------------

diff_effect <- function(cates, var) {
  upper <- cates %>% dplyr::filter(variable == var, quantile == max(quantile[cates$variable == var])) 
  lower <- cates %>% dplyr::filter(variable == var, quantile == min(quantile[cates$variable == var]))
  diff <-  round(upper[["ape"]] - lower[["ape"]],3)
  sd <- round(qnorm(0.975) * sqrt(upper[["ape_std"]]^2 + lower[["ape_std"]]^2),3)
  output <- list(diff = diff, sd = sd)
  return(output)
}


# CF-neg ------------------------------------------------------------------

# Funksjonen er ikke perfekt, burde være mulig å gjøre dette enda raskere.
gdp_neg <- diff_effect(cates_neg, "gdp")
agri_neg <- diff_effect(cates_neg, "agri_ih")
empl_neg <- diff_effect(cates_neg, "empl_agr")
excl_neg <- diff_effect(cates_neg, "excluded")
pop_neg <-  diff_effect(cates_neg, "pop")
shdi_neg <- diff_effect(cates_neg, "shdi")

df_neg <- rbind(gdp_neg, agri_neg, empl_neg, excl_neg, pop_neg, shdi_neg) %>% as.data.frame()

colnames(df_neg) <- c("Estimated difference", "Standard deviation")
rownames(df_neg) <- c("GDP", "Agricultural area in cell", "Employment in agriculture", 
                  "Excluded", "Population density", "SHDI")


stargazer::stargazer(df_neg, type = "text", summary = F)

# CF-pos ------------------------------------------------------------------

empl_pos <- diff_effect(cates_pos, "empl_agr")
pop_pos <-  diff_effect(cates_pos, "pop")
global_pos <- diff_effect(cates_pos, "global_ind")
irrig_pos <- diff_effect(cates_pos, "irrig_sum")

df_pos <- rbind(empl_pos, pop_pos, global_pos, irrig_pos) %>% as.data.frame()

rownames(df_pos) <- c("Employment in agriculture", "Population density", "Globalization", "Irrigation")

stargazer::stargazer(df_pos, type = "text", summary = F)

