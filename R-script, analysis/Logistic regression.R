### Logistic regression ###

library(tidyverse)
library(stargazer)

sample_final <- readRDS("./Egne datasett/resampled_data_lagged.rds")

# Renomve prefix for all variables so that easier to work with (lagged conflict stays the same as starts only with lag_)
sample_final <- sample_final %>% rename_at(vars(starts_with("lag_1_")), 
                                           funs(str_replace(., "lag_1_", "")))

# Make the spei variables binary
sample_final <- sample_final %>% mutate(spei3_neg = ifelse(spei3_neg < (-1), 1, 0),
                                        spei3_pos = ifelse(spei3_pos > 1, 1, 0))

### Models: Only look at the variables with significant treatment heterogeneity (therefore only SPEI3 neg)

# All variables with treatment heterogeneity
m <- glm(conflict ~ spei3_neg + spei3_neg*agri_ih + spei3_neg*empl_agr + 
               spei3_neg*pop + spei3_neg*gdp, data = sample_final, family = "binomial", na.action = "na.exclude")

# Seperate models for each interactive term
m_agri <- glm(conflict ~ spei3_neg + spei3_neg*agri_ih, data = sample_final, family = "binomial", na.action = "na.exclude")
m_empl <- glm(conflict ~ spei3_neg + spei3_neg*empl_agr, data = sample_final, family = "binomial", na.action = "na.exclude")
m_pop <- glm(conflict ~ spei3_neg + spei3_neg*pop, data = sample_final, family = "binomial", na.action = "na.exclude")
m_gdp <- glm(conflict ~ spei3_neg + spei3_neg*gdp, data = sample_final, family = "binomial", na.action = "na.exclude")


# Make table with all models
stargazer(m, m_agri, m_empl, m_pop, m_gdp,
          column.labels = c("Full model", "Agricultural area cell", "Employment agriculture", "Population density", "GDP"))

