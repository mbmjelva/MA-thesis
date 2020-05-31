### Descriptive statistics - logistic regression of contextual variables ###

library(tidyverse)
library(stargazer)

sample_final <- readRDS("./Egne datasett/resampled_data_lagged.rds")

# Renomve prefix for all variables so that easier to work with (lagged conflict stays the same as starts only with lag_)
sample_final <- sample_final %>% rename_at(vars(starts_with("lag_1_")), 
                                           funs(str_replace(., "lag_1_", "")))

# Make the spei variables binary
sample_final <- sample_final %>% mutate(spei3_neg = ifelse(spei3_neg < (-1), 1, 0),
                                        spei3_pos = ifelse(spei3_pos > 1, 1, 0))


# All contextual variables plus spei3
m <- glm(conflict ~ spei3_neg + spei3_pos + ttime_mean + non_state_conflict + temp + agri_ih + irrig_sum + bdist3 + capdist + pop + empl_agr +          
        excluded + shdi + libdem + global_ind + gdp + lag_conflict, data = sample_final, family = "binomial", na.action = "na.exclude")


# Make table
stargazer(m)

