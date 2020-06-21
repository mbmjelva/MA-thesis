### Missing analysis ###

# Follow this guideline: https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html

library(finalfit)
library(tidyverse)
library(stargazer)
library(naniar)
library(corrplot)

final <- read_rds("./Egne datasett/final_dataset.rds")

final <- final %>% filter(!is.na(spei3))

# Check that all variables are coded correctly (acting as expected) ----------------------------
ff_glimpse(final)

#' First have a look at all the variables (both lagged and not lagged). The coding seems okay. 
#' No difference in percent of missing between lagged and not lagged variables.

#' Continue the analysis only with variables that are not lagged. Unemployment will not be part of the analysis (too much missingness)
final <- select(final, -starts_with("lag"), -unempl_tot, -events, -best, -bdist3, -continent)

missing_info <- final %>% 
  miss_var_summary() %>% # Can keep the original order of rows if order = F. Can alternatively use ff_glimpse, gives more info.
  as.data.frame() %>% 
  mutate("Not missing" = ((nrow(final)) - (n_miss))) %>% # Add number of observations without NA for each variable.
  select("Variable" = variable, "Not missing", "Number missing" = n_miss, "Percent missing" = pct_miss) # Reorder order of the columns


# Make tabl of missing data in the appendix 
stargazer(missing_info,
          summary = F, 
          digits = 1)


# Identify missing values in each variable --------------------------------
# Look for patterns
missing_plot(final) 

#' There does not seem to be much systemacy in the missing data


# Investigate if system in missing based on time --------------------------

final %>% group_by(year) %>% miss_var_summary() %>% arrange(desc(pct_miss)) %>% filter(pct_miss > 5, variable == unempl_tot) %>% print(n = Inf)

#' Does not seem to be any systematic difference in missing values based on years after filling in the years for variables that are completely missing
#' For geographically missingness, see rscript with "map of missing data"
#' Only four variables (in addition to the cru variables) have big amounts of missing. For unemployment, almost all is in Africa.


# Only look at the vars with a lot of missing
final_select <- final %>% select(excluded, shdi, agri_ih) 
missing_pattern(final_select) 


# Identify missing values in each variable --------------------------------

# Gives the mean (and sd) for each value on the dependent variable
confl_dist <- final %>% 
  summary_factorlist(dependent = "conflict", 
                     explanatory = c("spei3_pos", "spei3_neg", "temp", "agri_ih", "irrig_sum", "bdist3", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, p=TRUE)

stargazer(confl_dist, type = "text", summary = F)



## Mirca missing values
prio_ucdp <- read_rds("./Egne datasett/prio_ucdp_merged.rds")
mirca <- readRDS("./Egne datasett/mirca_growseas_tibble.rds")
mirca$month <- sapply(mirca$names, function(x) grep(paste("(?i)",x,sep=""),month.abb))

pp <- left_join(prio_ucdp, mirca, by = c("xcoord" = "x", "ycoord" = "y"))

ff_glimpse(pp)
ff_glimpse(final)

# Proof that MIRCA is the reason for the missing data in the SPEI variables


# Chi square test ---------------------------------------------------------

# Make the variables into dummies for correlation test


final_select_NA <- final %>% select(excluded, shdi, agri_ih) %>% as.data.frame()

i <- 1
while(i<=ncol(final_select_NA)) {
  final_select_NA[[i]] = ifelse(is.na(final_select_NA[[i]]) == TRUE, 1, 0)
  i <- i + 1
}


cor(final_select_NA)
corrplot(cor(final_select_NA))
# Not too bad.



