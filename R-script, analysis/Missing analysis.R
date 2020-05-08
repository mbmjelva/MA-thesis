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

#' Continue the analysis only with variables that are not lagged.
final <- select(final, -starts_with("lag"))

missing_info <- final %>% 
  miss_var_summary() %>% # Can keep the original order of rows if order = F. Can alternatively use ff_glimpse, gives more info.
  as.data.frame() %>% 
  mutate("Not missing" = ((nrow(final)) - (n_miss))) %>% # Add number of observations without NA for each variable.
  select("Variable" = variable, "Not missing", "Number missing" = n_miss, "Percent missing" = pct_miss) # Reorder order of the columns


# Make table 1 (?) in the appendix
stargazer(missing_info,
          summary = F, 
          digits = 1)

gg_miss_var(final, show_pct = T) + ylim(0,100) + labs(y = "") # This graph shows the same thing as the table

# About 40% missing in the CRU-variables because of aggregation method (only places with arable land included)

# Identify missing values in each variable --------------------------------
# Look for patterns
missing_plot(final) + scale_x_continuous(breaks = seq(0, 1000000, 1200000)) # Observation ID on the x-axis, variable on the y-axis 

# Egentlig var det seq(0, 1800000, 2000000)

#' There does not seem to be much systemacy in the missing data


# Investigate if system in missing based on time --------------------------

final %>% group_by(year) %>% miss_var_summary() %>% arrange(desc(pct_miss)) %>% filter(pct_miss > 5, variable == unempl_tot) %>% print(n = Inf)

#' Does not seem to be any systematic difference in missing values based on years after filling in the years for variables that are completely missing
#' For geographically missingness, see rscript with "map of missing data"
#' Only four variables (in addition to the cru variables) have big amounts of missing. For unemployment, almost all is in Africa.


# Only look at the vars with a lot of missing
final_select <- final %>% select(unempl_tot, excluded, shdi, agri_ih) 
missing_pattern(final_select) 


# Identify missing values in each variable --------------------------------

# Gives the mean (and sd) for each value on the dependent variable
confl_dist <- final %>% 
  summary_factorlist(dependent = "conflict", 
                     explanatory = c("spei3_pos", "spei3_neg", "temp", "agri_ih", "irrig_sum", "bdist3", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "unempl_tot", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, p=TRUE)

stargazer(confl_dist, type = "text", summary = F)


# There seem to be some pattern in the missing data. Observations with very high or very low gid-numbers seem to have a lot of missing on several of the variables.

missing_pairs(final, dependent = "spei3",
              explanatory = c("temp", "agri_ih", "irrig_sum", "bdist3", "capdist", "ttime_mean", "pop",
                              "empl_agr", "unempl_tot", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
              position = "fill")


## Mirca missing values
prio_ucdp <- read_rds("./Egne datasett/prio_ucdp_merged.rds")
mirca <- readRDS("./Egne datasett/mirca_growseas_tibble.rds")
mirca$month <- sapply(mirca$names, function(x) grep(paste("(?i)",x,sep=""),month.abb))

pp <- left_join(prio_ucdp, mirca, by = c("xcoord" = "x", "ycoord" = "y"))

ff_glimpse(pp)
ff_glimpse(final)

# Det er mirca som er årsak til det store antallet missing i SPEI


# Visualize the relationship between missing in variables -----------------
gg_miss_upset(final_select) # På denne bør jeg kanskje velge ut noen variabler

# As there seem to be a high number of observations that have missing values on both temp, spei and excluded, I investigate whether there is a big difference for the different excluded categories
final_select$excluded <- as.factor(final_select$excluded)
gg_miss_fct(final_select, fct = excluded)

final_select %>% group_by(excluded) %>% miss_var_summary() %>% arrange(desc(pct_miss)) %>% print(n = Inf) # Unsure of what to say about this




# Chi square test ---------------------------------------------------------

# Make the variables into dummies for correlation test

final_select_NA <- final %>% select(unempl_tot, excluded, shdi, agri_ih, spei3, temp, year) %>% as.data.frame()

i <- 1
while(i<=ncol(final_select_NA)) {
  final_select_NA[[i]] = ifelse(is.na(final_select_NA[[i]]) == TRUE, 1, 0)
  i <- i + 1
}


cor(final_select_NA)
corrplot(cor(final_select_NA))
# Not too bad.


# Graph of missing data in unempl_tot over years
final_select <- final %>% select(unempl_tot, excluded, shdi, agri_ih, year) 

gg_miss_var(final_select, facet = year, show_pct = T) + ylim(0,100) + labs(y = "") # %>% facet_grid(~year)

final_select$unempl_tot_NA <- ifelse(is.na(final_select$unempl_tot), 1, 0)
final_select %>% group_by(year) %>% summarise(percent = length(unempl_tot_NA == 1)/length(unempl_tot_NA))

final_select_NA %>% mutate(pct = (pct(unempl_tot == 1)))
ggplot(final_select_NA) + geom_abline(aes(x = year, y = pct(unempl_tot == 1)))

prop.table(table(final_select_NA$unempl_tot))


unempl_miss <- final %>% group_by(year) %>% miss_var_summary() %>% arrange(desc(pct_miss)) %>% filter(variable == "unempl_tot") %>% as.data.frame()

final %>% 
  group_by(year) %>% 
  miss_var_summary() %>% 
  arrange(desc(pct_miss)) %>% 
  filter(variable == "unempl_tot") %>%
  ggplot() + 
  geom_line(aes(year, pct_miss), stat = "identity") +
  theme_bw() +
  labs(x = "Year", y = "Percentage missing", title = "Percentage of missing information for unemployment") +
  scale_x_continuous(breaks = seq(1990, 2015, 5))
  
  


