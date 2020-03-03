
#### Descriptive statistics ######

library(tidyverse)
library(stargazer)
library(ggpubr)

final <- read_rds("./Egne datasett/final_dataset.rds")

# Make table of descriptive statistics
final_df <- data.frame(final) # For å kunne bruke stargazer, kan ikke være tibble.

stargazer(final_df,
          title = "Descriptive statistics", 
          covariate.labels = c("Gid", "Year", "gwno", "lon", "lat",
                               "Events", "Best estimate number of deaths",
                               "SPEI3", "SPEI3 negative", "SPEI3 positive", "Temperature", "Agricultural area in cell (percentage)",
                               "Distance to nearest border (km)", "Distance to capital (km)",
                               "Travel time to nearest\nurban center (min)",
                               "Population", "Employment in agriculture",
                               "Total unemployment", "Number of excluded groups",
                               "SHDI", "Liberal Democracy Index", "Globalization index",
                               "GDP"),
          out = "./R-script, descriptive analysis/Figurer/Table_variables")


# Conflict
ggplot(final) + 
  geom_bar(aes(conflict), fill = "#808000") + 
  theme_minimal() +
  labs(x = "Conflict", y = "")

ggsave("./Figurer/conflict_distribution.png")

(a <- ggplot(final) + 
  geom_bar(aes(events), fill = "#BDB76B") + 
  theme_minimal() +
  scale_x_continuous(limits = c(0,361)) +
  scale_y_continuous(limits = c(0,2000)) +
  labs(x = "Number of events within a year", y = ""))

ggsave("./Figurer/numberofevents.png")

(b <- ggplot(final) + 
  geom_bar(aes(best), fill = "#BDB76B") + 
  theme_minimal() +
  scale_x_continuous(limits = c(25,2000), breaks = c(25, 200, 1000, 2000)) +
  labs(x = "Death toll within a year", y = ""))

ggsave("./Figurer/deathtoll.png")

ggarrange(a,b)

ggsave("./Figurer/deathtoll and numberevents.png")

prop.table(table(final$conflict))
?ggarrange

# SPEI3 - variation
(spei <- ggplot(final) + 
  geom_histogram(aes(spei3), fill = "#808000", col = "black", binwidth = 0.1) +
  theme_minimal() +
    theme(text = element_text(size = 20)) +
  labs(x = "SPEI3", y =""))
spei

ggsave("./figurer/spei_distibution.png")

(temp <- ggplot(final) + 
  geom_histogram(aes(temp), fill = "#808000", col = "black", binwidth = 0.5) +
  theme_minimal() +
    theme(text = element_text(size = 20)) +
  labs(x = "Temperature", y = ""))

ggsave("./Figurer/temp_distribution.png")

ggarrange(spei, temp, align = "v")

ggsave("./Figurer/speiogtemp.png")



