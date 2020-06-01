## CATT, speidich, only conflict ##

library(tidyverse)

# Spei neg
load(file = "./R-script, analysis/Models/catt_neg_speidich_only_conflict.rds")

catt_neg %>% filter(variable %in% c("libdem", "temp", "capdist", "bdist3", "ttime_mean", "global_ind", "pop")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = catt, xmin = cattmin, xmax = cattmax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.1, 0.2), breaks = seq(-0.1, 0.2, 0.05)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATT, CF-neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/catt_neg, speidich, only conflict, seven variables 1.png", height = 15, width = 11)

catt_neg %>% filter(variable %in% c("gdp", "shdi", "empl_agr", "irrig_sum", "agri_ih", "excluded", "lag_conflict", "non_state_conflict")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = catt, xmin = cattmin, xmax = cattmax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.1, 0.2), breaks = seq(-0.1, 0.2, 0.05)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATT, CF-neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/catt_neg, speidich, only conflict, seven variables 2.png", height = 15, width = 11)


# Spei pos
load(file = "./R-script, analysis/Models/cates_pos_speidich_only_conflict.rds")

catt_pos %>% filter(variable %in% c("libdem", "temp", "capdist", "bdist3", "ttime_mean", "global_ind", "pop")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = catt, xmin = cattmin, xmax = cattmax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.1, 0.2), breaks = seq(-0.1, 0.2, 0.05)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATT, CF-pos") +
  geom_vline(xintercept = 0, linetype = 3)


ggsave("./Figurer/catt_pos, speidich, only conflict, seven variables 1.png",  height = 15, width = 11)

#labels <- c("Agricultural area\in cell", "Employment\agriculture", "Excluded", "GDP", "Irrigation", "Conflict\lagged", "Non-state\conflict", "SHDI")

catt_pos %>% filter(variable %in% c("gdp", "shdi", "empl_agr", "irrig_sum", "agri_ih", "excluded", "lag_conflict", "non_state_conflict")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = catt, xmin = cattmin, xmax = cattmax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.1, 0.2), breaks = seq(-0.1, 0.2, 0.05)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATT, SPEI3 pos") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/catt_pos, speidich, only conflict, seven variables 2.png", height = 15, width = 11)


