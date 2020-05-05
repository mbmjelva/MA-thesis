## CATEs, speidich ##

library(tidyverse)

# Spei neg
load(file = "./R-script, analysis/Models/cates_neg_speidich_with_target.rds")

cates_neg %>% filter(variable %in% c("libdem", "temp", "capdist", "bdist", "ttime_mean", "global_ind", "pop")) %>% 
  mutate(as.factor(quantile)) %>% 
  ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
  geom_point() + facet_grid(vars(variable), scales = "free") +
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.065, 0.01), breaks = seq(-0.06, 0.01, 0.01)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "Quantile", x = "CATE, SPEI3 neg") +
  geom_vline(xintercept = 0, linetype = 3)


ggsave("./Figurer/cates_neg, speidich, target, seven variables 1.png", height = 18, width = 10)

cates_neg %>% filter(variable %in% c("gdp", "shdi", "empl_agr", "irrig_sum", "agri_ih", "excluded", "lag_conflict", "non_state_conflict")) %>% 
  mutate(as.factor(quantile)) %>% 
  ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
  geom_point() + facet_grid(vars(variable), scales = "free") +
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.06, 0.01), breaks = seq(-0.06, 0.01, 0.01)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "Quantile", x = "CATE, SPEI3 neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates_neg, speidich, target, seven variables 2.png", height = 18, width = 10)


# Spei pos
load(file = "./R-script, analysis/Models/cates_pos_speidich_with_target.rds")

cates_pos %>% filter(variable %in% c("libdem", "temp", "capdist", "bdist", "ttime_mean", "global_ind", "pop")) %>% 
  mutate(as.factor(quantile)) %>% 
  ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
  geom_point() + facet_grid(vars(variable), scales = "free") +
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.065, 0.01), breaks = seq(-0.06, 0.01, 0.01)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "Quantile", x = "CATE, SPEI3 neg") +
  geom_vline(xintercept = 0, linetype = 3)


ggsave("./Figurer/cates_pos, speidich, target, seven variables 1.png",  height = 15, width = 11)

cates_pos %>% filter(variable %in% c("gdp", "shdi", "empl_agr", "irrig_sum", "agri_ih", "excluded", "lag_conflict", "non_state_conflict")) %>% 
  mutate(as.factor(quantile)) %>% 
  ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
  geom_point() + facet_grid(vars(variable), scales = "free") +
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.06, 0.01), breaks = seq(-0.06, 0.01, 0.01)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "Quantile", x = "CATE, SPEI3 neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates_pos, speidich, target, seven variables 2.png", height = 15, width = 11)


