## Plots of CATEs, speidich, overlap ##

# This script creates the plots of CATEs for subgroups, presented in the Analysis and Appendix C
# With binary treatment variable

library(tidyverse)

load(file = "./R-script, analysis/Models/cates_neg_speidich_overlap.rds")
load(file = "./R-script, analysis/Models/cates_pos_speidich_overlap.rds")

# Spei neg
cates_neg %>% filter(variable %in% c("empl_agr", "shdi", "gdp")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = cate, xmin = catemin, xmax = catemax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.1)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATE, CF-neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates, speidich, overlap.png") #, height = 15, width = 11)

# Spei pos

cates_pos %>% filter(variable %in% c("ttime_mean", "capdist", "pop", "empl_agr", "excluded", "libdem", "global_ind", "gdp")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = cate, xmin = catemin, xmax = catemax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.1)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATE, CF-pos") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates pos, speidich, overlap.png", height = 15, width = 11)



# Plots of CATE across all variables  -------------------------------------

# Neg
cates_neg %>% filter(variable %in% c("ttime_mean", "non_state_conflict", "temp", "agri_ih", "irrig_sum",
                                     "capdist", "pop")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = cate, xmin = catemin, xmax = catemax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.1)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATE, CF-neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates neg, speidich, overlap, all vars 1.png", height = 15, width = 11)

cates_neg %>% filter(variable %in% c("empl_agr", "excluded", "shdi", "libdem", "global_ind",
                                     "gdp", "lag_conflict")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = cate, xmin = catemin, xmax = catemax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.1)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATE, CF-neg") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates neg, speidich, overlap, all vars 2.png", height = 15, width = 11)

# Spei pos
cates_pos %>% filter(variable %in% c("ttime_mean", "non_state_conflict", "temp", "agri_ih", "irrig_sum",
                                      "capdist", "pop")) %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = cate, xmin = catemin, xmax = catemax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  #scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.1)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATE, CF-pos") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates pos, speidich, overlap, all vars 1.png", height = 15, width = 11)

cates_pos %>% filter(variable %in% c("empl_agr", "excluded", "shdi", "libdem", "global_ind",
                                     "gdp", "lag_conflict"))  %>% 
  ggplot(aes(y = as.factor(round(quantile, digits = 1)), x = cate, xmin = catemin, xmax = catemax, color = significant)) + 
  facet_grid(vars(variable), scales = "free") +
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_x_continuous(limits = c(-0.6, 0.2), breaks = seq(-0.6, 0.2, 0.3)) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    text = element_text(size = 20)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  labs(y = "", x = "CATE, CF-pos") +
  geom_vline(xintercept = 0, linetype = 3)

ggsave("./Figurer/cates pos, speidich, overlap, all vars 2.png", height = 15, width = 11)


