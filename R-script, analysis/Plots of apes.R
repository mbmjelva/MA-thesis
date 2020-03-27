## Plot of apes ##

library(tidyverse)

# Spei neg
load(file = "./R-script, analysis/Models/apes_neg_25interval.rds")

shdi <- apes_neg %>% filter(variable == "shdi") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "SHDI", y = "") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#008279", "#3C1518")) +
  geom_hline(yintercept = 0, linetype = 3)

unempl_tot <- apes_neg %>% filter(variable == "unempl_tot") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Unemployment", y = "") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#008279", "#3C1518")) +
  geom_hline(yintercept = 0, linetype = 3)

libdem <- apes_neg %>% filter(variable == c("libdem")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Liberal democracy", y = "") +
  theme_light() +
  coord_flip()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
  ) +
  scale_color_manual(values = c("#2D2A32", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

temp <- apes_neg %>% filter(variable == c("temp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Temperature", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

pop <- apes_neg %>% filter(variable == c("pop")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Population", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

empl_agr <- apes_neg %>% filter(variable == c("empl_agr")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Employment agriculture", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

capdist <- apes_neg %>% filter(variable == c("capdist")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Distance to capital", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

bdist <- apes_neg %>% filter(variable == c("bdist3")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Distance to border", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

global_ind <- apes_neg %>% filter(variable == c("global_ind")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Globalization", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

gdp <- apes_neg %>% filter(variable == c("gdp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "GDP", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

irrig_sum <- apes_neg %>% filter(variable == c("irrig_sum")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Irrigation", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

agri_ih <- apes_neg %>% filter(variable == c("agri_ih")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Agriculture in cell", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

excluded <- apes_neg %>% filter(variable == c("excluded")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "excluded", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    #axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

ttime_mean <- apes_neg %>% filter(variable == c("ttime_mean")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.05, 0.35)) +
  labs(x = "Travel time urban center", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank()
  ) +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

ggpubr::ggarrange(temp, libdem, capdist, bdist, global_ind,
                  pop, unempl_tot, ttime_mean, gdp, shdi, empl_agr, irrig_sum, agri_ih,
                  excluded, ncol = 1, nrow = 14, align = "v",
                  common.legend = TRUE, legend = "bottom") # Må ha temperatur først fordi den har både sig og ikke sig resultater, slik at legend blir riktig.

ggsave("./Figurer/apesneg all variables.png", height = 20, width = 7)

