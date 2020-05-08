## Plot of apes ##

library(tidyverse)

# Spei neg
load(file = "./R-script, analysis/Models/apes_neg_25interval.rds")

# Alle variablene i analysen (også de som er lagged)
apes_neg %>% 
  filter(variable == c("temp", "libdem", "capdist", "bdist3", "global_ind",
                                 "pop", "unempl_tot", "ttime_mean", "gdp","shdi", "empl_agr", "irrig_sum", "agri_ih",
                                 "excluded")) %>%
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  geom_point() + 
  geom_errorbar() + 
  facet_wrap(variable~., scales = "free") + 
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom"
  ) +
  coord_flip() +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

ggsave("./Figurer/apes with lagged variables.png")

# Under: individuelle plots, men også sammensatt, da uten mange av variablene
(shdi <- apes_neg %>% filter(variable == "shdi") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "SHDI", y = "") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(unempl_tot <- apes_neg %>% filter(variable == "unempl_tot") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Unemployment", y = "") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(libdem <- apes_neg %>% filter(variable == c("libdem")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Liberal\ndemocracy", y = "") +
  theme_light() +
  coord_flip()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    #axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(temp <- apes_neg %>% filter(variable == c("temp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Temperature", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(pop <- apes_neg %>% filter(variable == c("pop")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Population", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(empl_agr <- apes_neg %>% filter(variable == c("empl_agr")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Employment\nagriculture", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(capdist <- apes_neg %>% filter(variable == c("capdist")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Distance\nto capital", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(bdist <- apes_neg %>% filter(variable == c("bdist3")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Distance\nto border", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(global_ind <- apes_neg %>% filter(variable == c("global_ind")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Globalization", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    #axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(gdp <- apes_neg %>% filter(variable == c("gdp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "GDP", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(irrig_sum <- apes_neg %>% filter(variable == c("irrig_sum")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Irrigation", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(agri_ih <- apes_neg %>% filter(variable == c("agri_ih")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Agriculture\nin cell", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(excluded <- apes_neg %>% filter(variable == c("excluded")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Excluded", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    #axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(ttime_mean <- apes_neg %>% filter(variable == c("ttime_mean")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-0.15, 0.6)) +
  labs(x = "Travel time\nurban center", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

# Alle variablene
ggpubr::ggarrange(libdem, temp, capdist, bdist, global_ind,
                  pop, unempl_tot, ttime_mean, gdp, shdi, empl_agr, irrig_sum, agri_ih,
                  excluded, ncol = 1, nrow = 14, align = "v",
                  common.legend = TRUE, legend = "bottom") # Må ha temperatur først fordi den har både sig og ikke sig resultater, slik at legend blir riktig.

ggsave("./Figurer/apesneg all variables.png", height = 21, width = 9)

# Den øvre halvdelen av variablene (de syv most important)
ggpubr::ggarrange(temp, libdem, capdist, bdist, global_ind,
                  pop, unempl_tot, ncol = 1, nrow = 7, align = "v",
                  common.legend = TRUE, legend = "bottom")
ggsave("./Figurer/apesneg seven most important vars.png", height = 20, width = 8)


# Nedre halvdel av variablene (de syv minst viktige)
ggpubr::ggarrange(ttime_mean, gdp, shdi, empl_agr, irrig_sum, agri_ih,
                  excluded, ncol = 1, nrow = 7, align = "v",
                  common.legend = TRUE, legend = "bottom")
ggsave("./Figurer/apesneg seven least important vars.png", height = 20, width = 8)



# SPEI pos ----------------------------------------------------------------


load(file = "./R-script, analysis/Models/apes_pos_25interval.rds")

# Alle variablene i analysen (også de som er lagged)
apes_pos %>% 
  filter(variable == c("temp", "libdem", "capdist", "bdist3", "global_ind",
                       "pop", "unempl_tot", "ttime_mean", "gdp","shdi", "empl_agr", "irrig_sum", "agri_ih",
                       "excluded")) %>%
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  geom_point() + 
  geom_errorbar() + 
  facet_wrap(variable~., scales = "free") + 
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom"
  ) +
  coord_flip() +
  scale_color_manual(values = c("#3C1518", "#008279")) +
  geom_hline(yintercept = 0, linetype = 3)

#ggsave("./Figurer/apes with lagged variables.png")

# Under: individuelle plots, men også sammensatt, da uten mange av variablene
(shdi <- apes_pos %>% filter(variable == "shdi") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "SHDI", y = "") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(unempl_tot <- apes_pos %>% filter(variable == "unempl_tot") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_light() +
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Unemployment", y = "") +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(libdem <- apes_pos %>% filter(variable == c("libdem")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Liberal\ndemocracy", y = "") +
  theme_light() +
  coord_flip()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "none",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(temp <- apes_pos %>% filter(variable == c("temp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Temperature", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(pop <- apes_pos %>% filter(variable == c("pop")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Population", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(empl_agr <- apes_pos %>% filter(variable == c("empl_agr")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Employment\nagriculture", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(capdist <- apes_pos %>% filter(variable == c("capdist")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Distance\nto capital", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(bdist <- apes_pos %>% filter(variable == c("bdist3")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-1.2, 0.3)) +
    labs(x = "Distance\nto border", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      axis.text.x = element_blank(),
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(global_ind <- apes_pos %>% filter(variable == c("global_ind")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Globalization", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(gdp <- apes_pos %>% filter(variable == c("gdp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "GDP", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(irrig_sum <- apes_pos %>% filter(variable == c("irrig_sum")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Irrigation", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(agri_ih <- apes_pos %>% filter(variable == c("agri_ih")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Agriculture\nin cell", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(excluded <- apes_pos %>% filter(variable == c("excluded")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Excluded", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    #axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

(ttime_mean <- apes_pos %>% filter(variable == c("ttime_mean")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_y_continuous(limits = c(-1.2, 0.3)) +
  labs(x = "Travel time\nurban center", y = "") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text.y = element_text(colour = "black"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    text = element_text(size = 15)
  ) +
  scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
  geom_hline(yintercept = 0, linetype = 3))

# Alle variablene
ggpubr::ggarrange(unempl_tot, libdem, temp, capdist, bdist, global_ind,
                  pop, ttime_mean, gdp, shdi, empl_agr, irrig_sum, agri_ih,
                  excluded, ncol = 1, nrow = 14, align = "v",
                  common.legend = TRUE, legend = "bottom") # Må ha temperatur først fordi den har både sig og ikke sig resultater, slik at legend blir riktig.

ggsave("./Figurer/apespos all variables.png", height = 21, width = 9)


