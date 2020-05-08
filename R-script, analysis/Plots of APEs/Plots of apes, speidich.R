## Plot of apes, speidich ##

library(tidyverse)

# Spei neg
load(file = "./R-script, analysis/Models/apes_neg_speidich.rds")

# Under: individuelle plots, men også sammensatt, da uten mange av variablene
(shdi <- apes_neg %>% filter(variable == "shdi") %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "SHDI", y = "") +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(unempl_tot <- apes_neg %>% filter(variable == "unempl_tot") %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Unemployment", y = "") +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(libdem <- apes_neg %>% filter(variable == c("libdem")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Liberal\ndemocracy", y = "") +
    theme_light() +
    coord_flip()+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(temp <- apes_neg %>% filter(variable == c("temp")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Temperature", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(pop <- apes_neg %>% filter(variable == c("pop")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Population", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(empl_agr <- apes_neg %>% filter(variable == c("empl_agr")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Employment\nagriculture", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(capdist <- apes_neg %>% filter(variable == c("capdist")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Distance\nto capital", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(bdist <- apes_neg %>% filter(variable == c("bdist3")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Distance\nto border", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(global_ind <- apes_neg %>% filter(variable == c("global_ind")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Globalization", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(gdp <- apes_neg %>% filter(variable == c("gdp")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "GDP", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(irrig_sum <- apes_neg %>% filter(variable == c("irrig_sum")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Irrigation", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(agri_ih <- apes_neg %>% filter(variable == c("agri_ih")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Agriculture\nin cell", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(excluded <- apes_neg %>% filter(variable == c("excluded")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Excluded", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(ttime_mean <- apes_neg %>% filter(variable == c("ttime_mean")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Travel time\nurban center", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(lag_conflict <- apes_neg %>% filter(variable == c("lag_conflict")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Conflict lagged", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(non_state_conflict <- apes_neg %>% filter(variable == c("non_state_conflict")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-3.5, 0.1)) +
    labs(x = "Non-state conflict", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

# Alle variablene, to figurer
ggpubr::ggarrange(libdem, temp, capdist, bdist, ttime_mean, global_ind,
                  pop, unempl_tot,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")

ggsave("./Figurer/apesneg, speidich, seven variables 1.png", height = 21, width = 9)

ggpubr::ggarrange(gdp, shdi, empl_agr, irrig_sum, agri_ih, excluded, lag_conflict, non_state_conflict,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")

ggsave("./Figurer/apesneg, speidich, seven variables 2.png", height = 21, width = 9)


# SPEI pos ----------------------------------------------------------------

load(file = "./R-script, analysis/Models/apes_pos_speidich.rds")

# Under: individuelle plots, men også sammensatt, da uten mange av variablene
(shdi <- apes_pos %>% filter(variable == "shdi") %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    scale_x_continuous(breaks = round(apes_pos$quantile, 2)) +
    labs(x = "SHDI", y = "") +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))


(unempl_tot <- apes_pos %>% filter(variable == "unempl_tot") %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Unemployment", y = "") +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))


(libdem <- apes_pos %>% filter(variable == c("libdem")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Liberal\ndemocracy", y = "") +
    theme_light() +
    coord_flip()+
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(temp <- apes_pos %>% filter(variable == c("temp")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Temperature", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(pop <- apes_pos %>% filter(variable == c("pop")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Population", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(empl_agr <- apes_pos %>% filter(variable == c("empl_agr")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Employment\nagriculture", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(capdist <- apes_pos %>% filter(variable == c("capdist")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Distance\nto capital", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(bdist <- apes_pos %>% filter(variable == c("bdist3")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Distance\nto border", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(global_ind <- apes_pos %>% filter(variable == c("global_ind")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    #scale_y_continuous(limits = c(-6.2, 0.15)) +
    #labs(x = "Globalization", y = "") +
    theme_light() +
   # coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(gdp <- apes_pos %>% filter(variable == c("gdp")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "GDP", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(irrig_sum <- apes_pos %>% filter(variable == c("irrig_sum")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Irrigation", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(agri_ih <- apes_pos %>% filter(variable == c("agri_ih")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Agriculture\nin cell", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(excluded <- apes_pos %>% filter(variable == c("excluded")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Excluded", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(ttime_mean <- apes_pos %>% filter(variable == c("ttime_mean")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    #scale_y_continuous(limits = c(-6.2, 0.15)) +
    #labs(x = "Travel time\nurban center", y = "") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279"))) 
    #geom_hline(yintercept = 0, linetype = 3)) #+
    #coord_flip() # turn it around

(lag_conflict <- apes_pos %>% filter(variable == c("lag_conflict")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Conflict lagged", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

(non_state_conflict <- apes_pos %>% filter(variable == c("non_state_conflict")) %>% 
    ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_y_continuous(limits = c(-6.2, 0.15)) +
    labs(x = "Non-state conflict", y = "") +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_hline(yintercept = 0, linetype = 3))

# Alle variablene, to figurer
ggpubr::ggarrange(libdem, temp, capdist, bdist, ttime_mean, global_ind,
                  pop, unempl_tot,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")


ggsave("./Figurer/apespos, speidich, seven variables 1.png", height = 21, width = 9)

ggpubr::ggarrange(gdp, shdi, empl_agr, irrig_sum, agri_ih, excluded, lag_conflict, non_state_conflict,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")

ggsave("./Figurer/apespos, speidich, seven variables 2.png", height = 21, width = 9)


