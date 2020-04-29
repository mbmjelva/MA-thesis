## Plot of apes, speidich, clusters ##

library(tidyverse)

# Spei neg
load(file = "./R-script, analysis/Models/apes_neg_speidich_cl_gwno.rds")

# Under: individuelle plots, men også sammensatt, da uten mange av variablene
(shdi <- apes_neg %>% filter(variable == "shdi") %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    scale_y_continuous(breaks = round(apes_pos$quantile, 2)) +
    labs(x = "", y = "SHDI") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))


(unempl_tot <- apes_neg %>% filter(variable == "unempl_tot") %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Unemployment") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))


(libdem <- apes_neg %>% filter(variable == c("libdem")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Liberal\ndemocracy") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(temp <- apes_neg %>% filter(variable == c("temp")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Temperature") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(pop <- apes_neg %>% filter(variable == c("pop")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Population") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(empl_agr <- apes_neg %>% filter(variable == c("empl_agr")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Employment\nagriculture") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(capdist <- apes_neg %>% filter(variable == c("capdist")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Distance\nto capital") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(bdist <- apes_neg %>% filter(variable == c("bdist3")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Distance\nto border") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(global_ind <- apes_neg %>% filter(variable == c("global_ind")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Globalization") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(gdp <- apes_neg %>% filter(variable == c("gdp")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "GDP") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(irrig_sum <- apes_neg %>% filter(variable == c("irrig_sum")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Irrigation") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(agri_ih <- apes_pos %>% filter(variable == c("agri_ih")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Agriculture\nin cell") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(excluded <- apes_neg %>% filter(variable == c("excluded")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Excluded") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(ttime_mean <- apes_neg %>% filter(variable == c("ttime_mean")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Travel time\nurban center") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) + 
    geom_vline(xintercept = 0, linetype = 3))

(lag_conflict <- apes_neg %>% filter(variable == c("lag_conflict")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Conflict lagged") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(non_state_conflict <- apes_neg %>% filter(variable == c("non_state_conflict")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Non-state conflict") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))


# Alle variablene, to figurer
ggpubr::ggarrange(libdem, temp, capdist, bdist, ttime_mean, global_ind,
                  pop, unempl_tot,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")

ggsave("./Figurer/apesneg, speidich, clustgwno, seven variables 1.png", height = 21, width = 9)

ggpubr::ggarrange(gdp, shdi, empl_agr, irrig_sum, agri_ih, excluded, lag_conflict, non_state_conflict,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")

ggsave("./Figurer/apesneg, speidich, clustgwno, seven variables 2.png", height = 21, width = 9)


# SPEI pos ----------------------------------------------------------------

load(file = "./R-script, analysis/Models/apes_pos_speidich_cl_gwno.rds")

# Under: individuelle plots, men også sammensatt, da uten mange av variablene
(shdi <- apes_pos %>% filter(variable == "shdi") %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    scale_y_continuous(breaks = round(apes_pos$quantile, 2)) +
    labs(x = "", y = "SHDI") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))


(unempl_tot <- apes_pos %>% filter(variable == "unempl_tot") %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    theme_light() +
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Unemployment") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))


(libdem <- apes_pos %>% filter(variable == c("libdem")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Liberal\ndemocracy") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(temp <- apes_pos %>% filter(variable == c("temp")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Temperature") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(pop <- apes_pos %>% filter(variable == c("pop")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Population") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(empl_agr <- apes_pos %>% filter(variable == c("empl_agr")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Employment\nagriculture") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(capdist <- apes_pos %>% filter(variable == c("capdist")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Distance\nto capital") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(bdist <- apes_pos %>% filter(variable == c("bdist3")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Distance\nto border") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(global_ind <- apes_pos %>% filter(variable == c("global_ind")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Globalization") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(gdp <- apes_pos %>% filter(variable == c("gdp")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "GDP") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(irrig_sum <- apes_pos %>% filter(variable == c("irrig_sum")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Irrigation") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(agri_ih <- apes_pos %>% filter(variable == c("agri_ih")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Agriculture\nin cell") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(excluded <- apes_pos %>% filter(variable == c("excluded")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Excluded") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(ttime_mean <- apes_pos %>% filter(variable == c("ttime_mean")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Travel time\nurban center") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) + 
    geom_vline(xintercept = 0, linetype = 3))

(lag_conflict <- apes_pos %>% filter(variable == c("lag_conflict")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Conflict lagged") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

(non_state_conflict <- apes_pos %>% filter(variable == c("non_state_conflict")) %>% 
    ggplot(aes(y = quantile, x = ape, xmin = apemin, xmax = apemax, color = significant)) + 
    geom_point() + 
    geom_errorbar() + 
    scale_x_continuous(limits = c(-0.3, 0.35)) +
    labs(x = "", y = "Non-state conflict") +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      text = element_text(size = 15)
    ) +
    scale_color_manual(values = c("no" = "#3C1518", "yes" = "#008279")) +
    geom_vline(xintercept = 0, linetype = 3))

# Alle variablene, to figurer
ggpubr::ggarrange(libdem, temp, capdist, bdist, ttime_mean, global_ind,
                  pop, unempl_tot,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")


ggsave("./Figurer/apespos, speidich, clustgwno, seven variables 1.png", height = 21, width = 9)

ggpubr::ggarrange(gdp, shdi, empl_agr, irrig_sum, agri_ih, excluded, lag_conflict, non_state_conflict,
                  ncol = 1, nrow = 8, align = "v",
                  common.legend = TRUE, legend = "bottom")

ggsave("./Figurer/apespos, speidich, clustgwno, seven variables 2.png", height = 21, width = 9)


