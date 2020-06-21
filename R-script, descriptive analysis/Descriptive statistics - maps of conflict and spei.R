### Descriptive statistics - maps of conflict and spei ###

# Create maps of frequency and location of conflict and SPEI events


library(tidyverse)
library(viridis)
library(ggpubr)

final <- read_rds("./Egne datasett/final_dataset.rds")

# Make spei binary

final <- final %>% mutate(spei3_neg = ifelse(spei3_neg < (-1), 1, 0),
                                        spei3_pos = ifelse(spei3_pos > 1, 1, 0))


# Maps with only countries with conflict ----------------------------------

# Remove NA from SPEI-variables
final_conf <- final %>% drop_na(spei3_neg) %>% drop_na(spei3_pos)

# Select cells with conflict equal to 1 and spei == 1, so that easier to see variation
conf <- final_conf %>% dplyr::select(lon, lat, conflict, best) %>% filter(conflict == 1)

speineg <- final_conf %>% group_by(lon, lat, spei3_neg) %>% 
  count() %>% ungroup(lon, lat, spei3_neg)

speineg$n <- ifelse(speineg$spei3_neg == 1, speineg$n, 0)


speipos <- final_conf %>% group_by(lon, lat, spei3_pos) %>% 
  count() %>% ungroup(lon, lat, spei3_pos)

speipos$n <- ifelse(speipos$spei3_pos == 1, speipos$n, 0)

# Remove the zeros from the plot
speineg_without_zero <- speineg %>% filter(n > 0)

# SPEI3_neg variation
gg_neg <- ggplotGrob(ggplot(speineg_without_zero, aes(n)) + 
                       geom_histogram(fill = viridis(30, begin = 0.5)) + 
                       theme_bw() + 
                       scale_x_continuous(breaks = seq(0, 9, 1)) +
                       labs(x = "", y = ""))

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speineg, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_raster(data = speineg_without_zero, aes(x = lon, y = lat, fill = as.factor(n))) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis_d(begin = 0.5) +
  theme_bw() +
  #scale_fill_gradient(low = "#008279", middle = "#77CBB9", high = "yellow") +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "Frequency of scarce rainfall events", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  annotation_custom(gg_neg, xmin = -200, xmax = -90, ymin = -65, ymax = 0)


ggsave("./Figurer/frequency of scarce rainfall events.png")



# SPEI3_pos variation

# Remove the zeros from the plot
speipos_without_zero <- speipos %>% filter(n > 0)

gg_pos <- ggplotGrob(ggplot(speipos_without_zero, aes(n)) + 
                       geom_histogram(fill = viridis(30, begin = 0.5)) + 
                       theme_bw() + 
                       scale_x_continuous(breaks = seq(0, 9, 1)) +
                       labs(x = "", y = ""))

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speipos, aes(x = lon, y = lat), fill = "#403B3B") +
  geom_raster(data = speipos_without_zero, aes(x = lon, y = lat, fill = as.factor(n))) +
  #geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.2) + 
  coord_fixed(ratio = 1) +
  #scale_fill_gradient(low = "#77CBB9", high = "#002926")  +
  scale_fill_viridis_d(begin = 0.5) +
  theme_bw() +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "Frequency of excess rainfall events", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  annotation_custom(gg_pos, xmin = -200, xmax = -90, ymin = -65, ymax = 0)
  #labs(title = "Percentage of missing SPEI values by country, after weighted by growing season")

ggsave("./Figurer/frequency of excess rainfall events.png")

## SPEI events over time
# SPEI_neg
final_conf %>% filter(spei3_neg == 1) %>% group_by(year, spei3_neg) %>% summarize(n = n()) %>%
  ggplot() + 
  geom_line(aes(x = year, y = n)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "Number of scarce rainfall events") +
  scale_x_continuous(breaks = seq(1990, 2018, 5))

ggsave("./Figurer/speineg over time.png")

# SPEI_pos
final_conf %>% filter(spei3_pos == 1) %>% group_by(year, spei3_pos) %>% summarize(n = n()) %>%
  ggplot() + 
  geom_line(aes(x = year, y = n)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  labs(x = "Year", y = "Number of excess rainfall events") +
  scale_x_continuous(breaks = seq(1990, 2018, 5))

ggsave("./Figurer/speipos over time.png")



# Maps of conflict --------------------------------------------------------
gg_pos <- ggplotGrob(ggplot(speipos, aes(n)) + 
                       geom_histogram(fill = viridis(30)) + 
                       theme_bw() + 
                       scale_x_continuous(breaks = seq(0, 9, 1)) +
                       labs(x = "", y = ""))

ggplot(final_conf, aes(x = year, y = conflict)) + 
  geom_line(fill = viridis(30)) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 9, 1)) +
  labs(x = "", y = "")

gg_conf_time <- ggplotGrob(final_conf %>% filter(conflict == 1) %>% group_by(year, conflict) %>% summarize(n = n()) %>%
             ggplot() + 
             geom_line(aes(x = year, y = n)) +
             theme_bw() +
             labs(x = "Year", y = "") +
             scale_x_continuous(breaks = seq(1990, 2018, 10))) 

ggplot() + geom_raster(data = final, aes(x= lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speipos, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.2, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_colour_manual(values = "#00A398") +
  labs(title = "Violent, state-based conflict", x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  annotation_custom(gg_conf_time, xmin = -200, xmax = -90, ymin = -65, ymax = 0)

ggsave("./Figurer/conflict geographic dist and time.png")


# Make table of top ten countries with highest amount of state-based conflict
conf_countries_df <- final_conf %>% filter(conflict == 1) %>% group_by(gwno, conflict) %>% summarize(Conflicts = n()) %>% 
  arrange(desc(Conflicts)) %>% ungroup(gwno, conflict) %>% as.data.frame()

conf_countries_df <- conf_countries_df %>% filter(Conflicts > 500) # Only include top 10 countries

conf_countries_df$Country <- c("Afghanistan", "India", "Colombia", "Philippines", "Turkey", 
                               "Algerie", "Pakistan", "Iraq", "Angola", "Sudan")

conf_countries_df <- conf_countries_df %>% select(-conflict, -gwno)

stargazer::stargazer(conf_countries_df, #type = "text", 
                     summary = F)








