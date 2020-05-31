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

# Select cells within countries that have experienced conflict 
# (where at least one of the cells within one of the year within that country have conflict == 1)
final_conf <- final_conf %>% group_by(gwno) %>% filter(any(conflict == 1)) %>% ungroup(gwno)

# Select cells with conflict equal to 1 and spei == 1, so that easier to see variation
conf <- final_conf %>% dplyr::select(lon, lat, conflict) %>% filter(conflict == 1)

speineg <- final_conf %>% group_by(lon, lat, spei3_neg) %>% 
  count() %>% ungroup(lon, lat, spei3_neg)

speineg$n <- ifelse(speineg$spei3_neg == 1, speineg$n, 0)


speipos <- final_conf %>% group_by(lon, lat, spei3_pos) %>% 
  count() %>% ungroup(lon, lat, spei3_pos)

speipos$n <- ifelse(speipos$spei3_pos == 1, speipos$n, 0)


# SPEI3_neg variation
gg_neg <- ggplotGrob(ggplot(speineg, aes(n)) + 
                       geom_histogram(fill = viridis(30)) + 
                       theme_bw() + 
                       scale_x_continuous(breaks = seq(0, 9, 1)) +
                       labs(x = "", y = ""))

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speineg, aes(x = lon, y = lat, fill = as.factor(n))) +
  #geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.2) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis_d() +
  theme_bw() +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "Frequency of scarce rainfall events", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  annotation_custom(gg_neg, xmin = -200, xmax = -90, ymin = -65, ymax = 0)


# SPEI3_pos variation
gg_pos <- ggplotGrob(ggplot(speipos, aes(n)) + 
                       geom_histogram(fill = viridis(30)) + 
                       theme_bw() + 
                       scale_x_continuous(breaks = seq(0, 9, 1)) +
                       labs(x = "", y = ""))

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speipos, aes(x = lon, y = lat, fill = as.factor(n))) +
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.2) + 
  coord_fixed(ratio = 1) +
  #scale_fill_gradient(low = "#77CBB9", high = "#002926")  +
  scale_fill_viridis_d() +
  theme_bw() +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "Frequency of scarce rainfall events", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical") +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  annotation_custom(gg_pos, xmin = -200, xmax = -90, ymin = -65, ymax = 0)
  #labs(title = "Percentage of missing SPEI values by country, after weighted by growing season")

# #3C1518

# Maps with all countries -------------------------------------------------

?scale_fill_continuous()



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

ggplot() + geom_raster(data = final, aes(x= lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speipos, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.2, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_colour_manual(values = "#00A398") +
  labs(title = "State-based conflict") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
