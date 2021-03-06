### Kartlegging av missing (forsøk) ###


library(tidyverse)
library(naniar)
library(sf)
library(viridis)
library(ggpubr)


final <- read_rds("./Egne datasett/final_dataset.rds")
final <- dplyr::select(final, -starts_with("lag"))


# SPEI --------------------------------------------------------------------

# Make datasets with counts of missing spei data
final_miss <- final %>% group_by(gid) %>% miss_var_summary() %>% filter(variable == "spei3") %>% as.data.frame() %>% ungroup(gid)
final_coun <- final %>% group_by(gwno) %>% miss_var_summary() %>% filter(variable == "spei3") %>% as.data.frame() %>% ungroup(gwno)

final_miss2 <- left_join(final_miss, final)
final_miss3 <- left_join(final_coun, final)

# Make dataset to plot all areas with conflict for plots below
conf <- final_miss2 %>% dplyr::select(lon, lat, conflict) %>% filter(conflict == 1)

# Plots of missing spei data
ggplot() + geom_raster(data = final_miss2, aes(x = lon, y = lat, fill = pct_miss)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw() +
  labs(title = "Missing SPEI values after weighted by growing season")

ggplot() + geom_raster(data = final_miss3, aes(x= lon, y = lat, fill = pct_miss)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw() +
  labs(title = "Percentage of missing SPEI values by country, after weighted by growing season")

# SPEI3 variation
ggplot(final) + geom_raster(aes(x = lon, y = lat, fill = spei3)) + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.2) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1, begin = 0.4) +
  theme_bw() +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "SPEI3", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")
  #labs(title = "Percentage of missing SPEI values by country, after weighted by growing season")

# Plot binary SPEI3 values
final <- final %>% mutate(spei3_neg = ifelse(spei3_neg < (-1), 1, 0),
                                        spei3_pos = ifelse(spei3_pos > 1, 1, 0))

ggplot(final) + geom_histogram(aes(year, spei3_neg), stat = "identity") + 
  theme_bw() + labs(x = "", y = "")

ggplot(final) + geom_histogram(aes(year, spei3_pos), stat = "identity") + 
  theme_bw() + labs(x = "", y = "")

ggplot(final) + geom_histogram(aes(year, conflict), stat = "identity") + 
  theme_bw() + labs(x = "", y = "")

ggplot(final) + geom_raster(aes(x = lon, y = lat, fill = spei3)) + 
  coord_fixed(ratio = 1) +
  theme_bw() +
  scale_fill_viridis(direction = -1) +
  labs(fill = "Unemployment")# +
  #annotation_custom(gg_exc_unempl, xmin = -200, xmax = -90, ymin = -65, ymax = 0) 


# Conflict ----------------------------------------------------------------

# Conflict with SPEI missing values
ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat, fill = as.factor(pct_miss))) + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("#403B3B", "darkgrey"), labels = c("Not missing", "Missing")) +
  scale_colour_manual(values = "#00A398") +
  labs(fill = "SPEI3", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")

ggsave("./Figurer/Missing values SPEI, w conflict.png")

# Same graph, but without conflict
ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat, fill = as.factor(pct_miss))) + 
  #geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("#403B3B", "darkgrey"), labels = c("Not missing", "Missing")) +
  scale_colour_manual(values = "#00A398") +
  labs(fill = "SPEI3", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")

ggsave("./Figurer/Missing values SPEI.png")

#values = c("#9AE5E6", "#008279")

# Conflict blank
ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat), fill = "darkgrey") + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.2, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_colour_manual(values = "#403B3B") +
  labs(title = "State-based conflict") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

ggsave("./Figurer/conflict geographic distribution.png")


# Other variables with much missing ---------------------------------------


# Excluded
gg_exc_bar <- ggplotGrob(ggplot(final) + geom_bar(aes(as.factor(excluded), fill = as.factor(excluded))) + 
                           theme_bw() + 
                           theme(legend.position = "none") +
                           scale_fill_viridis_d(direction = -1, na.value = "grey50") +
                           labs(x = "", y = ""))

ggplot(final) + geom_raster(aes(x= lon, y = lat, fill = as.factor(excluded))) + 
  coord_fixed(ratio = 1) +
  theme_bw() + 
  labs(fill = "Excluded") +
  scale_fill_viridis_d(direction = -1, na.value = "grey50") +
  annotation_custom(gg_exc_bar, xmin = -200, xmax = -90, ymin = -65, ymax = 0)

ggsave("./Figurer/geogr_dist_excluded.png")


# SHDI
gg_shdi_bar <- ggplotGrob(ggplot(final, aes(shdi)) + 
  geom_histogram(fill = viridis(30)) + 
  theme_bw() + 
  labs(x = "", y = ""))

ggplot(final) + geom_raster(aes(x= lon, y = lat, fill = shdi)) + 
  coord_fixed(ratio = 1) +
  theme_bw() + 
  labs(fill = "SHDI") +
  scale_fill_viridis() +
  annotation_custom(gg_shdi_bar, xmin = -200, xmax = -90, ymin = -65, ymax = 0)

ggsave("./Figurer/geogr_dist_shdi.png")

# Agriculture in cell
gg_agri_bar <- ggplotGrob(ggplot(final, aes(agri_ih)) + 
    geom_histogram(fill = viridis(30)) + 
    theme_bw() + 
    labs(x = "", y = ""))

ggplot(final) + geom_raster(aes(x= lon, y = lat, fill = agri_ih)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  labs(fill = "Percentage of\nagricultural\narea in cell") +
  scale_fill_viridis() +
  annotation_custom(gg_agri_bar, xmin = -200, xmax = -90, ymin = -65, ymax = 0)

ggsave("./Figurer/geogr_dist_agri.png")

