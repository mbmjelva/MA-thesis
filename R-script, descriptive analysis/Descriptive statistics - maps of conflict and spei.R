### Descriptive statistics - maps of conflict and spei ###

# Create maps of frequency and location of conflict and SPEI events


library(tidyverse)
library(viridis)

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
ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speineg, aes(x = lon, y = lat, fill = n)) +
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.2) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis() +
  theme_bw() +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "SPEI3", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")
#labs(title = "Percentage of missing SPEI values by country, after weighted by growing season")


# SPEI3_pos variation
ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speipos, aes(x = lon, y = lat, fill = n)) +
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.2) + 
  coord_fixed(ratio = 1) +
  scale_fill_gradient(low = "#B77659", high = "#3C1518")  +
  #scale_fill_viridis() +
  theme_bw() +
  scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "SPEI3", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical") 
#labs(title = "Percentage of missing SPEI values by country, after weighted by growing season")

# #3C1518

# Maps with all countries -------------------------------------------------

?scale_fill_continuous()
