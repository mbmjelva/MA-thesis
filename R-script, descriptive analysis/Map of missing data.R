### Kartlegging av missing (forsøk) ###


library(tidyverse)
library(naniar)
library(sf)
library(viridis)


final <- read_rds("./Egne datasett/final_dataset.rds")
final <- dplyr::select(final, -starts_with("lag"))


# SPEI --------------------------------------------------------------------


final_miss <- final %>% group_by(gid) %>% miss_var_summary() %>% filter(variable == "spei3") %>% as.data.frame() %>% ungroup(gid)
final_coun <- final %>% group_by(gwno) %>% miss_var_summary() %>% filter(variable == "spei3") %>% as.data.frame() %>% ungroup(gid)

final_miss2 <- left_join(final_miss, final)
final_miss3 <- left_join(final_coun, final)


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



# Conflict ----------------------------------------------------------------

conf <- final_miss2 %>% dplyr::select(lon, lat, conflict) %>% filter(conflict == 1)

# Conflict with SPEI missing values
ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat)) + #, fill = as.factor(pct_miss))) + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("#9AE5E6", "#008279"), labels = c("Not missing", "Missing")) +
  scale_colour_manual(values = "black") +
  labs(fill = "SPEI3", colour = element_blank()) +
  theme_bw() +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")

# Conflict blank
ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat), fill = "#4EAAAD") + 
  geom_point(data = conf, aes(x = lon, y = lat, colour = "Conflict"), size = 0.5, alpha = 0.5) + 
  coord_fixed(ratio = 1) +
  scale_colour_manual(values = "black") +
  labs(title = "State-based conflict") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))



# Other variables with much missing ---------------------------------------

ggplot(final_miss2) + geom_raster(aes(x = lon, y = lat, fill = unempl_tot)) + 
  coord_fixed(ratio = 1) +
  theme_bw() +
  scale_fill_viridis(direction = -1) +
  #scale_fill_manual(values = wes_palette(name = "Royal1")) +
  #scale_fill_gradientn(colours = c("#9AE5E6", "#008279", "#DB504A", "darkred")) +
  labs(fill = "Unemployment")


ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat, fill = excluded)) + 
  coord_fixed(ratio = 1) +
  theme_bw() +
  scale_fill_viridis(direction = -1)

ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat, fill = shdi)) + 
  coord_fixed(ratio = 1) +
  theme_bw() +
  scale_fill_viridis(direction = -1)


ggplot(final_miss2) + geom_raster(aes(x= lon, y = lat, fill = agri_ih)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  scale_fill_viridis(direction = -1)




