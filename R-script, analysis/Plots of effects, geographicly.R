## Map of effects ##

# This script makes the maps of ITEs at the end of Chapter 6

library(tidyverse)
library(grf)
library(viridis)
library(ggpubr)

# Load necessary files
load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")
load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich.rds")
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich.rds")


# Include final dataset and final without missing SPEI values to add layers to plots
final <- read_rds("./Egne datasett/final_dataset.rds")
final_conf <- final %>% drop_na(spei3_neg) %>% drop_na(spei3_pos)

# Fix data to plot
area_of_overlap <- cf_neg_speidich$W.hat > 0.1 & cf_neg_speidich$W.hat < 0.9
area_of_overlap_pos <- cf_pos_speidich$W.hat > 0.1 & cf_pos_speidich$W.hat < 0.9

tau.hat <- predict(cf_neg_speidich)$predictions
tau.hat_pos <- predict(cf_pos_speidich)$predictions

tau.hat_overlap <- tau.hat[which(area_of_overlap,)]
tau.hat_pos_overlap <- tau.hat_pos[which(area_of_overlap_pos,)]

final_conf$preds_neg <- tau.hat[which(area_of_overlap,)] 
final_conf$preds_pos <- tau.hat_pos_overlap 


X <- train_speineg[, (names(train_speineg) %in% 
                        c("shdi", "agri_ih", "empl_agr", 
                          "irrig_sum", "capdist", "ttime_mean", 
                          "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                          "temp", "libdem", "lon", "lat", "spei3_neg"))] 

X_pos <- train_speipos[, (names(train_speipos) %in% 
                            c("shdi", "agri_ih", "empl_agr", 
                              "irrig_sum", "capdist", "ttime_mean", 
                              "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                              "temp", "libdem", "lon", "lat", "spei3_pos"))] 

obs <- X[which(area_of_overlap),]

obs2 <- X_pos[which(area_of_overlap_pos),]

speineg <- final_conf %>% group_by(lon, lat, spei3_neg) %>% 
  count() %>% ungroup(lon, lat, spei3_neg)

speineg$n <- ifelse(speineg$spei3_neg == 1, speineg$n, 0)


speipos <- final_conf %>% group_by(lon, lat, spei3_pos) %>% 
  count() %>% ungroup(lon, lat, spei3_pos)

speipos$n <- ifelse(speipos$spei3_pos == 1, speipos$n, 0)

# Remove the zeros from the plot
speineg_without_zero <- speineg %>% filter(n > 0)

summary(tau.hat_overlap)

# Gjør om til faktor slik at man lettere kan se forskjeller på kartet
tau.hat_overlap_fac <- ifelse(tau.hat_overlap < 0 & tau.hat_overlap >= (-0.1), "-0.1 to 0",
                          ifelse(tau.hat_overlap < (-0.1) & tau.hat_overlap >= (-0.4), "-0.4 to -0.1",
                                 ifelse(tau.hat_overlap < (-0.4), "Lower than -0.4", 
                                        ifelse(tau.hat_overlap == 0, "0",
                                               ifelse(tau.hat_overlap > 0 & tau.hat_overlap <= 0.1, "0 to 0.1",
                                                      ifelse(tau.hat_overlap > 0.1 & tau.hat_overlap <= 0.4, "0.1 to 0.4",
                                                             ifelse(tau.hat_overlap > 0.4, "Greater than 0.4", NA)))))))
                                 
tau.hat_overlap_fac <- factor(tau.hat_overlap_fac, levels = c("Lower than -0.4", "-0.4 to -0.1", "-0.1 to 0", "0 to 0.1", "0.1 to 0.4", "Greater than 0.4"))
                          
tau.hat_pos_overlap_fac <- ifelse(tau.hat_pos_overlap < 0 & tau.hat_pos_overlap >= (-0.1), "-0.1 to 0",
                              ifelse(tau.hat_pos_overlap < (-0.1) & tau.hat_pos_overlap >= (-0.4), "-0.4 to -0.1",
                                     ifelse(tau.hat_pos_overlap < (-0.4), "Lower than -0.4", 
                                            ifelse(tau.hat_pos_overlap == 0, "0",
                                                   ifelse(tau.hat_pos_overlap > 0 & tau.hat_pos_overlap <= 0.1, "0 to 0.1",
                                                          ifelse(tau.hat_pos_overlap > 0.1 & tau.hat_pos_overlap <= 0.4, "0.1 to 0.4",
                                                                 ifelse(tau.hat_pos_overlap > 0.4, "Greater than 0.4", NA)))))))

tau.hat_pos_overlap_fac <- factor(tau.hat_pos_overlap_fac, levels = c("Lower than -0.4", "-0.4 to -0.1", "-0.1 to 0", "0 to 0.1", "0.1 to 0.4", "Greater than 0.4"))

# Plot of variation in treatment effects

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  #geom_raster(data = final_conf, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_raster(data = obs, aes(x = lon, y = lat, fill = tau.hat_overlap_fac)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis_d() +
  theme_bw() +
  #scale_fill_gradient(low = "#008279", middle = "#77CBB9", high = "yellow") +
  #scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "Individual treatment effects", title = "CF-neg", colour = element_blank()) +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")# +
  #guides(colour = guide_legend(override.aes = list(size=4)))# +
  #annotation_custom(gg_neg, xmin = -200, xmax = -90, ymin = -65, ymax = 0)

ggsave("./Figurer/plot of ites, area of overlap.png")

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  #geom_raster(data = final_conf, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_raster(data = obs2, aes(x = lon, y = lat, fill = tau.hat_pos_overlap_fac)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis_d() +
  theme_bw() +
  #scale_fill_gradient(low = "#008279", middle = "#77CBB9", high = "yellow") +
  #scale_colour_manual(values = "#8E1D1D") +
  labs(fill = "Individual treatment effects", title = "CF-pos", colour = element_blank()) +
  theme(legend.position = c(fill = "bottom"),
        legend.box = "vertical")


ggsave("./Figurer/plot of ites, area of overlap, pos.png")






