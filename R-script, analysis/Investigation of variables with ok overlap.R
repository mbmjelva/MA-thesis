## Investigate data with overlap ##

# Investigate geographic location etc. of data with overlap - to be used in Chapter 6

## PS: Dette dokumentet er kaotisk nå. Må fjerne alt som dreier seg om land kun med conflict. Går bort fra det nå.

library(tidyverse)
library(grf)
library(finalfit)

final <- read_rds("./Egne datasett/final_dataset.rds")

load(file = "./R-script, analysis/Models/cf_neg_lagged_speidich.rds")
load(file = "./R-script, analysis/Models/cf_pos_lagged_speidich.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged_speidich.rds")
train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged_speidich.rds")

# Fix data for plots
# Remove NA from SPEI-variables
final_conf <- final %>% drop_na(spei3_neg) %>% drop_na(spei3_pos)

speineg <- final_conf %>% group_by(lon, lat, spei3_neg) %>% 
  count() %>% ungroup(lon, lat, spei3_neg)

speipos <- final_conf %>% group_by(lon, lat, spei3_pos) %>% 
  count() %>% ungroup(lon, lat, spei3_pos)

# Find area with overlap
area_of_overlap <- cf_neg_speidich$W.hat > 0.1 & cf_neg_speidich$W.hat < 0.9
area_of_overlap_pos <- cf_pos_speidich$W.hat > 0.1 & cf_pos_speidich$W.hat < 0.9 

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


# Plot
ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speineg, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_raster(data = obs, aes(x = lon, y = lat), fill = "#B77659") +
  labs(title = "SPEI3 neg, 0.1-0.9") +
  theme_bw()

ggsave("./Figurer/data with propscores 0.1 and 0.9, neg.png")

ggplot() + geom_raster(data = final, aes(x = lon, y = lat), fill = "darkgrey") + 
  geom_raster(data = speipos, aes(x = lon, y = lat), fill = "#403B3B") + 
  geom_raster(data = obs2, aes(x = lon, y = lat), fill = "#B77659") +
  labs(title = "SPEI3 pos, 0.1-0.9") +
  theme_bw()

ggsave("./Figurer/data with propscores 0.1 and 0.9, pos.png")




# Sjekker forskjell i gjennomsnitt ----------------------------------------

X <- train_speineg[, (names(train_speineg) %in% 
                        c("shdi", "agri_ih", "empl_agr", 
                          "irrig_sum", "capdist", "ttime_mean", 
                          "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                          "temp", "libdem", "lon", "lat", "spei3_neg"))] # Fjern lon/lat senere

X_pos <- train_speipos[, (names(train_speipos) %in% 
                            c("shdi", "agri_ih", "empl_agr", 
                              "irrig_sum", "capdist", "ttime_mean", 
                              "pop", "excluded", "global_ind", "gdp", "non_state_conflict",
                              "temp", "libdem", "lon", "lat", "spei3_pos"))] # Fjern lon/lat senere

obs <- X[which(area_of_overlap),]
obs2 <- X_pos[which(area_of_overlap_pos),]

obs$spei3_neg_fact <- as.factor(obs$spei3_neg)
obs2$spei3_pos_fact <- as.factor(obs2$spei3_pos)

# Mean speineg with overlap
spei_dist_neg_overlap <- obs %>% 
  summary_factorlist(dependent = "spei3_neg_fact", 
                     explanatory = c("temp", "agri_ih", "irrig_sum", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, add_col_totals = T)

diff_mean <- function(spei3_pos, var) {
  mean_1 <- mean(var[spei3_pos == 1], na.rm = T)
  mean_0 <- mean(var[spei3_pos == 0], na.rm = T)
  diff <-  round((mean_1 - mean_0),1)
  output <- list(diff = diff)
  return(output)
}

a <- diff_mean(obs$spei3_neg, obs$temp)
b <- diff_mean(obs$spei3_neg, obs$agri_ih)
c <- diff_mean(obs$spei3_neg, obs$irrig_sum)
d <- diff_mean(obs$spei3_neg, obs$capdist)
e <- diff_mean(obs$spei3_neg, obs$ttime_mean)
f <- diff_mean(obs$spei3_neg, obs$pop)
g <- diff_mean(obs$spei3_neg, obs$empl_agr)
h <- diff_mean(obs$spei3_neg, obs$excluded)
i <- diff_mean(obs$spei3_neg, obs$shdi)
j <- diff_mean(obs$spei3_neg, obs$libdem)
k <- diff_mean(obs$spei3_neg, obs$global_ind)
l <- diff_mean(obs$spei3_neg, obs$gdp)

spei_dist_neg_overlap$difference <- c("", a, b, c, d, e, f, g, h, i, j, k, l)

stargazer::stargazer(spei_dist_neg_overlap, summary = F, #type = "text",
                     title = "Mean and SD, SPEI3 negative, with overlap", digits = 1)

# Speipos mean with overlap
spei_dist_pos_overlap <- obs2 %>% 
  summary_factorlist(dependent = "spei3_pos_fact", 
                     explanatory = c("temp", "agri_ih", "irrig_sum", "capdist", "ttime_mean", "pop",
                                     "empl_agr", "excluded", "shdi", "libdem", "global_ind", "gdp"), 
                     na_include=TRUE, add_col_totals = T)

diff_mean <- function(spei3_pos, var) {
  mean_1 <- mean(var[spei3_pos == 1], na.rm = T)
  mean_0 <- mean(var[spei3_pos == 0], na.rm = T)
  diff <-  round((mean_1 - mean_0),1)
  output <- list(diff = diff)
  return(output)
}

a <- diff_mean(obs2$spei3_pos, obs2$temp)
b <- diff_mean(obs2$spei3_pos, obs2$agri_ih)
c <- diff_mean(obs2$spei3_pos, obs2$irrig_sum)
d <- diff_mean(obs2$spei3_pos, obs2$capdist)
e <- diff_mean(obs2$spei3_pos, obs2$ttime_mean)
f <- diff_mean(obs2$spei3_pos, obs2$pop)
g <- diff_mean(obs2$spei3_pos, obs2$empl_agr)
h <- diff_mean(obs2$spei3_pos, obs2$excluded)
i <- diff_mean(obs2$spei3_pos, obs2$shdi)
j <- diff_mean(obs2$spei3_pos, obs2$libdem)
k <- diff_mean(obs2$spei3_pos, obs2$global_ind)
l <- diff_mean(obs2$spei3_pos, obs2$gdp)

spei_dist_pos_overlap$difference <- c("", a, b, c, d, e, f, g, h, i, j, k, l)

stargazer::stargazer(spei_dist_pos_overlap, summary = F, #type = "text",
                     title = "Mean and SD, SPEI3 positive, with overlap")




# Make table with number of observations before and after -----------------

area_of_overlap_28 <- cf_neg_speidich$W.hat > 0.2 & cf_neg_speidich$W.hat < 0.8
area_of_overlap_pos_28 <- cf_pos_speidich$W.hat > 0.2 & cf_pos_speidich$W.hat < 0.8 #

obs_28 <- X[which(area_of_overlap_28),]
obs2_28 <- X_pos[which(area_of_overlap_pos_28),]

# Make table out of the variation
obs_ov_28 <- table(obs_28$spei3_neg)
obs_ov_2_28 <- table(obs2_28$spei3_pos)
obs_ov <- table(obs$spei3_neg)
obs2_ov <- table(obs2$spei3_pos)
obs_normal <- table(sample_final$spei3_neg)
obs_normal2 <- table(sample_final$spei3_pos)


tb <- rbind(obs_normal, obs_ov, obs_ov_28)
tb <- cbind(tb, Total = rowSums(tb))

tb2 <- rbind(obs_normal2, obs2_ov, obs_ov_2_28)
tb2 <- cbind(tb2, Total = rowSums(tb2))

tot <- rbind(tb, tb2)
rownames(tot) <- c("SPEI3 neg, all","SPEI3 neg, propensity scores between 0.1 and 0.9", "SPEI3 neg, propensity scores between 0.2 and 0.8",
             "SPEI3 pos, all","SPEI3 pos, propensity scores between 0.1 and 0.9", "SPEI3 pos, propensity scores between 0.2 and 0.8")

stargazer::stargazer(tot, summary = F, type = "text")


