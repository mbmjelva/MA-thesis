## Plots of variable importance ##

library(tidyverse)
library(grf)

# SPEI neg ----------------------------------------------------------------

load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

varimp_neg <- cf_neg %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))

# Remove variables that are not explanatory so that not included in the plot
varimp_neg_wo <- subset(varimp_neg, !variable %in% c("gid", "lon", "lat", "gwno"))

# Chose colors to fill in relevant variables
area_color <- c("#3C1518", "#B77659", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#B77659", "#3C1518",
                "#3C1518", "#3C1518", "#3C1518", "#B77659", "#3C1518", "#3C1518", "#3C1518", 
                "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#B77659")

(spei_neg <- ggplot(varimp_neg_wo) + 
    geom_bar(aes(reorder(variable, V1), V1), stat = "identity", fill = area_color) + # Reorder order the chategories depending on the values of a second variable (V1)
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(x = "", y = "") +
    coord_flip())

ggsave("./Figurer/speineg_varimp.png")


# SPEI pos ----------------------------------------------------------------

load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")

varimp_pos <- cf_pos %>% 
  variable_importance() %>% 
  as.data.frame() %>% 
  mutate(variable = colnames(cf_neg$X.orig)) %>% 
  arrange(desc(V1))

# Remove variables that are not explanatory so that not included in the plot
varimp_pos_wo <- subset(varimp_pos, !variable %in% c("gid", "lon", "lat", "gwno"))

# Chose colors to fill in relevant variables
area_color_pos <- c("#B77659", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                    "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518",
                    "#3C1518", "#B77659", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#B77659", 
                    "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518", "#3C1518")

(spei_pos <- ggplot(varimp_pos_wo) + 
    geom_bar(aes(reorder(variable, V1), V1), stat = "identity", fill = area_color_pos) + # Reorder order the chategories depending on the values of a second variable (V1)
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.2)) +
    labs(x = "", y = "") +
    coord_flip())

ggsave("./Figurer/speipos_varimp.png")

# Combine the plots -------------------------------------------------------

ggpubr::ggarrange(spei_neg, spei_pos)

ggsave("./Figurer/variable_importence_lagged.png")


