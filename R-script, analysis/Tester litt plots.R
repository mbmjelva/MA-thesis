### Tester litt plots ###

library(tidyverse)
library(grf)

# Colors: #B77659, #3C1518, #2D2A32


load(file = "./R-script, analysis/Models/apes_neg_25interval.rds")


ggplot(apes_neg, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  geom_point() + geom_errorbar() + facet_wrap(~variable, scales = "free")


apes_neg %>% filter(variable == "shdi") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax)) + #, color = significant)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal()


load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged.rds")

test_speineg$lag_conflict <- as.numeric(as.character(test_speineg$lag_conflict))
train_speineg$lag_conflict <- as.numeric(as.character(train_speineg$lag_conflict))

test_speineg <- dplyr::select(test_speineg, -conflict, -spei3_neg, -lon, -lat, -lag_1_lon, -lag_1_lat, -lag_1_spei3, -lag_1_spei3_pos, -lag_1_spei3_neg,
                              -lag_1_gwno,-lag_1_events, lag_1_best, -lag_1_temp, -lag_1_agri_ih, -lag_1_irrig_sum,
                               -lag_1_bdist3, -lag_1_capdist, -lag_1_ttime_mean, -lag_1_pop, -lag_1_empl_agr, -lag_1_unempl_tot, -lag_1_excluded, -lag_1_shdi,      
                                -lag_1_libdem, -lag_1_global_ind, -lag_1_gdp, -lag_conflict)
train_speineg <- dplyr::select(train_speineg, -conflict, -spei3_neg, -lon, -lat, -lag_1_lon, -lag_1_lat, -lag_1_spei3, -lag_1_spei3_pos, -lag_1_spei3_neg,
                               -lag_1_gwno,-lag_1_events, lag_1_best, -lag_1_temp, -lag_1_agri_ih, -lag_1_irrig_sum,
                               -lag_1_bdist3, -lag_1_capdist, -lag_1_ttime_mean, -lag_1_pop, -lag_1_empl_agr, -lag_1_unempl_tot, -lag_1_excluded, -lag_1_shdi,      
                               -lag_1_libdem, -lag_1_global_ind, -lag_1_gdp, -lag_conflict)
names(test_speineg)

apes_neg <- tibble()
vars <- names(test_speineg)

for(var in vars){
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0.05, 0.95, 0.10))
  
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    
    if(i == 1){
      res <- average_partial_effect(cf_neg, subset = train_speineg[[var]] <= qtl)
    } else if (qtl == qtl_l) {
      res <- average_partial_effect(cf_neg, subset = train_speineg[[var]] <= qtl)
    } else {
      res <- average_partial_effect(cf_neg, subset = (train_speineg[[var]] <= qtl) & (train_speineg[[var]] > qtl_l))
      
    }
    
    apes_neg <- bind_rows(apes_neg, tibble("variable" = var, "quantile" = qtl, "ape" = res[1], "ape_std" = res[2]))
    
  }
  
}



apes_neg$apemin <- apes_neg$ape - (1.96*apes_neg$ape_std)

apes_neg$apemax <- apes_neg$ape + (1.96*apes_neg$ape_std)


apes_neg$significant <- if_else(apes_neg$apemin > 0 | apes_neg$apemax < 0, "yes", "no")

saveRDS(apes_neg, file = "./R-script, analysis/Models/apes_neg_10.rds")


apes_neg <- readRDS(file = "./R-script, analysis/Models/apes_neg_10.rds")

ggplot(apes_neg, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  
  geom_point() + geom_errorbar() #+ facet_wrap(~variable, scales = "free")


apes_neg %>% filter(variable == "shdi") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_line() + 
  geom_errorbar() + 
  theme_minimal()

apes_neg %>% filter(variable == "unempl_tot") %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_minimal()

apes_neg %>% filter(variable == c("empl_agr", "agri_ih")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_minimal()

apes_neg %>% filter(variable == c("pop")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_minimal()

apes_neg %>% filter(variable == c("bdist3")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_minimal()

apes_neg %>% filter(variable == c("libdem")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_minimal()

apes_neg %>% filter(variable == c("temp")) %>% 
  ggplot(aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_minimal()

library(ggthemes)
ggplot(apes_neg, aes(x = quantile, y = ape)) +
  geom_rangeframe() +
  theme_tufte() + 
  geom_point()
 # geom_ribbon(data=plot_data, aes(y=fit, ymin=low, ymax=high), alpha=.2) +
 # geom_line(data=plot_data, aes(y=fit))
?geom_rangeframe()

final <- readRDS("./Egne datasett/final_dataset.rds")

summary(final)

?average_treatment_effect
ate <- average_partial_effect(cf_neg)

het_test <- test_calibration(cf_neg)
stargazer::stargazer(het_test)


load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")

het_test_pos <- test_calibration(cf_pos)
stargazer::stargazer(het_test, het_test_pos, type = "text")

?regression_forest

tau.hat <- predict(cf_neg)$predictions
hist(tau.hat, density = 20, breaks = 40)

summary(tau.hat)

?predict?hist
?predict

tau.hat_pos <- predict(cf_pos)$predictions
hist(tau.hat_pos, density = 20, breaks = 40)
summary(tau.hat_pos)

?test_calibration

predict(cf_pos)

test_speineg <- readRDS("./R-script, analysis/Models/test_speineg_lagged.rds")

# Tester med Marks metode
preds <- predict(
  object = cf_neg, 
  newdata = model.matrix(~., data = test_speineg[, !(names(test_speineg) %in% c("conflict", "spei3_neg"))]), 
  estimate.variance = TRUE
)

#test_speineg$preds <- preds$predictions

p1 <- ggplot(test_speineg, aes(x = libdem, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p2 <- ggplot(test_speineg, aes(x = temp, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p3 <- ggplot(test_speineg, aes(x = unempl_tot, y = preds)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  theme_light()

p1

test_speineg <- select(test_speineg, -preds)

preds_response <- predict(
  object = cf_neg, 
  newdata = model.matrix(~., data = test_speineg[, !(names(test_speineg) %in% c("conflict", "spei3_neg"))]), 
  estimate.variance = TRUE # Trenger ikke response, det har ikke noe for seg
)

test_speineg$preds <- preds_response$predictions

ggplot(test_speineg, aes(x = preds, color = factor(conflict), fill = factor(conflict))) +
  geom_density(alpha = .25) +
  #scale_y_continuous(limits = c(0,25)) +
  theme_bw()

# Sjekker hvor godt prediksjonene stemmer med faktiske verdier på y. probs viser estimated change in probability of Y associated with treatment W. 
ggplot(test_speineg, aes(factor(conflict), preds)) +
  geom_boxplot(fill = "#B77659") +
  theme_bw()
# Denne grafen sier egentlig ingenting. Viser hvilken effect på konflikt de som har 0 på konflikt trolig har og hvilken effekt på konflikt de som har 1 har. Hva sir det oss egentlig? 

ggplot(test_speineg, aes(spei3_neg, preds)) +
  geom_point(colour = "#3C1518", alpha = 0.5) +
  theme_bw() +
  labs(y = "Estimated treatment effect", x = "SPEI3 negative")

ggsave("./Figurer/speineg_vs_preds.png")

# Denne kanskje mer interessant? Viser at samme spei-verdi kan ha både positive og negative effekter på y

(p1 <- ggplot(test_speineg, aes(x = libdem, y = preds)) +
  geom_point() +
  theme_light())

p2 <- ggplot(test_speineg, aes(x = temp, y = preds)) +
  geom_point() +
  theme_light()

p3 <- ggplot(test_speineg, aes(x = unempl_tot, y = preds)) +
  geom_point() +
  theme_light()
p2
p3

ggplot(test_speineg, aes(x = excluded, y = preds)) +
  geom_point() +
  theme_light()

table(test_speineg$excluded)

plot_htes <- function(cf_preds, ci = FALSE, z = 1.96) {
  if (is.null(cf_preds$predictions) || nrow(cf_preds$predictions) == 0)
    stop("cf_preds must include a matrix called 'predictions'")
  
  out <- ggplot(
    mapping = aes(
      x = rank(cf_preds$predictions), 
      y = cf_preds$predictions
    )
  ) +
    geom_point() +
    labs(x = "Rank", y = "Estimated Treatment Effect") +
    theme_light()
  
  if (ci && nrow(cf_preds$variance.estimates) > 0) {
    out <- out +
      geom_errorbar(
        mapping = aes(
          ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates),
          ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates)
        )
      )
  }
  
  return(out)
}

plot_htes(preds)



## Sannsynligheter basert på plot 
library(tidyverse)
final_lagged <- readRDS("./Egne datasett/final_lagged.rds")

## Trinn 2: Lager datasett med den uavh. var hp og kontrollvariabelen qsec. Jeg er interessert i effekten av hp, og lar denne variabelen variere i datasettet jeg lager. Jeg velger medianverdien til qsec.
data_for_prediction <- tibble(agri_ih = seq(min(final_lagged$agri_ih, na.rm = T),
                                         max(final_lagged$agri_ih, na.rm = T), .1),
                              libdem = median(final_lagged$libdem))


## Trinn 3: Lager nytt datasett med predikerte verdier for avhengig variabel, og standardfeil (i logit):
predicted_data <- predict(cf_neg, newdata = data_for_prediction,  type = "link",  
                          se=TRUE)

## Trinn 4: Kombinerer data fra trinn 2 og 3: 
plot_data <- cbind(predicted_data, data_for_prediction)

## Trinn 5: Kalkulerer konfidensintervall med standardfeil fra trinn 3 og legger til plot_data fra trinn 4. Her lager jeg 95% CI med vanlige standardfeil. Regner om fra logit til predikerte sannsynligheter.
plot_data$low  <- exp(plot_data$fit - 1.96*plot_data$se)/(1 + exp(plot_data$fit - 1.96*plot_data$se))
plot_data$high <- exp(plot_data$fit + 1.96*plot_data$se)/(1 + exp(plot_data$fit + 1.96*plot_data$se))
plot_data$fit <- exp(plot_data$fit)/(1+ exp(plot_data$fit))

## Trinn 6: Plot
library(ggthemes)
p <- ggplot(mtcars, aes(x = hp, y = am)) +
  geom_rangeframe() +
  ggtitle("Cars")  + 
  theme_tufte() + 
  ylab("Predicted probabilities for automatic") +
  xlab("Weight") + 
  geom_point() +
  geom_ribbon(data=plot_data, aes(y=fit, ymin=low, ymax=high), alpha=.2) +
  geom_line(data=plot_data, aes(y=fit))
p



