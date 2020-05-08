## Plotte effekter with lagged variables ##

# Nb, koden er ikke endret fra scriptet til generalized random forest, bortsett fra å bruke et annet datasett og endre navn på lagrede produkt. 
# Altså vil dette scriptet overkjøre det andre hvis man kjører samtidig.

library(tidyverse)
library(grf)

load(file = "./R-script, analysis/Models/cf_neg_lagged.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg_lagged.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg_lagged.rds")

# Tester med numerisk variabel
test_speineg$lag_conflict <- as.numeric(as.character(test_speineg$lag_conflict))
train_speineg$lag_conflict <- as.numeric(as.character(train_speineg$lag_conflict))

test_speineg <- dplyr::select(test_speineg, -conflict, -spei3_neg, -lon, -lat)
train_speineg <- dplyr::select(train_speineg, -conflict, -spei3_neg, -lon, -lat)

apes_neg <- tibble()

vars <- names(test_speineg)

for(var in vars){
  
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0.05, 0.95, 0.25))
  
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


ggplot(apes_neg, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  
  geom_point() + geom_errorbar() + facet_wrap(~variable, scales = "free")

save(apes_neg, file = "./R-script, analysis/Models/apes_neg_25interval.rds")
# Hvorfor er alle positive her? Hva betyr dette vs det vi får ut fra predict? 

# Positive, 25 quantiler
load(file = "./R-script, analysis/Models/cf_pos_lagged.rds")

train_speipos <- readRDS(file = "./R-script, analysis/Models/train_speipos_lagged.rds")
test_speipos <- readRDS(file = "./R-script, analysis/Models/test_speipos_lagged.rds")

# Tester med numerisk variabel
test_speipos$lag_conflict <- as.numeric(as.character(test_speipos$lag_conflict))
train_speipos$lag_conflict <- as.numeric(as.character(train_speipos$lag_conflict))

test_speipos <- dplyr::select(test_speipos, -conflict, -spei3_pos, -lon, -lat)
train_speipos <- dplyr::select(train_speipos, -conflict, -spei3_pos, -lon, -lat)

apes_pos <- tibble()

vars <- names(test_speipos)

for(var in vars){
  var_quantiles <- quantile(train_speipos[[var]], probs = seq(0.05, 0.95, 0.25))
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_partial_effect(cf_pos, subset = train_speipos[[var]] <= qtl)
    } else if (qtl == qtl_l) {
      res <- average_partial_effect(cf_pos, subset = train_speipos[[var]] <= qtl)
    } else {
      res <- average_partial_effect(cf_pos, subset = (train_speipos[[var]] <= qtl) & (train_speipos[[var]] > qtl_l))
    }
    apes_pos <- bind_rows(apes_pos, tibble("variable" = var, "quantile" = qtl, "ape" = res[1], "ape_std" = res[2]))
  }
  
}


apes_pos$apemin <- apes_pos$ape - (1.96*apes_pos$ape_std)

apes_pos$apemax <- apes_pos$ape + (1.96*apes_pos$ape_std)


apes_pos$significant <- if_else(apes_pos$apemin > 0 | apes_pos$apemax < 0, "yes", "no")




ggplot(apes_pos, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  
  geom_point() + geom_errorbar() + facet_wrap(~variable, scales = "free")

save(apes_pos, file = "./R-script, analysis/Models/apes_pos_25interval.rds")



print.grf_tree(cf_neg)
plot.grf_tree(cf_neg)

?average_partial_effect

