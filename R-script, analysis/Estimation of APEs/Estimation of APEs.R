## Tester Jonas-metode for å plotte APE ###

library(tidyverse)

load(file = "./R-script, analysis/Models/cf_neg.rds")

train_speineg <- readRDS(file = "./R-script, analysis/Models/train_speineg.rds")
test_speineg <- readRDS(file = "./R-script, analysis/Models/test_speineg.rds")

# Tester med numerisk variabel
test_speineg$conflict <- as.numeric(as.character(test_speineg$conflict))
train_speineg$conflict <- as.numeric(as.character(train_speineg$conflict))


test_speineg <- dplyr::select(test_speineg, -conflict, -spei3_neg, -lon, -lat, -gid)
train_speineg <- dplyr::select(train_speineg, -conflict, -spei3_neg, -lon, -lat, -gid)



apes_neg <- tibble()

vars <- names(test_speineg)

for(var in vars){
  
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0.05, 0.95, 0.05))
  
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


save(apes_neg, file = "./R-script, analysis/Models/apes_neg_correct.rds")

#Land nummer 100 har veldig store avvik. Colombia. 


load(file = "./R-script, analysis/Models/apes_neg.rds")

ggplot(apes_neg, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  
  geom_point() + geom_errorbar() + facet_wrap(~variable, scales = "free") + theme_bw()



# SPEI_pos ----------------------------------------------------------------
# Tester med numerisk variabel
test_speipos$conflict <- as.numeric(as.character(test_speipos$conflict))
train_speipos$conflict <- as.numeric(as.character(train_speipos$conflict))
table(train_speipos$conflict)

test_speipos <- dplyr::select(test_speipos, -conflict, -spei3_pos, -lon, -lat, -gid)
train_speipos <- dplyr::select(train_speipos, -conflict, -spei3_pos, -lon, -lat, -gid)

apes_pos <- tibble()

vars <- names(test_speipos)

for(var in vars){
  
  var_quantiles <- quantile(train_speipos[[var]], probs = seq(0.05, 0.95, 0.05))
  
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


save(apes_pos, file = "./R-script, analysis/Models/apes_pos.rds")


ggplot(apes_pos, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  
  geom_point() + geom_errorbar() + facet_wrap(~variable, scales = "free") + theme_bw()


### Andre oppløsninger (quantiler)


apes_pos <- tibble()

vars <- names(test_speipos)

for(var in vars){
  
  var_quantiles <- quantile(train_speipos[[var]], probs = seq(0.05, 0.95, 0.20))
  
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

save(apes_pos, file = "./R-script, analysis/Models/apes_pos_20interval.rds")


## SPEI neg 


apes_neg <- tibble()

vars <- names(test_speineg)

for(var in vars){
  
  var_quantiles <- quantile(train_speineg[[var]], probs = seq(0.05, 0.95, 0.20))
  
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


save(apes_neg, file = "./R-script, analysis/Models/apes_neg_correct.rds")

