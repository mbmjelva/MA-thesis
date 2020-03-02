
##### Notater randomforest #####




# OLS -----------------------------------------------------------------


# Må gjøre om inf og nan til NA slik at man kan kjøre lm. Spørsmålet er: hvorfor har jeg inf og nan i tillegg til na?
final2 <- final %>% mutate(spei = ifelse(spei == Inf | NaN, NA, spei))


# Kjører ols
ols <- glm(conflict ~ pop, data = final, na.action = "na.omit")




# Random forest -----------------------------------------------------------

library(randomForest)

set.seed(135)

#' denne tar det veldig lang tid å kjøre. Får ut følgende error: Warning message:
# In randomForest.default(m, y, ...) :
#   The response has five or fewer unique values.  Are you sure you want to do regression?
#rf <- randomForest(conflict ~ pop, data = final, ntree = 100, importance = T, na.action = "na.exclude")


summary(rf)
table(rf$coefs)

rf$coefs

# set.seed-funksjonen: man oppgir et tilfeldig tall som er det tallet som vil brukes til å produsere random tall. Dermed vil du få det samme tallet ved hver reproduksjon. Gjør det enklere å reprodusere analyse
# http://www.datasciencemadesimple.com/generate-sample-set-seed-function-r/. Litt usikker på hvorfor jeg trenger det her, men det er vel for å få den samme tilfeldige delingen?? Eller noe annet?
?randomForest
?set.seed


?rfImpute
#'
#'Brukes for å fylle inn missing values der det finnes. x = den variabelen man vil at skal fylles in, y = de variablene man skal bruke info på å fylle inn fra.
#'Som beskrevet i youtube-videoen vil man da fylle inn med "best guess". Kanskje lurt å gjøre slik at ikke all missing kastes ut? Ikke mange observasjoner uten noe missing
#'iter spør hvor mange random forests vi ønsker å bruke. Vanligvis er 4 til 6 nok. Man kan slevsagt sjekke om resultetet forbedres ved å øke iter. Gjør vanligvis ikke det.
#'Lagrer som et nytt datasett. Printer ut out-of-bag -error (printet som OOB). Den bør bli mindre når estimatene blir bedre.
#'Jeg har ingen NA på conflict. Trenger man å fylle inn missing for alle andre da?
#'

?randomForest
#' proximity=T vil gi en proximity-matrix som man kan bruke for å clustre samples senere.
#' Når modellen printes får vi ut OOB, som gir oss svar på hvor mange prosent som er feil klassifisert, og dermde hvor mange som er riktig (ved å ta 100-OOB) :)
#' Confusion matrix gir antallet som er riktig labelled etc.

mod <- randomForest(conflict ~ pop, data = training, proximity = T, na.action = na.exclude)
# Error: cannot allocate vector of size 801.3 Gb - Hm. Samme problem oppstår ved å endre på variablene. Både når bruker opprinnelig datasett og training.

# Causual forest ----------------------------------------------------------

library(grf)
?causal_forest

?grf


cf <- causal_forest(conflict, shdi, spei, data = final)

?createDataPartition


# Make bootstrapped dataset -----------------------------------------------

# Koden over ser ikke ut til å funke på et helt datasett, må spesifisere statistikk som skal bootstrappes (for eksempel "mean").

# Bruker rsample-pakken for å splitte datasettet

# Splitter datasettet i 70/30%. Ønsker å bruke 70% til å lage (trene) random forest (training) og 30% til å sjekke hvor god skogen er (test). 
# Bruker set.seed for at man skal få samme resultat ved neste gjennomgang

# Fjerner na, sjekker om det er problemet. Det er det ikke. Må kanskje ha stratifisert filtring
final_nona <- dplyr::filter(.data=filter, !is.na(conflict))
final$conflict_nona <- na.omit(final$conflict)
table(final$conflict_nona)

set.seed(123)

final_split <- initial_split(final, prop = .05)
training <- training(final_split)
testing <- testing(final_split)

mod <- randomForest(conflict ~ libdem, data = training, proximity = T, na.action = na.exclude, nodesize = 10, ntree = 100)
?randomForest
# Må ta stratifisert utvalg - altså passe på at ikke mister alle de variablene som har 1 på av. Må passe på å ha nok variasjon der, se bok.
# Må fjerne alle variablene som er hav. Se på koden som Jonas skrev.

table(is.na(final$spei3))


# Forsøker cf

table(complete.cases(final))


cf <- causal_forest(conflict, shdi, spei)

table(is.na(final$shdi))



# Følger bokeksempel ------------------------------------------------------

library(rpart)
library(rpart.plot)
library(ranger)

# Growing a decision tree
m <- rpart(conflict ~ ., data = training,
           method = "class")
m
rpart.plot(m)

# random forest
set.seed(12)
model <- caret::train(conflict ~ .,
                      data = final,
                      method = "ranger",
                      na.action = na.exclude)
print(model)

?caret::train



# Lager subsample ---------------------------------------------------------

sample_size = 10000
set.seed(12)
idxs = sample(1:nrow(final),sample_size,replace=F)
subsample = final[idxs,]

pvalues = list()

for (col in names(final)) {
  if (class(final[,col]) %in% c("numeric", "integer")) {
    # Numeric variable. Using Kolmogorov-Smirnov test
    
    pvalues[[col]] = ks.test(subsample[[col]],final[[col]])$p.value
    
  } else {
    # Categorical variable. Using Pearson's Chi-square test
    
    probs = table(final[[col]])/nrow(final)
    pvalues[[col]] = chisq.test(table(subsample[[col]]), p = probs)$p.value
    
  }
}
pvalues
str(final)

?subsample
?sample

set.seed(34)
split <- final[sample(nrow(final), size = 500000, replace = FALSE),]


table(split$conflict)

mod <- randomForest(conflict ~ ., data = split, proximity = T, na.action = na.exclude, ntree = 1000)  
mod


trainresample <- createResample(final$conflict, times = 10)

# Deler opp til training og test set basert på det stratifiserte random samplet. Bør ha ca. like mange class vars i hver.


train_re <- final[trainresample,]
test_re <- final[-trainresample,]

str(training)

library(PerformanceAnalytics)
?chart.Correlation

chart.Correlation(training)

mod <- randomForest(conflict ~ libdem, data = training, proximity = T, na.action = na.exclude, nodesize = 10, ntree = 100) # Kun conflict = 0. 


# Resampling --------------------------------------------------------------
# Bootstrapped resampling. Andre metoder som er bedre? Også fra caret-pakken
# Står litt info om detet i boka, men også i denne boka av samme forfattere: https://bookdown.org/max/FES/resampling.html

?createResample

# Lager 10 bootstrapped samples basert på training set. Noe usikker på om det egentlig bør være basert på det opprinnelige datasettet? Ser ikke sånn ut da man ikke kan velge proportion osv.
# Kan lage flere også, ved behov.

set.seed(12)

trainrows <- createDataPartition(final$conflict, times = 20, p = 0.7) # Fra caret-pakka

fold <- trainrows[[1]]
train1 <- training[fold,]

str(trainrows)
?createDataPartition
# Deler opp til training og test set basert på det stratifiserte random samplet. Bør ha ca. like mange class vars i hver.
training <- final[trainrows,]
test <- final[-trainrows,]


sample_size = 10000

set.seed(1)

idxs = sample(1:nrow(final),sample_size,replace=F)

subsample = final[idxs,]


resample <- createResample(training, times = 5)

str(resample)

get_resamplelist <- function(resample) {
  
  listofdfs <- list()
  
  for(i in 1:length(list(resample))) {
    boot <- resample[[i]]
    fold <- training[boot,]
    listofdfs[[i]] <- fold
  }
  
  return(listofdfs)
}


get_resamplelist <- function(resample) {
  
  listofdfs <- list()
  
  for(i in 1:length(list(resample))) {
    listofdfs <- training[resample[[i]],]
  }
  
  return(listofdfs)
}

b <- get_resamplelist(resample)


boot1 <- resample[[1]]
fold1 <- training[boot1,]

boot1

mod <- randomForest(conflict ~ libdem, data = fold1, proximity = T, na.action = na.exclude, nodesize = 10, ntree = 100) # Kun conflict = 0. 

split1 <- trainsub[[1]]

training_sub1 <- final[split1,]

table(training_sub1$conflict)


# Uses a subset of the Iris data set with different proportions of the Species factor
set.seed(42)

stratified_sample <- final %>%
  group_by(conflict) %>%
  mutate(num_rows=n()) %>%
  sample_frac(.3, weight=num_rows) %>%
  ungroup

# These results should be equal
table(final$conflict) / nrow(final)
table(stratified_sample$conflict) / nrow(stratified_sample)



# Gjør det på nytt for å få et mindre sample som det går an å jobbe med (tar fortsatt et stratifisert utvalg av training sample, passer på at AV får riktig distribusjon, men velger kun ut 30% av training samplet)
# Ser ut til å gå fint å jobbe med.

set.seed(456)

trainsub <- createDataPartition(training$conflict, p = 0.5, list = F) # Fra caret-pakka
training_sub <- training[trainsub,]
test_sub <- training[-trainsub,] # Denne blir til gjengjeld mye større, så blir vel egentlig litt feil å bruke den slik?

s <- final[sample(nrow[final], 100000),]

sample_n(final, 1000, replace = T, weight = conflict)



### Alternative 2
mod2 <- randomForest(conflict ~ ., data = training_prop, proximity = T, importance = T, na.action = na.exclude)
mod2

varImpPlot(mod2)
getTree(mod)
importance(mod)

plot.randomForest(mod)
mod$forest

## Så model 2 predikerer helt perfekt? Noe som er rart med måten det er trekt ut på. For stor andel med konflikt relativt sett
names(training)





# Kode for å lage graf
apes <- tibble()

vars <- names(X.test)

for(var in vars){
  var_quantiles <- quantile(X[[var]], probs = seq(0.05, 0.95, 0.05))
  for(i in 1:length(var_quantiles)){
    qtl <- var_quantiles[i]
    qtl_l <- var_quantiles[i-1]
    if(i == 1){
      res <- average_partial_effect(rf, subset = X[[var]] <= qtl)
    } else if (qtl == qtl_l) {
      res <- average_partial_effect(rf, subset = X[[var]] <= qtl)
    } else {
      res <- average_partial_effect(rf, subset = (X[[var]] <= qtl) & (X[[var]] > qtl_l))
      
    }
    apes <- bind_rows(apes, tibble("variable" = var, "quantile" = qtl, "ape" = res[1], "ape_std" = res[2]))
    
  }
  
}

apes$apemin <- apes$ape - (1.96*apes$ape_std)
apes$apemax <- apes$ape + (1.96*apes$ape_std)
apes$significant <- if_else(apes$apemin > 0 | apes$apemax < 0, "yes", "no")


ggplot(apes, aes(x = quantile, y = ape, ymin = apemin, ymax = apemax, color = significant)) +
  geom_point() + geom_errorbar() + facet_wrap(~variable, scales = "free")





