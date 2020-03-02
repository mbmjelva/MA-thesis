
##### Random forest #####

library(tidyverse)
library(grf)
library(randomForest) 
library(caret)
library(rpart.plot) 

# Load the data for alternative 1
sample_final <- read_rds("./Egne datasett/sample_of_finaldata_for_rf.rds")


# Stratified random sampling based on outcome variable -------------------------------------------------------------
# The alternative 1 and alternative 2 follows the alternative 1 and 2 from the sub-sampling process. Until decided which to choose. 

### Alternative 1
# stratifiserer basert på de to levelsene av conflict, slik at får en even distribution
set.seed(125)

trainrows <- createDataPartition(sample_final$conflict, p = 0.7, list = F) # Fra caret-pakka
training <- sample_final[trainrows,]
test <- sample_final[-trainrows,]
# Dette blir riktig. 

table(training$conflict) / nrow(training)
table(test$conflict) / nrow(test)
table(complete.cases(sample_final))

### Alternative 2
set.seed(125)

trainrows <- createDataPartition(sample_final_prop$conflict, p = 0.7, list = F) # Fra caret-pakka
training_prop <- sample_final_prop[trainrows,]
test_prop <- sample_final_prop[-trainrows,]
# Dette blir riktig. 

table(training_prop$conflict) / nrow(training_prop)
table(test_prop$conflict) / nrow(test_prop)


# Train model -------------------------------------------------------------

# Må finne den optimale node size, mtry og ntrees https://www.guru99.com/r-random-forest-tutorial.html

training <- dplyr::select(training, -spei3_neg, -spei3_pos) # Får ikke til å kjøre modellen hvis disse er med. Hvorfor? 

### Alternative 1
mod <- randomForest(conflict ~ ., data = training, proximity = T, importance = T, na.action = na.exclude)

varImpPlot(mod)
varImp(mod)

# Plotter et tre, brukt som eksempel på decision tree
mod_rpart <- rpart(conflict ~ ., data = training, na.action = na.exclude)
rpart.plot(mod_rpart)


# grf --------------------------------------------------------------------

cf <- causal_forest(
  X = model.matrix(~., data = training[, !(names(training) %in% c("conflict", "spei3"))]), # exclude the outcome and treatment variables
  Y = as.numeric(training$conflict) - 1, # convert outcome to 0 or 1 (når gjør om til numeric vil levels bli 1 og 2, trekker fra 1 for å få 0 og 1 i stedet)
  W = training$spei3[!is.na(training$spei3)], # Must be without NA
  num.trees = 1000,
  seed = 2865
)

#' Error in validate_observations(Y, X) : 
#' length of observation (W, Y, or Z) does not equal nrow(X).


X = model.matrix(~., data = training[, !(names(training) %in% c("conflict", "spei3"))])

nrow(X) # 7304
nrow(training) # 350001

table(complete.cases(X)) # 7304
table(complete.cases(training)) # 7304

# Det skjer noe galt i x-leddet. Ser fint ut når jeg ser på tabellen, men nrow gir et mye lavere antall rader, like mange som complete cases. Hvorfor? Hvordan fikse?



