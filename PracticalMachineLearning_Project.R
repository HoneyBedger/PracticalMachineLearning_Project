## Load packages.
library(caret)
library(randomForest)
library(ggplot2)
library(gridExtra)

## Download and read data.
if(!file.exists("pml-training.csv")){
  trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(trainURL, destfile = "pml-training.csv")
}
if(!file.exists("pml-testing.csv")){
  testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(testURL, destfile = "pml-testing.csv")
}
pml_train <- read.csv("pml-training.csv", na.strings = c("NA", ""))
pml_test <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

## Exploratory analysis
str(pml_train)
## So, we have 160 variables, some of them are averages, maxima, minima etc, thus they are
## NAs almost everywhere. 
## The first 7 variables seem to be auxiliary and unnessessary for the model.
## Get rid of the 'avg', 'max', 'min' etc. variables which are NAs in almost all the observations.
pml_train <- pml_train[, colSums(is.na(pml_train)) < 1]

## Try a number of models (tree model, boosting, bagging, and random forest).
tree_fit <- train(x = pml_train[8:59], y = pml_train$classe, method = "rpart")
print(tree_fit$times)
boost_fit <- train(x = pml_train[8:59], y = pml_train$classe, method = "gbm", verbose = F)
bag_fit <- train(x = pml_train[8:59], y = pml_train$classe, method = "treebag")

## Set a seed for reproducibility.
## Try random forest with default settings (`caret` package calls
## `randomForest` function when `method = "rf"` is specified. But when the function
## is called directly it runs much faster)
set.seed(241291)
rf_fit <- randomForest(x = pml_train[8:59], y = pml_train$classe)
print(rf_fit)

## Cross-validation.
## 5-fold CV to test the accuracy of the final model.
set.seed(2431)
nfolds <- 5
folds <- createFolds(y = pml_train$classe, k = nfolds, list = T, returnTrain = F)
## Now each element of the `folds` list contains indeces of the corresponding 5 test sets.
## So, function `cv` does the following:
## builds a model using the raws that are not test rows;
## predicts and constructs confusion matrix for the corresponding test set.
## The function is then applied to each element of the `folds` list.
cv <- function(fold) {
  cv_fit <- randomForest(x = pml_train[-fold, 8:59], y = pml_train[-fold, 60])
  confusionMatrix(pml_train[fold, 60], predict(cv_fit, pml_train[fold,]))
}
cv_confMat <- lapply(folds, cv)
cv_accuracy <- sapply(cv_confMat, function(fold){fold$overall[1]})
print(cv_accuracy)
mean(cv_accuracy)

## OK, now see what we get with the test set!
## (remove the columns with NAs first)
pml_test <- pml_test[, colSums(is.na(pml_test)) < 1]
answers <- as.character(predict(rf_fit, pml_test))
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)