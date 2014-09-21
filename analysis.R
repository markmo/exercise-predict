library(caret)
library(randomForest)

training <- read.csv("data/pml-training.csv", na.strings=c("", "NA", "#DIV/0!"))
testing <- read.csv("data/pml-testing.csv", na.strings=c("", "NA", "#DIV/0!"))

training <- training[, -(1:7)]
testing <- testing[, -(1:7)]

set.seed(1234)
in.train <- createDataPartition(training$classe, p=0.6, list=F)
train <- training[in.train, ]
test <- training[-in.train, ]

train.y <- train$classe
in.y <- grep("classe", colnames(train))
train <- train[, -in.y]
test.y <- test$classe
test <- test[, -in.y]

nzv <- nearZeroVar(train)
train <- train[, -nzv]
test <- test[, -nzv]
testing <- testing[, -nzv]

na.columns <- apply(train, 2, function (x) any(is.na(x)))
train <- train[, !na.columns]
test <- test[, !na.columns]
testing <- testing[, !na.columns]

cor <- cor(train)
highlyCor <- findCorrelation(cor, cutoff=0.95)
train <- train[, -highlyCor]
test <- test[, -highlyCor]
testing <- testing[, -highlyCor]

modFit <- randomForest(train, train.y)
predictions <- predict(modFit, newdata=test)
confusionMatrix(predictions, test.y)

equalPredictions <- (predictions == test.y)
qplot(test.y, colour=equalPredictions, data=test)

vi <- varImp(modFit)
top.vars <- rownames(vi)[order(-vi)[1:10]]

answers <- predict(modFit, newdata=testing)
answers

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#pml_write_files(answers)
