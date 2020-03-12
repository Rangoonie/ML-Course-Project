download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "./training.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "./testing.csv")

training <- read.csv("training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("testing.csv", na.strings = c("NA", "#DIV/0!", ""))

library(caret)
library(rpart)
library(RColorBrewer)
library(rattle)
library(randomForest)

## Cleaning Data
## Partionining the training set into subtraining set and subtesting set

training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]

inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
subtrain <- training[inTrain, ]
subtest <- training[-inTrain, ]

set.seed(1995)
## Decision Tree Model
modfit.dec <- rpart(classe ~ ., method = "class", data = subtrain)
fancyRpartPlot(modfit.dec)
pred.dec <- predict(modfit.dec, subtest, type = "class")
confusionMatrix(pred.dec, subtest$classe)

## Random Forest Model
modfit.rf <- randomForest(classe ~ ., data = subtrain)
pred.rf <- predict(modfit.rf, subtest, type = "class")
confusionMatrix(pred.rf, subtest$classe)

## Random Forest on Testing Set
pred.test <- predict(modfit.rf, newdata = testing, type = "class")
table(pred.test)
