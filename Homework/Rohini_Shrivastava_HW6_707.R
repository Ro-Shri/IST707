
library(wordcloud)
library(tm)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext)
library(plyr)
library(ggplot2)
library(factoextra)
#library(mclust)
library(dplyr)
library(rpart)
require(rpart.plot)

install.packages("rattle")
library(rattle)
library(naivebayes)
library(caret)
library(e1071)

install.packages('e1071')

install.packages('caret', dependencies=TRUE)

install.packages("naivebayes")
install.packages("factoextra")
install.packages("SnowballC")
install.packages("rpart")

DigitTotalDF <- read.csv("C:/Users/shriv/Downloads/digit-recognizer/train.csv", 
                         header = TRUE, na.strings = "", stringsAsFactors = TRUE)

DigitTestDF <- read.csv("C:/Users/shriv/Downloads/digit-recognizer/test.csv", 
                         header = TRUE, na.strings = "", stringsAsFactors = TRUE)

smp_size <- floor(0.25 * nrow(DigitTotalDF))
DigitTotalDF$label <- as.factor(DigitTotalDF$label)
#train/test set
set.seed(25) 

train_ind <- sample(seq_len(nrow(DigitTotalDF)), size = smp_size)

train <- DigitTotalDF[train_ind, ]

test_ind <- sample(seq_len(nrow(DigitTestDF)), size = smp_size)

test_DF <- DigitTestDF[train_ind, ]


TrainTree <- rpart(label ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 5, maxdepth = 10))
summary(TrainTree)
rsq.rpart(TrainTree)

predicted_test <- predict(TrainTree, test_DF, type="class")
predicted_test

#(confusionMatrix(predicted_test, DigitDF_Test$label))

rpart.plot(TrainTree, extra=104, box.palette="GnBu")

#Take 2
TrainTree2 <- rpart(label ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 2, maxdepth = 13))
summary(TrainTree2)
rsq.rpart(TrainTree2)

predicted_test <- predict(TrainTree2, test_DF, type="class")
predicted_test

rpart.plot(TrainTree2, extra=104, box.palette="GnBu")

#Take 3

startTime <- proc.time()
TrainTree3 <- rpart(label ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 8, maxdepth = 5))
summary(TrainTree3)
rsq.rpart(TrainTree3)

predicted_test <- predict(TrainTree3, test_DF, type="class")
predicted_test

rpart.plot(TrainTree3, extra=104, box.palette="GnBu")

proc.time() - startTime

#NAIVE BAYES

startTime <- proc.time()

train_naive<-naiveBayes(label~., data=train, na.action = na.pass)
train_naive
summary(train_naive)

#Naive Bayes model Prediction 

DF_Test <- train[holdout[[1]], ]
DF_Train=train[-holdout[[1]], ]

TestNL<-DF_Test[-c(1)]
TestL<-DF_Test$label

train_naibayes<-naiveBayes(as.factor(label)~., data=DF_Train, na.action = na.pass)
train_naibayes

NaivePred <- predict(train_naibayes, TestNL)
NaivePred
proc.time() - startTime

#confusion matrix for this
(confusionMatrix(nb_Pred, DF_Test$label))

