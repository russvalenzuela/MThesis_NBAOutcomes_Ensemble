library(dplyr)
library(tidyr)
library(e1071)
library(neuralnet)
library(ggplot2)
library(readxl)
library(caret)
library(zoo)
library(randomForest)

#Train-Test Split (80-20)
set.seed(2)
sample <- sample.int(n=nrow(X0809_season_OAHR),size=floor(.80*nrow(X0809_season_OAHR)),replace = F)
train <- X0809_season_OAHR[sample, ]
test <- X0809_season_OAHR[-sample, ]

#Spread Only Accuracy (for Testing Set)
test$spread_correct <- (test$HOME_WIN & test$SPREAD_HOME<0) | (!test$HOME_WIN & test$SPREAD_HOME>0)
spread_correct_rate_testX0809 <- sum(test$spread_correct)/length(test$spread_correct)

print(spread_correct_rate_testX0809*100)

#Drop Spread from Dataset
train<-select(train,-SPREAD_HOME)
test<-select(test,-SPREAD_HOME,-spread_correct)

#Fit Models
model_logistic <- glm(HOME_WIN ~ ., data=train, family=binomial (link="logit"))
model_svm <- svm(HOME_WIN ~ ., data=train, type='C')
model_nb <- naiveBayes(HOME_WIN ~ ., data=train, type='C')

train_inputs <-select(train,-HOME_WIN)
test_inputs <- select(test,-HOME_WIN)

pred_logistic <- predict(model_logistic,test_inputs,type = "response")
pred_svm <- predict(model_svm,test_inputs)
pred_nb <- predict(model_nb,test_inputs)

#Confusion Matricies
confusionMatrix(as.factor(pred_logistic>0.5),as.factor(test$HOME_WIN),positive = "TRUE")
confusionMatrix(pred_svm,as.factor(test$HOME_WIN),positive = "TRUE")
confusionMatrix(as.factor(pred_nb),as.factor(test$HOME_WIN),positive = "TRUE")