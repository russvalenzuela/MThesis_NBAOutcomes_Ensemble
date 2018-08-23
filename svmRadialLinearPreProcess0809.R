library(caret)
library(caTools)
library(dplyr)

X0809_season_OAHR <- data.frame(lapply(X0809_season_OAHR, as.numeric))
X0809_season_OAHR <- select(X0809_season_OAHR,-SPREAD_HOME)

# Making dependent variable factor and label values
X0809_season_OAHR$HOME_WIN <- as.factor(X0809_season_OAHR$HOME_WIN)
X0809_season_OAHR$HOME_WIN <- factor(X0809_season_OAHR$HOME_WIN,
                                     levels = c(0,1),
                                     labels = c("LOSS", "WIN"))

#Train-Test Split (80-20)
set.seed(2)
sample <- sample.int(n=nrow(X0809_season_OAHR),size=floor(.80*nrow(X0809_season_OAHR)),replace = F)
train <- X0809_season_OAHR[sample, ]
test <- X0809_season_OAHR[-sample, ]

train_inputs <-select(train,-HOME_WIN)
test_inputs <- select(test,-HOME_WIN)

ctrl <- trainControl(method = "cv",number = 10,savePredictions = 'final',classProbs = T)

#SVM - Radial####

set.seed(2)
model_svm_radial <- train(HOME_WIN~.,data=train,method="svmRadial",preProcess="range",
                          metric="Accuracy",trControl=ctrl)

pred_svm_radial_test <- predict.train(model_svm_radial,test_inputs,type="raw")
confusionMatrix(pred_svm_radial_test,test$HOME_WIN)

model_svm_radial

#SVM - Linear####

#Prelim
set.seed(2)
model_svm_linear <- train(HOME_WIN~.,data=train,method="svmLinear",preProcess="range",
                          metric="Accuracy",trControl=ctrl)

#Prelim Model
model_svm_linear

pred_svm_linear_test <- predict.train(model_svm_linear,test_inputs,type="raw")
confusionMatrix(pred_svm_linear_test,test$HOME_WIN)


#Top 20 Variables
varImp_svmLinear <- varImp(model_svm_linear)

varImp_svmLinear <- data.frame(varImp_svmLinear$importance)
reduced_inputs_svmLinear <- rownames(varImp_svmLinear)[order(varImp_svmLinear$WIN,decreasing = TRUE)[1:20]]

train_reduced_svmLinear <- select(train, HOME_WIN, one_of(reduced_inputs_svmLinear))

#Tune Grid
grid_svm_linear <- expand.grid(C=c(seq(0,4.0,.25)))

#Tune Model
set.seed(2)
model_svm_linear_tune <- train(HOME_WIN~.,data=train_reduced_svmLinear,method="svmLinear",preProcess="range",
                               metric="Accuracy",trControl=ctrl, tuneGrid=grid_svm_linear)

#tuned Model
model_svm_linear_tune

test_inputs_reduced_svmLinear <- select(test,one_of(reduced_inputs_svmLinear))
pred_svm_linear_tune_test <- predict.train(model_svm_linear_tune,test_inputs_reduced_svmLinear,type="raw")
confusionMatrix(pred_svm_linear_tune_test,test$HOME_WIN)