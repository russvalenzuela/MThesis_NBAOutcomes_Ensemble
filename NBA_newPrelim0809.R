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

#Training Control
ctrl <- trainControl(method = "cv",number = 10,savePredictions = 'final',classProbs = T)

#Training Models - BASELINE
set.seed(2)
model_logit <- train(HOME_WIN~.,data=train,method="glm",family="binomial",metric="Accuracy",
                     trControl=ctrl)
set.seed(2)
model_svm <- train(HOME_WIN~.,data=train,method="svmRadial",metric="Accuracy",
                   trControl=ctrl)
set.seed(2)
model_nb <- train(HOME_WIN~.,data=train,method="naive_bayes",metric="Accuracy",
                  trControl=ctrl)
set.seed(2)
model_nn <- train(HOME_WIN~.,data=train,method="nnet",preProcess="range",metric="Accuracy",
                  trControl=ctrl, maxit=1500)
set.seed(2)
model_rf <- train(HOME_WIN~.,data=train,method="rf",metric="Accuracy",
                  trControl=ctrl)

#Check Correlation Matrix of Accuracy
results <- resamples(list(Logistic=model_logit, SVM=model_svm, NB=model_nb,NN=model_nn,RF=model_rf))
modelCor(results)

#Accuracies for k-folds 
summary(results)

#Predict on Test Set - BASELINE
pred_logit_test <- predict.train(model_logit,test_inputs,type="raw")
pred_svm_test <- predict.train(model_svm,test_inputs,type="raw")
pred_nb_test <- predict.train(model_nb,test_inputs,type="raw")
pred_nn_test <- predict.train(model_nn,test_inputs,type="raw")
pred_rf_test <- predict.train(model_rf,test_inputs,type="raw")

#Confusion Matricies - BASELINE
confusionMatrix(pred_logit_test,test$HOME_WIN)
confusionMatrix(pred_svm_test,test$HOME_WIN)
confusionMatrix(pred_nb_test,test$HOME_WIN)
confusionMatrix(pred_nn_test,test$HOME_WIN)
confusionMatrix(pred_rf_test,test$HOME_WIN)

#Finding Important Variables for each Model
varImp_logit <- varImp(model_logit)
varImp_svm <- varImp(model_svm)
varImp_nb <- varImp(model_nb)
varImp_nn <- varImp(model_nn)
varImp_rf <- varImp(model_rf)

#Print varImps
varImp_logit
varImp_svm
varImp_nb
varImp_nn
varImp_rf

#Plot VarImp
plot(varImp_logit, top=20)
plot(varImp_svm, top=20)
plot(varImp_nb, top=20)
plot(varImp_nn, top=20)
plot(varImp_rf, top=20)
