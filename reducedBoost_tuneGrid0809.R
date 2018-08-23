#Training Control
ctrl <- trainControl(method = "cv",number = 10,savePredictions = 'final',classProbs = T)

#Get Reduced Inputs####
#Logistic#
varImp_logit <- data.frame(varImp_logit$importance)
reduced_inputs_logistic <- rownames(varImp_logit)[order(varImp_logit$Overall,decreasing = TRUE)[1:20]]

train_reduced_logistic <- select(train, HOME_WIN, one_of(reduced_inputs_logistic))

#SVM#
varImp_svm <- data.frame(varImp_svm$importance)
reduced_inputs_svm <- rownames(varImp_svm)[order(varImp_svm$WIN,decreasing = TRUE)[1:20]]

train_reduced_svm <- select(train, HOME_WIN, one_of(reduced_inputs_svm))

#NB#
varImp_nb <- data.frame(varImp_nb$importance)
reduced_inputs_nb <- rownames(varImp_nb)[order(varImp_nb$WIN,decreasing = TRUE)[1:20]]

train_reduced_nb <- select(train, HOME_WIN, one_of(reduced_inputs_nb))

#NN#
varImp_nn <- data.frame(varImp_nn$importance)
reduced_inputs_nn <- rownames(varImp_nn)[order(varImp_nn$Overall,decreasing = TRUE)[1:20]]

train_reduced_nn <- select(train, HOME_WIN, one_of(reduced_inputs_nn))

#RF#
varImp_rf <- data.frame(varImp_rf$importance)
reduced_inputs_rf <- rownames(varImp_rf)[order(varImp_rf$Overall,decreasing = TRUE)[1:20]]

train_reduced_rf <- select(train, HOME_WIN, one_of(reduced_inputs_rf))

#Set up Tune Grid####
tune_grid_boost <- expand.grid(maxdepth=c(1),iter=c(seq(100,1300,100)),nu=c(seq(0.1,0.5,0.1)))

#Implement Boosting####
set.seed(2)
model_boost <- train(HOME_WIN~.,data=train_reduced_logistic,method="ada",metric="Accuracy",
                     trControl=ctrl, tuneGrid=tune_grid_boost)

model_boost
plot(model_boost)

#Predict on Test Set####
test_inputs_boost <- select(test,one_of(reduced_inputs_logistic))
pred_boost_test <- predict.train(model_boost,test_inputs_boost,type="raw")

#Confusion Matrix####
confusionMatrix(pred_boost_test,test$HOME_WIN)