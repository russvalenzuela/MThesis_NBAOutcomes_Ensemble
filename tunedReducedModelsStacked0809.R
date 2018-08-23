train_stack <- select(train,HOME_WIN)
test_stack <- select(test,HOME_WIN)

#Get CV Predicted OOF Probs####
train_stack$pred_logistic_prob <- model_logit_reduced$pred$WIN[order(model_logit_reduced$pred$rowIndex)]
train_stack$pred_svm_prob <- model_svm_tune_reduced$pred$WIN[order(model_svm_tune_reduced$pred$rowIndex)]
train_stack$pred_nb_prob <- model_nb_tune_reduced$pred$WIN[order(model_nb_tune_reduced$pred$rowIndex)]
train_stack$pred_nn_prob <- model_nn_tune_reduced$pred$WIN[order(model_nn_tune_reduced$pred$rowIndex)]
train_stack$pred_rf_prob <- model_rf_tune_reduced$pred$WIN[order(model_rf_tune_reduced$pred$rowIndex)]

#Get Predicted Test Probs####
test_stack$pred_logistic_prob <- predict.train(model_logit_reduced,test_inputs_reduced_logit,type="prob")["WIN"]
test_stack$pred_svm_prob <- predict.train(model_svm_tune_reduced,test_inputs_reduced_svm,type="prob")["WIN"]
test_stack$pred_nb_prob <- predict.train(model_nb_tune_reduced,test_inputs_reduced_nb,type="prob")["WIN"]
test_stack$pred_nn_prob <- predict.train(model_nn_tune_reduced,test_inputs_reduced_nn,type="prob")["WIN"]
test_stack$pred_rf_prob <- predict.train(model_rf_tune_reduced,test_inputs_reduced_rf,type="prob")["WIN"]


#Check Correlation of Predicted Probs####
model_pred_probs <- select(train_stack,-HOME_WIN)
cor(model_pred_probs)

#Fit Stacked Model - Logistic w/ all models#####
model_stacked_all <- train(HOME_WIN~.,data=train_stack, method="glm", family="binomial",
                       metric="Accuracy", trControl=ctrl)
model_stacked_all

#Fit Stacked Model - Logistic w/ Logistic+NB#####
model_stacked_logitNB <- train(HOME_WIN~pred_logistic_prob+pred_nb_prob,data=train_stack,
                               method="glm", family="binomial",
                               metric="Accuracy", trControl=ctrl)
model_stacked_logitNB

#Fit Stacked Model - Logistic w/ Logistic+RF#####
model_stacked_logitRF <- train(HOME_WIN~pred_logistic_prob+pred_rf_prob,data=train_stack,
                               method="glm", family="binomial",
                               metric="Accuracy", trControl=ctrl)
model_stacked_logitRF

#Fit Stacked Model - Logistic w/ NB+NN#####
model_stacked_NBNN <- train(HOME_WIN~pred_nb_prob+pred_nn_prob,data=train_stack,
                               method="glm", family="binomial",
                               metric="Accuracy", trControl=ctrl)
model_stacked_NBNN

#Fit Stacked Model - Logistic w/ NN+RF#####
model_stacked_NNRF <- train(HOME_WIN~pred_nn_prob+pred_rf_prob,data=train_stack,
                               method="glm", family="binomial",
                               metric="Accuracy", trControl=ctrl)
model_stacked_NNRF

#Fit Stacked Model - Logistic w/ NB+RF#####
model_stacked_NBRF <- train(HOME_WIN~pred_nb_prob+pred_rf_prob,data=train_stack,
                            method="glm", family="binomial",
                            metric="Accuracy", trControl=ctrl)
model_stacked_NBRF

#Fit Stacked Model - Logistic w/ Logistic+NB+RF#####
model_stacked_LogisticNBRF <- train(HOME_WIN~pred_logistic_prob+pred_nb_prob+pred_rf_prob,data=train_stack,
                            method="glm", family="binomial",
                            metric="Accuracy", trControl=ctrl)
model_stacked_LogisticNBRF

#Fit Stacked Model - Logistic w/ NB+NN+RF#####
model_stacked_NBNNRF <- train(HOME_WIN~pred_nb_prob+pred_nn_prob+pred_rf_prob,data=train_stack,
                                    method="glm", family="binomial",
                                    metric="Accuracy", trControl=ctrl)
model_stacked_NBNNRF

#Check Correlation of Predicted Probs####
cor(model_pred_probs)


#Predict Stacked Models on Test Set####
test_inputs_stack <- select(test_stack,-HOME_WIN)
test_inputs_stack$pred_logistic_prob <- unlist(test_inputs_stack$pred_logistic_prob)
test_inputs_stack$pred_svm_prob <- unlist(test_inputs_stack$pred_svm_prob)
test_inputs_stack$pred_nb_prob <- unlist(test_inputs_stack$pred_nb_prob)
test_inputs_stack$pred_nn_prob <- unlist(test_inputs_stack$pred_nn_prob)
test_inputs_stack$pred_rf_prob <- unlist(test_inputs_stack$pred_rf_prob)

#All
pred_stack_test_all <- predict.train(model_stacked_all,test_inputs_stack,type="raw")

confusionMatrix(pred_stack_test_all,test_stack$HOME_WIN)


#Logistic+RF
test_inputs_stack_logitRF <- select(test_inputs_stack,pred_logistic_prob,
                                         pred_rf_prob)

pred_stack_test_logitRF <- predict.train(model_stacked_logitRF,test_inputs_stack_logitRF,type="raw")

confusionMatrix(pred_stack_test_logitRF,test_stack$HOME_WIN)