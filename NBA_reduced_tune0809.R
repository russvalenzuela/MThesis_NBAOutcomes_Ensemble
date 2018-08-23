  #Training Control
  ctrl <- trainControl(method = "cv",number = 10,savePredictions = 'final',classProbs = T)
  
  #Get Reduced Inputs####
  #Logistic####
  varImp_logit <- data.frame(varImp_logit$importance)
  reduced_inputs_logistic <- rownames(varImp_logit)[order(varImp_logit$Overall,decreasing = TRUE)[1:20]]
  
  train_reduced_logistic <- select(train, HOME_WIN, one_of(reduced_inputs_logistic))
  
  #SVM####
  varImp_svm <- data.frame(varImp_svm$importance)
  reduced_inputs_svm <- rownames(varImp_svm)[order(varImp_svm$WIN,decreasing = TRUE)[1:20]]
  
  train_reduced_svm <- select(train, HOME_WIN, one_of(reduced_inputs_svm))
  
  #NB####
  varImp_nb <- data.frame(varImp_nb$importance)
  reduced_inputs_nb <- rownames(varImp_nb)[order(varImp_nb$WIN,decreasing = TRUE)[1:20]]
  
  train_reduced_nb <- select(train, HOME_WIN, one_of(reduced_inputs_nb))
  
  #NN####
  varImp_nn <- data.frame(varImp_nn$importance)
  reduced_inputs_nn <- rownames(varImp_nn)[order(varImp_nn$Overall,decreasing = TRUE)[1:20]]
  
  train_reduced_nn <- select(train, HOME_WIN, one_of(reduced_inputs_nn))
  
  #RF####
  varImp_rf <- data.frame(varImp_rf$importance)
  reduced_inputs_rf <- rownames(varImp_rf)[order(varImp_rf$Overall,decreasing = TRUE)[1:20]]
  
  train_reduced_rf <- select(train, HOME_WIN, one_of(reduced_inputs_rf))
  
  
  #Set up Tuning Grids####
  grid_svm <- expand.grid(sigma=c(2^(-7:0)), C=c(seq(0,4.0,.25)))
  grid_nb <- expand.grid(usekernel=c(TRUE,FALSE),laplace=c(0,0.5,1),adjust=c(0,0.5,1))
  grid_nn <- expand.grid(size=c(1:15),decay=c(seq(0.1,1.1,0.1)))
  grid_rf <- expand.grid(mtry=c(1:20))
  
  #Tune Models####
  set.seed(2)
  model_logit_reduced <- train(HOME_WIN~.,data=train_reduced_logistic,method="glm",family="binomial",
                               metric="Accuracy",trControl=ctrl)
  
  set.seed(2)
  model_svm_tune_reduced <- train(HOME_WIN~.,data=train_reduced_svm,method="svmRadial",metric="Accuracy",
                          trControl=ctrl, tuneGrid=grid_svm)
  set.seed(2)
  model_nb_tune_reduced <- train(HOME_WIN~.,data=train_reduced_nb,method="naive_bayes",metric="Accuracy",
                            trControl=ctrl, tuneGrid=grid_nb)
  set.seed(2)
  model_nn_tune_reduced <- train(HOME_WIN~.,data=train_reduced_nn,method="nnet",preProcess="range",metric="Accuracy",
                         trControl=ctrl, tuneGrid=grid_nn, maxit=1500)
  set.seed(2)
  model_rf_tune_reduced <- train(HOME_WIN~.,data=train_reduced_rf,method="rf",metric="Accuracy",
                         trControl=ctrl, tuneGrid=grid_rf)
  
  ##Check Correlation Matrix of Accuracy####
  results_tune_reduced <- resamples(list(Logistic=model_logit_reduced, SVM=model_svm_tune_reduced,
                                 NB=model_nb_tune_reduced,NN=model_nn_tune_reduced,
                                 RF=model_rf_tune_reduced))
  modelCor(results_tune_reduced)
  
  #Accuracies for k-folds####
  summary(results_tune_reduced)
  
  #Predict on Test Set - TUNE####
  test_inputs_reduced_logit <- select(test,one_of(reduced_inputs_logistic))
  test_inputs_reduced_svm <- select(test,one_of(reduced_inputs_svm))
  test_inputs_reduced_nb <- select(test,one_of(reduced_inputs_nb))
  test_inputs_reduced_nn <- select(test,one_of(reduced_inputs_nn))
  test_inputs_reduced_rf <- select(test,one_of(reduced_inputs_rf))
  
  pred_logit_test_red <- predict.train(model_logit_reduced,test_inputs_reduced_logit,type="raw")
  pred_svm_tune_test_red <- predict.train(model_svm_tune_reduced,test_inputs_reduced_svm,type="raw")
  pred_nb_tune_test_red <- predict.train(model_nb_tune_reduced,test_inputs_reduced_nb,type="raw")
  pred_nn_tune_test_red <- predict.train(model_nn_tune_reduced,test_inputs_reduced_nn,type="raw")
  pred_rf_tune_test_red <- predict.train(model_rf_tune_reduced,test_inputs_reduced_rf,type="raw")
  
  #Confusion Matricies - TUNE####
  confusionMatrix(pred_logit_test_red,test$HOME_WIN)
  confusionMatrix(pred_svm_tune_test_red,test$HOME_WIN)
  confusionMatrix(pred_nb_tune_test_red,test$HOME_WIN)
  confusionMatrix(pred_nn_tune_test_red,test$HOME_WIN)
  confusionMatrix(pred_rf_tune_test_red,test$HOME_WIN)