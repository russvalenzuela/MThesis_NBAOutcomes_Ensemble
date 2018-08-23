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

#Logistic - PCA####
ctrl_pca <- trainControl(method = "cv",number = 10,savePredictions = 'final',classProbs = T,
                         preProcOptions = list(pcaComp=20))
set.seed(2)
model_logitPCA <- train(HOME_WIN~.,data=train,method="glm",family="binomial",preProcess="pca",
                        metric="Accuracy",trControl=ctrl_pca)
model_logitPCA

pred_logitPCA_test <- predict.train(model_logitPCA,test,type="raw")
confusionMatrix(pred_logitPCA_test,test$HOME_WIN)

#SVM - Linear####
ctrl <- trainControl(method = "cv",number = 10,savePredictions = 'final',classProbs = T)

set.seed(2)
model_svm_linear <- train(HOME_WIN~.,data=train,method="svmLinear",metric="Accuracy",
                          trControl=ctrl)

pred_svm_linear_test <- predict.train(model_svm_linear,test_inputs,type="raw")
confusionMatrix(pred_svm_linear_test,test$HOME_WIN)

model_svm_linear