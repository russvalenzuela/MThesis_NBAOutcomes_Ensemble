library(caret)

#Train-Test Split (70-30)
set.seed(2)
sample <- sample.int(n=nrow(X0809_season_OAHR),size=floor(.70*nrow(X0809_season_OAHR)),replace = F)
train <- X0809_season_OAHR[sample, ]
test <- X0809_season_OAHR[-sample, ]

#Drop Spread from Dataset
train<-select(train,-`SPREAD_HOME`)
test<-select(test,-`SPREAD_HOME`)

train_inputs <- select(train,-HOME_WIN)
test_inputs <- select(test,-HOME_WIN)

correlationMatrix <- cor(train_inputs)

highlyCorrelated <- findCorrelation(correlationMatrix,cutoff = 0.75,names = TRUE)
print(highlyCorrelated)

train_fs <- select(train,-one_of(highlyCorrelated))
test_fs <- select(test,-one_of(highlyCorrelated))

#Fit Models
model_logistic_fs <- glm(HOME_WIN ~ ., data=train_fs, family=binomial (link="logit"))
model_svm_fs <- svm(HOME_WIN ~ ., data=train_fs, type='C')
model_nb_fs <- naiveBayes(HOME_WIN ~ ., data=train_fs, type='C')

train_inputs_fs <-select(train_fs,-HOME_WIN)
test_inputs_fs <- select(test_fs,-HOME_WIN)

pred_logistic_fs <- predict(model_logistic_fs,test_inputs_fs,type = "response")
pred_svm_fs <- predict(model_svm_fs,test_inputs_fs)
pred_nb_fs <- predict(model_nb_fs,test_inputs_fs)

#Confusion Matricies
confusionMatrix(as.factor(pred_logistic_fs>0.5),as.factor(test_fs$HOME_WIN),positive = "TRUE")
confusionMatrix(pred_svm_fs,as.factor(test_fs$HOME_WIN),positive = "TRUE")
confusionMatrix(as.factor(pred_nb_fs),as.factor(test_fs$HOME_WIN),positive = "TRUE")