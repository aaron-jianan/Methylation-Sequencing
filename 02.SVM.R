
library(e1071)
load(file = "Remove the high-level raw data.Rdata")

##ra/non_RA
set.seed(1)
index <- sample(2,301,replace = TRUE,prob=c(0.7,0.3))
traindata <- rawdata_new[index==1,]
testdata <- rawdata_new[index==2,]

library(caret)
names(getModelInfo())
fitControl <- trainControl(
  method = 'cv',                   
  number = 10,                      
  savePredictions = 'final',      
  classProbs = F,                 
  summaryFunction=defaultSummary  
) 

model_SVM = train(group ~ ., 
                 data=traindata, 
                 method="svmLinear", 
                 tuneLength=5, 
                 trControl = fitControl)
model_SVM

m_SVM <- predict(model_SVM,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_SVM)
svm_model <- svm(group~., data = traindata, kernel = "linear",type = "C-classification", cost = 1)
svm_pred <- predict(svm_model, newdata = testdata)
svm_pred

library(pROC)
library(caret)
svm_pred <- as.numeric(svm_pred)
svm_auc <- roc(testdata$group, svm_pred)
auc(svm_auc)

##Double Negative RA
set.seed(123411)
index <- sample(2,71,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulenegtive=methythlation_exp_RA_RF_CCP_doulenegtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==2,]
model_SVM = train(group ~ ., 
                  data=traindata, 
                  method="svmLinear", 
                  tuneLength=5, 
                  trControl = fitControl)
model_SVM
m_SVM <- predict(model_SVM,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_SVM)
svm_model <- svm(group~., data = traindata, kernel = "linear",type = "C-classification", cost = 1)
svm_pred <- predict(svm_model, newdata = testdata)
svm_pred
library(pROC)
library(caret)
svm_pred <- as.numeric(svm_pred)
svm_auc <- roc(testdata$group, svm_pred)
auc(svm_auc)

##Double positive RA
set.seed(1234)
index <- sample(2,238,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulepostive=methythlation_exp_RA_RF_CCP_doulepostive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulepostive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulepostive[index==2,]
model_SVM = train(group ~ ., 
                  data=traindata, 
                  method="svmLinear", 
                  tuneLength=5, 
                  trControl = fitControl)
model_SVM
m_SVM <- predict(model_SVM,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_SVM)
svm_model <- svm(group~., data = traindata, kernel = "linear",type = "C-classification", cost = 1)
svm_pred <- predict(svm_model, newdata = testdata)
svm_pred
library(pROC)
library(caret)
svm_pred <- as.numeric(svm_pred)
svm_auc <- roc(testdata$group, svm_pred)
auc(svm_auc)

##RF single negative
set.seed(1234)
index <- sample(2,84,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_negtive=methythlation_exp_RA_RF_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_negtive[index==1,]
testdata <- methythlation_exp_RA_RF_negtive[index==2,]
model_SVM = train(group ~ ., 
                  data=traindata, 
                  method="svmLinear", 
                  tuneLength=5, 
                  trControl = fitControl)
model_SVM
m_SVM <- predict(model_SVM,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_SVM)
svm_model <- svm(group~., data = traindata, kernel = "linear",type = "C-classification", cost = 1)
svm_pred <- predict(svm_model, newdata = testdata)
svm_pred
library(pROC)
library(caret)
svm_pred <- as.numeric(svm_pred)
svm_auc <- roc(testdata$group, svm_pred)
auc(svm_auc)

##CCP single negative
set.seed(1234456)
index <- sample(2,68,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_CCP_negtive=methythlation_exp_RA_CCP_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_CCP_negtive[index==1,]
testdata <- methythlation_exp_RA_CCP_negtive[index==2,]

model_SVM = train(group ~ ., 
                  data=traindata, 
                  method="svmLinear", 
                  tuneLength=5, 
                  trControl = fitControl)
model_SVM
m_SVM <- predict(model_SVM,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_SVM)
svm_model <- svm(group~., data = traindata, kernel = "linear",type = "C-classification", cost = 1)
svm_pred <- predict(svm_model, newdata = testdata)
svm_pred
library(pROC)
library(caret)
svm_pred <- as.numeric(svm_pred)
svm_auc <- roc(testdata$group, svm_pred)
auc(svm_auc)


