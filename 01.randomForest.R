load(file = "Remove the high-level raw data.Rdata")
library(randomForest)

##ra/non_RA
set.seed(1)
index <- sample(2,301,replace = TRUE,prob=c(0.7,0.3))
traindata <- rawdata_new[index==1,]
testdata <- rawdata_new[index==2,]

library(caret)
# Define the training control 
fitControl <- trainControl(
  method = 'cv',                   
  number = 10,                      
  savePredictions = 'final',       
  classProbs = F,                  
  summaryFunction=defaultSummary  
) 

model_rf = train(group ~ ., 
                 data=traindata, 
                 method='rf', 
                 tuneLength=5, 
                 trControl = fitControl)
model_rf
#
#mtry  Accuracy   Kappa       
#9  

predictions <- model_rf$pred
cm <- confusionMatrix(predictions$pred, predictions$obs)
print(cm)
library(pROC)
train_prob <- predict(model_rf, type = "prob")[, 2]
train_labels <- traindata$group
train_auc <- roc(train_labels, train_prob)$auc
print(paste("Train AUC:", train_auc))

test_prob <- predict(model_rf, newdata = testdata, type = "prob")[, 2]
test_labels <- testdata$group
test_auc <- roc(test_labels, test_prob)$auc
print(paste("Test AUC:", test_auc))
m_rf <- predict(model_rf,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_rf)
##Important variables
rf_ntree <- randomForest(group~.,data=rawdata_new)
plot(rf_ntree)
randomForest_results <- randomForest(group~.,data=traindata,mtry=9,
                                     ntree=100, proximity=TRUE)
importance(randomForest_results)
varImpPlot(randomForest_results)

##Double Negative RA
set.seed(1234)
index <- sample(2,71,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulenegtive=methythlation_exp_RA_RF_CCP_doulenegtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==2,]

library(caret)
# Train the model using rf
model_rf = train(group ~ ., 
                 data=traindata, 
                 method='rf', 
                 tuneLength=5, 
                 trControl = fitControl)
model_rf
#
#mtry  Accuracy   Kappa       
#17    0.9500000  0.78

predictions <- model_rf$pred
cm <- confusionMatrix(predictions$pred, predictions$obs)
print(cm)
library(pROC)
train_prob <- predict(model_rf, type = "prob")[, 2]
train_labels <- traindata$group
train_auc <- roc(train_labels, train_prob)$auc
print(paste("Train AUC:", train_auc))

test_prob <- predict(model_rf, newdata = methythlation_exp_RA_RF_CCP_doulenegtive, type = "prob")[, 2]
test_labels <- methythlation_exp_RA_RF_CCP_doulenegtive$group
test_auc <- roc(test_labels, test_prob)$auc
print(paste("Test AUC:", test_auc))
m_rf <- predict(model_rf,newdata = methythlation_exp_RA_RF_CCP_doulenegtive)
confusionMatrix(reference=methythlation_exp_RA_RF_CCP_doulenegtive$group,data=m_rf)
##Important variables
rf_ntree <- randomForest(group~.,data=methythlation_exp_RA_RF_CCP_doulenegtive)
plot(rf_ntree)
randomForest_results <- randomForest(group~.,data=traindata,mtry=17,
                                     ntree=100, proximity=TRUE)
importance(randomForest_results)
varImpPlot(randomForest_results)

##Double positive RA
set.seed(1234)
index <- sample(2,238,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulepostive=methythlation_exp_RA_RF_CCP_doulepostive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulepostive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulepostive[index==2,]

library(caret)
# Train the model using rf
model_rf = train(group ~ ., 
                 data=traindata, 
                 method='rf', 
                 tuneLength=5, 
                 trControl = fitControl)
model_rf
#
#mtry  Accuracy   Kappa       
#9    0.7843137   0.11886157
predictions <- model_rf$pred
cm <- confusionMatrix(predictions$pred, predictions$obs)
print(cm)
library(pROC)
train_prob <- predict(model_rf, type = "prob")[, 2]
train_labels <- traindata$group
train_auc <- roc(train_labels, train_prob)$auc
print(paste("Train AUC:", train_auc))

test_prob <- predict(model_rf, newdata = testdata, type = "prob")[, 2]
test_labels <- testdata$group
test_auc <- roc(test_labels, test_prob)$auc
print(paste("Test AUC:", test_auc))

m_rf <- predict(model_rf,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_rf)
##Important variables
rf_ntree <- randomForest(group~.,data=methythlation_exp_RA_RF_CCP_doulepostive)
plot(rf_ntree)
randomForest_results <- randomForest(group~.,data=traindata,mtry=9,
                                     ntree=100, proximity=TRUE)
importance(randomForest_results)
varImpPlot(randomForest_results)




##RF single negative
set.seed(1234)
index <- sample(2,84,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_negtive=methythlation_exp_RA_RF_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_negtive[index==1,]
testdata <- methythlation_exp_RA_RF_negtive[index==2,]

library(caret)
# Train the model using rf
model_rf = train(group ~ ., 
                 data=traindata, 
                 method='rf', 
                 tuneLength=5, 
                 trControl = fitControl)
model_rf
#
#mtry  Accuracy   Kappa       
#25    0.7583333  0.3469471

predictions <- model_rf$pred
cm <- confusionMatrix(predictions$pred, predictions$obs)
print(cm)
library(pROC)
train_prob <- predict(model_rf, type = "prob")[, 2]
train_labels <- traindata$group
train_auc <- roc(train_labels, train_prob)$auc
print(paste("Train AUC:", train_auc))

test_prob <- predict(model_rf, newdata = testdata, type = "prob")[, 2]
test_labels <- testdata$group
test_auc <- roc(test_labels, test_prob)$auc
print(paste("Test AUC:", test_auc))
m_rf <- predict(model_rf,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_rf)
##Important variables
rf_ntree <- randomForest(group~.,data=methythlation_exp_RA_RF_negtive)
plot(rf_ntree)
randomForest_results <- randomForest(group~.,data=traindata,mtry=25,
                                     ntree=100, proximity=TRUE)
importance(randomForest_results)
varImpPlot(randomForest_results)


##CCP single negative
set.seed(1234)
index <- sample(2,68,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_CCP_negtive=methythlation_exp_RA_CCP_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_CCP_negtive[index==1,]
testdata <- methythlation_exp_RA_CCP_negtive[index==2,]

library(caret)
# Train the model using rf
model_rf = train(group ~ ., 
                 data=traindata, 
                 method='rf', 
                 tuneLength=5, 
                 trControl = fitControl)
model_rf
#
#mtry  Accuracy   Kappa       
#2   

predictions <- model_rf$pred
cm <- confusionMatrix(predictions$pred, predictions$obs)
print(cm)
library(pROC)
train_prob <- predict(model_rf, type = "prob")[, 2]
train_labels <- traindata$group
train_auc <- roc(train_labels, train_prob)$auc
print(paste("Train AUC:", train_auc))

test_prob <- predict(model_rf, newdata = testdata, type = "prob")[, 2]
test_labels <- testdata$group
test_auc <- roc(test_labels, test_prob)$auc
print(paste("Test AUC:", test_auc))
m_rf <- predict(model_rf,newdata = methythlation_exp_RA_CCP_negtive)
confusionMatrix(reference=methythlation_exp_RA_CCP_negtive$group,data=m_rf)
##Important variables
rf_ntree <- randomForest(group~.,data=methythlation_exp_RA_CCP_negtive)
plot(rf_ntree)
randomForest_results <- randomForest(group~.,data=traindata,mtry=2,
                                     ntree=100, proximity=TRUE)
importance(randomForest_results)
varImpPlot(randomForest_results)




