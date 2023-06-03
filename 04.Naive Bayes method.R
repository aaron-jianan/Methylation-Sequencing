
library(naivebayes)
library(caret)

load(file = "Remove the high-level raw data.Rdata")
##ra/non_RA

set.seed(1)
index <- sample(2,301,replace = TRUE,prob=c(0.7,0.3))
traindata <- rawdata_new[index==1,]
testdata <- rawdata_new[index==2,]

nb_model <- naive_bayes(group~., data = traindata)
test_pred <- predict(nb_model, newdata = testdata)
confusionMatrix(test_pred, testdata$group)

library(ROCR)
library(gplots)
test_predict=predict(nb_model,newdata = testdata,type = "prob")
predi <- prediction(test_predict[,2], testdata$group)
auc <- performance(predi, "auc")
slotNames(auc)
auc = auc@y.values[[1]]
auc = round(as.numeric(auc),3)
auc 

##Double Negative RA

set.seed(123444)
index <- sample(2,71,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulenegtive=methythlation_exp_RA_RF_CCP_doulenegtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==2,]

nb_model <- naive_bayes(group~., data = traindata)
test_pred <- predict(nb_model, newdata = testdata)
confusionMatrix(test_pred, testdata$group)

library(ROCR)
library(gplots)
test_predict=predict(nb_model,newdata = testdata,type = "prob")
predi <- prediction(test_predict[,2], testdata$group)
auc <- performance(predi, "auc")
slotNames(auc)
auc = auc@y.values[[1]]
auc = round(as.numeric(auc),3)
auc 

##Double positive RA

set.seed(1234)
index <- sample(2,238,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulepostive=methythlation_exp_RA_RF_CCP_doulepostive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulepostive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulepostive[index==2,]

nb_model <- naive_bayes(group~., data = traindata)
test_pred <- predict(nb_model, newdata = testdata)
confusionMatrix(test_pred, testdata$group)

library(ROCR)
library(gplots)
test_predict=predict(nb_model,newdata = testdata,type = "prob")
predi <- prediction(test_predict[,2], testdata$group)
auc <- performance(predi, "auc")
slotNames(auc)
auc = auc@y.values[[1]]
auc = round(as.numeric(auc),3)
auc 

##RF single negative
set.seed(12)
index <- sample(2,84,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_negtive=methythlation_exp_RA_RF_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_negtive[index==1,]
testdata <- methythlation_exp_RA_RF_negtive[index==2,]

nb_model <- naive_bayes(group~., data = traindata)
test_pred <- predict(nb_model, newdata = testdata)
confusionMatrix(test_pred, testdata$group)
#AUC
library(ROCR)
library(gplots)
test_predict=predict(nb_model,newdata = testdata,type = "prob")
predi <- prediction(test_predict[,2], testdata$group)
auc <- performance(predi, "auc")
slotNames(auc)
auc = auc@y.values[[1]]
auc = round(as.numeric(auc),3)
auc 

##CCP single negative

set.seed(12344)
index <- sample(2,68,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_CCP_negtive=methythlation_exp_RA_CCP_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_CCP_negtive[index==1,]
testdata <- methythlation_exp_RA_CCP_negtive[index==2,]

nb_model <- naive_bayes(group~., data = traindata)
test_pred <- predict(nb_model, newdata = testdata)
confusionMatrix(test_pred, testdata$group)

library(ROCR)
library(gplots)
test_predict=predict(nb_model,newdata = testdata,type = "prob")
predi <- prediction(test_predict[,2], testdata$group)
auc <- performance(predi, "auc")
slotNames(auc)
auc = auc@y.values[[1]]
auc = round(as.numeric(auc),3)
auc 



