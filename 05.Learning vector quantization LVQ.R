library(caret)
load(file = "Remove the high-level raw data.Rdata")
##ra/non_RA
set.seed(123)
index <- sample(2,301,replace = TRUE,prob=c(0.7,0.3))
traindata <- rawdata_new[index==1,]
testdata <- rawdata_new[index==2,]

# Define the training control 
fitControl <- trainControl(
  method = 'cv',                   
  number = 10,                      
  savePredictions = 'final',      
  classProbs = F,                  
  summaryFunction=defaultSummary  
) 

model_lvq = train(group ~ ., 
                  data=traindata, 
                  method="lvq", 
                  tuneLength=5, 
                  trControl = fitControl)

m_lvq <- predict(model_lvq,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_lvq)

##Double Negative RA
set.seed(1234)
index <- sample(2,71,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulenegtive=methythlation_exp_RA_RF_CCP_doulenegtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==2,]
model_lvq = train(group ~ ., 
                  data=traindata, 
                  method="lvq", 
                  tuneLength=5, 
                  trControl = fitControl)

m_lvq <- predict(model_lvq,newdata = methythlation_exp_RA_RF_CCP_doulenegtive)
confusionMatrix(reference=methythlation_exp_RA_RF_CCP_doulenegtive$group,data=m_lvq)

##Double positive RA

set.seed(1111)
index <- sample(2,238,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulepostive=methythlation_exp_RA_RF_CCP_doulepostive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulepostive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulepostive[index==2,]
model_lvq = train(group ~ ., 
                  data=traindata, 
                  method="lvq", 
                  tuneLength=5, 
                  trControl = fitControl)

m_lvq <- predict(model_lvq,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_lvq)


##RF single negative

set.seed(12)
index <- sample(2,84,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_negtive=methythlation_exp_RA_RF_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_negtive[index==1,]
testdata <- methythlation_exp_RA_RF_negtive[index==2,]
model_lvq = train(group ~ ., 
                  data=traindata, 
                  method="lvq", 
                  tuneLength=5, 
                  trControl = fitControl)

m_lvq <- predict(model_lvq,newdata = testdata)
confusionMatrix(reference=testdata$group,data=m_lvq)

##CCP single negative

set.seed(123456)
index <- sample(2,68,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_CCP_negtive=methythlation_exp_RA_CCP_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_CCP_negtive[index==1,]
testdata <- methythlation_exp_RA_CCP_negtive[index==2,]
model_lvq = train(group ~ ., 
                  data=traindata, 
                  method="lvq", 
                  tuneLength=5, 
                  trControl = fitControl)
m_lvq <- predict(model_lvq,newdata = methythlation_exp_RA_CCP_negtive)
confusionMatrix(reference=methythlation_exp_RA_CCP_negtive$group,data=m_lvq)



save.image(file ="20230525学习矢量量化LVQ.Rdata")


