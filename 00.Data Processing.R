library(data.table)
mydata=fread("Methylation expression.csv")
mydata=as.data.frame(mydata)

library(tidyverse)
mydata$group=ifelse(str_detect(mydata$Target,"RA"),"1","0")
mydata$group=as.factor(mydata$group)
library(randomForest)

##Fill in the missing values
mydata=mydata[,-17]
set.seed(20230522)
library(mice)
newdata<-mydata
data<-mice(newdata,m=5,method = "pmm",maxit = 100,seed=1)
summary(data)
data$imp
completedata<-complete(data)
head(completedata)
anyNA(completedata)
densityplot(data)
stripplot(data,pch=12)
mydata1=completedata

library(readr)
clinical <- read_csv("clinical.csv")
View(clinical)
colnames(clinical)[6]="disease_age"
colnames(clinical)[7]="Number_of_tenderness_joints"
colnames(clinical)[8]="Swollen_joint_number"
colnames(clinical)[1]="id"
clinical=mice(clinical,m=5,method = "pmm",maxit = 100,seed=1)
completeclinicaldata<-complete(clinical)
anyNA(completeclinicaldata)
write.csv(completeclinicaldata,file = "completeclinicaldata.csv")
write.csv(completedata,file = "completedata.csv")

rawdata=completedata[,-1]

rawdata1=rawdata[,-50]
cor_matrix <- cor(rawdata1)
high_cor <- which(abs(cor_matrix) > 0.5, arr.ind=TRUE)
rawdata_new <- rawdata1[, -unique(high_cor[high_cor[,1] != high_cor[,2], 2])]
rawdata_new
rawdata_new$group=rawdata$group

rawdata_new2=rawdata_new
rawdata_new2$RF=completeclinicaldata$RF
rawdata_new2$CCP=completeclinicaldata$CCP
rownames(rawdata_new2)=completeclinicaldata$id
library(tidyverse)
which(rownames(rawdata_new2)=="RA_1")##241
which(rownames(rawdata_new2)=="OA_12")##271
which(rownames(rawdata_new2)=="HC_10")##301
##All RA patients
methythlation_exp_RA=rawdata_new2[1:241,]
##HC/OA patients
methythlation_exp_OA=rawdata_new2[242:271,]
methythlation_exp_HC=rawdata_new2[272:301,]
##RF/CCP Double Negative patients
methythlation_exp_RA$group=ifelse(methythlation_exp_RA$RF<20 & methythlation_exp_RA$CCP<25,1,0)
methythlation_exp_RA_RF_CCP_doulenegtive=methythlation_exp_RA[methythlation_exp_RA$group=="1",]
methythlation_exp_RA_RF_CCP_doulenegtive=rbind(methythlation_exp_RA_RF_CCP_doulenegtive,methythlation_exp_OA,methythlation_exp_HC)
methythlation_exp_RA_RF_CCP_doulenegtive$group=ifelse(str_detect(rownames(methythlation_exp_RA_RF_CCP_doulenegtive),"RA"),1,0)
methythlation_exp_RA_RF_CCP_doulenegtive$group=as.factor(methythlation_exp_RA_RF_CCP_doulenegtive$group)
##RF/CCP Double positive patients
methythlation_exp_RA$group=ifelse(methythlation_exp_RA$RF>20 & methythlation_exp_RA$CCP>25,1,0)
methythlation_exp_RA_RF_CCP_doulepostive=methythlation_exp_RA[methythlation_exp_RA$group=="1",]
methythlation_exp_RA_RF_CCP_doulepostive=rbind(methythlation_exp_RA_RF_CCP_doulepostive,methythlation_exp_OA,methythlation_exp_HC)
methythlation_exp_RA_RF_CCP_doulepostive$group=ifelse(str_detect(rownames(methythlation_exp_RA_RF_CCP_doulepostive),"RA"),1,0)
methythlation_exp_RA_RF_CCP_doulepostive$group=as.factor(methythlation_exp_RA_RF_CCP_doulepostive$group)
##RF Single negative patient
methythlation_exp_RA$group=ifelse(methythlation_exp_RA$RF<20 & methythlation_exp_RA$CCP>25,1,0)
methythlation_exp_RA_RF_negtive=methythlation_exp_RA[methythlation_exp_RA$group=="1",]
methythlation_exp_RA_RF_negtive=rbind(methythlation_exp_RA_RF_negtive,methythlation_exp_OA,methythlation_exp_HC)
methythlation_exp_RA_RF_negtive$group=ifelse(str_detect(rownames(methythlation_exp_RA_RF_negtive),"RA"),1,0)
methythlation_exp_RA_RF_negtive$group=as.factor(methythlation_exp_RA_RF_negtive$group)
##CCP Single negative patient
methythlation_exp_RA$group=ifelse(methythlation_exp_RA$RF>20 & methythlation_exp_RA$CCP<25,1,0)
methythlation_exp_RA_CCP_negtive=methythlation_exp_RA[methythlation_exp_RA$group=="1",]
methythlation_exp_RA_CCP_negtive=rbind(methythlation_exp_RA_CCP_negtive,methythlation_exp_OA,methythlation_exp_HC)
methythlation_exp_RA_CCP_negtive$group=ifelse(str_detect(rownames(methythlation_exp_RA_CCP_negtive),"RA"),1,0)
methythlation_exp_RA_CCP_negtive$group=as.factor(methythlation_exp_RA_CCP_negtive$group)

save.image(file = "Remove the high-level raw data.Rdata")



