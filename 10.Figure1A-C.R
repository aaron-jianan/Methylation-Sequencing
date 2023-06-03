load(file = "Remove the high-level raw data.Rdata")

exp0=rawdata_new
exp0=cbind(rawdata_new,completeclinicaldata)
rownames(exp0)=exp0$id
str(exp0)
colnames(exp0)
exp0=exp0[,-c(34:36,51)]
colnames(exp0)

as_numeric_data_frame<- function(.dta,colname=''){
  if (length(colname)==0) colname=names(.dta)
  if (length(colname) != sum(colname %in% names(.dta))) stop('Col Name not match');
  .dta[,colname] <-  lapply( .dta[,colname], as.numeric)
  .dta
}
##
exp0=as_numeric_data_frame(.dta=exp0,colname=colnames(exp0))
colnames(exp0)
exp_HC_OA=exp0[242:301,]
exp_RA=exp0[1:241,]

median(exp_RA$stage)#10
median(exp_RA$disease_age)#47
median(exp_RA$Number_of_tenderness_joints)#1
mean(exp_RA$Swollen_joint_number)#1.53527

library(tidyverse)

exp_RA$stage=ifelse(exp_RA$stage>10,1,0)
exp_RA_stage_H=exp_RA[exp_RA$stage==1,]
exp_RA_stage_L=exp_RA[!exp_RA$stage==1,]
exp_RA_stage_L$stage=1

exp_RA$disease_age=ifelse(exp_RA$disease_age>47,1,0)
exp_RA_disease_age_H=exp_RA[exp_RA$disease_age==1,]
exp_RA_disease_age_L=exp_RA[!exp_RA$disease_age==1,]
exp_RA_disease_age_L$disease_age=1


exp_RA$Number_of_tenderness_joints=ifelse(exp_RA$Number_of_tenderness_joints>2,1,0)
exp_RA_Number_of_tenderness_joints_H=exp_RA[exp_RA$Number_of_tenderness_joints==1,]
exp_RA_Number_of_tenderness_joints_L=exp_RA[!exp_RA$Number_of_tenderness_joints==1,]
exp_RA_Number_of_tenderness_joints_L$Number_of_tenderness_joints=1

exp_RA$Swollen_joint_number=ifelse(exp_RA$Swollen_joint_number>1.754153,1,0)
exp_RA_Swollen_joint_number_H=exp_RA[exp_RA$Swollen_joint_number==1,]
exp_RA_Swollen_joint_number_L=exp_RA[!exp_RA$Swollen_joint_number==1,]
exp_RA_Swollen_joint_number_L$Swollen_joint_number=1


exp_RA$ESR=ifelse(exp_RA$ESR>20,1,0)
exp_RA_ESR_H=exp_RA[exp_RA$ESR==1,]
exp_RA_ESR_L=exp_RA[!exp_RA$ESR==1,]
exp_RA_ESR_L$ESR=1

exp_RA$CRP=ifelse(exp_RA$CRP>10,1,0)
exp_RA_CRP_H=exp_RA[exp_RA$CRP==1,]
exp_RA_CRP_L=exp_RA[!exp_RA$CRP==1,]
exp_RA_CRP_L$CRP=1

exp_RA$VAS=ifelse(exp_RA$VAS>4,1,0)
exp_RA_VAS_H=exp_RA[exp_RA$VAS==1,]
exp_RA_VAS_L=exp_RA[!exp_RA$VAS==1,]
exp_RA_VAS_L$VAS=1

exp_RA$DAS28ESR=ifelse(exp_RA$DAS28ESR>3.2,1,0)
exp_RA_DAS28ESR_H=exp_RA[exp_RA$DAS28ESR==1,]
exp_RA_DAS28ESR_L=exp_RA[!exp_RA$DAS28ESR==1,]
exp_RA_DAS28ESR_L$DAS28ESR=1

exp_RA$DAS28CRP=ifelse(exp_RA$DAS28CRP>3.2,1,0)
exp_RA_DAS28CRP_H=exp_RA[exp_RA$DAS28CRP==1,]
exp_RA_DAS28CRP_L=exp_RA[!exp_RA$DAS28CRP==1,]
exp_RA_DAS28CRP_L$DAS28CRP=1

exp_RA$RF=ifelse(exp_RA$RF>20,1,0)
exp_RA_RF_H=exp_RA[exp_RA$RF==1,]
exp_RA_RF_L=exp_RA[!exp_RA$RF==1,]
exp_RA_RF_L$RF=1

exp_RA$CCP=ifelse(exp_RA$CCP>25,1,0)
exp_RA_CCP_H=exp_RA[exp_RA$CCP==1,]
exp_RA_CCP_L=exp_RA[!exp_RA$CCP==1,]
exp_RA_CCP_L$CCP=1
colnames(exp_HC_OA)
exp_HC_OA[,37:47]=0



##stage：1/0
exp_stage_H=rbind(exp_RA_stage_H,exp_HC_OA)
colnames(exp_stage_H)
exp_stage_H$stage=as.factor(exp_stage_H$stage)
exp_stage_L=rbind(exp_RA_stage_L,exp_HC_OA)
colnames(exp_stage_L)
exp_stage_L$stage=as.factor(exp_stage_L$stage)
library(pROC)
##exp_stage_H
##
{
  results <- lapply(exp_stage_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_stage_H, family = binomial(link="logit"))
})
results

library( purrr)
summaries<- map(results, summary)

library(purrr)

get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})

sig_vars <- discard(sig_vars, ~ length(.x) == 0)

sig_vars_df <- bind_rows(sig_vars)

write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#cg00771778	cg04603130	cg05496363	cg09894276	cg17184477	cg25783497

model <- glm(stage~cg00771778+cg04603130+cg05496363+cg09894276+cg17184477+cg25783497, data=exp_stage_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(stage~cg00771778+cg04603130+cg05496363+cg09894276+cg17184477+cg25783497, data=exp_stage_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc1 <- roc(response = exp_stage_H$stage, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_stage_H$stage))))
plot(auc1,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

}

##exp_stage_L

  results <- lapply(exp_stage_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_stage_L, family = binomial(link="logit"))
})
  results
  library( purrr)
  summaries<- map(results, summary)
  library(purrr)
  get_p_value <- function(summary) {
    coef_table <- summary$coefficients
    p_value <- coef_table[, "Pr(>|z|)"]
    return(p_value)
  }
  
  sig_vars <- map(results, ~ {
    summary <- summary(.x)
    p_value <- get_p_value(summary)
    sig_vars <- names(p_value[p_value < 0.01])
    return(sig_vars)
  })
  sig_vars <- discard(sig_vars, ~ length(.x) == 0)
  sig_vars_df <- bind_rows(sig_vars)
  write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
  #cg00771778	cg00849570	cg12697442	cg17184477	cg20432680
  
  model <- glm(stage~cg00771778+cg00849570+cg12697442+cg17184477+cg20432680, data=exp_stage_L, family = binomial(link="logit"),x=T)  
  summary(model)
  
  model.stepp <- step(model,direction='backward')
  summary(model.stepp)
  
  model.step<-glm(stage~cg00771778+cg00849570+cg12697442+cg17184477+cg20432680, data=exp_stage_L,family = binomial(link="logit"),x=T)
  summary(model.step)
  
  
  auc2 <- roc(response = exp_stage_L$stage, 
             predictor = predict(model.step, type = "response"), 
             levels = rev(levels(factor(exp_stage_L$stage))))
  plot(auc2,
       legacy.axes = TRUE,
       main="ROC",
       thresholds="best",
       print.thres="best") 
 
}

##disease_age：1/0
exp_disease_age_H=rbind(exp_RA_disease_age_H,exp_HC_OA)
exp_disease_age_L=rbind(exp_RA_disease_age_L,exp_HC_OA)
###exp_disease_age_H
results <- lapply(exp_disease_age_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_disease_age_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.02])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#cg17458101	age


model <- glm(disease_age~cg17458101+age, data=exp_disease_age_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(disease_age~cg17458101+age, data=exp_disease_age_H,family = binomial(link="logit"),x=T)
summary(model.step)


auc3<- roc(response = exp_disease_age_H$disease_age, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_disease_age_H$disease_age))))
plot(auc3,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", # 
     print.thres="best") 
}

###exp_disease_age_L
results <- lapply(exp_disease_age_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_disease_age_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#cg00771778	cg05496363	cg17184477	age

model <- glm(disease_age~cg00771778+age+cg05496363+cg17184477, data=exp_disease_age_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)
model.step<-glm(disease_age~cg00771778+age+cg05496363+cg17184477, data=exp_disease_age_L, family = binomial(link="logit"),x=T)
summary(model.step)

auc4 <- roc(response = exp_disease_age_L$disease_age, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_disease_age_L$disease_age))))
plot(auc,
     legacy.axes = TRUE,
     main="RO点",
     thresholds="best", 
     print.thres="best") 


#Number_of_tenderness_joints：1/0
exp_Number_of_tenderness_joints_H=rbind(exp_RA_Number_of_tenderness_joints_H,exp_HC_OA)
exp_Number_of_tenderness_joints_L=rbind(exp_RA_Number_of_tenderness_joints_L,exp_HC_OA)
###Number_of_tenderness_joints_H
results <- lapply(exp_Number_of_tenderness_joints_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_Number_of_tenderness_joints_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.05])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg00636427","cg05496363","cg14046074","cg17184477","cg20500144","age"


model <- glm(Number_of_tenderness_joints~cg00636427+age+cg05496363+cg14046074+cg17184477+cg20500144, data=exp_Number_of_tenderness_joints_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(Number_of_tenderness_joints~cg00636427+age+cg05496363+cg14046074+cg17184477+cg20500144, data=exp_Number_of_tenderness_joints_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc5 <- roc(response = exp_Number_of_tenderness_joints_H$Number_of_tenderness_joints, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_Number_of_tenderness_joints_H$Number_of_tenderness_joints))))

plot(auc5,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

###Number_of_tenderness_joints_L
results <- lapply(exp_Number_of_tenderness_joints_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_Number_of_tenderness_joints_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg05496363","cg25783497"


model <- glm(Number_of_tenderness_joints~cg05496363+cg25783497, data=exp_Number_of_tenderness_joints_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)
model.step<-glm(Number_of_tenderness_joints~cg05496363+cg25783497, data=exp_Number_of_tenderness_joints_L, family = binomial(link="logit"),x=T)
summary(model.step)

auc6 <- roc(response = exp_Number_of_tenderness_joints_L$Number_of_tenderness_joints, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_Number_of_tenderness_joints_L$Number_of_tenderness_joints))))
auc6
plot(auc6,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 


#Swollen_joint_number：1/0
exp_Swollen_joint_number_H=rbind(exp_RA_Swollen_joint_number_H,exp_HC_OA)
exp_Swollen_joint_number_L=rbind(exp_RA_Swollen_joint_number_L,exp_HC_OA)

###Swollen_joint_number_H
results <- lapply(exp_Swollen_joint_number_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_Swollen_joint_number_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.05])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg11913894","cg17184477"


model <- glm(Swollen_joint_number~cg11913894+cg17184477, data=exp_Swollen_joint_number_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(Swollen_joint_number~cg11913894+cg17184477, data=exp_Swollen_joint_number_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc7 <- roc(response = exp_Swollen_joint_number_H$Swollen_joint_number, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_Swollen_joint_number_H$Swollen_joint_number))))
auc7
plot(auc7,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 
###Swollen_joint_number_L
results <- lapply(exp_Swollen_joint_number_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_Swollen_joint_number_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg04603130","cg05496363","age"


model <- glm(Swollen_joint_number~cg04603130+cg05496363+age, data=exp_Swollen_joint_number_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(Swollen_joint_number~cg04603130+cg05496363+age, data=exp_Swollen_joint_number_L, family = binomial(link="logit"),x=T)
summary(model.step)


auc8 <- roc(response = exp_Swollen_joint_number_L$Swollen_joint_number, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_Swollen_joint_number_L$Swollen_joint_number))))
auc8
plot(auc8,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

##ESR
exp_ESR_H=rbind(exp_RA_ESR_H,exp_HC_OA)
exp_ESR_L=rbind(exp_RA_ESR_L,exp_HC_OA)

###ESR_H
results <- lapply(exp_ESR_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_ESR_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg05496363","cg06734169","cg11913894"


model <- glm(ESR~cg05496363+cg06734169+cg11913894, data=exp_ESR_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(ESR~cg05496363+cg06734169+cg11913894, data=exp_ESR_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc9 <- roc(response = exp_ESR_H$ESR, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_ESR_H$ESR))))
auc9
plot(auc9,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 
###ESR_L
results <- lapply(exp_ESR_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_ESR_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg12176476","cg25783497"


model <- glm(ESR~cg12176476+cg25783497, data=exp_ESR_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(ESR~cg12176476+cg25783497, data=exp_ESR_L, family = binomial(link="logit"),x=T)
summary(model.step)


auc10 <- roc(response = exp_ESR_L$ESR, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_ESR_L$ESR))))
auc
plot(auc,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", #
     print.thres="best") 

##CRP
exp_CRP_H=rbind(exp_RA_CRP_H,exp_HC_OA)
exp_CRP_L=rbind(exp_RA_CRP_L,exp_HC_OA)
###CRP_H
results <- lapply(exp_CRP_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_CRP_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.02])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg04115006","cg05496363"


model <- glm(CRP~cg05496363+cg04115006, data=exp_CRP_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(CRP~cg05496363+cg04115006, data=exp_CRP_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc11<- roc(response = exp_CRP_H$CRP, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_CRP_H$CRP))))
auc
plot(auc,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", #
     print.thres="best") 
###CRP_L
results <- lapply(exp_CRP_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_CRP_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.02])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg00771778","cg12176476","cg25783497","age"


model <- glm(CRP~cg00771778+cg12176476+cg25783497+age, data=exp_CRP_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(CRP~cg00771778+cg12176476+cg25783497+age, data=exp_CRP_L, family = binomial(link="logit"),x=T)
summary(model.step)


auc12 <- roc(response = exp_CRP_L$CRP, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_CRP_L$CRP))))
auc12
plot(auc12,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 
##CCP
exp_CCP_H=rbind(exp_RA_CCP_H,exp_HC_OA)
exp_CCP_L=rbind(exp_RA_CCP_L,exp_HC_OA)

###CCP_H
results <- lapply(exp_CCP_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_CCP_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg04603130","cg05496363","cg17458101","age"


model <- glm(CCP~cg04603130+cg05496363+cg17458101+age, data=exp_CCP_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(CCP~cg04603130+cg05496363+cg17458101+age,data=exp_CCP_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc13 <- roc(response = exp_CCP_H$CCP, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_CCP_H$CCP))))
auc13
plot(auc13,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 
###CCP_L
results <- lapply(exp_CCP_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_CCP_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#""cg12697442",


model <- glm(CCP~cg12697442, data=exp_CCP_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(CCP~cg12697442,data=exp_CCP_L, family = binomial(link="logit"),x=T)
summary(model.step)


auc14 <- roc(response = exp_CCP_L$CCP, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_CCP_L$CCP))))
auc14
plot(auc14,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 


##RF
exp_RF_H=rbind(exp_RA_RF_H,exp_HC_OA)
exp_RF_L=rbind(exp_RA_RF_L,exp_HC_OA)
###RF_H
results <- lapply(exp_RF_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_RF_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg04603130","cg05496363","cg17458101"


model <- glm(RF~cg04603130+cg05496363+cg17458101, data=exp_RF_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(RF~cg04603130+cg05496363+cg17458101,data=exp_RF_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc15 <- roc(response = exp_RF_H$RF, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_RF_H$RF))))
auc15
plot(auc15,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best",
     print.thres="best") 
###RF_L
results <- lapply(exp_RF_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_RF_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg00771778","cg25783497"


model <- glm(RF~cg00771778+cg25783497, data=exp_RF_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(RF~cg00771778+cg25783497,data=exp_RF_L, family = binomial(link="logit"),x=T)
summary(model.step)


auc16 <- roc(response = exp_RF_L$RF, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_RF_L$RF))))
auc16
plot(auc16,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

#VAS
exp_VAS_H=rbind(exp_RA_VAS_H,exp_HC_OA)
exp_VAS_L=rbind(exp_RA_VAS_L,exp_HC_OA)
###VAS_H
results <- lapply(exp_VAS_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_VAS_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg09894276","cg17458101"


model <- glm(VAS~cg09894276+cg17458101, data=exp_VAS_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(VAS~cg04603130+cg05496363+cg17458101,data=exp_VAS_H, family = binomial(link="logit"),x=T)
summary(model.step)


auc17 <- roc(response = exp_VAS_H$VAS, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_VAS_H$VAS))))
auc17
plot(auc17,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

##VAS_L
results <- lapply(exp_VAS_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_VAS_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg00771778","cg04603130","cg05496363","cg12176476"


model <- glm(VAS~cg00771778+cg04603130+cg05496363+cg12176476, data=exp_VAS_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(VAS~cg00771778+cg04603130+cg05496363+cg12176476, data=exp_VAS_L, family = binomial(link="logit"),x=T)
summary(model.step)


auc18<- roc(response = exp_VAS_L$VAS, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_VAS_L$VAS))))
auc18
plot(auc18,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

#DAS28CRP
exp_DAS28CRP_H=rbind(exp_RA_DAS28CRP_H,exp_HC_OA)
exp_DAS28CRP_L=rbind(exp_RA_DAS28CRP_L,exp_HC_OA)

###DAS28CRP_H
results <- lapply(exp_DAS28CRP_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_DAS28CRP_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.02])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg04115006","cg17184477","cg17458101"


model <- glm(DAS28CRP~cg04115006+cg17184477+cg17458101, data=exp_DAS28CRP_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)
model.step<-glm(DAS28CRP~cg04115006+cg17184477+cg17458101, data=exp_DAS28CRP_H,  family = binomial(link="logit"),x=T)
summary(model.step)


auc19 <- roc(response = exp_DAS28CRP_H$DAS28CRP, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_DAS28CRP_H$DAS28CRP))))
auc19
plot(auc19,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 
###DAS28CRP_L
results <- lapply(exp_DAS28CRP_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_DAS28CRP_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg00771778","cg05496363"


model <- glm(DAS28CRP~cg00771778+cg05496363, data=exp_DAS28CRP_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(DAS28CRP~cg00771778+cg05496363, data=exp_DAS28CRP_L,  family = binomial(link="logit"),x=T)
summary(model.step)


auc20 <- roc(response = exp_DAS28CRP_L$DAS28CRP, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_DAS28CRP_L$DAS28CRP))))
auc20
plot(auc20,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 

#DAS28ESR
exp_DAS28ESR_H=rbind(exp_RA_DAS28ESR_H,exp_HC_OA)
exp_DAS28ESR_L=rbind(exp_RA_DAS28ESR_L,exp_HC_OA)

###DAS28ESR_H
results <- lapply(exp_DAS28ESR_H[,1:36], function(x) {
  glm(stage ~ x, data = exp_DAS28ESR_H, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.02])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#"cg05496363","cg17184477"


model <- glm(DAS28ESR~cg05496363+cg17184477, data=exp_DAS28ESR_H, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)
model.step<-glm(DAS28ESR~cg05496363+cg17184477, data=exp_DAS28ESR_H,  family = binomial(link="logit"),x=T)
summary(model.step)


auc21<- roc(response = exp_DAS28ESR_H$DAS28ESR, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_DAS28ESR_H$DAS28ESR))))
auc21
plot(auc21,
     legacy.axes = TRUE,
     main="ROC",
     thresholds="best", 
     print.thres="best") 
###DAS28ESR_L
results <- lapply(exp_DAS28ESR_L[,1:36], function(x) {
  glm(stage ~ x, data = exp_DAS28ESR_L, family = binomial(link="logit"))
})
results
library( purrr)
summaries<- map(results, summary)
library(purrr)
get_p_value <- function(summary) {
  coef_table <- summary$coefficients
  p_value <- coef_table[, "Pr(>|z|)"]
  return(p_value)
}

sig_vars <- map(results, ~ {
  summary <- summary(.x)
  p_value <- get_p_value(summary)
  sig_vars <- names(p_value[p_value < 0.01])
  return(sig_vars)
})
sig_vars <- discard(sig_vars, ~ length(.x) == 0)
sig_vars_df <- bind_rows(sig_vars)
write.csv(sig_vars_df, file = "sig_vars.csv", row.names = FALSE)
#""cg25783497"


model <- glm(DAS28ESR~cg25783497, data=exp_DAS28ESR_L, family = binomial(link="logit"),x=T)  
summary(model)

model.stepp <- step(model,direction='backward')
summary(model.stepp)

model.step<-glm(DAS28ESR~cg25783497, data=exp_DAS28ESR_L,  family = binomial(link="logit"),x=T)
summary(model.step)


auc22 <- roc(response = exp_DAS28ESR_L$DAS28ESR, 
           predictor = predict(model.step, type = "response"), 
           levels = rev(levels(factor(exp_DAS28ESR_L$DAS28ESR))))
auc22

colors <- rainbow(22)
colors[1]

pdf("1-8ROC.pdf",width = 6,height = 5)
plot(auc1,
     legacy.axes = TRUE,
     col = colors[1],
     main="ROC",
     thresholds="best"
     )
plot(auc2,
     col = colors[2],
     add = TRUE,
     print.auc = F)
plot(auc3,
     col = colors[3],
     add = TRUE,
     print.auc = F)
plot(auc4,
     col = colors[4],
     add = TRUE,
     print.auc = F)
plot(auc5,
     col = colors[5],
     add = TRUE,
     print.auc = F)
plot(auc6,
     col = colors[6],
     add = TRUE,
     print.auc = F)
plot(auc7,
     col = colors[7],
     add = TRUE,
     print.auc = F)
plot(auc8,
     col = colors[8],
     add = TRUE,
     print.auc = F)

legend("bottomright",
       legend = c("stage_H:0.78", "stage_L:0.74", "disease_age_H:0.68","disease_age_L:0.79",
                  "Number_of_tenderness_joints_H:0.78","Number of_tenderness_joints_L:0.70",
                  "Swollen_joint_number_H:0.71","Swollen_joint_number_L:0.69"), col = c(colors[1], colors[2], colors[3],
                                                      colors[4],colors[5],colors[6],colors[7],colors[8]),lty = 1,cex = 0.55)
dev.off()

pdf("9_16ROC.pdf",width = 6,height = 5)
plot(auc9,
     legacy.axes = TRUE,
     col = colors[9],
     main="ROC",
     thresholds="best")

plot(auc10,
     col = colors[10],
     add = TRUE,
     print.auc = F)
plot(auc11,
     col = colors[11],
     add = TRUE,
     print.auc = F)
plot(auc12,
     col = colors[12],
     add = TRUE,
     print.auc = F)
plot(auc13,
     col = colors[13],
     add = TRUE,
     print.auc = F)
plot(auc14,
     col = colors[14],
     add = TRUE,
     print.auc = F)
plot(auc15,
     col = colors[15],
     add = TRUE,
     print.auc = F)
plot(auc16,
     col = colors[16],
     add = TRUE,
     print.auc = F)

legend("bottomright",
       legend = c("ESR_H:0.64","ESR_L:0.68",
                  "CRP_H:0.72","CRP_L:0.68","CCP_H:0.66","CCP_L:0.63","RF_H:0.65","RF_L:0.72"), 
       col = c(colors[9], colors[10], colors[11],colors[12],colors[13],colors[14],colors[15],colors[16]),lty = 1,cex = 0.55)
dev.off()

pdf("17_22ROC.pdf",width = 6,height = 5)
plot(auc17,
     legacy.axes = TRUE,
     col = colors[17],
     main="ROC",
     thresholds="best")

plot(auc18,
     col = colors[18],
     add = TRUE,
     print.auc = F)
plot(auc19,
     col = colors[19],
     add = TRUE,
     print.auc = F)
plot(auc20,
     col = colors[20],
     add = TRUE,
     print.auc = F)
plot(auc21,
     col = colors[21],
     add = TRUE,
     print.auc = F)
plot(auc22,
     col = colors[22],
     add = TRUE,
     print.auc = F)


legend("bottomright",
       legend = c("VAS_H:0.69","VAS_L:0.71",
                  "DAS28CRP_H:0.75","DAS28CRP_L:0.68",
                  "DAS28ESR_H：0.71","DAS28ESR_L：0.69"), col = c(colors[17],colors[18],colors[19],colors[20],
                                                      colors[21],colors[22]),lty = 1,cex = 0.42)
dev.off()









