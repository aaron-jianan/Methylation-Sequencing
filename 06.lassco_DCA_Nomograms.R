##lasso
load(file = "Remove the high-level raw data.Rdata")
##ra/non_RA

set.seed(1)
index <- sample(2,301,replace = TRUE,prob=c(0.7,0.3))
traindata <- rawdata_new[index==1,]
testdata <- rawdata_new[index==2,]

library(glmnet)
library(foreign)
y<-as.matrix(rawdata_new[,34])
x=as.matrix(rawdata_new[,1:33])
f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1)
print(f1)
plot(f1, xvar="lambda", label=F)
set.seed(123456)
cvfit=cv.glmnet(x,y,family="binomial",nfolds = 10)
print(cvfit)
plot(cvfit)
cvfit$lambda.min
#[1]  0.03517457
cvfit$lambda.1se
#[1] 0.08918035
l.coef1<-coef(cvfit$glmnet.fit,s=0.08918035,exact = F)
l.coef1
l.coef2<-coef(cvfit$glmnet.fit,s=0.03517457,exact = F)
l.coef2
predict(cvfit,newx = x,type="class",s=0.03517457)

set.seed(20230525)
rlsocv=cv.glmnet( x ,y,family="binomial",relax = T)
predict(rlsocv,newx = x,type="coefficients",s=0.03517457,gamma = 0)
library(rms)
library(Hmisc);library(lattice);library(survival);library(Formula);library(ggplot2);library(SparseM)
NM<-datadist(rawdata_new)
options(datadist='NM')
lgrfit<-lrm(group~cg00771778+cg04115006+cg05496363+cg17184477+cg20432680+cg25783497,data = traindata,x=T,y=T)
lgrfit
#C Indexes       0.766 
Nomg<-nomogram(lgrfit,fun=plogis,fun.at=c(.001,.01,.05,0.1,seq(.2,.8,by=.2),0.9,.95,.99,.999),lp=F,funlabel=" Rate")
plot(Nomg)
##Beautification
library(regplot)
logtistic_fit<-glm(group~cg00771778+cg04115006+cg05496363+cg17184477+cg20432680+cg25783497,family=binomial(link = logit), data=traindata)
regplot(logtistic_fit, plots=c("density","boxes") ,observation=FALSE,points=TRUE,dencol="#82B0D2",boxcol="#BEB8DC")

##Calibration curve
Cal<-calibrate(lgrfit,method='boot',B=100)
plot(Cal,xlim=c(0,1.0),ylim=c(0,1.0))
library(ResourceSelection)
hoslem.test(traindata$group,fitted(logtistic_fit),g=10)

pre.test<- predict(logtistic_fit,testdata,type = "response")
hoslem.test(testdata$group,pre.test,g=10)
library(pROC)
auc(testdata$group,pre.test)
#0.7592
roc(testdata$group,pre.test,plot=TRUE,col="black",legacy.axes=TRUE,print.auc=T,print.auc.x=0.42,print.auc.y=0.1,print.thres=T,print.thres.col="black",
    identity.lty=2,identity.lwd=1)



##Double Negative RA

set.seed(1234)
index <- sample(2,71,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulenegtive=methythlation_exp_RA_RF_CCP_doulenegtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulenegtive[index==2,]


library(glmnet)
library(foreign)
y<-as.matrix(methythlation_exp_RA_RF_CCP_doulenegtive[,34])
x=as.matrix(methythlation_exp_RA_RF_CCP_doulenegtive[,1:33])
f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1)
print(f1)
plot(f1, xvar="lambda", label=F)
set.seed(123456)
cvfit=cv.glmnet(x,y,family="binomial",nfolds = 10)
print(cvfit)
plot(cvfit)
cvfit$lambda.min
#[1]  0.01227901
cvfit$lambda.1se
#[1]  0.1043413
l.coef1<-coef(cvfit$glmnet.fit,s=0.01227901,exact = F)
l.coef1
l.coef2<-coef(cvfit$glmnet.fit,s=0.1043413,exact = F)
l.coef2

set.seed(20230525)
rlsocv=cv.glmnet( x ,y,family="binomial",relax = T)
predict(rlsocv,newx = x,type="coefficients",s=0.1043413,gamma = 0)
library(rms)
library(Hmisc);library(lattice);library(survival);library(Formula);library(ggplot2);library(SparseM)
NM<-datadist(methythlation_exp_RA_RF_CCP_doulenegtive)
options(datadist='NM')

lgrfit<-lrm(group~cg04115006+cg09174690+cg11963436+cg14046074,data = traindata,x=T,y=T)
lgrfit
#C Indexes       0.766 
Nomg<-nomogram(lgrfit,fun=plogis,fun.at=c(.001,.01,.05,0.1,seq(.2,.8,by=.2),0.9,.95,.99,.999),lp=F,funlabel=" Rate")
plot(Nomg)

library(regplot)
logtistic_fit<-glm(group~cg04115006+cg09174690+cg11963436+cg14046074,family=binomial(link = logit), data=traindata)
regplot(logtistic_fit, plots=c("density","boxes") ,observation=FALSE,points=TRUE,dencol="#82B0D2",boxcol="#BEB8DC")

Cal<-calibrate(lgrfit,method='boot',B=100)
plot(Cal,xlim=c(0,1.0),ylim=c(0,1.0))
library(ResourceSelection)
hoslem.test(traindata$group,fitted(logtistic_fit),g=10)

pre.test<- predict(logtistic_fit,testdata,type = "response")
hoslem.test(testdata$group,pre.test,g=10)

library(pROC)
auc(testdata$group,pre.test)
roc(testdata$group,pre.test,plot=TRUE,col="black",legacy.axes=TRUE,print.auc=T,print.auc.x=0.42,print.auc.y=0.1,print.thres=T,print.thres.col="black",
    identity.lty=2,identity.lwd=1)

##Double positive RA

set.seed(1234)
index <- sample(2,238,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_CCP_doulepostive=methythlation_exp_RA_RF_CCP_doulepostive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_CCP_doulepostive[index==1,]
testdata <- methythlation_exp_RA_RF_CCP_doulepostive[index==2,]

library(glmnet)
library(foreign)
y<-as.matrix(methythlation_exp_RA_RF_CCP_doulepostive[,34])
x=as.matrix(methythlation_exp_RA_RF_CCP_doulepostive[,1:33])
f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1)
print(f1)
plot(f1, xvar="lambda", label=F)
set.seed(123456)
cvfit=cv.glmnet(x,y,family="binomial",nfolds = 10)
print(cvfit)
plot(cvfit)
cvfit$lambda.min
#[1]  0.0432757
cvfit$lambda.1se
#[1]   0.1097197
l.coef1<-coef(cvfit$glmnet.fit,s=0.0432757,exact = F)
l.coef1
l.coef2<-coef(cvfit$glmnet.fit,s= 0.1097197,exact = F)
l.coef2

set.seed(20230525)
rlsocv=cv.glmnet( x ,y,family="binomial",relax = T)
predict(rlsocv,newx = x,type="coefficients",s=0.0432757,gamma = 0)
library(rms)
library(Hmisc);library(lattice);library(survival);library(Formula);library(ggplot2);library(SparseM)
NM<-datadist(methythlation_exp_RA_RF_CCP_doulepostive)
options(datadist='NM')

lgrfit<-lrm(group~cg00771778+cg00849570+cg04115006+cg05496363+cg12176476+cg15989608+cg17184477+cg20432680+cg25783497
              ,data = traindata,x=T,y=T)
lgrfit
#C Indexes       0.831
Nomg<-nomogram(lgrfit,fun=plogis,fun.at=c(.001,.01,.05,0.1,seq(.2,.8,by=.2),0.9,.95,.99,.999),lp=F,funlabel=" Rate")
plot(Nomg)

library(regplot)
logtistic_fit<-glm(group~cg00771778+cg00849570+cg04115006+cg05496363+cg12176476+cg15989608+cg17184477+cg20432680+cg25783497
                   ,family=binomial(link = logit), data=traindata)
regplot(logtistic_fit, plots=c("density","boxes") ,observation=FALSE,points=TRUE,dencol="#82B0D2",boxcol="#BEB8DC")


Cal<-calibrate(lgrfit,method='boot',B=100)
plot(Cal,xlim=c(0,1.0),ylim=c(0,1.0))
library(ResourceSelection)
hoslem.test(traindata$group,fitted(logtistic_fit),g=10)

pre.test<- predict(logtistic_fit,testdata,type = "response")
hoslem.test(testdata$group,pre.test,g=10)
library(pROC)
auc(testdata$group,pre.test)
roc(testdata$group,pre.test,plot=TRUE,col="black",legacy.axes=TRUE,print.auc=T,print.auc.x=0.42,print.auc.y=0.1,print.thres=T,print.thres.col="black",
    identity.lty=2,identity.lwd=1)

##RF single negative
set.seed(1234)
index <- sample(2,84,replace = TRUE,prob=c(0.7,0.3))
methythlation_exp_RA_RF_negtive=methythlation_exp_RA_RF_negtive[,-(35:36)]
traindata <- methythlation_exp_RA_RF_negtive[index==1,]
testdata <- methythlation_exp_RA_RF_negtive[index==2,]

library(glmnet)
library(foreign)
y<-as.matrix(methythlation_exp_RA_RF_negtive[,34])
x=as.matrix(methythlation_exp_RA_RF_negtive[,1:33])
f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1)
print(f1)
plot(f1, xvar="lambda", label=F)
set.seed(123456)
cvfit=cv.glmnet(x,y,family="binomial",nfolds = 10)
print(cvfit)
plot(cvfit)
cvfit$lambda.min
#[1]  0.07297758
cvfit$lambda.1se
#[1]    0.1536108
l.coef1<-coef(cvfit$glmnet.fit,s= 0.07297758,exact = F)
l.coef1
l.coef2<-coef(cvfit$glmnet.fit,s=  0.1536108,exact = F)
l.coef2

set.seed(20230525)
rlsocv=cv.glmnet( x ,y,family="binomial",relax = T)
predict(rlsocv,newx = x,type="coefficients",s=0.1536108,gamma = 0)
library(rms)
library(Hmisc);library(lattice);library(survival);library(Formula);library(ggplot2);library(SparseM)
NM<-datadist(methythlation_exp_RA_RF_negtive)
options(datadist='NM')

lgrfit<-lrm(group~cg05496363+cg14046074
            ,data = traindata,x=T,y=T)
lgrfit
#C Indexes        0.786
Nomg<-nomogram(lgrfit,fun=plogis,fun.at=c(.001,.01,.05,0.1,seq(.2,.8,by=.2),0.9,.95,.99,.999),lp=F,funlabel=" Rate")
plot(Nomg)

library(regplot)
logtistic_fit<-glm(group~cg05496363+cg14046074
                   ,family=binomial(link = logit), data=traindata)
regplot(logtistic_fit, plots=c("density","boxes") ,observation=FALSE,points=TRUE,dencol="#82B0D2",boxcol="#BEB8DC")


Cal<-calibrate(lgrfit,method='boot',B=100)
plot(Cal,xlim=c(0,1.0),ylim=c(0,1.0))
library(ResourceSelection)
hoslem.test(traindata$group,fitted(logtistic_fit),g=10)

pre.test<- predict(logtistic_fit,testdata,type = "response")
hoslem.test(testdata$group,pre.test,g=10)

library(pROC)
auc(testdata$group,pre.test)
roc(testdata$group,pre.test,plot=TRUE,col="black",legacy.axes=TRUE,print.auc=T,print.auc.x=0.42,print.auc.y=0.1,print.thres=T,print.thres.col="black",
    identity.lty=2,identity.lwd=1)


# # CCP single negative 
# # sample size too small, modeling failure





##Double Negative RA -DCA
exp2=methythlation_exp_RA_RF_CCP_doulenegtive
exp2$group=as.numeric(exp2$group)
library(tidyverse)
exp2$group=ifelse(str_detect(exp2$group,"2"),1,0)
fit2 <- decision_curve(group~cg04115006+cg09174690+cg11963436+cg14046074,data = exp2, 
                       study.design = "case-control", 
                       bootstraps = 50 
)


plot_decision_curve(fit2, curve.names = "RF-/CCP- RA",
                    cost.benefit.axis = T, 
                    confidence.intervals = "none"
)



