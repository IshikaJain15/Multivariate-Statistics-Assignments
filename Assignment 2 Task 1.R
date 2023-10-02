#################################
############ TASK 1 #############     
#################################


library(psych) #fa()
library(class) ##knn() and knn.cv()
library(MASS) ##lda() and qda()
library(HDclassif) #hdda()
library(nnet) # for multinom function
library(VGAM) #vglm
library(candisc) #candisc
library(randomForest)


#########Functions##########
err<-function(observed,prob,cutoff)
{tab<-table(observed,ifelse(prob>=0.5,1,0))
err<-1-sum(diag(tab))/sum(tab)
return(err)
}

hitrat<-function(observed,prob,cutoff)
{tab<-table(observed,ifelse(prob>=cutoff,1,0))
hitrat<-sum(diag(tab))/sum(tab)
return(hitrat)
}

hitratknn<-function(observed,predicted)
{tab<-table(observed,predicted)
hitratknn<-sum(diag(tab))/sum(tab)
return(hitratknn)
}

performance<-function(tab){
  error<-1-sum(diag(tab))/sum(tab)
  sensitivity<-tab[2,2]/(tab[2,1]+tab[2,2])
  FPrate<-tab[1,2]/(tab[1,1]+tab[1,2])
  performance<-c(error=error, sensitivity=sensitivity,FPrate=FPrate)
}

library(ISLR2) 

head(College)
#data<-College

priv<-matrix(0, nrow= 777, ncol=1)
for(i in 1:777){
  priv[i]<-ifelse(College$Private[i]== "Yes",1,0)}
data<-cbind(College,priv)
set.seed(299)
trainsize<-388
sel<-(2:19)
train = sample(nrow(data), trainsize)
data.train<-data[train,]
data.test<-data[-train,]
target.train<-data[train,19]
target.test<-data[-train,19]
data.train<-data.train[,sel]
data.test<-data.test[,sel]

data_c<-scale(data[2:18],center=TRUE)
data_s<-scale(data[2:18], center=TRUE, scale = TRUE)
data_ctest<-scale(data.test,center=TRUE)
data_ctrain<-scale(data.train, center=TRUE)
data_stest<-scale(data.test,center=TRUE, scale = TRUE)
data_strain<-scale(data.train, center=TRUE, scale = TRUE)

dfl<-log(data.train[,1:17])
df_sktr<-cbind(dfl[1],dfl[2],dfl[3],dfl[4],data.train[5],dfl[6],dfl[7],data.train[8],data.train[9],dfl[10],dfl[11],data.train[12],data.train[13],data.train[14],data.train[15],dfl[16],data.train[17],data.train[18])
dfl<-log(data.test[,1:17])
df_sktst<-cbind(dfl[1],dfl[2],dfl[3],dfl[4],data.test[5],dfl[6],dfl[7],data.test[8],data.test[9],dfl[10],dfl[11],data.test[12],data.test[13],data.test[14],data.test[15],dfl[16],data.test[17],data.test[18])
dfl<-log(data[,2:18])
df_sk<-cbind(dfl[1],dfl[2],dfl[3],dfl[4],data[6],dfl[6],dfl[7],data[9],data[10],dfl[10],dfl[11],data[13],data[14],data[15],data[16],dfl[16],data[18])
df_skc<-scale(df_sk[,1:17],center=TRUE)
df_skstd<-scale(df_sk[,1:17],center=TRUE, scale= TRUE)
#####candisc#####
sel2<-(2:18)
#canonical discriminant analysis
lm.out<-lm(as.matrix(data[,sel2])~data$priv, data=data)
candisc.out<-candisc(lm.out)
print(candisc.out)
par(pty="s")
plot(candisc.out, Xlabel= "Private college()")
round(candisc.out$structure,3)


#LDA
names(College)
lm.out<-lm(cbind(Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergrad, Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, perc.alumni, Expend, Grad.Rate)~ as.factor(College$Private),data=College)
summary(manova(lm.out),test=c("Wilks"))

lda.out<-lda(College[,2:18],College$Private)
print(lda.out)

#hit rate training data
predlda.train<-predict(lda.out,College[,2:18])
tab<-table(College$Private,predlda.train$class)
normal_hitrate<-sum(diag(tab))/sum(tab)



#loocv hit rate, with prior prob
pred.loocv<-lda(College[,2:18],College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
loocvhitrate_LDA<-sum(diag(tab))/sum(tab)
#Sensitivity 
loocv_sensitivity_LDA<-tab[2,2]/sum(tab[2,])
#specificity
loocv_specificity_LDA<-tab[1,1]/sum(tab[1,])
#loocv hit rate, with equal prior prob
pred.loocv<-lda(College[,2:18],College$Private,prior=c(1,1)/2,  CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
loocvhitrate_eqprior_LDA<-sum(diag(tab))/sum(tab)
#Sensitivity 
loocv_sensitivity_eqprior_LDA<-tab[2,2]/sum(tab[2,])
#specificity
loocv_specificity_eqprior_LDA<-tab[1,1]/sum(tab[1,])


####Centered data LDA
lda.center.out<-lda(data_c,College$Private)
print(lda.center.out)

#hit rate training data
predlda.data_c<-predict(lda.center.out,data_c)
tab<-table(College$Private,predlda.data_c$class)
sum(diag(tab))/sum(tab)
data_c_hitrate<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-lda(data_c,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCVhitrate_center<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_center<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_center<-tab[1,1]/sum(tab[1,])
#loocv hit rate, with equal prior prob
pred.loocv<-lda(data_c,College$Private, CV=TRUE, prior=c(1,1)/2)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)

LOOCVhitrate_eqprior_center<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_eqprior_center<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_eqprior_center<-tab[1,1]/sum(tab[1,])

#########LDA Log-transformation of skewed variables followed by centering of all variables 
lda.log.center.out<-lda(df_skc,College$Private)
print(lda.log.center.out)

#hit rate training data
predlda.df_skc<-predict(lda.log.center.out,df_skc)
tab<-table(College$Private,predlda.df_skc$class)
sum(diag(tab))/sum(tab)
df_skc_hitrate<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-lda(df_skc,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCVhitrate_center_log<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_center_log<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_center_log<-tab[1,1]/sum(tab[1,])
#loocv hit rate, with equal prior prob
pred.loocv<-lda(df_skc,College$Private, CV=TRUE, prior=c(1,1)/2)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCVhitrate_eqprior_center_log<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_eqprior_center_log<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_eqprior_center_log<-tab[1,1]/sum(tab[1,])



########## Standardization of all variables 
lda.std.out<-lda(data_s,College$Private)
print(lda.std.out)

#hit rate training data
predlda.data_s<-predict(lda.std.out,data_s)
tab<-table(College$Private,predlda.data_s$class)
sum(diag(tab))/sum(tab)
data_s_hitrate<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-lda(data_s,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCVhitrate_std<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_std<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_std<-tab[1,1]/sum(tab[1,])
#loocv hit rate, with equal prior prob
pred.loocv<-lda(data_s,College$Private, CV=TRUE, prior=c(1,1)/2)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)

LOOCVhitrate_eqprior_std<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_eqprior_std<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_eqprior_std<-tab[1,1]/sum(tab[1,])


#########LDA Log-transformation of skewed variables followed by standardizing of all variables 
lda.log.std.out<-lda(df_skstd,College$Private)
print(lda.log.std.out)

#hit rate training data
predlda.df_skstd<-predict(lda.log.std.out,df_skstd)
tab<-table(College$Private,predlda.df_skstd$class)
sum(diag(tab))/sum(tab)
df_skstd_hitrate<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-lda(df_skstd,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCVhitrate_log_std<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_log_std<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_log_std<-tab[1,1]/sum(tab[1,])
#loocv hit rate, with equal prior prob
pred.loocv<-lda(df_skstd,College$Private, CV=TRUE, prior=c(1,1)/2)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCVhitrate_eqprior_log_std<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity_eqprior_log_std<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity_eqprior_log_std<-tab[1,1]/sum(tab[1,])


LDA_Summary<-c("Model", "Training Hit Rate", "LOOCV Hit Rate", "Sensitivity", "specificity", "LOOCV Hit Rate with equal prior", "Sensitivity with equal prior", "specificity with equal prior")
Normal<-c("Normal", normal_hitrate, loocvhitrate_LDA, loocv_sensitivity_LDA, loocv_specificity_LDA, loocvhitrate_eqprior_LDA, loocv_sensitivity_eqprior_LDA, loocv_specificity_eqprior_LDA )
Center<-c("Centered", data_c_hitrate, LOOCVhitrate_center, LOOCV_sensitivity_center, LOOCV_specificity_center, LOOCVhitrate_eqprior_center, LOOCV_sensitivity_eqprior_center, LOOCV_specificity_eqprior_center )
center_log<-c("center_logtransformed", df_skc_hitrate, LOOCVhitrate_center_log, LOOCV_sensitivity_center_log, LOOCV_specificity_center_log, LOOCVhitrate_eqprior_center_log, LOOCV_sensitivity_eqprior_center_log, LOOCV_specificity_eqprior_center_log )
Std<-c("stded", data_s_hitrate, LOOCVhitrate_std, LOOCV_sensitivity_std, LOOCV_specificity_std, LOOCVhitrate_eqprior_std, LOOCV_sensitivity_eqprior_std, LOOCV_specificity_eqprior_std )
log_std<-c("log_stded", df_skstd_hitrate, LOOCVhitrate_log_std, LOOCV_sensitivity_log_std, LOOCV_specificity_log_std, LOOCVhitrate_eqprior_log_std, LOOCV_sensitivity_eqprior_log_std, LOOCV_specificity_eqprior_log_std )
LDA_summary1<-cbind.data.frame(LDA_Summary, Normal, Center, center_log, Std, log_std)
LDA_summary1
##BOX Test
boxM(College[, 2:18], College[, "Private"]) 


#QDA 
qda.out<-qda(College[,2:18],College$Private)
print(qda.out)

#hit rate training data
predqda.train<-predict(qda.out,College[,2:18])
tab<-table(College$Private,predqda.train$class)
sum(diag(tab))/sum(tab)
qda_0HR<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-qda(College[,2:18],College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCV_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity<-tab[1,1]/sum(tab[1,])
#loocv hit rate, with equal prior prob
pred.loocv<-qda(College[,2:18],College$Private, CV=TRUE, prior=c(1,1)/2)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
#Sensitivity 
tab[2,2]/sum(tab[2,])
#specificity
tab[1,1]/sum(tab[1,])




####Centered variables

qda.out<-qda(data_c,College$Private)
print(qda.out)

#hit rate training data
predqda.train<-predict(qda.out,data_c)
tab<-table(College$Private,predqda.train$class)
sum(diag(tab))/sum(tab)
qda_1HR<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-qda(data_c,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCV_HR1<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity1<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity1<-tab[1,1]/sum(tab[1,])

#####Log transformed and centered
qda.out<-qda(df_skc,College$Private)
print(qda.out)

#hit rate training data
predqda.train<-predict(qda.out,df_skc)
tab<-table(College$Private,predqda.train$class)
sum(diag(tab))/sum(tab)
qda_2HR<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-qda(df_skc,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCV_HR2<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity2<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity2<-tab[1,1]/sum(tab[1,])

####Standardizeded variables

qda.out<-qda(data_s,College$Private)
print(qda.out)

#hit rate training data
predqda.train<-predict(qda.out,data_s)
tab<-table(College$Private,predqda.train$class)
sum(diag(tab))/sum(tab)
qda_3HR<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-qda(data_s,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCV_HR3<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity3<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity3<-tab[1,1]/sum(tab[1,])

#####Log transformed and standardized
qda.out<-qda(df_skstd,College$Private)
print(qda.out)
#hit rate training data
predqda.train<-predict(qda.out,df_skstd)
tab<-table(College$Private,predqda.train$class)
sum(diag(tab))/sum(tab)
qda_4HR<-sum(diag(tab))/sum(tab)

#loocv hit rate, with prior prob
pred.loocv<-qda(df_skstd,College$Private, CV=TRUE)
tab<-table(College$Private,pred.loocv$class)
sum(diag(tab))/sum(tab)
LOOCV_HR4<-sum(diag(tab))/sum(tab)
#Sensitivity 
LOOCV_sensitivity4<-tab[2,2]/sum(tab[2,])
#specificity
LOOCV_specificity4<-tab[1,1]/sum(tab[1,])

QDA_Summary<-c("Model", "Training Hit Rate", "LOOCV Hit Rate", "Sensitivity", "specificity")
Normal<-c("Normal", qda_0HR, LOOCV_HR0, LOOCV_sensitivity, LOOCV_specificity)
Center<-c("Centered", qda_1HR, LOOCV_HR1, LOOCV_sensitivity1, LOOCV_specificity1)
center_log<-c("Log Centered", qda_2HR, LOOCV_HR2, LOOCV_sensitivity2, LOOCV_specificity2)
Std<-c("Standardized", qda_3HR, LOOCV_HR3, LOOCV_sensitivity3, LOOCV_specificity3)
log_std<-c("Log Standardized", qda_4HR, LOOCV_HR4, LOOCV_sensitivity4, LOOCV_specificity4)
QDA_summary1<-cbind.data.frame(QDA_Summary, Normal, Center, center_log, Std, log_std)
QDA_summary1

#KNN
#plot knn classification for different values of k
#training and test error rate as a function of k
set.seed(1)
knnmax<-100

err<-matrix(rep(0,knnmax),nrow=knnmax)
for (j in 1:knnmax){
  predknn.loocv<- knn.cv(College[,2:18], College$Private, k=j)
  err[j,1]<-1-hitratknn(College$Private,predknn.loocv)
}
which.min(err) #test error is minimum for obs no. 12
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.1),col="red",type="b",xlab="K",ylab="error")
lines(c(1:knnmax),err[,1],col="red")

## K=12
predknn.loocv<- knn.cv(College[,2:18], College$Private, k=12)
loocv_err<-1-hitratknn(College$Private,predknn.loocv)
tab<-table(College$Private,predknn.loocv)
knn_hitrate<-sum(diag(tab))/sum(tab)
#Sensitivity 
sensitivity_knn<-tab[2,2]/sum(tab[2,])
#specificity
specificity_knn<-tab[1,1]/sum(tab[1,])

#######Centered data#######
set.seed(1)
knnmax<-100

err<-matrix(rep(0,knnmax),nrow=knnmax)
for (j in 1:knnmax){
  predknn.loocv<- knn.cv(data_c, College$Private, k=j)
  err[j,1]<-1-hitratknn(College$Private,predknn.loocv)
}
which.min(err) #test error is minimum for obs no. 6
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.1),col="red",type="b",xlab="K",ylab="error")
lines(c(1:knnmax),err[,1],col="red")

## K=6
predknn.loocv<- knn.cv(data_c, College$Private, k=6)
loocv_err<-1-hitratknn(College$Private,predknn.loocv)
tab<-table(College$Private,predknn.loocv)
knn_hitrate_center<-sum(diag(tab))/sum(tab)
#Sensitivity 
sensitivity_knn_center<-tab[2,2]/sum(tab[2,])
#specificity
specificity_knn_center<-tab[1,1]/sum(tab[1,])


#####Log centered####
set.seed(1)
knnmax<-100

err<-matrix(rep(0,knnmax),nrow=knnmax)
for (j in 1:knnmax){
  predknn.loocv<- knn.cv(df_skc, College$Private, k=j)
  err[j,1]<-1-hitratknn(College$Private,predknn.loocv)
}
which.min(err) #test error is minimum for obs no. 10
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.1),col="red",type="b",xlab="K",ylab="error")
lines(c(1:knnmax),err[,1],col="red")

## K=10
predknn.loocv<- knn.cv(df_skc, College$Private, k=10)
loocv_err<-1-hitratknn(College$Private,predknn.loocv)
tab<-table(College$Private,predknn.loocv)
knn_hitrate_center_log<-sum(diag(tab))/sum(tab)
#Sensitivity 
sensitivity_knn_center_log<-tab[2,2]/sum(tab[2,])
#specificity
specificity_knn_center_log<-tab[1,1]/sum(tab[1,])

########Standardized#####
set.seed(1)
knnmax<-100

err<-matrix(rep(0,knnmax),nrow=knnmax)
for (j in 1:knnmax){
  predknn.loocv<- knn.cv(data_s, College$Private, k=j)
  err[j,1]<-1-hitratknn(College$Private,predknn.loocv)
}
which.min(err) #test error is minimum for obs no. 6
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.1),col="red",type="b",xlab="K",ylab="error")
lines(c(1:knnmax),err[,1],col="red")

## K=6
predknn.loocv<- knn.cv(data_s, College$Private, k=6)
loocv_err<-1-hitratknn(College$Private,predknn.loocv)
tab<-table(College$Private,predknn.loocv)
knn_hitrate_std<-sum(diag(tab))/sum(tab)
#Sensitivity 
sensitivity_knn_std<-tab[2,2]/sum(tab[2,])
#specificity
specificity_knn_std<-tab[1,1]/sum(tab[1,])

######log standardized###
set.seed(1)
knnmax<-100

err<-matrix(rep(0,knnmax),nrow=knnmax)
for (j in 1:knnmax){
  predknn.loocv<- knn.cv(df_skstd, College$Private, k=j)
  err[j,1]<-1-hitratknn(College$Private,predknn.loocv)
}
which.min(err) #test error is minimum for obs no. 10
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.1),col="red",type="b",xlab="K",ylab="error")
lines(c(1:knnmax),err[,1],col="red")

## K=10
predknn.loocv<- knn.cv(df_skstd, College$Private, k=10)
loocv_err<-1-hitratknn(College$Private,predknn.loocv)
tab<-table(College$Private,predknn.loocv)
knn_hitrate_log_std<-sum(diag(tab))/sum(tab)
#Sensitivity 
sensitivity_knn_log_std<-tab[2,2]/sum(tab[2,])
#specificity
specificity_knn_log_std<-tab[1,1]/sum(tab[1,])

#######Table########
knn_Summary<-c("Model", "LOOCV Hit Rate", "Sensitivity", "specificity")
Normal<-c("Normal", knn_hitrate, sensitivity_knn, specificity_knn)
Center<-c("Centered",knn_hitrate_center, sensitivity_knn_center, specificity_knn_center)
center_log<-c("Log Centered", knn_hitrate_center_log, sensitivity_knn_center_log, specificity_knn_center_log)
Std<-c("Standardized", knn_hitrate_std, sensitivity_knn_std, specificity_knn_std)
log_std<-c("Log Standardized", knn_hitrate_log_std, sensitivity_knn_log_std, specificity_knn_log_std)
knn_summary1<-cbind.data.frame(knn_Summary, Normal, Center, center_log, Std, log_std)
knn_summary1
#####Comparison########
plot(-10,-10,xlim=c(1,knnmax),ylim=c(0,0.1),col="red",type="b",xlab="K",ylab="LOOCV error")
lines(c(1:knnmax),err[,1],col="red")
abline(a=1-LOOCV_HR4,b=0, col="blue")
abline(a=1-LOOCVhitrate_log_std, b=0,col="green")
legend("bottomright",c("LOOCV Error KNN", "LOOCV
error QDA", "LOOCV Error LDA"),lty=c(1,1,1),col=c("red","blue","green"))


plot(-10,-10,xlim=c(1,knnmax),ylim=c(0.9,1),col="red",type="b",xlab="K",ylab="Sensitivity")
abline(b=0,a=(sensitivity_knn_log_std),col="red")
abline(a=LOOCV_sensitivity4,b=0, col="blue")
abline(a=LOOCV_sensitivity_log_std, b=0,col="green")
legend("bottomright",c("KNN with k=10", "QDA", "LDA"),lty=c(1,1,1),col=c("red","blue","green"))

plot(-10,-10,xlim=c(1,knnmax),ylim=c(0.7,1),col="red",type="b",xlab="K",ylab="Specificity")
abline(b=0,a=(specificity_knn_log_std),col="red")
abline(a=LOOCV_specificity4,b=0, col="blue")
abline(a=LOOCV_specificity_log_std, b=0,col="green")
legend("bottomright",c("KNN with k=10", "QDA", "LDA"),lty=c(1,1,1),col=c("red","blue","green"))




######################
###bagging####
#################


####################no changes########


set.seed(1)
rf.mod=randomForest( as.factor(target.train) ~ .,data=data.train[,1:17],mtry=17,ntree=1000,importance=TRUE)
rf.mod

#predictions training data
pred.train<-predict(rf.mod,newdata=data.train,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])






#predictions test data
pred.test<-predict(rf.mod,newdata=data.test,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])
rf.test.unequal<-performance(tab)






#########Centered################
set.seed(1)
rf.mod=randomForest( as.factor(target.train) ~ .,data=data_ctrain[,1:17],mtry=17,ntree=1000,importance=TRUE)
rf.mod

#predictions training data
pred.train<-predict(rf.mod,newdata=data_ctrain,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])




#predictions test data
pred.test<-predict(rf.mod,newdata=data_ctest,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])
rf.test.unequal<-performance(tab)




########Standardized###########


set.seed(1)
rf.mod=randomForest(as.factor(target.train)~.,data=data_strain[,1:17],mtry=17,ntree=388,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=data_strain,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])
#unequal classification costs





#predictions test data
pred.test<-predict(rf.mod,newdata=data_stest,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])


rf.test.equal<-performance(tab)
#unequal classification costs

rf.test.unequal<-performance(tab)


#########log centered###############
df_sktrc<-scale(df_sktr[,1:17],center=TRUE)
df_sktstc<-scale(df_sktst[,1:17],center=TRUE)

set.seed(1)
rf.mod=randomForest(as.factor(target.train)~.,data=df_sktrc,mtry=17,ntree=389,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=df_sktrc,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#unequal classification costs

rf.train.unequal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



#predictions test data
pred.test<-predict(rf.mod,newdata=df_sktstc,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)
#unequal classification costs

#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



########LOG Standardized###########
df_sktrc<-scale(df_sktr[,1:17],center=TRUE, scale= TRUE)
df_sktstc<-scale(df_sktst[,1:17],center=TRUE, scale=TRUE)

set.seed(1)
rf.mod=randomForest(as.factor(target.train)~.,data=df_sktrc,mtry=17,ntree=389,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=df_sktrc,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)


#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



#predictions test data
pred.test<-predict(rf.mod,newdata=df_sktstc,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)

#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])




######################
###Random forrest####
####################


####################no changes########


set.seed(1)
rf.mod=randomForest( as.factor(target.train) ~ .,data=data.train[,1:17],mtry=8,ntree=1000,importance=TRUE)
rf.mod

#predictions training data
pred.train<-predict(rf.mod,newdata=data.train,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])






#predictions test data
pred.test<-predict(rf.mod,newdata=data.test,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])
rf.test.unequal<-performance(tab)






#########Centered################
set.seed(1)
rf.mod=randomForest( as.factor(target.train) ~ .,data=data_ctrain[,1:17],mtry=8,ntree=1000,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=data_ctrain,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])




#predictions test data
pred.test<-predict(rf.mod,newdata=data_ctest,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



########Standardized###########


set.seed(1)
rf.mod=randomForest(as.factor(target.train)~.,data=data_strain[,1:17],mtry=8,ntree=388,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=data_strain,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])




#predictions test data
pred.test<-predict(rf.mod,newdata=data_stest,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])
rf.test.equal<-performance(tab)



#########log centered###############
df_sktrc<-scale(df_sktr[,1:17],center=TRUE)
df_sktstc<-scale(df_sktst[,1:17],center=TRUE)

set.seed(1)
rf.mod=randomForest(as.factor(target.train)~.,data=df_sktrc,mtry=8,ntree=389,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=df_sktrc,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)

#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



#predictions test data
pred.test<-predict(rf.mod,newdata=df_sktstc,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)
#unequal classification costs


#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



########LOG Standardized###########
df_sktrc<-scale(df_sktr[,1:17],center=TRUE, scale= TRUE)
df_sktstc<-scale(df_sktst[,1:17],center=TRUE, scale=TRUE)

set.seed(1)
rf.mod=randomForest(as.factor(target.train)~.,data=df_sktrc,mtry=8,ntree=389,importance=TRUE)
rf.mod


#predictions training data
pred.train<-predict(rf.mod,newdata=df_sktrc,type="prob")

#Bayes classifier
class.train<-ifelse(pred.train>0.5,1,0)
tab<-table(target.train,class.train[,2])
rf.train.equal<-performance(tab)

#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])



#predictions test data
pred.test<-predict(rf.mod,newdata=df_sktstc,type="prob")
#Bayes classifier
class.test<-ifelse(pred.test>0.5,1,0)
tab<-table(target.test,class.test[,2])
rf.test.equal<-performance(tab)

#hitrate test data
Bagging_HR0<-sum(diag(tab))/sum(tab)
#Sensitivity 
Bagging_sensitivity<-tab[2,2]/sum(tab[2,])
#specificity
Bagging_specificity<-tab[1,1]/sum(tab[1,])















