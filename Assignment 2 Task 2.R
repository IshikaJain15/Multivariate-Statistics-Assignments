#################################
############ TASK 2 #############     
#################################

rm(list=ls())

setwd("C:/Users/anaso/Desktop/SOFIA MENDES/KU Leuven/Year 1/Semester 1/Multivariate Statistics/Assignment 2")   #!!CHANGE THIS!!


library(MASS)
library(HDclassif) 
library(mclust) 
#library(CrossClustering) 


load("fashion.RData")

X<-scale(train.data,center=TRUE,scale=FALSE) #we want unstandardized PC
class<-train.target  #class labels "0", "1" and "7"

 




#### QUESTION a) ####
set.seed(1)


### using HDClassif
dim <- c(2,3,4,5,6)

### with model "AkjBkQkD"
ari_model1 <- rep(0,5)

for (i in dim){
hddc_model1.out<-hddc(X,K=3,model="AkjBkQkD", com_dim=i) 
hddc_model1.out
#compute classification table 
tab1<-table(hddc_model1.out$class,class) 
tab1
#adjusted rand index 
j <- i-1
ari_model1[j] <- adjustedRandIndex(hddc_model1.out$class,class)} #loop takes some time to run

print(ari_model1)



### same process with model "AkjBQkD"
ari_model2 <- rep(0,5)

for (i in dim){
hddc_model2.out<-hddc(X,K=3,model="AkjBQkD", com_dim=i)
hddc_model2.out
tab6<-table(hddc_model2.out$class,class) 
hddc_model2.out
j <- i-1 
ari_model2[j] <- adjustedRandIndex(hddc_model2.out$class,class)}   

print(ari_model2)



#best HDDClassif model (AkjBQkD with com_dim=4) - further analysis
hddc_best.out<-hddc(X,K=3,model="AkjBQkD", com_dim=4)
hddc_best.out
table_hddc<-table(hddc_best.out$class,class)
mapClass(hddc_best.out$class,class)
table_hddc<-table_hddc[c(2,1,3),]  
table_hddc
#percentage of correct classification 
sum(diag(table_hddc))/sum(table_hddc)    #0.9782222

hddc_best.out$loglik
hddc_best.out$complexity
hddc_best.out$BIC


#table with the results
model <- rbind("","AkjBkQkD","AkjBQkD")
cdim <- c("n=2","n=3","n=4","n=5","n=6")
step <- rbind(cdim,ari_model1, ari_model2)
results_hddc <- cbind(model,step )
results_hddc





### using MClust
prcomp.out<-prcomp(X) 
plot(prcomp.out$sd^2) 
cumsum(prcomp.out$sd^2/sum(prcomp.out$sd^2)) 
#first 2 components account for 59% of the variance 
#first 3 components account for 65% of the variance 
#first 4 components account for 68% of the variance 
#first 5 components account for 71% of the variance 
#first 6 components account for 72% of the variance 

comp<-X%*%prcomp.out$rotation 

indices <- c(2,3,4,5,6)

#3-cluster model for VVE model
ari_vve <- rep(0,5)
for (i in indices){
mclust_vve.out<-Mclust(comp[,1:i],G=3, modelNames="VVE") 
summary(mclust_vve.out)
tab1m<-table(class,mclust_vve.out$classification) 
tab1m
mapClass(mclust_vve.out$class,class)
j <- i-1
ari_vve[j] <- adjustedRandIndex(mclust_vve.out$class,class)}  #loop takes some time to run

print(ari_vve)



#for VEV model
ari_vev <- rep(0,5)
for (i in indices){
mclust_vev.out<-Mclust(comp[,1:i],G=3, modelNames="VEV")
summary(mclust_vev.out)
tab2m<-table(class,mclust_vev.out$classification) 
tab2m
mapClass(mclust_vev.out$class,class)    
j <- i-1
ari_vev[j] <- adjustedRandIndex(mclust_vev.out$class,class)}    

print(ari_vev)


#for EVV model
ari_evv <- rep(0,5)
for (i in indices){
mclust_evv.out<-Mclust(comp[,1:i],G=3, modelNames="EVV")
summary(mclust_evv.out)
tab3m<-table(class,mclust_evv.out$classification) 
tab3m
mapClass(mclust_evv.out$class,class)    
j <- i-1
ari_evv[j] <- adjustedRandIndex(mclust_evv.out$class,class)}    

print(ari_evv)


#for VVV model
ari_vvv <- rep(0,5)
for (i in indices){
mclust_vvv.out<-Mclust(comp[,1:i],G=3, modelNames="VVV")
summary(mclust_vvv.out)
tab3m<-table(class,mclust_vvv.out$classification) 
tab3m
mapClass(mclust_vvv.out$class,class)    
j <- i-1
ari_vvv[j] <- adjustedRandIndex(mclust_vvv.out$class,class)}    

print(ari_vvv)




#best MClust model (EVV with 6 first PC) - further analysis
mclust_best.out<-Mclust(comp[,1:6],G=3, modelNames="EVV")
summary(mclust_best.out)
table_mclust<-table(class,mclust_best.out$classification)
mapClass(mclust_best.out$class,class)
table_mclust<-table_mclust[c(2,1,3),]  
table_mclust
#percentage of correct classification 
sum(diag(table_mclust))/sum(table_mclust)   #0.9790556
#summary() also gives these results
mclust_best.out$loglik
mclust_best.out$df
mclust_best.out$BIC



#table with the results
model2 <- rbind("","VVE","VEV","EVV","VVV")
pc <- cbind("first 2 PC","fisrt 3 PC","first 4 PC","first 5 PC","first 6 PC")
step2 <- rbind(pc, ari_vve, ari_vev, ari_evv, ari_vvv)
results_mclust <- cbind(model2,step2 )
results_mclust








#### QUESTION b) ####


# BEST MODEL BETWEEN HDCLASSIF AND MCLUST: EVV with 6 first PC

### PCA plot
#conduct PCA on the data - done before (not necessary)
prcomp.out<-prcomp(X) 
comp<-as.matrix(X)%*%prcomp.out$rotation 
round(head(prcomp.out$sd^2/sum(prcomp.out$sd^2)),3)
#VAF components
round(head(prcomp.out$sd^2/sum(prcomp.out$sd^2)),3)

#comp<-predict(prcomp.out,X)

par(mfrow=c(1,2)) 

#plot observed clusters 
plot(comp,main="observed clusters") 
points(comp[class==0,],col="yellow",pch=5)
points(comp[class==1,],col="red",pch=5) 
points(comp[class==7,],col="black",pch=5) 
legend("topleft",c("T-Shirt/Top","Trouser","Sneaker"),col=c("yellow","red","black"),pch=c(19,19,19),bty="n")


#plot clusters extracted with HDDC in space of first two principal components 
#use the clustering model with the best value of ARI
plot(comp,main="derived clusters") 
points(comp[mclust_best.out$class==2,],col="yellow",pch=5)  
points(comp[mclust_best.out$class==1,],col="red",pch=5) 
points(comp[mclust_best.out$class==3,],col="black",pch=5)  
legend("topleft",c("T-Shirt/Top","Trouser","Sneaker"),col=c("yellow","red","black"),pch=c(19,19,19),bty="n")


#standardize training data
X_std <-scale(train.data,center=TRUE,scale=TRUE) 
prcomp.out_std<-prcomp(X_std) 

#VAF components
round(head(prcomp.out_std$sd^2/sum(prcomp.out_std$sd^2)),3)
