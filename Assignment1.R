###########################
#          TASK 1         #
###########################

#libraries
library(lavaan)
library(psych)
library(GPArotation)

#Setting working directory and loading the data
setwd("C:/Users/anaso/Desktop/SOFIA MENDES/KU Leuven/Year 1/Semester 1/Multivariate Statistics/Assignment 1")
load("cosmetics.Rdata")

#function composite reliability
comp_rel<-function(x){
  A<-(sum(x))^2
  B<-sum(1-x^2)
  return(A/(A+B))
}

#compute centered data
ccos<-cosmetics
ccos[,1:18]<-scale(cosmetics[,1:18],center=TRUE,scale=FALSE)



################
## QUESTION A ##
################

# step 1

#compute covariance matrix and correlation matrix
covmat<-cov(ccos[,1:9])
cormat<-cor(ccos[,1:9])
sdvar<-apply(ccos[,1:9],2,sd)

#Confirmatory Factor Analysis
cfa1<- 'Att_organic=~NA*Attitude_organic1+Attitude_organic2+Attitude_organic3 
Att_packaging=~NA*Attitude_packaging1+Attitude_packaging2+Attitude_packaging3
Att_crueltyfree=~NA*Attitude_crueltyfree1+Attitude_crueltyfree2+Attitude_crueltyfree3
Att_organic ~~1*Att_organic     
Att_packaging ~~1*Att_packaging
Att_crueltyfree ~~1*Att_crueltyfree
Att_organic ~~Att_packaging
Att_packaging ~~Att_crueltyfree
Att_crueltyfree~~Att_organic'

#fit model on covariance matrix
fitcfa1<-cfa(cfa1,data=ccos, sample.cov=covmat,sample.nobs=150)
#summary of results
summary(fitcfa1,fit.measures=TRUE)
#print fit measures
fitmeasures(fitcfa1,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))

#ask for standardized solution
standardizedSolution(fitcfa1)
modificationIndices(fitcfa1)

d<-standardizedSolution(fitcfa1)
#composite reliability Att_Organic
comp_rel(d[1:3,4])
#composite reliability Att_packaging
comp_rel(d[4:6,4])
#composite reliability Att_crueltyfree
comp_rel(d[7:9,4])


#Overview of composite reliability
factorscore<-c("Att_Organic","Att_packaging","Att_crueltyfree")
reliability<-round(c(comp_rel(d[1:3,4]),comp_rel(d[4:6,4]),comp_rel(d[7:9,4])),3)
data.frame(factorscore,reliability)




#step2

#Confirmatory Factor Analysis
cfa2<- 'Att_organic=~NA*Attitude_organic1+Attitude_organic2+Attitude_organic3  #add att_packaging2 modindices value >10
Att_packaging=~NA*Attitude_packaging1+Attitude_packaging2+Attitude_packaging3
Att_crueltyfree=~NA*Attitude_crueltyfree1+Attitude_crueltyfree2+Attitude_crueltyfree3
Att_organic ~~1*Att_organic
Att_packaging ~~1*Att_packaging
Att_crueltyfree ~~1*Att_crueltyfree
Att_organic ~~Att_packaging
Att_packaging ~~Att_crueltyfree
Att_crueltyfree~~Att_organic
Attitude_organic1  ~~c*Attitude_packaging1
Attitude_organic1  ~~c*Attitude_crueltyfree1
Attitude_crueltyfree1 ~~c*Attitude_packaging1
Attitude_organic2 ~~d*Attitude_packaging2
Attitude_organic2 ~~d*Attitude_crueltyfree2
Attitude_crueltyfree2 ~~d*Attitude_packaging2
Attitude_organic3 ~~e*Attitude_packaging3
Attitude_organic3 ~~e*Attitude_crueltyfree3
Attitude_crueltyfree3 ~~e*Attitude_packaging3'


#fit model on covariance matrix
fitcfa2<-cfa(cfa2,data=ccos, sample.cov=covmat,sample.nobs=150)
#summary of results
summary(fitcfa2,fit.measures=TRUE)
#print fit measures
fitmeasures(fitcfa2,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))

#print standardized solution
standardizedSolution(fitcfa2)
modificationIndices(fitcfa2)

d2<-standardizedSolution(fitcfa2)
#composite reliability Att_Organic
comp_rel(d2[1:3,5])
#composite reliability Att_packaging
comp_rel(d2[4:6,5])
#composite reliability Att_crueltyfree
comp_rel(d2[7:9,5])


#Overview of composite reliability
factorscore<-c("Att_Organic","Att_packaging","Att_crueltyfree")
reliability2<-round(c(comp_rel(d2[1:3,5]),comp_rel(d2[4:6,5]),comp_rel(d2[7:9,5])),3)
data.frame(factorscore,reliability2)



#step 3

#comparing fit
fitmeasures1=fitmeasures(fitcfa1,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))
fitmeasures2=fitmeasures(fitcfa2,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))
fit1<-rbind(fitmeasures1,fitmeasures2)
rownames(fit1)<-c("cfa model Att","cfa extended model Att")
chidf<-fit1[,1]/fit1[,2]
fit1<-cbind(fit1,chidf)
round(fit1, 3)





################
## QUESTION B ##
################

# Step 1

covmat<-cov(ccos[,10:18])
cormat<-cor(ccos[,10:18])
sdvar<-apply(ccos[,10:18],2,sd)

#Confirmatory Factor Analysis
cfab1<- 'BI_organic=~NA*BI_organic1+BI_organic2+BI_organic3 
BI_packaging=~NA*BI_packaging1+BI_packaging2+BI_packaging3
BI_crueltyfree=~NA*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3
BI_organic ~~1*BI_organic
BI_packaging ~~1*BI_packaging
BI_crueltyfree ~~1*BI_crueltyfree
BI_organic ~~BI_packaging
BI_packaging ~~BI_crueltyfree
BI_crueltyfree ~~BI_organic'


#fit model on covariance matrix
fitcfab1<-cfa(cfab1,data=ccos, sample.cov=covmat,sample.nobs=150)
#summary of results
summary(fitcfab1,fit.measures=TRUE)
#print fit measures
fitmeasures(fitcfab1,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))

#ask for standardized solution
standardizedSolution(fitcfab1)
modificationIndices(fitcfab1)

e<-standardizedSolution(fitcfab1)
#composite reliability BI_Organic
comp_rel(e[1:3,4])
#composite reliability BI_packaging
comp_rel(e[4:6,4])
#composite reliability BI_crueltyfree
comp_rel(e[7:9,4])

#Overview of composite reliability
factorscoreb<-c("BI_Organic","BI_packaging","BI_crueltyfree")
reliabilityb<-round(c(comp_rel(e[1:3,4]),comp_rel(e[4:6,4]),comp_rel(e[7:9,4])),3)
data.frame(factorscoreb,reliabilityb)



#step 2

#Confirmatory Factor Analysis
cfab2<- 'BI_organic=~NA*BI_organic1+BI_organic2+BI_organic3 
BI_packaging=~NA*BI_packaging1+BI_packaging2+BI_packaging3
BI_crueltyfree=~NA*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3
BI_organic ~~1*BI_organic
BI_packaging ~~1*BI_packaging
BI_crueltyfree ~~1*BI_crueltyfree
BI_organic ~~BI_packaging
BI_packaging ~~BI_crueltyfree
BI_crueltyfree ~~BI_organic
BI_organic1  ~~c*BI_packaging1
BI_organic1  ~~c*BI_crueltyfree1
BI_crueltyfree1 ~~c*BI_packaging1
BI_organic2 ~~d*BI_packaging2
BI_organic2 ~~d*BI_crueltyfree2
BI_crueltyfree2 ~~d*BI_packaging2
BI_organic3 ~~e*BI_packaging3
BI_organic3 ~~e*BI_crueltyfree3
BI_crueltyfree3 ~~e*BI_packaging3'



#fit model on covariance matrix
fitcfab2<-cfa(cfab2,data=cosmetics, sample.cov=covmat,sample.nobs=150)
#summary of results
summary(fitcfab2,fit.measures=TRUE)
#print fit measures
fitmeasures(fitcfab2,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))

#ask for standardized solution
standardizedSolution(fitcfab2)
modificationIndices(fitcfab2)


e2<-standardizedSolution(fitcfab2)
#composite reliability social trust
comp_rel(e2[1:3,5])
#composite reliability satisfaction country
comp_rel(e2[4:6,5])
#composite reliability job autonomy
comp_rel(e2[7:9,5])


#Overview of composite reliability
factorscoreb<-c("BI_Organic","BI_packaging","BI_crueltyfree")
reliabilityb2<-round(c(comp_rel(e2[1:3,5]),comp_rel(e2[4:6,5]),comp_rel(e2[7:9,5])),3)
data.frame(factorscoreb,reliabilityb2)



#step 3

#comparing fit
fitmeasuresb1=fitmeasures(fitcfab1,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))
fitmeasuresb2=fitmeasures(fitcfab2,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))
fit2<-rbind(fitmeasuresb1,fitmeasuresb2)
rownames(fit2)<-c("cfa model BI","cfa extended model BI")
chidf<-fit2[,1]/fit2[,2]
fit2<-cbind(fit2,chidf)
round(fit2, 3)





################
## QUESTION C ##
################

# Step 1

#compute covariance matrix and correlation matrix
covmatc<-cov(ccos)
cormatc<-cor(ccos)
sdvarc<-apply(ccos[,1:18],2,sd)

#specify structural equation model
sem1<-'#measurement model
Att_organic=~NA*Attitude_organic1+Attitude_organic2+Attitude_organic3 
Att_packaging=~NA*Attitude_packaging1+Attitude_packaging2+Attitude_packaging3
Att_crueltyfree=~NA*Attitude_crueltyfree1+Attitude_crueltyfree2+Attitude_crueltyfree3
Att_organic ~~Att_packaging
Att_packaging ~~Att_crueltyfree
Att_crueltyfree~~Att_organic
Attitude_organic1  ~~c*Attitude_packaging1
Attitude_organic1  ~~c*Attitude_crueltyfree1
Attitude_crueltyfree1 ~~c*Attitude_packaging1
Attitude_organic2 ~~d*Attitude_packaging2
Attitude_organic2 ~~d*Attitude_crueltyfree2
Attitude_crueltyfree2 ~~d*Attitude_packaging2
Attitude_organic3 ~~e*Attitude_packaging3
Attitude_organic3 ~~e*Attitude_crueltyfree3
Attitude_crueltyfree3 ~~e*Attitude_packaging3

BI_organic=~1*BI_organic1+BI_organic2+BI_organic3 
BI_packaging=~1*BI_packaging1+BI_packaging2+BI_packaging3
BI_crueltyfree=~1*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3
BI_organic ~~BI_packaging
BI_packaging ~~BI_crueltyfree
BI_crueltyfree ~~BI_organic
BI_organic1  ~~f*BI_packaging1
BI_organic1  ~~f*BI_crueltyfree1
BI_crueltyfree1 ~~f*BI_packaging1
BI_organic2 ~~g*BI_packaging2
BI_organic2 ~~g*BI_crueltyfree2
BI_crueltyfree2 ~~g*BI_packaging2
BI_organic3 ~~h*BI_packaging3
BI_organic3 ~~h*BI_crueltyfree3
BI_crueltyfree3 ~~h*BI_packaging3

#structural model
BI_organic ~Att_organic
BI_packaging~ Att_packaging
BI_crueltyfree~ Att_crueltyfree
 
#variances latent variables
Att_organic ~~1*Att_organic
Att_packaging ~~1*Att_packaging
Att_crueltyfree ~~1*Att_crueltyfree
BI_organic ~~BI_organic
BI_packaging ~~BI_packaging
BI_crueltyfree ~~BI_crueltyfree'


#print model output
fitsem1<-sem(sem1,ccos)
summary(fitsem1,std=TRUE)

#print fit measures
fitmeasures(fitsem1,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))

#ask for standardized solution
standardizedSolution(fitsem1)
modificationIndices(fitsem1)




# Step 2 

covmatc<-cov(ccos)
cormatc<-cor(ccos)
sdvarc<-apply(ccos[,1:18],2,sd)

#specify structural equation model
sem2<-'#measurement model
Att_organic=~NA*Attitude_organic1+Attitude_organic2+Attitude_organic3 
Att_packaging=~NA*Attitude_packaging1+Attitude_packaging2+Attitude_packaging3
Att_crueltyfree=~NA*Attitude_crueltyfree1+Attitude_crueltyfree2+Attitude_crueltyfree3
Att_organic ~~Att_packaging
Att_packaging ~~Att_crueltyfree
Att_crueltyfree~~Att_organic
Attitude_organic1  ~~c*Attitude_packaging1
Attitude_organic1  ~~c*Attitude_crueltyfree1
Attitude_crueltyfree1 ~~c*Attitude_packaging1
Attitude_organic2 ~~d*Attitude_packaging2
Attitude_organic2 ~~d*Attitude_crueltyfree2
Attitude_crueltyfree2 ~~d*Attitude_packaging2
Attitude_organic3 ~~e*Attitude_packaging3
Attitude_organic3 ~~e*Attitude_crueltyfree3
Attitude_crueltyfree3 ~~e*Attitude_packaging3

BI_organic=~1*BI_organic1+BI_organic2+BI_organic3 
BI_packaging=~1*BI_packaging1+BI_packaging2+BI_packaging3
BI_crueltyfree=~1*BI_crueltyfree1+BI_crueltyfree2+BI_crueltyfree3
BI_organic ~~BI_packaging
BI_packaging ~~BI_crueltyfree
BI_crueltyfree ~~BI_organic
BI_organic1  ~~f*BI_packaging1
BI_organic1  ~~f*BI_crueltyfree1
BI_crueltyfree1 ~~f*BI_packaging1
BI_organic2 ~~g*BI_packaging2
BI_organic2 ~~g*BI_crueltyfree2
BI_crueltyfree2 ~~g*BI_packaging2
BI_organic3 ~~h*BI_packaging3
BI_organic3 ~~h*BI_crueltyfree3
BI_crueltyfree3 ~~h*BI_packaging3

 #structural model
 BI_organic ~z*Att_organic
 BI_packaging~z*Att_packaging
 BI_crueltyfree~z*Att_crueltyfree
 
 #variances latent variables
Att_organic ~~1*Att_organic
Att_packaging ~~1*Att_packaging
Att_crueltyfree ~~1*Att_crueltyfree
BI_organic ~~BI_organic
BI_packaging ~~BI_packaging
BI_crueltyfree ~~BI_crueltyfree'


#print model output
fitsem2<-sem(sem2,sample.cov=covmatc,sample.nobs=150)
summary(fitsem2,std=TRUE)

#standardized solution
standardizedSolution(fitsem2)

#print fit measures
fitmeasures(fitsem2,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))

#ask for standardized solution
standardizedSolution(fitsem2)
modificationIndices(fitsem2)



#step 3

#comparing fit
fitmeasuress1=fitmeasures(fitsem1,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))
fitmeasuress2=fitmeasures(fitsem2,c("chisq","df","pvalue","gfi","agfi","cfi","tli","rmsea","srmr"))
fit3<-rbind(fitmeasuress1,fitmeasuress2)
rownames(fit3)<-c("sem","adapted sem")
chidf<-fit3[,1]/fit3[,2]
fit3<-cbind(fit3,chidf)
round(fit3, 3)


#LR test
anova(fitsem1,fitsem2)












###########################
#          TASK 2         #
###########################

#libraries
library(lavaan)
library(psych)
library(GPArotation)
library(candisc)


#Setting working directory and loading the data
setwd("C:/Users/anaso/Desktop/SOFIA MENDES/KU Leuven/Year 1/Semester 1/Multivariate Statistics/Assignment 1")
load("benefits.Rdata")



################
## QUESTION A ##
################


#standardize variables
C_Ben<-benefits
C_Ben[,2:14]<-scale(C_Ben[,2:14],scale=TRUE,center=TRUE)


#conduct canonical correlation analysis
cancor.out<-cancor(cbind(SL_pensioners, SL_unemployed, SL_old_gvntresp, SL_unemp_gvntresp)~SB_strain_economy+SB_prevent_poverty+SB_equal_society+
                     SB_taxes_business+SB_make_lazy+SB_caring_others+unemployed_notmotivated+
                     SB_often_lessthanentitled+SB_often_notentitled, data= C_Ben)
cancor.out
summary(cancor.out)


#compute redundancies
R2tu<-cancor.out$cancor^2
VAFYbyt<-apply(cancor.out$structure$Y.yscores^2,2,sum)/4
redund<-R2tu*VAFYbyt
round(cbind(R2tu,VAFYbyt,redund,total=cumsum(redund)),5)






################
## QUESTION B ##
################

#validation analysis
#split data
train<-benefits[seq(2,3310,by=2),]
valid<-benefits[seq(1,3310,by=2),]


#standardize
train[,2:14]<-scale(train[,2:14],center=TRUE,scale=TRUE)
valid[,2:14]<-scale(valid[,2:14],center=TRUE,scale=TRUE)


#conduct CCA on training data
cancor.train<-cancor(cbind(SL_pensioners, SL_unemployed, SL_old_gvntresp, SL_unemp_gvntresp)~SB_strain_economy+SB_prevent_poverty+SB_equal_society+
                       SB_taxes_business+SB_make_lazy+SB_caring_others+unemployed_notmotivated+
                       SB_often_lessthanentitled+SB_often_notentitled,data=train)
summary(cancor.train)
cancor.train$structure$X.xscores
cancor.train$structure$Y.yscores



#conduct CCA on validation data
cancor.valid<-cancor(cbind(SL_pensioners, SL_unemployed, SL_old_gvntresp, SL_unemp_gvntresp)~SB_strain_economy+SB_prevent_poverty+SB_equal_society+
                       SB_taxes_business+SB_make_lazy+SB_caring_others+unemployed_notmotivated+
                       SB_often_lessthanentitled+SB_often_notentitled, data= valid)
summary(cancor.valid)
cancor.valid$structure$X.xscores
cancor.valid$structure$Y.yscores


# canonical variates calibration set
train.X1<-cancor.train$score$X
train.Y1<-cancor.train$score$Y


# compute canonical variates using data of calibration set and coefficients estimated on validation set
train.X2<-as.matrix(train[,c(6:14)])%*%cancor.valid$coef$X
train.Y2<-as.matrix(train[,c(2:5)])%*%cancor.valid$coef$Y



# comparisons to assess the validity of the solution

#R(T,T*) and R(U,U*)
round(cor(train.Y1,train.Y2),3)
round(cor(train.X1,train.X2),3)

#R(U*,T*) versus R(U,T)
round(cor(train.X1,train.Y1),3)
round(cor(train.X2,train.Y2),3)

#R(T*,T*) and R(U*,U*)
round(cor(train.Y2,train.Y2),3)
round(cor(train.X2,train.X2),3)






################
## QUESTION C ##
################


#print canonical loadings
round(cancor.out$structure$X.xscores,2)
round(cancor.out$structure$Y.yscores,2)


# plot first pair of canonical variates
plot(1,1, xlim=c(-4,4), ylim=c(-4,4), xlab="u1",ylab="t1")

points(cancor.out$scores$X[C_Ben$cntry=="BE",1],cancor.out$scores$Y[C_Ben$cntry=="BE",1],col="red")
points(cancor.out$scores$X[C_Ben$cntry=="GB",1],cancor.out$scores$Y[C_Ben$cntry=="GB",1],col="blue")
legend("topleft",c("Belgium","UK"),col=c("red","blue"),pch=c(1,1))


#plot second pair of canonical variates
plot(1,1, xlim=c(-4,4), ylim=c(-4,4), xlab="u2",ylab="t2")

points(cancor.out$scores$X[C_Ben$cntry=="BE",2],cancor.out$scores$Y[C_Ben$cntry=="BE",2],col="red")
points(cancor.out$scores$X[C_Ben$cntry=="GB",2],cancor.out$scores$Y[C_Ben$cntry=="GB",2],col="blue")
legend("topleft",c("Belgium","UK"),col=c("red","blue"),pch=c(1,1))
