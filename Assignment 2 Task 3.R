#################################
############ TASK 3 #############     
#################################

rm(list=ls())

# loading the data and setting up the working directory #
setwd("C:/Users/anaso/Desktop/SOFIA MENDES/KU Leuven/Year 1/Semester 1/Multivariate Statistics/Assignment 2")    #!!CHANGE THIS!!
library(smacof)
data(rectangles)
data(rect_constr)
zrect_constr<-scale(rect_constr,center=TRUE,scale=TRUE)
print(rect_constr) 


#### QUESTION a) ####

# conducting MDS with two dimensions using different levels #

#ratio 
m1<-smacofSym(delta=rectangles, ndim=2, type="ratio", init="torgerson")
#interval
m2<-smacofSym(delta=rectangles, ndim=2, type="interval", init="torgerson")
#ordinal
m3<-smacofSym(delta=rectangles, ndim=2, type="ordinal", init="torgerson")
#mspline
m4<-smacofSym(delta=rectangles, ndim=2 ,type="mspline",spline.degree =4 , spline.intKnots = 4, init="torgerson")


# evaluating the goodness of fit #

#stress-1 values
round(c(m1$stress,m2$stress,m3$stress,m4$stress),3)

#from the stress-1 values obtained, it can be said that ordinal has the least 
#stress-1 value followed by mspline.

par(mfrow=c(2,2)) 
plot(m3,plot.type="resplot",main="residual plot ordinal MDS")
plot(m3,plot.type="Shepard",main="Shepard diagram ordinal MDS")
plot(m4,plot.type="resplot",main="residual plot spline MDS")
plot(m4,plot.type="Shepard",main="Shepard diagram spline MDS")

#the correlation between the normalized dissimilarities and configurational distances
cor(c(m3$dhat),c(m3$confdist))
#configuration ordinal MDS
par(mfrow=c(1,1))
plot(m3,plot.type="conf",main="configuration ordinal MDS",pch='',label.conf = list(label = TRUE, pos = 3, col = 1, cex = 1))



#### QUESTION b) ####

#ordinal MDS
#stress norm
set.seed(1)
rstress<-randomstress(n=16,ndim=2,nrep=500,type="ordinal")
#distribution of stress for random data
mean(rstress)-2*sd(rstress)
#the value is much higher than stress-1 value of 0.089, which means that the
#two-dimensional ordinal MDS solution has a satisfactory fit.

#permutation test
set.seed(1)
perm.car<-permtest(m3,nrep=500)

#plot distribution stress
par(mfrow=c(1,2),pty="s")
hist(rstress,main="stress random data")
hist(perm.car$stressvec,main="stress permuted data")
#the observed stress-1 value for ordinal MDS solution on rectangle is much less 
#than the distributed stress-1 value for random data or permuted data.
#Hence, we can conclude that a 2 dimensional ordinal MDS solution has a 
#satisfactory fit.

#stability of solution using jackknife
par(mfrow=c(1,1))
jack.car<-jackmds(m3)
plot(jack.car,xlim=c(-1.2,1.2),ylim=c(-1,1))

#from the jackknife plot we can see that the MDS solution is very stable. The 
#stability measure reported is 0.998, which also indicates the stability.



#### QUESTION c) ####

# MDS Biplot #
biRect <- biplotmds(m3, zrect_constr)   
coef(biRect)


# project external variables in the MDS solution
plot(biRect, main = "Biplot Vector Representation", vecscale = 0.8, 
     xlim = c(-1.5, 1.5), vec.conf = list(col = "brown"), pch = 20, cex = 0.5)

#print R-square of regressions
round(biRect$R2vec,3)

#print regression coefficients
round(biRect$coefficients,3)

#correlation of rect_constr and dimensions
round(cor(m3$conf,zrect_constr),3)


#regressing width on D1 and D2
summary(lm(zrect_constr[,1]~m3$conf))
#regressing height on D1 and D2
summary(lm(zrect_constr[,2]~m3$conf)) 
