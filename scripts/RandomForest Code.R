
################################################################################
## Random Forests: fit medv~lstat using random forests.
## Plot: oob error esitmation.
## Plot: fit from random forests for three different number of trees in forest.
################################################################################


library(randomForest)
library(MASS)
data(Boston)
attach(Boston)

#--------------------------------------------------
#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!
set.seed(99)
n = nrow(Boston)
ntreev = c(10,500,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
  cat('doing Boston rf: ',i,'\n')
  rffit = randomForest(medv~lstat,data=Boston,ntree=ntreev[i],maxnodes=15)
  fmat[,i] = predict(rffit)
}
#--------------------------------------------------
#plot oob error using last fitted rffit which has the largest ntree.


par(mfrow=c(1,1))
plot(rffit)

#--------------------------------------------------
#plot fits

par(mfrow=c(1,3))
oo = order(Boston$lstat)
for(i in 1:nset) {
  plot(Boston$lstat,Boston$medv,xlab='lstat',ylab='medv')
  lines(Boston$lstat[oo],fmat[oo,i],col=i,lwd=3)
  title(main=paste('bagging ntrees = ',ntreev[i]))
}

#--------------------------------------------------
rm(list=ls())

################################################################################
## Fit medv~lstat, Boston Housing using boosting.
## Plot: fits for three different values of number of trees.
################################################################################


install.packages("gbm")
library(gbm) #boost package
library(MASS)
data(Boston)
attach(Boston)
#--------------------------------------------------
#fit boosting for various number of trees
set.seed(99)
n = nrow(Boston)
ntreev = c(5,20,100)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
  cat('doing Boston boost: ',i,'\n')
  boostfit = gbm(medv~lstat,data=Boston,distribution='gaussian',
                 interaction.depth=2,n.trees=ntreev[i],shrinkage=.2)
  fmat[,i] = predict(boostfit,n.trees=ntreev[i])
}
#--------------------------------------------------
#plot fits

par(mfrow=c(1,3))
oo = order(Boston$lstat)
for(i in 1:nset) {
  plot(Boston$lstat,Boston$medv,xlab='lstat',ylab='medv')
  lines(Boston$lstat[oo],fmat[oo,i],col=i+1,lwd=3,lty=1)
  title(main=paste('boosting, ntree= ',ntreev[i]))
}

#--------------------------------------------------
rm(list=ls())


################################################################################
## Variable Importance:
## Fit boosting and random forests and plot variable importance.
################################################################################

library(gbm) #boost package
library(randomForest) 
library(MASS)
data(Boston)
attach(Boston)

#--------------------------------------------------
#fit boost and plot  variable importance
boostfit = gbm(medv~.,data=Boston,distribution='gaussian',
               interaction.depth=2,n.trees=100,shrinkage=.2)

par(mfrow=c(1,1))
p=ncol(Boston)-1
vsum=summary(boostfit,plotit=FALSE) #this will have the variable importance info
row.names(vsum)=NULL #drop varable names from rows.

#plot variable importance
#the package does this automatically, but I did not like the plot

plot(vsum$rel.inf,axes=F,pch=16,col='red')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')

#--------------------------------------------------
#fit random forest and plot variable importance

rffit = randomForest(medv~.,data=Boston,mtry=3,ntree=500)

varImpPlot(rffit)


rm(list=ls())