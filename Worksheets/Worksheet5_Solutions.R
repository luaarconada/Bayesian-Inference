rm(list=ls())
help(cars)
data(cars)
#
# 1.
#(a)
plot(cars)
cor(cars$speed, cars$dist)
#(b)
model <- lm(dist ~ speed, data=cars)  
summary(model)
confint(model,level=.95)
plot(cars$dist, predict(model),pch=16)
xnew <- data.frame(speed=20)
predict(model,xnew,interval="confidence")
predict(model,xnew,interval="prediction")
# (c)
library(MCMCpack)
#i
bayes.model <- MCMCregress(dist ~ speed, data=cars, thin=10,  burnin= 1000, mcmc = 10000)
summary(bayes.model)
# ii.
plot(bayes.model)
beta=bayes.model[,1:2]
sigma2=bayes.model[,3]
#iii.
apply(beta, 2,mean)
apply(beta, 2,median)
mean(sigma2)
median(sigma2)
# iv.
apply(bayes.model, 2,quantile, probs = c(0.025,0.975))
# v.
n = length(cars$speed)
X = cbind(rep(1,n),cars$speed)
plot(cars$dist,X%*%apply(beta, 2,mean))
# vi.
speed.new = 20
x.new = c(1,speed.new)
x.new%*%apply(beta,2,mean)
quantile(x.new%*%t(beta),probs=c(0.025,0.975))
# vii
y.pred=rnorm(dim(beta)[1],x.new%*%t(beta),sqrt(sigma2))
quantile(y.pred,probs=c(0.025,0.975))
# 
#
# 2. 
rm(list=ls())
help(stackloss)
data(stackloss)
attach(stackloss)
# a
pairs(stackloss)
cor(stackloss)
# b
model <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=stackloss)  
summary(model)
confint(model,level=.95)
plot(stackloss$stack.loss, predict(model),pch=16)
xnew <- data.frame(Air.Flow = 60, Water.Temp = 20, Acid.Conc.=80)
predict(model,xnew,interval="confidence")
predict(model,xnew,interval="prediction")
# (c)
library(MCMCpack)
bayes.model <- MCMCregress(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=stackloss, thin=10,  burnin= 1000, nitt = 10000)
summary(bayes.model)
plot(bayes.model)
beta = bayes.model[,1:4]
sigma2 = bayes.model[,5]
apply(beta, 2,mean)
apply(beta, 2,median)
mean(sigma2)
median(sigma2)
apply(bayes.model, 2,quantile, probs = c(0.025,0.975))
X = cbind(rep(1,length(stack.loss)), Air.Flow,Water.Temp,Acid.Conc.)
plot(stackloss$stack.loss, X%*%apply(beta, 2,mean))
Air.Flow.new = 60; Water.Temp.new = 20; Acid.Conc.new=80
x.new = c(1,Air.Flow.new,Water.Temp.new,Acid.Conc.new)
x.new%*%apply(beta, 2,mean)
quantile(x.new%*%t(beta),probs=c(0.025,0.975))
y.pred=rnorm(dim(beta)[1],x.new%*%t(beta),sqrt(sigma2))
quantile(y.pred,probs=c(0.025,0.975))
# 
#
# 3.
rm(list=ls())
data(PlantGrowth)
help(PlantGrowth)
PlantGrowth$group # group is a factor in R.
boxplot(weight ~ group, data=PlantGrowth)
summary(lm(weight ~ group, data=PlantGrowth))#
summary(aov(weight ~ group, data=PlantGrowth))#
TukeyHSD(aov(weight ~ group, data=PlantGrowth))

library(MCMCpack)
bayes.aov <- MCMCregress(weight ~ group, data=PlantGrowth)
summary(bayes.aov)
plot(bayes.aov)
beta=bayes.aov[,1:3]
sigma2=bayes.aov[,4]
plot(density(beta[,3]-beta[,2])) # difference between the mean in group tr2 and group tr1
quantile(beta[,3]-beta[,2],c(0.025,0.5,0.975))
mean(beta[,3]-beta[,2]>0) # There is a 99% probability that the mean in tr2 is larger than the mean in tr1.
#
