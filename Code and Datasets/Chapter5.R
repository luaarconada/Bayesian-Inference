rm(list=ls())
#
# Pg. 5
data(trees)
help(trees)
head(trees)
attach(trees)
# 1.
pairs(trees)
pairs(log(trees))
# 2.
freq.reg <- lm(log(Volume) ~ log(Height) + log(Girth))
summary(freq.reg)
# 3. 
confint(freq.reg,level=.95)
# 4.
predict(freq.reg)
plot(log(Volume), predict(freq.reg),pch=16)
# 5.
exp(predict(freq.reg,data.frame(Height= 75,Girth = 14), interval = "confidence"))
# 6.
exp(predict(freq.reg,data.frame(Height= 75,Girth = 14), interval = "prediction"))
#
# Pg. 9
# 1.
library(MCMCpack)
burnin = 1000 
mcmc  = 10000
bayes.reg <- MCMCregress(log(Volume) ~ log(Height) + log(Girth), data=trees, thin=1,  burnin= burnin, mcmc = mcmc)
summary(bayes.reg)
# 2.
plot(bayes.reg)
# 3.
apply(bayes.reg, 2,mean)
apply(bayes.reg, 2,median)
# 4.
apply(bayes.reg, 2,quantile, probs = c(0.025,0.975))
#
# Pg. 13
# 1.
n = dim(trees)[1]
beta = bayes.reg[,1:3] 
mean.beta = apply(beta, 2,mean)
mean.beta
X = cbind(rep(1,n),log(Height),log(Girth))
mean.y = X%*%mean.beta
plot(log(Volume), mean.y,pch=16)
# 2.
Height.new = 75
Girth.new = 14
x.new = c(1,log(Height.new),log(Girth.new))
pred.mean = x.new%*%t(beta)
quantile(exp(pred.mean),probs=c(0.025,0.975))
# 3.
sigma2 = bayes.reg[,4] 
pred.y = rnorm(mcmc, pred.mean,sqrt(sigma2))
quantile(exp(pred.y),probs=c(0.025,0.975))
