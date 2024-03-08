data(cars)
attach(cars)

# a) 
plot(cars$dist,cars$speed)
cor = cor(cars$dist,cars$speed)
cor

# b)
# Our model
freq.reg <- lm(dist ~ speed)
summary(freq.reg)
# Confidence intervals for model parameters 
confint(freq.reg,level=.95)
# Compare predicted with observed values
predict(freq.reg)
plot(dist, predict(freq.reg),pch=16)
# Confidence interval for  new observation with speed 20
predict(freq.reg,data.frame(speed= 20), interval = "confidence")
# Predictive interval for new observation with speed 20
predict(freq.reg,data.frame(speed= 20), interval = "prediction")


# c) 
library(MCMCpack)
burnin <- 1000 
mcmc  <- 10000
bayes.reg <- MCMCregress(dist ~ speed,data=cars, thin=1,  burnin= burnin, mcmc = mcmc)
summary(bayes.reg)
# 2.
plot(bayes.reg)
# 3.
apply(bayes.reg, 2,mean)
apply(bayes.reg, 2,median)
# 4.
apply(bayes.reg, 2,quantile, probs = c(0.025,0.975))
# 5.
n <- dim(cars)[1]
beta <- bayes.reg[,1:2] 
mean.beta <- apply(beta, 2,mean)
X <- cbind(rep(1,n),cars$speed)
mean.y <- X%*%mean.beta
plot(cars$dist, mean.y,pch=16)
# 6. 
speed.new <- 75
x.new = c(1,speed.new)
pred.mean = x.new%*%t(beta)
quantile(exp(pred.mean),probs=c(0.025,0.975))
# 7. 
sigma2 <- bayes.reg[,3] 
pred.y <- rnorm(mcmc, pred.mean,sqrt(sigma2))
quantile(pred.y,probs=c(0.025,0.975))

