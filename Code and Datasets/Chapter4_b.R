rm(list=ls())
#
# Pg. 7
mu <- 0.5
sigma <- 1
tosses <- 12
heads <- 9
tails <- tosses -heads
fprop <- function(theta,mu,sigma,heads,tails){
  f <- dnorm(theta,mu,sigma)*(theta^heads)*((1-theta)^tails)
  return(f)
}
burnin = 1000
iters = 10000
totits = burnin+iters
thetapost = rep(NA,iters)
theta = 0.5
pac = 0
for (i in 2:totits){
  thetac <- rnorm(1,theta,sd=0.1)
  if(thetac>0 & thetac<1){
    logal=log(fprop(thetac,mu,sigma,heads,tails))
    logal=logal-log(fprop(theta,mu,sigma,heads,tails))
    u=runif(1)
    if (log(u)<logal){
      theta=thetac; if (i>burnin){pac=pac+1}
    }
  }
  if (i>burnin){
    thetapost[i-burnin] <- theta
  }
}
hist(thetapost,freq=F)
mean(thetapost)
abline(v = mean(thetapost), col = 'red')
c <- integrate(fprop,lower=0,upper=1,mu=mu,sigma=sigma,heads=heads,tails=tails)$value
c
grid = seq(0,1,0.01)
lines(grid, fprop(grid,mu,sigma,heads,tails)/c,type="l")
#
# Pg. 11
plot(thetapost,type='l',ylab=expression(theta),xlab='iters')
plot(cumsum(thetapost)/c(1:iters),type='l',ylab=expression(bar(theta)),xlab='iters')
acf(thetapost,main='')

# We trim
thetapost2 = thetapost[seq(1,length(thetapost),by=10)]
N = length(thetapost2)
plot(thetapost2,type='l',ylab=expression(theta),xlab='iters')
plot(cumsum(thetapost2)/c(1:N),type='l',ylab=expression(bar(theta)),xlab='iters')
acf(thetapost2,main='')

mean(thetapost2)
median(thetapost2)
var(thetapost2)
quantile(thetapost2, probs = c(0.025,0.975))

x.pred = rbinom(N,10,thetapost2)
barplot(table(x.pred))
mean(x.pred)
var(x.pred)
#
# Pg 17
library(bayess)
data("normaldata")
names(normaldata)
attach(normaldata)
hist(x2,freq=F)
m=0; c=0.01; a=0.01; b=0.01;
n=length(x2)
xbar=mean(x2)
s2=var(x2)
burnin = 1000
iters = 10000
totits = burnin+iters
mupost = rep(NA,iters)
taupost = rep(NA,iters)
xpred = rep(NA,iters)
mu = 0
for (i in 1:totits){
  tau <- rgamma(1,0.5*(a+n),0.5*(b+(n-1)*s2+n*(mu-xbar)^2))
  mu <- rnorm(1,(c*m+n*tau*xbar)/(c+n*tau),1/sqrt(c+n*tau))
  if (i>burnin){
    mupost[i-burnin] <- mu
    taupost[i-burnin] <- tau
    xpred[i-burnin] <- rnorm(1,mu,1/sqrt(tau))
  }
}
#
# Convergence assesment
#
plot(mupost,type='l',lwd=2,col='red',ylab=expression(mu),xlab='iters')
plot(cumsum(mupost)/c(1:iters),type='l',lwd=2,col='red',ylab=expression(bar(mu)),xlab='iters')
acf(mupost,lwd=2,col='red',main='')
plot(taupost,type='l',lwd=2,col='red',ylab=expression(tau),xlab='iters')
plot(cumsum(taupost)/c(1:iters),type='l',lwd=2,col='red',ylab=expression(bar(tau)),xlab='iters')
acf(taupost,lwd=2,col='red',main='')
#
# Posterior distributions and credible intervals
hist(mupost)
hist(taupost)
quantile(mupost,c(0.025,0.975))
quantile(taupost,c(0.025,0.975))
#
# Predictive distributions and intervals
hist(xpred)
quantile(xpred,c(0.025,0.975))
#
