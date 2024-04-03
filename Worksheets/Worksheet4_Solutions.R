#
# Exercise 1
rm(list=ls())
tosses <- 12
heads <- 9
tails <- tosses -heads
fprop <- function(theta,heads,tails){
  f <- dexp(theta)*(theta^heads)*((1-theta)^tails)
  return(f)
}
# a
c <- integrate(fprop,lower=0,upper=1,heads=heads,tails=tails)$value
c
grid = seq(0,1,0.01)
plot(grid, fprop(grid,heads,tails)/c,type="l")
#
fmean <- function(theta,heads,tails,c){
  f <- theta * dexp(theta)*(theta^heads)*((1-theta)^tails)/c
  return(f)
}

post.mean <- integrate(fmean,lower=0,upper=1,heads=heads,tails=tails,c)$value
post.mean
#
#b
N <- 100000
theta <- runif(N)
w <- fprop(theta,heads,tails)/1
c.est <- mean(w)
c(c,c.est)
#
mean.est = mean(w*theta)/c.est
c(mean.est,post.mean)
#
#
# c
N2=N
thetapost <- sample(theta,size=N2,replace=TRUE,prob=w)
hist(thetapost,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,heads,tails)/c,lwd=2,col='green')
#
#
# d
M <- max(fprop(theta=grid,heads,tails))/1
N <- 100000
theta <- runif(N)
pac <- fprop(theta,heads,tails)/(M*1)
u <- runif(N)
thetaac <- theta[u<pac]
Nac <- length(thetaac)
hist(thetaac,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,heads,tails)/c,type='l',lwd=2,col='green')
Nac/N
c(post.mean,mean(thetaac))
#
# e
#
burnin = 1000
iters = 10000
totits = burnin+iters
thetapost = rep(NA,iters)
theta = 0.5
pac = 0
for (i in 2:totits){
  thetac <- rnorm(1,theta,sd=0.5)
  if(thetac>0 & thetac<1){
    logal=log(fprop(thetac,heads,tails))
    logal=logal-log(fprop(theta,heads,tails))
    u=runif(1)
    if (log(u)<logal){
      theta=thetac; if (i>burnin){pac=pac+1}
    }
  }
  if (i>burnin){
    thetapost[i-burnin] <- theta
  }
}
pac = pac / iters
pac
plot(thetapost,type='l',ylab=expression(theta),xlab='iters')
plot(cumsum(thetapost)/c(1:iters),type='l',ylab=expression(bar(theta)),xlab='iters')
acf(thetapost,main='')

thetapost2 = thetapost[seq(1,length(thetapost),by=10)]
N = length(thetapost2)
plot(thetapost2,type='l',ylab=expression(theta),xlab='iters')
plot(cumsum(thetapost2)/c(1:N),type='l',ylab=expression(bar(theta)),xlab='iters')
acf(thetapost2,main='')

hist(thetapost2,freq=F)
lines(grid,fprop(grid,heads,tails)/c,type='l',lwd=2,col='green')
c(post.mean,mean(thetapost))
quantile(thetapost2, probs = c(0.025,0.975))
mean(thetapost2>0.8)

# Exercise 2
#
rm(list=ls())
# a
n<-10
sum <- 35
a<-0.1
b<-0.1
fprop <- function(lambda,n,sum,a,b){
  f <- dbeta(lambda,a,b)*(lambda^n)*exp(-lambda*sum)
  return(f)
}
c <- integrate(fprop,lower=0,upper=1,n=n,sum=sum,a,b)$value
c
grid = seq(0,1,0.01)
plot(grid, fprop(grid,n,sum,a,b)/c,type="l")
#
fmean <- function(lambda,n,sum,a,b, c){
  f <- lambda *dbeta(lambda,a,b)*(lambda^n)*exp(-lambda*sum)/c
  return(f)
}
post.mean <- integrate(fmean,lower=0,upper=1,n,sum,a,b,c)$value
post.mean
# 
#b
N <- 100000
lambda <- rexp(N,1)
w <- fprop(lambda,n,sum,a,b)/dexp(lambda,1)
w[lambda>1]=0
c.est <- mean(w)
c(c,c.est)
#
mean.est = mean(w*lambda)/c.est
c(mean.est,post.mean)
#
# c
N2=N
lambdapost <- sample(lambda,size=N2,replace=TRUE,prob=w)
hist(lambdapost,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid,n,sum,a,b)/c,lwd=2,col='green')
#
# d
grid = seq(0.001,0.999,0.01)
M <- max(fprop(lambda=grid,n,sum,a,b)/dgamma(grid,3,12))
N <- 100000
lambda <- rgamma(N,3,12)
pac <- fprop(lambda,n,sum,a,b)/(M*dgamma(lambda,3,12))
pac[lambda>1] <- 0
u <- runif(N)
lambdaac <- lambda[u<pac]
Nac <- length(lambdaac)
hist(lambdaac,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid,n,sum,a,b)/c,type='l',lwd=2,col='green')
Nac/N
c(post.mean,mean(lambdaac))
#
# e
#
burnin = 1000
iters = 100000
totits = burnin+iters
lambdapost = rep(NA,iters)
lambda = 0.5
pac = 0
for (i in 2:totits){
  lambdac <- rnorm(1,lambda,sd=0.5)
  if(lambdac>0 & lambdac<1){
    logal=log(fprop(lambdac,n,sum,a,b))
    logal=logal-log(fprop(lambda,n,sum,a,b))
    u=runif(1)
    if (log(u)<logal){
      lambda=lambdac; if (i>burnin){pac=pac+1}
    }
  }
  if (i>burnin){
    lambdapost[i-burnin] <- lambda
  }
}
pac = pac / iters
pac
plot(lambdapost,type='l',ylab=expression(lambda),xlab='iters')
plot(cumsum(lambdapost)/c(1:iters),type='l',ylab=expression(bar(lambda)),xlab='iters')
acf(lambdapost,main='')

lambdapost2 = lambdapost[seq(1,length(lambdapost),by=15)]
N = length(lambdapost2)
N
plot(lambdapost2,type='l',ylab=expression(lambda),xlab='iters')
plot(cumsum(lambdapost2)/c(1:N),type='l',ylab=expression(bar(lambda)),xlab='iters')
acf(lambdapost2,main='')

hist(lambdapost2,freq=F)
lines(grid,fprop(grid,n,sum,a,b)/c,type='l',lwd=2,col='green')
c(post.mean,mean(lambdapost2))
quantile(lambdapost2, probs = c(0.025,0.975))
mean(lambdapost2>0.6)
#
# Exercise 3 
#
rm(list=ls())
n = sample(10:100,1);n
lambda=0.7
x = rpois(n,lambda)
sum <- sum(x)
fprop <- function(lambda,n,sum){
  f <- (lambda^sum)*exp(-lambda*n)
  return(f)
}
c <- integrate(fprop,lower=0,upper=1,n=n,sum=sum)$value
c
grid = seq(0,1,0.01)
plot(grid, fprop(grid,n,sum)/c,type="l")
#
fmean <- function(lambda,n,sum,c){
  f <- lambda * (lambda^sum)*exp(-lambda*n)/c
  return(f)
}
post.mean <- integrate(fmean,lower=0,upper=1,n,sum,c)$value
post.mean
# 
#b
N <- 100000
lambda <- runif(N)
w <- fprop(lambda,n,sum)/1
c.est <- mean(w)
c(c,c.est)
#
mean.est = mean(w*lambda)/c.est
c(mean.est,post.mean)
#
# c
N2=N
lambdapost <- sample(lambda,size=N2,replace=TRUE,prob=w)
hist(lambdapost,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid,n,sum)/c,lwd=2,col='green')
#
# d
M <- max(fprop(lambda=grid,n,sum))
N <- 100000
lambda <- runif(N)
pac <- fprop(lambda,n,sum)/(M*1)
u <- runif(N)
lambdaac <- lambda[u<pac]
Nac <- length(lambdaac)
hist(lambdaac,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid,n,sum)/c,type='l',lwd=2,col='green')
Nac/N
c(post.mean,mean(lambdaac))
#
# e
#
burnin = 1000
iters = 100000
totits = burnin+iters
lambdapost = rep(NA,iters)
lambda = 0.5
pac = 0
for (i in 2:totits){
  lambdac <- rnorm(1,lambda,sd=0.5)
  if(lambdac>0 & lambdac<1){
    logal=log(fprop(lambdac,n,sum))
    logal=logal-log(fprop(lambda,n,sum))
    u=runif(1)
    if (log(u)<logal){
      lambda=lambdac; if (i>burnin){pac=pac+1}
    }
  }
  if (i>burnin){
    lambdapost[i-burnin] <- lambda
  }
}
pac = pac / iters
pac
plot(lambdapost,type='l',ylab=expression(lambda),xlab='iters')
plot(cumsum(lambdapost)/c(1:iters),type='l',ylab=expression(bar(lambda)),xlab='iters')
acf(lambdapost,main='')

lambdapost2 = lambdapost[seq(1,length(lambdapost),by=10)]
N = length(lambdapost2)
N
plot(lambdapost2,type='l',ylab=expression(lambda),xlab='iters')
plot(cumsum(lambdapost2)/c(1:N),type='l',ylab=expression(bar(lambda)),xlab='iters')
acf(lambdapost2,main='')

hist(lambdapost2,freq=F)
lines(grid,fprop(grid,n,sum)/c,type='l',lwd=2,col='green')
c(post.mean,mean(lambdapost2))
quantile(lambdapost2, probs = c(0.025,0.975))
mean(lambdapost2>0.6)
#
# Exercise 4
#
rm(list=ls())
n = 5
lambda = 5
x = rpois(n,lambda)
sum <- sum(x)
# 
#a
fprop <- function(lambda,n,sum){
  f <- dlnorm(lambda, 0,1)* (lambda^sum)*exp(-lambda*n)
  return(f)
}
c <- integrate(fprop,lower=0,upper=10,n=n,sum=sum)$value
c
grid = seq(0,10,0.01)
plot(grid, fprop(grid,n,sum)/c,type="l")
#
fmean <- function(lambda,n,sum,c){
  f <- lambda * dlnorm(lambda, 0,1)*(lambda^sum)*exp(-lambda*n)/c
  return(f)
}
post.mean <- integrate(fmean,lower=0,upper=10,n,sum,c)$value
post.mean
# 
#b
N <- 100000
lambda <- rweibull(N,1,5)
w <- fprop(lambda,n,sum)/dweibull(lambda,1,5)
c.est <- mean(w)
c(c,c.est)
#
mean.est = mean(w*lambda)/c.est
c(mean.est,post.mean)
#
# c
N2=N
lambdapost <- sample(lambda,size=N2,replace=TRUE,prob=w)
hist(lambdapost,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid,n,sum)/c,lwd=2,col='green')
#
#
# d
M <- max(fprop(lambda=grid,n,sum)/dweibull(grid,1,5))
N <- 100000
lambda <- rweibull(N,1,5)
pac <- fprop(lambda,n,sum)/(M*dweibull(lambda,1,5))
u <- runif(N)
lambdaac <- lambda[u<pac]
Nac <- length(lambdaac)
hist(lambdaac,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid,n,sum)/c,type='l',lwd=2,col='green')
Nac/N
c(post.mean,mean(lambdaac))
#
# Exercise 5
#
rm(list=ls())
tosses <- 12
heads <- 9
tails <- tosses -heads
fprop <- function(theta,heads,tails){
  f <- 2*(theta^heads)*((1-theta)^tails)
  return(f)
}
c <- integrate(fprop,lower=0.5,upper=1,heads=heads,tails=tails)$value
c
grid = seq(0.5,0.99,0.01)
plot(grid, fprop(grid,heads,tails)/c,type="l")

# a
M <- max(fprop(theta=grid,heads,tails)/2)
N <- 100000
theta <- runif(N,0.5,1)
pac <- fprop(theta,heads,tails)/(M*2)
u <- runif(N)
thetaac <- theta[u<pac]
Nac <- length(thetaac)
hist(thetaac,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,heads,tails)/c,type='l',lwd=2,col='green')
Nac/N
#
# b
M <- max(fprop(theta=grid,heads,tails)/dbeta(grid,9,3))
N <- 100000
theta <- rbeta(N,9,3)
pac <- fprop(theta,heads,tails)/(M*dbeta(theta,9,3))
pac[theta<0.5] <- 0
u <- runif(N)
thetaac <- theta[u<pac]
Nac <- length(thetaac)
hist(thetaac,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,heads,tails)/c,type='l',lwd=2,col='green')
Nac/N

#
# Exercise 7
#
rm(list=ls())
x <- c(254, 249, 252, 252, 249, 249, 250, 247, 251, 252)
hist(x,freq=F)
m=250; c=0.01; a=0.01; b=0.01;
n=length(x)
xbar=mean(x)
s2=var(x)
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
# a 
plot(taupost,mupost)
# b
quantile(taupost,c(0.025,0.975))
quantile(mupost,c(0.025,0.975))
# c
plot(mupost,type='l',lwd=2,col='red',ylab=expression(mu),xlab='iters')
plot(cumsum(mupost)/c(1:iters),type='l',lwd=2,col='red',ylab=expression(bar(mu)),xlab='iters')
acf(mupost,lwd=2,col='red',main='')
plot(taupost,type='l',lwd=2,col='red',ylab=expression(tau),xlab='iters')
plot(cumsum(taupost)/c(1:iters),type='l',lwd=2,col='red',ylab=expression(bar(tau)),xlab='iters')
acf(taupost,lwd=2,col='red',main='')
# d 
hist(xpred)
mean(xpred>251)

#
# Exercise 8
#
rm(list=ls())
data = c(0.7, 1.3, 1.6)
n1 = 3;n2 = 2
n = n1 + n2
a = 0.01
b = 0.01
burnin = 1000
iters = 10000
totits = burnin+iters
lampost = rep(NA,iters)
lam = 1
for (i in 1:totits){
  data2 <- rexp(n2, lam )
  lam <- rgamma(1, a+n, b + sum(c(data,data2)))
  if (i>burnin){
    lampost[i-burnin] <- lam
    }
}

plot(lampost,type='l',lwd=2,col='red',ylab=expression(mu),xlab='iters')
plot(cumsum(lampost)/c(1:iters),type='l',lwd=2,col='red',ylab=expression(bar(mu)),xlab='iters')
acf(lampost,lwd=2,col='red',main='')

hist(lampost)
mean(lampost)
median(lampost)
quantile(lampost,c(0.025,0.975))
