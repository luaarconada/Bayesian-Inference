rm(list=ls())
#
# Pg. 3
mu <- 0.5
sigma <- 1
tosses <- 12
heads <- 9
tails <- tosses -heads
fprop <- function(theta,mu,sigma,heads,tails){
  f <- dnorm(theta,mu,sigma)*(theta^heads)*((1-theta)^tails)
  return(f)
}
c <- integrate(fprop,lower=0,upper=1,mu=mu,sigma=sigma,heads=heads,tails=tails)$value
c
grid = seq(0,1,0.01)
plot(grid, fprop(grid,mu,sigma,heads,tails)/c,type="l")
#
# Pg. 4
fmean <- function(theta,mu,sigma,heads,tails,c){
  f <- theta * dnorm(theta,mu,sigma)*(theta^heads)*((1-theta)^tails)/c
  return(f)
}

post.mean <- integrate(fmean,lower=0,upper=1,mu=mu,sigma=sigma,heads=heads,tails=tails,c)$value
post.mean
#
# Pg. 6 
N <- 100000
theta <- runif(N)
w <- fprop(theta,mu,sigma,heads,tails)/1
c.est <- mean(w)
c(c,c.est)
#
mean.est = mean(w*theta)/c.est
c(mean.est,post.mean)
#
# Pg. 7
N2=N
thetapost <- sample(theta,size=N2,replace=TRUE,prob=w)
hist(thetapost,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,mu,sigma,heads,tails)/c,lwd=2,col='green')
#
# Pg. 8
N <- 100000
theta <- rbeta(N,1,10)
w <- fprop(theta,mu,sigma,heads,tails)/dbeta(theta,1,10)
c.est2 <- mean(w)
c(c,c.est2)
lines(grid,dbeta(grid,1,10),col="magenta")
#
N2=N
thetapost <- sample(theta,size=N2,replace=TRUE,prob=w)
hist(thetapost,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,mu,sigma,heads,tails)/c,lwd=2,col='green')
#
# Pg. 10
M <- max(fprop(theta=grid,mu,sigma,heads,tails)/1)
N <- 100000
theta <- runif(N)
p <- fprop(theta,mu,sigma,heads,tails)/(M*1)
u <- runif(N)
thetaac <- theta[u<p]
Nac <- length(thetaac)
hist(thetaac,probability=TRUE,xlab=expression(theta),ylab='f',main='')
lines(grid,fprop(grid,mu,sigma,heads,tails)/c,type='l',lwd=2,col='green')
Nac/N
c(post.mean,mean(thetaac))

