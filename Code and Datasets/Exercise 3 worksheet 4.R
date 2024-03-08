# Exercise 3 worksheet 4
# We set parameters
n = sample(10:100,1)
n
lambda=0.7

# We sample a Poisson
x = rpois(n,lambda)
x

# The prior distribution for lambda is f(lambda) = 1 (uniform)

# a) Use numerical integration to approximate the normalizing constant and the posterior mean.

# We define a function for the posterior  
fprop <- function(lambda){
  f <- dunif(lambda)*exp(-n*lambda)*lambda^sum(x)/prod(factorial(x))
  return(f)
}

# Normalizing constant approximation
c = integrate(fprop,lower=0,upper=1)$value
c
#And plot of the posterior
grid = seq(0,1,0.01)
plot(grid, fprop(grid)/c,type="l")

# Posterior mean
fmean <- function(lambda,c){
  f <- lambda * dunif(lambda)*exp(-n*lambda)*lambda^sum(x)/(prod(factorial(x))*c)
  return(f)
}

post.mean <- integrate(fmean,lower=0,upper=1, c)$value
post.mean

# Add it to the plot
abline(v = post.mean, col='red')


# b) Considering a uniform U(0, 1) as the importance function, use importance sampling
# to approximate the normalizing constant and the posterior mean. Compare the estimated
# mean with that obtained by numerical integration. Interpret results.

# Our g(lambda) = uniform

# Normalizing constant
N <- 100000
lambda <- runif(N)
w <- fprop(lambda)/1 # 1 = dunif(lambda)
c.est <- mean(w)
c(c,c.est)

# Posterior mean 
mean.est = mean(w*lambda)/c.est
c(mean.est,post.mean)


# c) Using the weights from (b), implement a sampling importance resampling algorithm
# to obtain a sample from the posterior distribution. Obtain a histogram of the posterior
# sample and overlap the posterior density obtained with numerical integration.
# Interpret results.

N2=N
lambdapost <- sample(lambda, size=N2, replace=TRUE, prob=w)
hist(lambdapost, probability=TRUE, xlab=expression(lambda), ylab='f', main='')
lines(grid, fprop(grid)/c, lwd=2, col='green')


# d) Use a uniform distribution, U(0, 1), as the candidate density to obtain a sample from
# the posterior distribution by rejection sampling. Interpret results.

M <- max(fprop(lambda = grid)/1) # 1 = dunif(lambda)
N <- 100000
lambda <- runif(N)
u <- runif(N)
p <- fprop(lambda)/(M*1) # 1 = dunif(lambda)
lambdaac <- lambda[u<p]

# i. Obtain a histogram of the posterior sample and overlap the posterior density
# obtained with numerical integration.

hist(lambdaac,probability=TRUE,xlab=expression(lambda),ylab='f',main='')
lines(grid,fprop(grid)/c,type='l',lwd=2,col='green')

# ii. Obtain the proportion of acceptance values.
Nac <- length(lambdaac)
Nac/N

# iii. Compare the estimated mean with that obtained by numerical integration.
c(post.mean,mean(lambdaac))


# e) Implement a random walk Metropolis Hasting algorithm to obtain a sample from the
# posterior distribution.

burnin = 1000
iters = 10000
totits = burnin+iters
lambdapost = rep(NA,iters)
lambda = 0.7
pac = 0
for (i in 2:totits){
  lambdac <- runif(1)
  if(lambdac>0 & lambdac<1){
    logal=log(fprop(lambdac))
    logal=logal-log(fprop(lambda))
    u=runif(1)
    if (log(u)<logal){
      lambda=lambdac; if (i>burnin){pac=pac+1}
    }
  }
  if (i>burnin){
    lambdapost[i-burnin] <- lambda
  }
}

# i. Check for the convergence of the algorithm.

plot(lambdapost,type='l',ylab=expression(lambda),xlab='iters')
plot(cumsum(lambdapost)/c(1:iters),type='l',lwd=2,col='red',ylab=expression(bar(lambda)),xlab='iters')
acf(lambdapost,lwd=2,col='red',main='')

# ii. Obtain a histogram of the posterior sample and overlap the posterior density
# obtained with numerical integration.

hist(lambdapost,freq=F)
grid = seq(0,1,0.01)
lines(grid, fprop(grid)/c,type="l", col = 'red', lwd = 2)

# iii. Obtain the estimated posterior mean and compare with that obtained with numerical
# integration.

mean(lambdapost)
abline(v = mean(lambdapost), col = 'purple', lwd = 2)
c(mean(lambdapost), post.mean)

# iv. Obtain a 95% credible interval for lambda.

quantile(lambdapost,c(0.025,0.975))

# v. Obtain the posterior probability that lambda is larger than 0.6.

sum(lambdapost > 0.6)/length(lambdapost)
