# Simulation of 10000 repetitions of the experiment of flipping a coin.
n = 10000
x =  sample(0:1,n,replace = T)
p = rep(0,n)
p = cumsum(x)/c(1:n)
plot(1:n,p,type="l")

# Obtain the relative frequency the relative frequency of receiving a
# spam message for the first n individuals of the sample, for n = 1, 2,...,5559.
sms = read.csv("sms.csv",sep = ",")
attach(sms)
names(sms)

n=length(type)
p = rep(0,n)
p = cumsum(type=="spam")/c(1:n)
plot(1:n,p,type="l")

#
# Assume a Beta(2,4) prior for the probability of receiving a spam email. Obtain the posterior given 
# 100, 1000 and 5000 observed emails.
#
par(mfrow=c(2,2))
x = seq(0,1,length.out = 1000)
a = 2; b = 4
plot(x,dbeta(x,a,b),type="l", xlab = expression(theta),ylab = expression(f(theta)),main="Prior")
plot(x,dbeta(x,a+sum(type[1:100]=="spam"),b+sum(type[1:100]=="ham")),type="l", xlab = expression(theta),ylab = expression(f (theta  / data)),main="Posterior given n=100 ")
plot(x,dbeta(x,a+sum(type[1:1000]=="spam"),b+sum(type[1:1000]=="ham")),type="l", xlab = expression(theta),ylab = expression(f (theta  / data)),main="Posterior given n=1000 ")
plot(x,dbeta(x,a+sum(type[1:5000]=="spam"),b+sum(type[1:5000]=="ham")),type="l", xlab = expression(theta),ylab = expression(f (theta  / data)),main="Posterior given n=5000 ")
par(mfrow=c(1,1))
