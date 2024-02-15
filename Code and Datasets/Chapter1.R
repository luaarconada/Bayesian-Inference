setwd("C:/Users/causin/Dropbox/Grado.DSc.Bayes/Data")
#
# Pg. 8
Secondary = read.csv("Secondary.csv",sep = ";", dec = ",")
attach(Secondary)
names(Secondary)
t.test(Grade)
#
p.hat=9/12
z.al2=qnorm(0.975)
p.hat-z.al2*sqrt(p.hat*(1-p.hat)/12)
p.hat+z.al2*sqrt(p.hat*(1-p.hat)/12)
binom.test(9,12)
#
# Pg. 10
n=length(Grade)
(mean(Grade)-5)/(sd(Grade)/sqrt(n))
qt(0.95,n-1)
t.test(Grade,mu=5,alternative = "g")
#
(p.hat-0.5)/sqrt(0.5^2/12)
qnorm(0.95)
binom.test(9,12,alternative = "g")
#
# Pg. 11
1-pnorm(5,mean(Grade),sd(Grade))
#
1-pbinom(9,12,9/12)
#
# Pg. 12
x.axis = seq(0,1,0.01)
plot(x.axis,dbeta(x.axis,1,1),type="l",ylim=c(0,3))
lines(x.axis,dbeta(x.axis,5,5),type="l")
#
plot(x.axis,dbeta(x.axis,2,4),type="l",ylim=c(0,3.5))
lines(x.axis,dbeta(x.axis,2,8),type="l")
#
plot(x.axis,dbeta(x.axis,1,1),type="l",ylim=c(0,3.5))
lines(x.axis,dbeta(x.axis,5,5),type="l",ylim=c(0,3.5),col="red")
lines(x.axis,dbeta(x.axis,1,3),type="l",ylim=c(0,3.5),col="blue")
lines(x.axis,dbeta(x.axis,2,1),type="l",ylim=c(0,3.5),col="cyan")
lines(x.axis,dbeta(x.axis,4,2),type="l",ylim=c(0,3.5),col="magenta")
lines(x.axis,dbeta(x.axis,0.4,0.2),type="l",ylim=c(0,3.5),col="green")
#
# Pg. 14
sms = read.csv("sms.csv",sep = ",")
attach(sms)
names(sms)
x.axis = seq(0,1,0.001)
plot(x.axis,dbeta(x.axis,5,5),type="l",ylim=c(0,50))
x = sum(type[1:100]=="spam")
lines(x.axis,dbeta(x.axis,5+x,5+100-x),type="l")
x = sum(type[1:1000]=="spam")
lines(x.axis,dbeta(x.axis,5+x,5+1000-x),type="l")
x = sum(type[1:5000]=="spam")
lines(x.axis,dbeta(x.axis,5+x,5+5000-x),type="l")
#
# Pg. 16
n = 100
x = sum(type[1:n]=="spam")
al = 5 + x
bet = 5 + n - x
al/(a+bet) # Mean
qbeta(0.5,al,bet) # Median
(al-1)/(al+bet-2) # Mode
#
# Pg. 17
qbeta(0.025,5+9,5+3)
qbeta(0.975,5+9,5+3)
#
n = 100
x = sum(type[1:n]=="spam")
al = 5 + x
bet = 5 + n - x
qbeta(0.025,al,bet)
qbeta(0.975,al, bet)
#
# Pg. 18
pbeta(0.5,5+9,5+3)
#
x = sum(type[1:n]=="spam")
al = 5 + x
bet = 5 + n - x
pbeta(0.1,al,bet)

