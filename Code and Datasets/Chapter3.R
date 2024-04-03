rm(list=ls())
#
# Pg. 6
library(bayess)
help(normaldata)
data(normaldata)
attach(normaldata)
hist(x2,freq=F)
n=length(x2)
mean(x2) # MLE for the mean
(n-1)*var(x2)/n # MLE for the variance
t.test(x2)# CI for the mean
mean(x2)-qt(0.975, n-1)*sqrt(var(x2)/n) # lower bound CI for mu
mean(x2)+qt(0.975, n-1)*sqrt(var(x2)/n) # upper bound CI for mu
(n-1)*var(x2)/qchisq(0.975,n-1) # lower bound CI for the variance  
(n-1)*var(x2)/qchisq(0.025,n-1) # upper bound CI for the variance
#
# Pg 12
m=0;c=1;a=1;b=1;
m.ast = (c*m+n*mean(x2))/(c+n) # parameter of the posterior but also its mean
c.ast = c+n
a.ast = a+n 
b.ast = b+(n-1)*var(x2)+c*n*(m-mean(x2))^2/(c+n)
m.ast-qt(0.975,a.ast)*sqrt(b.ast/(a.ast*c.ast)) # lower bound CI for mu
m.ast+qt(0.975,a.ast)*sqrt(b.ast/(a.ast*c.ast)) # upper bound CI for mu
1/qgamma(0.975,a.ast/2,b.ast/2) # lower bound CI for sigma^2
1/qgamma(0.025,a.ast/2,b.ast/2) # lower bound CI for sigma^2
#
# Pg 15
hist(x2,freq= F)
pred.mean=m.ast
scale=sqrt((c.ast+1)*b.ast/(c.ast*a.ast))
grid= seq(-1,1,0.01)
lines(grid,dt((grid-pred.mean)/scale,a.ast)/scale)
#
# Pg 19
mean(x2)# Posterior mean of mu
mean(x2)-qt(0.975,n-1)*sqrt(var(x2)/n)# lower bound CI for mu
mean(x2)+qt(0.975,n-1)*sqrt(var(x2)/n)# upper bound CI for mu
(n-1)*var(x2)/(n-3)# Posterior mean of sigma^2
1/qgamma(0.975,(n-1)/2,(n-1)*var(x2)/2)# lower bound CI for sigma^2
1/qgamma(0.025,(n-1)/2,(n-1)*var(x2)/2)# lower bound CI for sigma^2
#
# Pg 22
hist(x2,freq= F)
lines(grid,dt((grid-mean(x2))/sqrt(var(x2)*(1+1/n)),n-1)/sqrt(var(x2)*(1+1/n)))