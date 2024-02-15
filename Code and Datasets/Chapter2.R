rm(list=ls())
#
# Pg. 5
a = 5
b = 5
n = 12
k = 9
a.ast = a + k
b.ast = b + n - k
a.ast/(a.ast+b.ast) # Posterior mean
m = 10
# Predictive distribution of the number of heads in 10 more tosses
plot(0:m,choose(10,0:m)*beta(a.ast+0:m,b.ast + m - 0:m)/beta(a.ast,b.ast),type="h") # Predictive distribution
m *a.ast/(a.ast+b.ast) # Predictive mean
m *a.ast *b.ast*(a.ast+b.ast+m)/((a.ast+b.ast)^2*(a.ast+b.ast+1))# Predictive variance
