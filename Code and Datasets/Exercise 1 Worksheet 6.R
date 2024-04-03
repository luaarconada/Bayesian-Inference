library(ISLR)
attach(Default)

# a) Obtain two boxplots of GRE score as a funcion of student admission. 
boxplot(UCLA$gre~UCLA$admit)

# b) Obtain two boxplots of GPA score as a funcion of student admission.
boxplot(UCLA$gpa~UCLA$admit)

# c) Obtain two boxplots of university rating as a funcion of student admission.
boxplot(UCLA$rank~UCLA$admit)

# (d) Fit a classical logistic regression model with the student admission as response variable
# and GRE socre, GPA score and university rating as predictors. Interpret results
fit = glm(admit~., data = UCLA, family = 'binomial')
summary(fit)

# The higher the rank of the university, the less probability there is to be admitted. With GPA and GRE is the contrary,
# the higher the more probability. We should leave the 3 predictors in the model.

# e) Predict the probability of admission of a candidate with GRE score equal to 790,
# GPA score equal to 3.8 and university rating equal to 1.
predict(fit, newdata = data.frame(gre = 790, gpa = 3.8, rank = 1))
# 75.53%

# f) Fit a bayesian logistic regression model with the student admission as response variable
# and GRE score, GPA score and university rating as predictors. Interpret results.
library(MCMCpack)
logit.mcmc <- MCMClogit(admit~., burnin=100, mcmc=1000, data=UCLA)
summary(logit.mcmc)
# The same interpretation as before.

# g) Check for the convergence of the mcmc chain and incorporate some thinning in the
# algorithm if necessary.
plot(logit.mcmc) # It is stationary
acf(logit.mcmc[,1]) 
acf(logit.mcmc[,2])
# We need thinning because we see the autocorrelations in the acf.
logit.mcmc2 <- MCMClogit(admit~., burnin=100, mcmc=1000, data=UCLA, thin = 25)
summary(logit.mcmc2)
plot(logit.mcmc2) # It is stationary
acf(logit.mcmc2[,1]) 
acf(logit.mcmc2[,2])

# h) Obtain the predictive density of the probability of admission of a candidate with GRE
# score equal to 790, GPA score equal to 3.8 and university rating equal to 1. Obtain
# the predictive mean, predictive median and predictive interval for this probability.
library(boot)
default.prob=inv.logit(logit.mcmc2[,1]+790*logit.mcmc2[,2]+3.8*logit.mcmc2[,3]+logit.mcmc2[,4])
hist(default.prob,freq=F)
mean(default.prob)
median(default.prob)
quantile(default.prob,c(0.025,0.975))






  