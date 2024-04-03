# Exercise 2 Worksheet 5
data(stackloss)
attach(stackloss)
str(stackloss)
head(stackloss)

# a) Obtain a scatterplot to visualize the data. Obtain the correlation matrix.
# Interpret results.
pairs(stackloss)
cor_matrix = cor(stackloss)
cor_matrix


# b) Fit a classical linear regression model with the stack loss as the response variable.
model = lm(stack.loss~., data = stackloss)
summary(model)

# i. Obtain confidence intervals for the model parameters.
confint(model, level = 0.95)

# ii. Compare predicted with observed values of stack loss.
predictions = predict(model)
plot(stack.loss, predictions, pch = 16)

# iii. Obtain a 95% confidence interval for the average stack loss with: Air.Flow = 60,
# Water.Temp = 20 and Acid.Conc=80.
predict(model, data.frame(Water.Temp = 20, Acid.Conc. = 80, Air.Flow = 60), interval = 'confidence')

# iv. Obtain a 95% predictive interval for the average stack loos with: Air.Flow = 60,
# Water.Temp = 20 and Acid.Conc=80.
predict(model, data.frame(Water.Temp = 20, Acid.Conc. = 80, Air.Flow = 60), interval = 'prediction')


# c) Fit a bayesian linear regression model with the stack loss as the response variable.
library(MCMCpack)

# i. Using the R package MCMCpack, implement a MCMC algorithm to obtain samples
# from the posterior distribution.
burnin = 1000
mcmc = 10000
bayes_model = MCMCregress(stack.loss~., data = stackloss, burnin = burnin, mcmc = mcmc)
summary(bayes_model)

# ii. Plot the traces of the sampled posterior. Evaluate the MCMC performance.
plot(bayes_model)

# iii. Obtain the posterior means and median of the model parameters.
mean = apply(bayes_model, 2, mean)
mean
median = apply(bayes_model, 2, median)
median

# iv. Obtain 95% Bayesian credible intervals for the model parameters.
apply(bayes_model, 2, quantile, probs = c(0.025, 0.975))

# v. Compare predicted with observed values.
n = dim(stackloss)[1]
beta = bayes_model[,1:4]
mean_beta = apply(beta, 2,mean) # mean vector but without the sigma parameter, only the estimated betas
X = cbind(rep(1,n), Air.Flow, Water.Temp, Acid.Conc.)
mean_y = X%*%mean_beta # (X*betas = y predicted)
plot(stack.loss, mean_y,pch=16)

# vi. Obtain a 95% confidence interval for the average stack loss with: Air.Flow = 60,
# Water.Temp = 20 and Acid.Conc=80.
Air.Flow.new = 60
Water.Temp.new = 20
Acid.Conc.new = 80
x_new = c(1, Air.Flow.new, Water.Temp.new, Acid.Conc.new)
pred_mean = x_new%*%t(beta) # estimated stack.loss (E(stack.loss|60, 20, 80))
quantile(pred_mean,probs=c(0.025,0.975))

# vii. Obtain a 95% predictive interval for the average stack loss with: Air.Flow = 60,
# Water.Temp = 20 and Acid.Conc=80.
sigma2 = bayes_model[,5] # Estimated sigma
pred_y = rnorm(mcmc, pred_mean, sqrt(sigma2)) # Predict a new value of stack.loss from our mean 
# sorting a N(E(stack.loss|60, 20, 80), sigma^2)
quantile(pred_y,probs=c(0.025,0.975))
