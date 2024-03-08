# Exercise 6, worksheet 3

# Wet a seed for reproducibility of the results.
set.seed(1313)

# Parameters.
n=300 #(sample size)
m=2
c=2
a=2
b=2

# The exercise tells that the distributions of the data and the conjugate prior.
# Hence, we first generate a random sample of our data that follows:
# X~N(mu,1/phi), using that:
# mu|phi~N(mu,1/(c*phi)) and phi~Gamma(a/2,b/2)

# We first we generate a random conjugate priors phi and mu|phi.
phi = rgamma(1, shape = a/2, rate = b/2)
mu = rnorm(1, mean = m, sd = 1/(c*phi))

# Now that we have mu and gamma, we can sample our data.
sample_prior = rnorm(n, mean = mu, sd = 1/phi)


# (a) Write an R code to generate a sample from the joint posterior distribution.

# We know that the posterior follows:
# f(mu, tau| data) = f(mu|tau, data)*f(tau| data) and 
# mu|tau, data~N(m2, 1/(c2*tau)) and tau|data~Gamma(a2/2, b2/2)
# Hence, we first compute m2, c2, a2 and b2
m2 = (c*m + n*mean(sample_prior)) / (c + n)
c2 = c + n
a2 = a + n
b2 = b + (n-1)*var(sample_prior) + c*n*(m - mean(sample_prior))^2 / (c + n)

# We sample the posterior tau
tau_posterior = rgamma(1, shape = a2/2, rate = b2/2)

# We sample the joint posterior mu
mu_posterior = rnorm(n, mean = m2, sd = 1/(c2*tau_posterior))


# (b) Compare the simulated marginal sample of mu with the theoretical scaled, shifted
# Student-t distribution that is known for mu.
x_seq = seq(-10, 10, 0.1)

# We first compute the probability density function of the shifted t-Student
density_mu = dt((x_seq - m2)/sqrt(b2/(a2*c2)), df = a2)*1 / sqrt(b2/(a2*c2))

# Now, to compare with the simulated marginal sample of mu, we plot both together
# Plot of the simulated sample in a probability histogram 
hist(mu_posterior, prob = TRUE, breaks = 20, col = 'skyblue')

# Plot of the probability density function of mu
lines(x_seq, density_mu, type = 'l', col = 'purple', lwd = 2.5)


# (c) Considering that X|mu, phi~N(mu,1/phi) use the Monte Carlo approximation to
# obtain a sample from the predictive distribution X|data.

# We first need to compute a final parameter
c3 = c2 / (c2 + 1)

# We create a vector with different sample sizes to do Monte Carlo
sample_sizes = c(20, 50, 150, 1000)
l = length(sample_sizes)

# Create a 2x2 layout for our plots
par(mfrow = c(2, 2))

# We do Monte Carlo
for (i in 1:l) {
  # We generate the sample
  sampled <- rnorm(sample_sizes[i], mean = m2, sd = 1/(c3*tau_posterior))
  
  # We plot a histogram of our sample
  hist(sampled, main = paste(sample_sizes[i], "samples"), breaks = 20, col = "lightblue")  
  
  # We remove the x-axis labels
  axis(1, at = seq(min(sampled), max(sampled), length.out = 5), labels = FALSE)
}