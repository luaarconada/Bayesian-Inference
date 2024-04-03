# Univariate example
x <- iris$Sepal.Length
groups <- iris$Species

# By default, the ks::hpi bandwidths are computed
kda_1 <- ks::kda(x = x, x.group = groups)

# Manual specification of bandwidths via ks::hkda (we have univariate data)
hs <- ks::hkda(x = x, x.group = groups, bw = "plugin")
kda_1 <- ks::kda(x = x, x.group = groups, hs = hs)

# Estimated class probabilities
kda_1$prior.prob

# Classification
head(kda_1$x.group.estimate)

# (Training) classification error
ks::compare(x.group = kda_1$x.group, est.group = kda_1$x.group.estimate)

# Classification of new observations
ind_1 <- c(5, 55, 105)
newx <- x[ind_1]
predict(kda_1, x = newx)
groups[ind_1] # Reality

# Classification regions (points on the bottom)
plot(kda_1, xlab = "Sepal length", drawpoints = TRUE, col = rainbow(3))
legend("topright", legend = c("Setosa", "Versicolor", "Virginica"),
       lwd = 2, col = rainbow(3))